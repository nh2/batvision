{-# LANGUAGE NamedFieldPuns, LambdaCase #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString as BS
import           Data.ByteString.Internal (toForeignPtr)
import           Data.Function (fix)
import           Data.IORef
import           Data.List (foldl')
import           Data.List.Split (chunksOf)
import           Data.Vector.Storable (Vector, (!), unsafeFromForeignPtr0)
import qualified Data.Vector.Storable as VS
import           Data.Vector.Storable.Internal (updPtr)
import           Data.Word
import           Foreign.ForeignPtr (castForeignPtr)
import           Foreign.Marshal.Array (advancePtr)
import           Sound.ALUT
import           System.Environment (getArgs)
import           Text.Printf (printf)


import Honi
import Honi.Types


main :: IO ()
main = do
  args <- getArgs

  case args of
    ["full"] -> withSource $ \source -> do
                  numInQueue <- newIORef 0
                  withFirstCamera (processFrame source numInQueue)
    ["point"] -> withSource $ \source -> do
                   numInQueue <- newIORef 0
                   toneRef <- newIORef 80
                   -- forkIO $ soundServer source toneRef
                   forkIO $ soundPitch source toneRef
                   withFirstCamera (processFramePoint source toneRef numInQueue)
    _ -> error "bad args"


bsToVector :: BS.ByteString -> Vector Word16
bsToVector bs = unsafeFromForeignPtr0 (castForeignPtr fp0) len16Bits
  where
    len16Bits = len `quot` 2
    (fp, off, len)  = toForeignPtr bs
    fp0 | off /= 0  = updPtr (`advancePtr` off) fp
        | otherwise = fp



processFramePoint :: Source -> IORef Float -> IORef Int -> OniFrame -> IO ()
processFramePoint source toneRef numInQueue frame@OniFrame{ frameWidth = w, frameHeight = h, frameData, frameStride = stride } = do

  let vec = bsToVector frameData

      midX = w `quot` 2
      midY = h `quot` 2

      mid = vec ! (w * midY + midX)

  print mid

  writeIORef toneRef (fixupPoint $ fromIntegral mid)
  -- playSound source numInQueue (fixupPoint $ fromIntegral mid) (1/60)


fixupPoint :: Float -> Float
fixupPoint x
  | x <= 0.0  = 80
  | otherwise = max 80 (4600 - x)


fixup :: Float -> Float
fixup x
  | x <= 0.0  = 80
  | otherwise = max 80 (2400 - x)


processFrame :: Source -> IORef Int -> OniFrame -> IO ()
processFrame source numInQueue frame@OniFrame{ frameWidth = w, frameHeight = h, frameData, frameStride = stride } = do

  print ("frameSensorType", frameSensorType frame)
  print ("frameTimestamp", frameTimestamp frame)
  print ("frameIndex", frameIndex frame)
  print ("frameVideoMode", frameVideoMode frame)
  print ("frameCroppingEnabled", frameCroppingEnabled frame)
  print ("frameCropOriginX", frameCropOriginX frame)
  print ("frameCropOriginY", frameCropOriginY frame)

  let vec = bsToVector frameData

  -- TOOD figure out why the last column is zero

  putStrLn ""
  print (w, h, stride, BS.length frameData)
  print (VS.length vec)

  -- Print every 8th value
  forM_ [0..((h-1) `quot` 8)] $ \y -> do
    forM_ (reverse [0..((w-1) `quot` 8)]) $ \x -> do
      printf "%4d " $ vec ! (w*(y * 8) + (x * 8))
    putStrLn ""

  -- Print every value
  -- forM_ [0..h-1] $ \y -> do
  --   -- print y
  --   forM_ (reverse [0..w-1]) $ \x -> do
  --     -- printf "%3d " $ frameData `BS.index` (w*y + x)
  --     printf "%4d " $ vec ! (w*y + x)
  --   putStrLn ""


  putStrLn ""
  let sums = closestRows vec w
  sparseSums <- forM (reverse [0..((w-1) `quot` 8)]) $ \x -> do
    printf "%4d " $ sums ! (x * 8)
    return $ sums ! (x * 8)
  putStrLn ""

  let delay = 2.0

  let distances = VS.toList $ VS.reverse sums

  let playSums = map head . chunksOf 1 $ distances
  let toPlay = map (fixup . fromIntegral) playSums

  -- announcer
  -- playSounds' source numInQueue [2000,80] $ 0.1

  -- distances
  -- print toPlay
  playSounds' source numInQueue toPlay $ delay

  -- threadDelay (delay * 1000000)


foldRows :: (VS.Storable a, VS.Storable b) => (a -> b -> a) -> Vector a -> Vector b -> Vector a
foldRows f acc v = foldl' (\res r -> VS.zipWith f res (VS.slice (r*cols) cols v)) acc [0..rows-1]
  where
    cols = case VS.length acc of
             n | n < 1 -> error $ "foldRows: acc vector has invalid size " ++ show cols
             n         -> n
    rows = case VS.length v `quotRem` cols of
             (q, 0) -> q
             _      -> error "foldRows: vector to fold over is not rectangular"


closestRows :: (VS.Storable a, Num a, Ord a) => Vector a -> Int -> Vector a
closestRows v nCols = foldRows minNon0 (VS.replicate nCols 0) v
  where
    minNon0 0 x = x
    minNon0 x 0 = x
    minNon0 a b = min a b


withFirstCamera :: (OniFrame -> IO ()) -> IO ()
withFirstCamera f = do
  initialize oniApiVersion
  Right (di:_) <- getDeviceList
  Right device <- deviceOpenInfo di
  Right stream <- deviceCreateStream device SensorDepth
  streamStart stream
  fix $ \loop ->
    streamReadFrame stream >>= \case
      Right frame -> f frame >> loop
      Left err    -> print err >> shutdown


playSound :: Source -> IORef Int -> Float -> Float -> IO ()
playSound source numInQueue freq duration = do
  when (freq == 0.0) $ error "playSounds: Bad frequency of 0.0"

  q <- get $ buffersQueued source
  p <- get $ buffersProcessed source
  print ("queued", q, "processed", p)


  buf <- createBuffer $ Sine freq 0 duration
  queueBuffers source [buf]

  q <- get $ buffersQueued source
  when (q < 5) $ do
    queueBuffers source [buf]

  q <- get $ buffersQueued source
  p <- get $ buffersProcessed source
  print ("queued", q, "processed", p)

  -- when (p > 5) $
  --   void $ unqueueBuffers source (5)
  void $ unqueueBuffers source p


  x <- readIORef numInQueue
  when (x == 0) $ do
    play [source]
    writeIORef numInQueue 1
  --   sleep (duration * 0.5)


  when (x /= 0) $ do
    sleep (duration )
    -- threadDelay (floor $ duration * 1000000.0)
    -- n <- get $ buffersProcessed source
    -- print n
    -- when (n > 5) $
    --   void $ unqueueBuffers source (n-1)

  q <- get $ buffersQueued source
  p <- get $ buffersProcessed source
  print ("queued", q, "processed", p)

  -- sleep (duration )
  print =<< (get $ sourceState source)

  -- void $ unqueueBuffers source 1

  -- stop [source]
  -- x <- readIORef numInQueue
  -- unqueueBuffers source (fromIntegral x)
  -- writeIORef numInQueue 1
  -- void $ unqueueBuffers source 1


soundPitch :: Source -> IORef Float -> IO ()
soundPitch source toneRef = do

  let duration = (1.0 / 100)

  loopingMode source $= Looping
  buf <- createBuffer $ Sine 1760 0 duration
  queueBuffers source [buf]
  play [source]

  forever $ do
    freq <- readIORef toneRef
    pitch source $= realToFrac (freq / 2400)
    sleep duration




soundServer :: Source -> IORef Float -> IO ()
soundServer source toneRef = forever $ do
  freq <- readIORef toneRef

  print ("freq", freq)

  when (freq == 0.0) $ error "playSounds: Bad frequency of 0.0"

  q <- get $ buffersQueued source
  p <- get $ buffersProcessed source
  print ("queued", q, "processed", p)

  let duration = 0.1

  buf <- createBuffer $ Sine freq 0 (duration)
  queueBuffers source [buf]

  q <- get $ buffersQueued source
  when (q < 8) $ do
    queueBuffers source [buf]
    queueBuffers source [buf]
    queueBuffers source [buf]
    queueBuffers source [buf]
    queueBuffers source [buf]

  q <- get $ buffersQueued source
  p <- get $ buffersProcessed source
  print ("queued", q, "processed", p)

  -- when (p > 5) $
  --   void $ unqueueBuffers source (5)
  void $ unqueueBuffers source p


  state <- get $ sourceState source
  when (state == Initial) $ do
    putStrLn "\n\n\n\nSTARTING\n\n\n\n"
    play [source]
  --   sleep (duration * 0.5)


  -- when (x /= 0) $ do
  sleep (duration)
  -- threadDelay (floor $ duration * 1000000.0)
    -- threadDelay (floor $ duration * 1000000.0)
    -- n <- get $ buffersProcessed source
    -- print n
    -- when (n > 5) $
    --   void $ unqueueBuffers source (n-1)

  q <- get $ buffersQueued source
  p <- get $ buffersProcessed source
  print ("queued", q, "processed", p)

  -- sleep (duration )
  print =<< (get $ sourceState source)

  -- void $ unqueueBuffers source 1

  -- stop [source]
  -- x <- readIORef numInQueue
  -- unqueueBuffers source (fromIntegral x)
  -- writeIORef numInQueue 1
  -- void $ unqueueBuffers source 1






playSounds' :: Source -> IORef Int -> [Float] -> Float -> IO ()
playSounds' source numInQueue freqs overDuration = do
  forM_ freqs $ \freq -> do
    let duration = overDuration / fromIntegral (length freqs)
    -- print (freq, duration)
    when (freq == 0.0) $ error "playSounds: Bad frequency of 0.0"
    buf <- createBuffer $ Sine freq 0 duration
    queueBuffers source [buf]

  stop [source]
  x <- readIORef numInQueue
  unqueueBuffers source (fromIntegral x)
  writeIORef numInQueue (length freqs)

  play [source]

  -- sleep (overDuration + 0.2)

  let steps = [-1,-0.97..1]
  let n = fromIntegral $ length steps
  forM_ steps $ \pos -> do
    sourcePosition source $= Vertex3 pos 0 0
    sleep (overDuration / n)


withSource :: (Source -> IO a) -> IO a
withSource f =
  withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ -> do
    Just device <- openDevice Nothing
    Just context <- createContext device []
    currentContext $= Just context

    [source] <- genObjectNames 1

    x <- f source

    closeDevice device
    return x
