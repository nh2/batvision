{-# LANGUAGE NamedFieldPuns, LambdaCase #-}

module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
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
import           Safe
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
                  withFirstCamera (processFrame source)

    ["point"] -> withSource $ \source -> do
                   toneRef <- newIORef 80
                   forkIO $ soundPitchServer source toneRef
                   withFirstCamera (processFramePoint source toneRef)

    _ -> abort "batvision [full|point]"


bsToVector :: BS.ByteString -> Vector Word16
bsToVector bs = unsafeFromForeignPtr0 (castForeignPtr fp0) len16Bits
  where
    len16Bits = len `quot` 2
    (fp, off, len)  = toForeignPtr bs
    fp0 | off /= 0  = updPtr (`advancePtr` off) fp
        | otherwise = fp



processFramePoint :: Source -> IORef Float -> OniFrame -> IO ()
processFramePoint source toneRef frame@OniFrame{ frameWidth = w
                                               , frameHeight = h
                                               , frameData } = do

  let vec = bsToVector frameData

      midX = w `quot` 2
      midY = h `quot` 2

      mid = vec ! (w * midY + midX)

  print mid

  writeIORef toneRef (fixupPoint $ fromIntegral mid)


fixupPoint :: Float -> Float
fixupPoint x
  | x <= 0.0  = 80
  | otherwise = max 80 (4600 - x)


fixup :: Float -> Float
fixup x
  | x <= 0.0  = 80
  | otherwise = max 80 (2400 - x)


processFrame :: Source -> OniFrame -> IO ()
processFrame source frame@OniFrame{ frameWidth = w
                                  , frameHeight = h
                                  , frameData
                                  , frameStride = stride } = do

  let vec = bsToVector frameData

  -- Print stats
  print (w, h, stride, BS.length frameData)

  putStrLn "Distance grid:"
  printGrid vec w h 8 -- Print every 8th value

  -- TOOD figure out why the last column is zero

  -- Calculate "closest" row
  let closest = closestRows vec w
      distances = VS.toList $ VS.reverse closest -- reverse x for cam view direction

  putStrLn "Closest line:"
  printGrid closest w 1 8 -- Print every 8th value of the row
  putStrLn ""

  let playTime = 2.0 -- seconds
  let frequencies = map (fixup . fromIntegral) distances

  playSounds source frequencies playTime



-- | Prints the grid represented by vec (w width, h height), and print
-- every s'th data point (starting with the first one).
-- Prints in order such that left is "left from the camera view direction".
printGrid :: Vector Word16 -> Int -> Int -> Int -> IO ()
printGrid vec w h s = do
  forM_ [0..((h-1) `quot` s)] $ \y -> do
    forM_ (reverse [0..((w-1) `quot` s)]) $ \x -> do -- reverse x for cam view direction
      printf "%4d " $ vec ! (w*(y * s) + (x * s))
    putStrLn ""


-- | Fold all rows of v with function f. Use acc as the initial row.
foldRows :: (VS.Storable a, VS.Storable b) => (a -> b -> a) -> Vector a -> Vector b -> Vector a
foldRows f acc v = foldl' (\res r -> VS.zipWith f res (VS.slice (r*cols) cols v)) acc [0..rows-1]
  where
    cols = case VS.length acc of
             n | n < 1 -> error $ "foldRows: acc vector has invalid size " ++ show cols
             n         -> n
    rows = case VS.length v `quotRem` cols of
             (q, 0) -> q
             _      -> error "foldRows: vector to fold over is not rectangular"


-- | Given a grid of distances, for each column calculates the closest one
-- (avoiding 0s if possible).
closestRows :: (VS.Storable a, Num a, Ord a) => Vector a -> Int -> Vector a
closestRows v nCols = foldRows minNon0 (VS.replicate nCols 0) v
  where
    minNon0 0 x = x
    minNon0 x 0 = x
    minNon0 a b = min a b


-- | Runs the give per-frame action on the first available OpenNI camera.
withFirstCamera :: (OniFrame -> IO ()) -> IO ()
withFirstCamera f = do
  initialize oniApiVersion
  devs   <- right "getDeviceList"      <$> getDeviceList
  device <- right "deviceOpenInfo"     <$> deviceOpenInfo (first devs)
  stream <- right "deviceCreateStream" <$> deviceCreateStream device SensorDepth
  streamStart stream
  fix $ \loop ->
    streamReadFrame stream >>= \case
      Right frame -> f frame >> loop
      Left err    -> print err >> shutdown
  where
    right msg (Left err) = abort $ "ERROR " ++ msg ++ ": " ++ show err
    right _   (Right x)  = x

    first devs = headDef (abort "no camera devices") devs


-- | Loops sound, updating the frequency from `toneRef`.
soundPitchServer :: Source -> IORef Float -> IO ()
soundPitchServer source toneRef = do

  let duration = 1.0 / 100

  -- Looping is the best way against clicks; http://benbritten.com/2010/05/04/streaming-in-openal
  loopingMode source $= Looping

  buf <- createBuffer $ Sine 1760 0 duration
  queueBuffers source [buf]
  play [source]

  forever $ do
    freq <- readIORef toneRef
    pitch source $= realToFrac (freq / 2400)
    sleep duration


-- | `playSounds source numInQueue freqs overDuration`:
-- Play `freqs` equally distributed over `overDuration` seconds.
playSounds :: Source -> [Float] -> Float -> IO ()
playSounds source freqs overDuration = do

  -- Remove finished buffers
  stop [source]
  unqueueBuffers source =<< get (buffersProcessed source)

  -- Queue new sounds
  forM_ freqs $ \freq -> do
    let duration = overDuration / fromIntegral (length freqs)
    -- print (freq, duration)
    when (freq == 0.0) $ error "playSounds: Bad frequency of 0.0"
    buf <- createBuffer $ Sine freq 0 duration
    queueBuffers source [buf]

  play [source]

  -- Pan from left to right while the sounds are played.
  let steps = [-1,-0.97..1]
  let n = fromIntegral $ length steps
  forM_ steps $ \pos -> do
    sourcePosition source $= Vertex3 pos 0 0
    sleep (overDuration / n)


-- | Do something with a sound source on the first available OpenAL audio device.
withSource :: (Source -> IO a) -> IO a
withSource f =
  withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ -> do
    device  <- just "no sound device"  <$> openDevice Nothing
    context <- just "no audio context" <$> createContext device []

    currentContext $= Just context
    [source] <- genObjectNames 1

    x <- finally (f source) (closeDevice device)

    return x
  where
    just _   (Just x) = x
    just msg Nothing  = abort $ "ERROR: " ++ msg
