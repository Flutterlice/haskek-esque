module HaskekEsque where

import qualified Data.ByteString           as BS (readFile)
import qualified Graphics.UI.GLFW          as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Vector.Storable      as V
import qualified Data.Vector.Storable.Mutable      as MV
import Data.Vector.Storable (unsafeWith)
import Graphics.Rendering.OpenGL (($=))
import qualified Data.StateVar as SV
import Control.Monad.State.Lazy
import Control.Concurrent.MVar
import Codec.Picture.Png
import Control.Lens
import System.Environment (getArgs)

import Graphics.UI.HamGui.BitMapFont
import Graphics.UI.HamGui.HamGui
import Graphics.UI.HamGui.Types
import Graphics.UI.HamGui.BDF
import Shaders
import Types
import Util
import GUI

import Criterion.Main

initGraphics :: MVar [InputEvent] ->  IO (GLFW.Window, GL.Program, BitMapFont)
initGraphics kq = do
  GLFW.setErrorCallback $ Just (\e s -> putStrLn $ unwords [show e, show s])
  glfwInitStatus <- GLFW.init
  unless glfwInitStatus $ error "Failed to initialize GLFW"
  GLFW.windowHint $ GLFW.WindowHint'Resizable True
  windowCreationStatus <- GLFW.createWindow 1024 1024 "IO Pepega" Nothing Nothing
  win <- case windowCreationStatus of
    Just win -> do
      GLFW.makeContextCurrent windowCreationStatus
      return win
    Nothing -> error "Window creation failed"
  GLFW.swapInterval 1
  prog <- GL.createProgram
  forM_ [(GL.GeometryShader, geomShader),
         (GL.FragmentShader, fragShader),
         (GL.VertexShader,   vertShader)] $ processShader prog
  GL.linkProgram prog
  log <- GL.programInfoLog prog
  unless (null log) $ putStrLn log
  GL.currentProgram $= Just prog
  GLFW.setKeyCallback win $ Just $ keyCallback kq
  GLFW.setCharCallback win $ Just $ charCallback kq
  GLFW.setCursorPosCallback win $ Just $ cursorCallback kq
  GLFW.setFramebufferSizeCallback win $ Just $ resizeCallback kq
  imageBS <- BS.readFile "assets/sprite.png"
  let img = decodePng imageBS
  _im <- case img of
       Left err -> putStrLn err >> undefined
       Right i -> return i
  imId <- GL.genObjectName
  GL.textureBinding  GL.Texture2D      $= Just imId
  GL.textureFilter   GL.Texture2D      $= ((GL.Nearest, Nothing), GL.Nearest)
  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Mirrored, GL.ClampToEdge)
  GL.textureWrapMode GL.Texture2D GL.T $= (GL.Mirrored, GL.ClampToEdge)
  font <- loadBDF "assets/Scientifica.bdf"
  let dat = _rgbaData font
  let side = round $ sqrt $ fromIntegral $ V.length dat
  unsafeWith dat (\ptr -> GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.R8 (GL.TextureSize2D side side) 0 (GL.PixelData GL.Red GL.UnsignedByte ptr))
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  return (win, prog, font)

processGameState :: [InputEvent] -> Game ()
processGameState kq =
  forM_ kq (\_ ->
      pure ()
    )

renderPre :: Game ()
renderPre = do
  liftIO $ do
    GL.clearColor $= GL.Color4 0 0 0 1
    GL.clear [GL.ColorBuffer]
    pure ()

renderPost :: Game ()
renderPost = do
  win <- use windowHandle
  liftIO $ do
    e <- SV.get GL.errors
    forM_ e $ print
    GLFW.swapBuffers win
    pure ()
  pure ()

renderState :: Game ()
renderState = do
  progMain   <- use programMain
  liftIO $ do
    bindProgram progMain
    pure ()

processUserInputs :: Game ()
processUserInputs = do
  liftIO GLFW.pollEvents
  return ()

shouldExit :: Game Bool
shouldExit = return False

keyCallback :: MVar [InputEvent] -> GLFW.KeyCallback
keyCallback kq _win key _k kstate _kmods = do
  when (kstate == GLFW.KeyState'Pressed) $ modifyMVar_ kq $ return.(:) (KeyEvent key)

charCallback :: MVar [InputEvent] -> GLFW.CharCallback
charCallback kq _win char = do
  modifyMVar_ kq $ return.(:) (CharEvent char)
  pure ()

cursorCallback :: MVar [InputEvent] -> GLFW.CursorPosCallback
cursorCallback kq _win x y =
  modifyMVar_ kq $ return.(:) (MouseEvent x y)

resizeCallback :: MVar [InputEvent] -> GLFW.FramebufferSizeCallback
resizeCallback kq _win x y = do
  modifyMVar_ kq $ return.(:) (ResizeEvent x y)
  GL.viewport $= ((GL.Position 0 0), (GL.Size (fromIntegral x) (fromIntegral y)))

terminateGraphics :: GLFW.Window -> IO ()
terminateGraphics win = do
  GLFW.destroyWindow win
  GLFW.terminate

processEvents :: Game ()
processEvents = do
  e <- getEventsOf (_UIEvent2 . _EA)
  forM_ e (\_ -> liftIO $ print 777)
  uiEvents <- consumeEventsOf _UIEvent
  forM_ uiEvents (\(UIEvent a) -> liftIO $ print a)

runGame :: MVar [InputEvent] -> Game ()
runGame kq = do
  win <- use windowHandle
  kqu <- liftIO $ modifyMVar kq $ pure . (,) []
  processGameState kqu
  processUserInputs
  when (has (traverse . _ResizeEvent) kqu) $ do
    let Just (sx, sy) = (firstOf (traverse . _ResizeEvent) kqu) 
    liftGUI $ screenSize .= (SS sx sy)
  runGUI
  renderPre
  renderState
  renderGUI
  renderPost
  processEvents
  clearEvents
  ifNotFinished <- unless <$> liftM2 (||) (liftIO $ GLFW.windowShouldClose win) shouldExit
  ifNotFinished $ runGame kq

benchmarkingRunGame :: Game ()
benchmarkingRunGame = do
  runGUI
  renderPre
  renderState
  renderGUI
  renderPost

initInState :: Game ()
initInState = do pure()

runHaskekEsque :: IO ()
runHaskekEsque = do
  isDebug <- (==["bench"]) <$> getArgs
  keyQueue                      <- newMVar []
  (win, progMain, loadedBMF)    <- initGraphics keyQueue
  (progHam, bufHamA, bufHamE)   <- initHamGui
  vMV <- MV.new 64
  eMV <- MV.new 64 
  (screen_x, screen_y) <- GLFW.getWindowSize win
  let screen_size = SS screen_x screen_y
  let state = GameState {
                  _windowHandle = win,
                  _hamGuiState  = initHamGuiData vMV eMV & bitMapFont .~ loadedBMF & screenSize .~ screen_size,
                  _programMain  = Program (Just progMain) (Nothing)      (Nothing)      (Nothing),
                  _programHG    = Program (Just progHam)  (Just bufHamA) (Just bufHamE) (Nothing),
                  _eventBus     = EventBus mempty
                }
  if isDebug then do
    defaultMain [ bgroup "bench" [ bench "1"  $ whnfIO (runStateT (initInState >> (benchmarkingRunGame)) state) ] ]
  else
    void $ runStateT (initInState >> (runGame keyQueue)) state
  terminateGraphics win