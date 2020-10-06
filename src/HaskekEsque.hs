module HaskekEsque where

import qualified Data.ByteString           as BS (readFile)
import qualified Graphics.UI.GLFW          as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Vector.Storable      as V
import Data.Vector.Storable (unsafeWith)
import Graphics.Rendering.OpenGL (($=))
import qualified Data.StateVar as SV
import Control.Monad.State.Lazy
import Control.Concurrent.MVar
import Codec.Picture.Png
import Control.Lens
import Data.Char

import Graphics.UI.HamGui.BitMapFont
import Graphics.UI.HamGui.Types
import Graphics.UI.HamGui.HamGui
import Graphics.UI.HamGui.BDF
import Shaders
import Types
import Util
import GUI

initGraphics :: MVar [InputEvent] ->  IO (GLFW.Window, GL.Program, BitMapFont)
initGraphics kq = do -- TODO: prettify this function, looks a bit ugly
  GLFW.setErrorCallback $ Just (\e s -> putStrLn $ unwords [show e, show s])
  glfwInitStatus <- GLFW.init
  unless glfwInitStatus $ error "Failed to initialize GLFW"
  GLFW.windowHint $ GLFW.WindowHint'Resizable False
  windowCreationStatus <- GLFW.createWindow 1024 1024 "IO Pepega" Nothing Nothing
  win <- case windowCreationStatus of
    Just win -> do
      GLFW.makeContextCurrent windowCreationStatus
      return win
    Nothing -> error "Window creation failed"
  GLFW.swapInterval 0
  prog <- GL.createProgram
  forM_ [(GL.GeometryShader, geomShader),
         (GL.FragmentShader, fragShader),
         (GL.VertexShader,   vertShader)] $ processShader prog
  GL.linkProgram prog
  log <- GL.programInfoLog prog
  unless (null log) $ putStrLn log
  GL.currentProgram $= Just prog
  GLFW.setKeyCallback win $ Just $ keyCallback kq
  GLFW.setCursorPosCallback win $ Just $ cursorCallback kq
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
  forM_ kq (\k ->
      case k of
        KeyEvent k -> do
          sca <- liftIO $ GLFW.getKeyScancode GLFW.Key'A
          scz <- liftIO $ GLFW.getKeyScancode GLFW.Key'Z
          sc <- liftIO $ GLFW.getKeyScancode k
          when ((sca <= sc) && (sc <= scz)) $ do
            hamGuiState . inputs . alphaNumPressed .= (Just $ [chr $ (sc - sca) + (ord 'a')])
        _ -> return ()
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
keyCallback kq _win key _k kstate _kmods =
  when (kstate == GLFW.KeyState'Pressed) $ modifyMVar_ kq $ return.(:) (KeyEvent key)

cursorCallback :: MVar [InputEvent] -> GLFW.CursorPosCallback
cursorCallback kq _win x y =
  modifyMVar_ kq $ return.(:) (MouseEvent x y)

terminateGraphics :: GLFW.Window -> IO ()
terminateGraphics win = do
  GLFW.destroyWindow win
  GLFW.terminate

runGame :: MVar [InputEvent] -> Game ()
runGame kq = do
  win <- use windowHandle
  kqu <- liftIO $ takeMVar kq
  liftIO $ putMVar kq []
  processGameState kqu -- TODO: something is not right
  processUserInputs
  hgs <- use hamGuiState
  -- TOOD: Inject inputs into HamGuiState
  (_ ,hgsn) <- liftIO $ runStateT (runGUI win) hgs
  hamGuiState .= hgsn -- TODO: make it look nicer
  renderPre
  renderState
  renderGUI
  renderPost
  ifNotFinished <- unless <$> liftM2 (||) (liftIO $ GLFW.windowShouldClose win) shouldExit
  ifNotFinished $ runGame kq

initInState :: Game ()
initInState = do pure()

runHaskekEsque :: IO ()
runHaskekEsque = do
  keyQueue                      <- newMVar []
  (win, progMain, bmf)          <- initGraphics keyQueue
  (progHam, bufHamA, bufHamE)   <- initHamGui
  let state = GameState {
                  _windowHandle = win,
                  _hamGuiState  = initHamGuiData & bitMapFont .~ bmf,
                  _programMain  = Program (Just progMain) (Nothing)      (Nothing)      (Nothing),
                  _programHG    = Program (Just progHam)  (Just bufHamA) (Just bufHamE) (Nothing)
                }
  runStateT (initInState >> (runGame keyQueue)) state
  terminateGraphics win