module Main where

import qualified Data.Vector.Storable      as V (fromList, length) 
import qualified Data.ByteString           as BS (readFile) 
import qualified Data.StateVar             as SV (get)
import qualified Graphics.UI.GLFW          as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Data.Vector.Storable (unsafeWith)
import Graphics.Rendering.OpenGL (($=))
import Control.Monad.State.Lazy
import Control.Concurrent.MVar
import Codec.Picture.Png
import Foreign.Storable
import Foreign.C.Types
import Control.Lens
import Foreign.Ptr

import Graphics.UI.BitMapFont
import Graphics.UI.Lib
import Graphics.UI.BDF
import Shaders
import Lib

data Program = Program {
    _program :: Maybe GL.Program,
    _bufferArray :: Maybe GL.BufferObject,
    _bufferElement :: Maybe GL.BufferObject,
    _bufferStorage :: Maybe GL.BufferObject
  }
makeLenses ''Program

data InputEvent = 
    KeyEvent GLFW.Key
  | MouseEvent Double Double

data GameState = GameState 
  {
    _windowHandle :: GLFW.Window,
    _hamGuiState  :: HamGuiData,
    _programMain  :: Program,
    _programHG    :: Program,
    _bitmapfont   :: BitMapFont
  }
makeLenses ''GameState

type Game a = StateT GameState IO a

initGraphics :: MVar [InputEvent] ->  IO (GLFW.Window, GL.Program, BitMapFont)
initGraphics kq = do
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
  im <- case img of
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
  a <- SV.get (GL.textureInternalFormat GL.Texture2D 0)
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  return (win, prog, font)

processGameState :: MonadState GameState m => [InputEvent] -> m ()
processGameState kq =
  forM_ kq (\k ->
      case k of 
        -- INPUT BE HERE
        _ -> return ()
    )

bindProgram :: Program -> IO ()
bindProgram prog = do
  GL.currentProgram                    $= prog ^. program
  GL.bindBuffer GL.ShaderStorageBuffer $= prog ^. bufferStorage
  GL.bindBuffer GL.ArrayBuffer         $= prog ^. bufferArray
  GL.bindBuffer GL.ElementArrayBuffer  $= prog ^. bufferElement

renderState :: Game ()
renderState = do
  win        <- use windowHandle
  progMain   <- use programMain
  progHam    <- use programHG
  hgs        <- use hamGuiState
  bmf        <- use bitmapfont
  (cx, cy)   <- liftIO $ do
    (cx, cy) <- GLFW.getCursorPos win
    pure (cx, cy)
  lmb <- liftIO $ GLFW.getMouseButton win GLFW.MouseButton'1
  (_ ,hgsn) <- liftIO $ 
    runStateT (do
                 clearBuffers 
                 setScreenSize (1024, 1024)
                 updateMouseState (round cx, 1024 - round cy) (lmb == GLFW.MouseButtonState'Pressed, False)
                 a <- button bmf (ObjectId "button 1") "pepega 1"
                 _ <- button bmf (ObjectId "button 2") "pepega 2"
                 _ <- button bmf (ObjectId "button 3") "pepega 3"
                 when a $ liftIO $ print "Clicked"
              ) hgs
  hamGuiState .= hgsn
  let numOfVertici  = (length $ view vertexDataL hgsn) -- TODO: do this thing without code dupe, all in IO thing
  let numOfElements = (length $ view elemDataL hgsn)
  liftIO $ do 
    GL.clearColor $= GL.Color4 0 0 0 1
    GL.clear [GL.ColorBuffer]
    bindProgram progHam
    _ <- runStateT (composeBuffers 
      (\p -> GL.bufferData GL.ArrayBuffer $= (fromIntegral $ sizeOf (1::CFloat) * numOfVertici, p, GL.StaticDraw))
      (\p -> GL.bufferData GL.ElementArrayBuffer $= (fromIntegral $ sizeOf (1::CInt) * numOfElements, p, GL.StaticDraw))) hgsn
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
    GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
    GL.vertexAttribArray (GL.AttribLocation 2) $= GL.Enabled
    GL.drawElements GL.Triangles (fromIntegral numOfElements) GL.UnsignedInt nullPtr
    e <- SV.get GL.errors
    forM_ e $ print
    pure ()

  liftIO $ GLFW.swapBuffers win

processUserInputs :: Game ()
processUserInputs = do
  liftIO GLFW.pollEvents
  return ()

shouldExit :: Game Bool
shouldExit = return False

keyCallback :: MVar [InputEvent] -> GLFW.KeyCallback
keyCallback kq win key k kstate kmods =
  when (kstate == GLFW.KeyState'Pressed) $ modifyMVar_ kq $ return.(:) (KeyEvent key)

cursorCallback :: MVar [InputEvent] -> GLFW.CursorPosCallback
cursorCallback kq win x y =
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
  processGameState kqu
  processUserInputs
  renderState
  ifNotFinished <- unless <$> liftM2 (||) (liftIO $ GLFW.windowShouldClose win) shouldExit
  ifNotFinished $ runGame kq

initInState :: Game ()
initInState = do pure()

main :: IO ()
main = do
  keyQueue                      <- newMVar []
  (win, progMain, bmf)          <- initGraphics keyQueue
  (progHam, bufHamA, bufHamE)   <- initHamGui
  let state = GameState {
                  _windowHandle = win,
                  _hamGuiState  = initHamGuiData,
                  _programMain  = Program (Just progMain) (Nothing)      (Nothing)      (Nothing),
                  _programHG    = Program (Just progHam)  (Just bufHamA) (Just bufHamE) (Nothing),
                  _bitmapfont   = bmf
                }
  runStateT (initInState >> (runGame keyQueue)) state
  terminateGraphics win
