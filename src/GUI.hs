module GUI where

import qualified Graphics.UI.GLFW             as GLFW
import qualified Graphics.Rendering.OpenGL.GL as GL
import Control.Monad.State.Strict
import Data.StateVar(($=))
import Foreign.Storable
import Foreign.C.Types
import Control.Lens
import Foreign.Ptr
import Data.Word

import Graphics.UI.HamGui.HamGui
import Graphics.UI.HamGui.Types
import Shaders
import Types
import Util

initHamGui :: IO (GL.Program, GL.BufferObject, GL.BufferObject)
initHamGui = do
  prog <- GL.createProgram
  forM_ [(GL.FragmentShader, fragShaderHG),
         (GL.VertexShader,   vertShaderHG)] $ processShader prog
  GL.linkProgram prog
  log <- GL.programInfoLog prog
  unless (null log) $ putStrLn log
  GL.currentProgram $= Just prog
  bufArray <- GL.genObjectName
  bufElementArray <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just bufArray
  GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float (7*4) nullPtr)
  GL.vertexAttribPointer (GL.AttribLocation 1) $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (7*4) (plusPtr (nullPtr::(Ptr Word8)) 8))
  GL.vertexAttribPointer (GL.AttribLocation 2) $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float (7*4) (plusPtr (nullPtr::(Ptr Word8)) 20))
  return (prog, bufArray, bufElementArray)

runGUI :: GLFW.Window -> HamGuiU Int ()
runGUI win = do
  newFrame
  setScreenSize (SPP 1024 1024)
  (mx, my) <- liftIO $ GLFW.getCursorPos win
  lmb <- fmap (==GLFW.MouseButtonState'Pressed) $ liftIO $ GLFW.getMouseButton win GLFW.MouseButton'1
  uploadMouseState (SPP (round mx) (1024 - round my)) (lmb, False)
  _ <- textLabel (ObjectId "label") "HamGui Test"
  pressed <- button (ObjectId $ "add one more") "add one more"
  when pressed $ userData += 1
  a <- use userData
  forM_ [1..a] $ (\x -> void $ button (ObjectId $ "button " ++ show x) "pepega 1" )
  _ <- textInput (ObjectId "i")
  _ <- checkbox (ObjectId "c")
  pure ()

renderGUI :: Game ()
renderGUI = do
  hgs        <- use hamGuiState
  progHam    <- use programHG
  numOfVertici <- use (hamGuiState . vI)
  numOfElements <- use (hamGuiState . eI)
  liftIO $ do
    bindProgram progHam
    _ <- runStateT (composeBuffers
      (\p -> GL.bufferData GL.ArrayBuffer $= (fromIntegral $ sizeOf (1::CFloat) * fromIntegral numOfVertici, p, GL.StaticDraw))
      (\p -> GL.bufferData GL.ElementArrayBuffer $= (fromIntegral $ sizeOf (1::CInt) * fromIntegral numOfElements, p, GL.StaticDraw))) hgs
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
    GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
    GL.vertexAttribArray (GL.AttribLocation 2) $= GL.Enabled
    GL.drawElements GL.Triangles (fromIntegral numOfElements) GL.UnsignedInt nullPtr
