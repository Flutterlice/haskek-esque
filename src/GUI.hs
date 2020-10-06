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

import Graphics.UI.HamGui.BitMapFont
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

runGUI :: GLFW.Window -> HamGui () -- TODO: Remove bmf somewhere somehow
runGUI win = do
  newFrame
  setScreenSize (1024, 1024)
  (mx, my) <- liftIO $ GLFW.getCursorPos win
  lmb <- fmap (==GLFW.MouseButtonState'Pressed) $ liftIO $ GLFW.getMouseButton win GLFW.MouseButton'1
  uploadMouseState (round mx, 1024 - round my) (lmb, False)
  a <- button (ObjectId "button 1") "pepega 1"
  _ <- button (ObjectId "button 2") "pepega 2"
  _ <- button (ObjectId "button 3") "pepega 3"
  text <- textInput (ObjectId "i")
  when a $ liftIO $ putStrLn "Clicked"

renderGUI :: Game ()
renderGUI = do
  hgs        <- use hamGuiState
  progHam    <- use programHG
  let numOfVertici  = (length $ view vertexDataL hgs) -- TODO: do this thing without code dupe, all in IO thing
  let numOfElements = (length $ view elemDataL hgs)
  liftIO $ do
    bindProgram progHam
    _ <- runStateT (composeBuffers
      (\p -> GL.bufferData GL.ArrayBuffer $= (fromIntegral $ sizeOf (1::CFloat) * numOfVertici, p, GL.StaticDraw))
      (\p -> GL.bufferData GL.ElementArrayBuffer $= (fromIntegral $ sizeOf (1::CInt) * numOfElements, p, GL.StaticDraw))) hgs
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
    GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
    GL.vertexAttribArray (GL.AttribLocation 2) $= GL.Enabled
    GL.drawElements GL.Triangles (fromIntegral numOfElements) GL.UnsignedInt nullPtr
