module GUI where

import qualified Graphics.UI.GLFW             as GLFW
import qualified Graphics.Rendering.OpenGL.GL as GL
import Control.Monad.State.Lazy
import Data.StateVar(($=))
import Foreign.Storable
import Foreign.C.Types
import Data.Typeable (Typeable)
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

data TypeSlider = TInt | TFloat deriving (Typeable, Show, Eq, Ord)

instance Slidable TypeSlider where
  slideBetween lower_bound higher_bound cursor _ _ = if ratio > 0.5 then TFloat else TInt
    where size  = higher_bound - lower_bound
          v     = cursor - lower_bound
          ratio = (fromIntegral v) / (fromIntegral size)
  fractionBetween _ _ val = case val of
    TInt -> 0.0
    TFloat -> 1.0

runGUI :: Game ()
runGUI = do
  liftGUI $ do 
    newFrame
    forM_ [0..9] $ (\_ -> addText "Pepega :mega: FOOR SAAN" (RA $ SRect (SP 0 400) (SS (100) (100))))

renderGUI :: Game ()
renderGUI = do
  hgs           <- use hamGuiState
  progHam       <- use programHG
  numOfVertici  <- use (hamGuiState . vI)
  numOfElements <- use (hamGuiState . eI)
  liftIO $ do
    bindProgram progHam
    runStateT (composeBuffers
      (\p -> GL.bufferData GL.ArrayBuffer        $= (fromIntegral $ sizeOf (1::CFloat) * fromIntegral numOfVertici, p, GL.StaticDraw))
      (\p -> GL.bufferData GL.ElementArrayBuffer $= (fromIntegral $ sizeOf (1::CInt) * fromIntegral numOfElements, p, GL.StaticDraw))) hgs
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
    GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
    GL.vertexAttribArray (GL.AttribLocation 2) $= GL.Enabled
    GL.drawElements GL.Triangles (fromIntegral numOfElements) GL.UnsignedInt nullPtr