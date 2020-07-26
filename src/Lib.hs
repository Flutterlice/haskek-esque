module Lib where

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Data.StateVar as SV
import Data.StateVar hiding (get)
import Control.Monad
import Foreign.Ptr
import Data.Word

import Shaders

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