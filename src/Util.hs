module Util where

import qualified Graphics.Rendering.OpenGL.GL as GL
import Data.StateVar (($=))
import Control.Lens

import Types

bindProgram :: Program -> IO ()
bindProgram prog = do
  GL.currentProgram                    $= prog ^. program
  GL.bindBuffer GL.ShaderStorageBuffer $= prog ^. bufferStorage
  GL.bindBuffer GL.ArrayBuffer         $= prog ^. bufferArray
  GL.bindBuffer GL.ElementArrayBuffer  $= prog ^. bufferElement
