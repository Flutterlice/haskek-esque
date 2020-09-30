module Shaders where

import qualified Graphics.Rendering.OpenGL.GL as GL
import Data.StateVar (($=))
import Text.RawString.QQ
import Control.Monad


type ShaderSource = String

processShader :: GL.Program -> (GL.ShaderType, String) -> IO ()
processShader prog (typ, source) = do
    shader <- GL.createShader   typ
    _      <- GL.shaderSourceBS shader $= GL.packUtf8 source
    _      <- GL.compileShader  shader
    log    <- GL.shaderInfoLog  shader
    _      <-                   unless (null log) $ putStrLn log
    _      <- GL.attachShader   prog shader
    return ()

geomShader :: ShaderSource
geomShader = [r|
#version 430

struct Pepega {
  float x;
  float y;
};

layout(std430, binding = 3) buffer layoutName
{
  Pepega pepega[];
};

layout(points) in;
layout(triangle_strip, max_vertices = 4) out;

layout(location = 1) uniform float xpos;
layout(location = 2) uniform float ypos;
layout(location = 3) uniform float sx;
layout(location = 4) uniform float sy;

out vec4 crd;

void main() {
  int id = gl_PrimitiveIDIn;
  float xoff = (-xpos) / (sx) * 2;
  float yoff = (-ypos) / (sy) * 2;
  for(int i = 0; i < 4; i++) {
    crd = vec4((i%2), (i/2), 0.0, 1.0);
    gl_Position = vec4((xoff + (pepega[id].x+(i%2)) / sx * 2), (yoff + (pepega[id].y+(i/2)) / sy * 2), 0.0, 1.0);
    EmitVertex();
  }
  EndPrimitive();
}
|]

vertShader :: ShaderSource
vertShader = [r|
#version 430
void main() {
  gl_Position = vec4(0.0, 0.0, 0.0, 1.0);
}
|]

fragShader :: ShaderSource
fragShader = [r|
#version 430

in vec4 crd;

uniform sampler2D tx;

out vec4 fragColor;

void main() {
  fragColor = texture(tx, crd.xy*0.25);
}
|]

vertShaderHG :: ShaderSource
vertShaderHG = [r|
#version 330

layout (location=0) in vec2 iPos;
layout (location=1) in vec3 iCol;
layout (location=2) in vec2 iUV;

out vec3 oCol;
out vec2 oUV;

void main() {
  gl_Position = vec4(iPos, 0.1, 1.0);
  oCol = iCol;
  oUV = iUV;
}
|]

fragShaderHG :: ShaderSource
fragShaderHG = [r|
#version 330

in vec3 oCol;
in vec2 oUV;
out vec4 fragColor;

uniform sampler2D tx;

void main() {
  if (oUV.x < 0) {
    fragColor = vec4(oCol, 1.0);
  } else {
    float r = texture(tx, oUV).r;
    if (r==0) {
      fragColor = vec4(0,0,0,0);
    } else {
      fragColor = vec4(r, r, r, 1.0);
    }
  }
}
|]