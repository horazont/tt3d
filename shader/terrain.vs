uniform vec2 offset;
uniform vec2 width;

uniform sampler2D heightMap;

varying vec3 untransformed;
varying vec3 position;
varying vec2 tcoord;

void main()
{
  vec2 rpos = gl_Vertex.xy + offset;
  tcoord = rpos / width;
  
  vec4 decomposeTmp = texture2D(heightMap, tcoord);
  //vec4 decomposeTmp = vec4(1.0, 0.0, 0.0, 0.0);

  vec4 v = vec4(rpos, decomposeTmp.r, 1.0);
  untransformed = vec3(v);
  position = vec3(gl_ModelViewMatrix * v);
  gl_Position = gl_ModelViewProjectionMatrix * v;
}
