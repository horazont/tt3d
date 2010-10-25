uniform vec2 offset;
uniform vec2 width;

uniform sampler2D heightfield;
uniform sampler2D normalMap;
uniform sampler2D tangentMap;

varying vec3 normal;
varying vec3 tangent;
varying vec2 tcoord;

void main()
{
  vec2 rpos = gl_Vertex.xy + offset;
  tcoord = rpos / width;
  vec4 v = vec4(rpos, texture2D(heightfield, tcoord).x, 1.0);
  
  gl_Position = gl_ModelViewProjectionMatrix * v;
  
  normal = texture2D(normalMap, tcoord).xyz;
  tangent = texture2D(tangentMap, tcoord).xyz;
}
