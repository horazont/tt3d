uniform vec2 offset;
uniform vec2 width;

uniform sampler2D normalMap;
uniform sampler2D tangentMap;

varying vec3 normal;
varying vec3 tangent;
varying vec3 untransformed;
varying vec3 position;

void main()
{
  vec2 rpos = gl_Vertex.xy + offset;
  vec2 tcoord = rpos / width;
  
  vec4 decomposeTmp = texture2D(normalMap, tcoord);
  
  normal = decomposeTmp.xyz;
  tangent = texture2D(tangentMap, tcoord).xyz;
  
  vec4 v = vec4(rpos, decomposeTmp.w, 1.0);
  untransformed = vec3(v);
  position = vec3(gl_ModelViewMatrix * v);
  gl_Position = gl_ModelViewProjectionMatrix * v;
}
