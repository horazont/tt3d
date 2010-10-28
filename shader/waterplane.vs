uniform float waterLine;
uniform sampler2D heightfield;
uniform vec2 size;
uniform vec2 offset;

varying vec3 v, vNormal, vTangent;
varying float height;

void main()
{
  gl_TexCoord[0] = vec4(gl_Vertex.xy + offset, waterLine, 1.0); 
  height = texture2D(heightfield, vec2(gl_TexCoord[0]) / size).a;
  v = vec3(gl_ModelViewMatrix * gl_TexCoord[0]); 
  vNormal = vec3(0.0, 0.0, 1.0);
  vTangent = vec3(1.0, 0.0, 0.0);
  gl_Position = gl_ModelViewProjectionMatrix * gl_TexCoord[0];
}
