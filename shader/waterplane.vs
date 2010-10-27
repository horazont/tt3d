uniform float waterLine;

void main()
{
  gl_Position = gl_ModelViewProjectionMatrix * vec4(gl_Vertex.xy, waterLine, 1.0);
}
