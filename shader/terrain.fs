varying vec3 normal;
varying vec3 tangent;
varying vec2 tcoord;

void main()
{
  float a = dot(normal, vec3(0.0, 0.0, 1.0)) * 0.5 + 0.5;
  gl_FragColor = vec4(normal * 0.5 + 0.5, 1.0);
}
