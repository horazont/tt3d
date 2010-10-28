uniform mat4 texMat;
uniform sampler2D reflectTex;
uniform sampler2D normalMap;
uniform vec2 size;
uniform vec2 coord;
uniform float waterLine;
uniform vec3 camPos;

varying vec3 v, vNormal, vTangent;
varying float height;

const float reflectScale = 1.0;

void main()
{    
  float fog = clamp(exp(length(camPos.xy - gl_TexCoord[0].xy) * 0.005) - 1.0, 0.0, 1.0); 
  if (fog == 1.0)
    discard;
  
  float depth = clamp(-(height - waterLine), 0.0, 1.0);
  vec3 normal = normalize(
    mix((texture2D(normalMap, (coord + vec2(gl_TexCoord[0])))).xyz,
        (texture2D(normalMap, (coord + vec2(gl_TexCoord[0]) * 0.05))).xyz, 0.2));
  normal = mix(vNormal, vec3(normal.xy, 1.0), depth * 0.0);
  vec2 offsetColor = normal.xy;
  normal = gl_NormalMatrix * mat3(vTangent, cross(vNormal, vTangent), vNormal) * normalize(normal);//vec3(0.0, 0.0, 1.0);
  
  
  vec4 reflectcoord = gl_TexCoord[0];
  reflectcoord.x += offsetColor.x * reflectScale;
  reflectcoord.y += offsetColor.y * reflectScale;
  reflectcoord = texMat * reflectcoord;
  vec4 reflectColor = texture2DProj(reflectTex, reflectcoord);   
  
  vec3 r 		= normalize(reflect(-(gl_NormalMatrix * gl_LightSource[0].position.xyz), normal));
  vec4 lighting = gl_LightSource[0].specular * pow(max(dot(r, normalize(-v)), 0.0), 5.0);
  
  vec4 fogColor = vec4(0.35, 0.5, 0.75, 1.0);
  gl_FragColor = mix(reflectColor + lighting, fogColor, fog);
  //float t = dot(r, normalize(-v));
  //gl_FragColor = vec4(normal * 0.5 + 0.5, 1.0);
}
