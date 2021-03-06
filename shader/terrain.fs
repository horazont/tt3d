uniform float waterLine;
uniform float snowLine;
uniform vec3 camPos;

uniform sampler2D normalMap;
uniform sampler2D tangentMap;
uniform sampler2D colorMap;
uniform sampler2D noise;
uniform sampler2D normalDetailMap;

varying vec2 tcoord;
varying vec3 position;
varying vec3 untransformed;

void main()
{
  vec3 normal = vec3(texture2D(normalMap, tcoord)) * 2.0 - 1.0;
  vec3 tangent = vec3(texture2D(tangentMap, tcoord)) * 2.0 - 1.0;

  float fog = clamp(exp(length(camPos.xy - untransformed.xy) * 0.005) - 1.0, 0.0, 1.0); 
  // fog = 0.0;
  if (untransformed.z < waterLine - 0.2)
    discard;
  if (fog == 1.0)
    discard;
  
  vec3 lightDir = gl_LightSource[0].position.xyz;
  
  vec3 norm = normalize(normal);
  vec3 tang = normalize(tangent);
  vec3 bitang = cross(norm, tang);
  
  float mapNoise = (
      texture2D(noise, vec2(untransformed * 0.0327)).r * 0.5
    + texture2D(noise, vec2(untransformed * 0.01)).r * 0.2) * 0.08 + 0.04;
    
  float heightcoord = ((untransformed.z - waterLine) / (snowLine - waterLine)) * 0.6 + 0.15;
  
  vec4 color = texture2D(colorMap, vec2(heightcoord, 1.0));
  
  mat3 normalSpace = mat3(tang, bitang, norm);
  vec3 mapnorm = normalSpace * normalize(texture2D(normalDetailMap, vec2(untransformed) * 0.3).rgb * 2.0 - 1.0);
  
  norm = mix(norm, mapnorm, color.a);
  
  float slopecoord = pow(dot(norm, vec3(0.0, 0.0, 1.0)), 2.0);
  
  color = texture2D(colorMap, vec2(heightcoord, slopecoord) + mapNoise);
  
  vec3 diffuse = color.rgb;
  //vec3 colorNoise = texture2D(noise, vec2(untransformed * 0.5)).rgb * 0.3 + texture2D(noise, vec2(untransformed * 0.15)).rgb * 0.6 + 0.1;
  //diffuse *= colorNoise * 0.5 + 0.5;
  vec3 fogColor = vec3(0.35, 0.5, 0.75);
  
  vec4 lighting = gl_LightSource[0].ambient 
    + gl_LightSource[0].diffuse * max(dot(norm, lightDir), 0.0);
  gl_FragColor = vec4(mix(diffuse * vec3(lighting), fogColor, fog), 1.0);
  //gl_FragColor = vec4(fog, fog, fog, 1.0);
}
