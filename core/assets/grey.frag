#version 120

uniform sampler2D u_texture;

varying vec4 v_color;
varying vec2 v_texCoords;

float low(float x) {
  return max(x - 0.5, 0);
}

void main()
{
	vec4 color = texture2D(u_texture, v_texCoords.st);
	vec4 base = vec4(low(v_color[0]), low(v_color[1]), low(v_color[2]), color[3]);
  gl_FragColor = mix(color, base, 0.5);
}