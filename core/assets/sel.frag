#version 120

uniform float size;
uniform float cursor;
varying vec4 pos;

void main()
{
  vec2 op = vec2(pos.x + 25.0, pos.y + 25.0); // add offset
  vec2 p = ( op.xy / vec2(size) ) - vec2(0.5);
  vec2 pn = normalize(p);
  float alpha = length(cross(vec3(pn[0], pn[1], 0), vec3(1.0, 0.0, 0.0)));
  float color1 = 1.0/(1.0 + 7.0* length(p) + cos(alpha*20.0 + cursor/5.0));
  float color2 = 1.0/(1.0 + 12.0* length(p) + cos(alpha*10.0 + cursor/5.0));
  float color = (clamp(pow(color1 + color2, 3.0), 0.1, 1) - 0.1) * 1.1;
  gl_FragColor = vec4(color, color, color, color);
}