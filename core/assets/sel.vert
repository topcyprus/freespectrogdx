
uniform sampler2D u_texture;
attribute vec4 a_position;

uniform mat4 u_projTrans;
//uniform vec2 offset;

varying   vec4 pos;

void main()
{
  pos = a_position;
  gl_Position = u_projTrans * a_position; //vec4(a_position[0] + offset[0], a_position[1] + offset[1], a_position[2], a_position[3]);
}