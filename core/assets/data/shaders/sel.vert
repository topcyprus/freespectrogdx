
attribute vec4 a_position;

uniform mat4 u_projTrans;

varying   vec4 pos;

void main()
{
  pos = a_position;
  gl_Position = u_projTrans * a_position;
}