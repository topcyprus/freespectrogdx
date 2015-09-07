
attribute vec4 a_position;
attribute vec4 a_color;
attribute vec2 a_texCoord;

uniform mat4 u_projModelView;
varying vec4 col;
varying vec2 texCoords;

varying vec4 pos;
void main()
{
  pos = gl_Vertex;
  gl_Position = u_projModelView * a_position;
}