#version 130

attribute vec4 a_position;

uniform mat4 u_projTrans;

varying float posx;
void main()
{
	gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0;
	gl_Position = u_projTrans * a_position;
	posx = gl_Position.x;
}
