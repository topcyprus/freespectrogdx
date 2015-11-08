
varying vec4 v_color;

void main()
{
    gl_FragColor =   vec4(0.5, v_color[0], 1, 1);
}