#version 330

//color from earlier stages
smooth in vec4 fg_SmoothColor;

out vec4 fg_FragColor;

void main(void)
{
//copy-shader
   fg_FragColor = fg_SmoothColor;
}