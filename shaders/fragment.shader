#version 330

//color from earlier stages
smooth in vec4 fg_SmoothColor;

//color of pixel
out vec4 fg_FragColor;

void main(void)
{
//copy-shader
   fg_FragColor = vec4(0.5,0.5,0.5,1.0);//fg_SmoothColor;
}