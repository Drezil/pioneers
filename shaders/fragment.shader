#version 140

#color from earlier stages
smooth in vec4 fg_SmoothColor;

#color of pixel
out vec4 fg_FragColor;

void main(void)
{
   fg_FragColor = fg_SmoothColor; #copy-shader
)