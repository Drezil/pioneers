#version 140

//constant projection matrix
uniform mat4 fg_ProjectionMatrix;

//vertex-data
in vec4 fg_Color;
in vec4 fg_Vertex;
in vec4 fg_Normal;

//output-data for later stages
smooth out vec4 fg_SmoothColor;

void main()
{
   fg_SmoothColor = fg_Color;
   gl_Position = fg_ProjectionMatrix * fg_Vertex;
}