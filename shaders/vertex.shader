#version 330

//constant projection matrix
uniform mat4 fg_ProjectionMatrix;

//vertex-data
in vec4 fg_Color;
in vec3 fg_VertexIn;
in vec3 fg_Normal;

//output-data for later stages
out vec4 fg_SmoothColor;

void main()
{
   //transform vec3 into vec4, setting w to 1
   vec4 fg_Vertex = vec4(fg_VertexIn.x, fg_VertexIn.y+0.1, fg_VertexIn.z, 1.0);
   fg_SmoothColor = fg_Color + 0.001* fg_Normal.xyzx;
   gl_Position = fg_ProjectionMatrix * fg_Vertex;
}