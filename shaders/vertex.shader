#version 330

//constant projection matrix
uniform mat4 fg_ProjectionMatrix;
uniform mat4 fg_ModelMatrix;

//vertex-data
in vec4 fg_Color;
in vec3 fg_VertexIn;
in vec3 fg_NormalIn;

//output-data for later stages
smooth out vec4 fg_SmoothColor;

void main()
{
   vec3 fg_Normal = fg_NormalIn; //vec3(0,1,0);
   //transform vec3 into vec4, setting w to 1
   vec4 fg_Vertex = vec4(fg_VertexIn, 1.0);
   vec4 light = vec4(1.0,1.0,1.0,1.0);
   vec4 dark  = vec4(0.0,0.0,0.0,1.0);
   //direction to sun from origin
   vec3 lightDir = normalize(vec3(5.0,5.0,1.0));
   
   
   float costheta = dot(normalize(fg_Normal), lightDir);
   float a = costheta * 0.5 + 0.5;
   
   fg_SmoothColor = fg_Color * mix(dark, light, a);// + 0.001* fg_Normal.xyzx;
   gl_Position = fg_ProjectionMatrix * fg_ModelMatrix * fg_Vertex;
}