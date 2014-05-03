#version 330

//vertex-data
in vec4 Color;
in vec3 Position;
in vec3 Normal;

//output-data for later stages
out vec4 vColor;
out vec3 vPosition;
out vec3 vNormal;

void main()
{
    vPosition = Position;
    vNormal = Normal;
    vColor = Color;
}