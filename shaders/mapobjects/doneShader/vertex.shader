#version 330

//vertex-data
in vec3 Position;
in vec3 Normal;
in vec2 TexCoord;

//output-data for later stages
out vec2 vTexCoord;
out vec3 vPosition;
out vec3 vNormal;

void main()
{
    vPosition = Position;
    vNormal = Normal;
    vTexCoord = TexCoord;
}
