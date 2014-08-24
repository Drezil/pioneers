#version 330

layout(location=0) in vec3 Position;
layout(location=1) in vec3 Normal;
layout(location=2) in vec2 TexCoord;
uniform mat4 ProjectionMatrix;
uniform mat4 ViewMatrix;
uniform mat3 NormalMatrix;
uniform vec3 PositionOffset = vec3(5,2,5);
uniform float TessLevelInner = 1.0; // controlled by keyboard buttons
uniform float TessLevelOuter = 1.0; // controlled by keyboard buttons

out vec3 vPosition;
out vec3 vNormal;

void main () {
   vPosition = Position;
   //gl_Position = vec4(Position,1);
   gl_Position = ProjectionMatrix * ViewMatrix * vec4(PositionOffset + Position, 1);
   vNormal = Normal;
}
