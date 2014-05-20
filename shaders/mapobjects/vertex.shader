#version 330

layout(location=0) in vec3 Position;
layout(location=1) in vec3 Normal;
uniform mat4 ProjectionMatrix;
uniform mat4 ViewMatrix;
uniform mat3 NormalMatrix;
uniform vec3 PositionOffset = vec3(25,5,25);
uniform float TessLevelInner = 1.0; // controlled by keyboard buttons
uniform float TessLevelOuter = 1.0; // controlled by keyboard buttons

out vec3 vPosition;
out vec3 vNormal;

void main () {
   vPosition = Position;
   gl_Position = ProjectionMatrix * ViewMatrix * vec4(PositionOffset + 10*Position, 1);
   vNormal = Normal;
}
