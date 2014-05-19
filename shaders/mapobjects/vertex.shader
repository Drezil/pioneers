#version 330

in vec3 Position;
in vec3 Normal;
uniform mat4 ProjectionMatrix;
uniform mat4 ViewMatrix;
uniform mat3 NormalMatrix;
uniform vec3 PositionOffset;
uniform float TessLevelInner = 1.0; // controlled by keyboard buttons
uniform float TessLevelOuter = 1.0; // controlled by keyboard buttons

out vec3 vPosition;
out vec3 vNormal;

void main () {
   vPosition = Position;
   gl_Position = vec4(Position, 1.0);
   vNormal = Normal;
}

