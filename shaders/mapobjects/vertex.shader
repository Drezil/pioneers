#version 330

in vec3 Position;
in vec3 Normal;

out vec3 vPosition;
out vec3 vNormal;

void main () {
   vPosition = Position;
   gl_Position = vec4(Position, 1.0);
   vNormal = Normal;
}

