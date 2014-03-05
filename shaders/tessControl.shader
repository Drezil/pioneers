#version 400

layout(vertices = 3) out;
in vec3 vPosition[];
in vec4 vColor[];
in vec3 vNormal[];
out vec3 tcPosition[];
out vec4 tcColor[];
out vec3 tcNormal[];
uniform float TessLevelInner = 1.0; // controlled by keyboard buttons
uniform float TessLevelOuter = 1.0; // controlled by keyboard buttons

#define ID gl_InvocationID

void main()
{
    tcPosition[ID] = vPosition[ID];
    tcColor[ID] = vColor[ID];
    tcNormal[ID] = vNormal[ID];
    if (ID == 0) {
        gl_TessLevelInner[0] = TessLevelInner;
        gl_TessLevelOuter[0] = TessLevelOuter;
        gl_TessLevelOuter[1] = TessLevelOuter;
        gl_TessLevelOuter[2] = TessLevelOuter;
    }
}