#version 330
#extension GL_ARB_tessellation_shader : require

layout(vertices = 3) out;
in vec3 vPosition[];
in vec4 vColor[];
in vec3 vNormal[];
out vec3 tcPosition[];
out vec4 tcColor[];
out vec3 tcNormal[];
uniform float TessLevelInner = 1.0; // controlled by keyboard buttons
uniform float TessLevelOuter = 1.0; // controlled by keyboard buttons
uniform mat4 ProjectionMatrix;
uniform mat4 ViewMatrix;
uniform mat3 NormalMatrix;

#define ID gl_InvocationID

void main()
{
    tcPosition[ID] = vPosition[ID];
    tcColor[ID] = vColor[ID];
    tcNormal[ID] = vNormal[ID];
    float dist = (ProjectionMatrix * ViewMatrix * vec4(vPosition[ID], 1)).z;
    if (ID == 0) {
	if (dist < 30) {
	        gl_TessLevelInner[0] = TessLevelInner;
        	gl_TessLevelOuter[0] = TessLevelOuter;
	        gl_TessLevelOuter[1] = TessLevelOuter;
        	gl_TessLevelOuter[2] = TessLevelOuter;
	} else if (dist < 50) {
	        gl_TessLevelInner[0] = max(TessLevelInner-1.0,1.0);
        	gl_TessLevelOuter[0] = max(TessLevelOuter-1.0,1.0);
	        gl_TessLevelOuter[1] = max(TessLevelOuter-1.0,1.0);
        	gl_TessLevelOuter[2] = max(TessLevelOuter-1.0,1.0);
	} else if (dist < 100) {
	        gl_TessLevelInner[0] = max(TessLevelInner-2.0,1.0);
        	gl_TessLevelOuter[0] = max(TessLevelOuter-2.0,1.0);
	        gl_TessLevelOuter[1] = max(TessLevelOuter-2.0,1.0);
        	gl_TessLevelOuter[2] = max(TessLevelOuter-2.0,1.0);
	} else {
	        gl_TessLevelInner[0] = 1.0;
        	gl_TessLevelOuter[0] = 1.0;
	        gl_TessLevelOuter[1] = 1.0;
        	gl_TessLevelOuter[2] = 1.0;
	}
    }
}
