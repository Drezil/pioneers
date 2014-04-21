#version 330

#extension GL_ARB_tessellation_shader : require

layout(triangles, equal_spacing, cw) in;
in vec3 tcPosition[];
in vec4 tcColor[];
in vec3 tcNormal[];
out vec4 teColor;
smooth out vec3 tePosition;
smooth out vec3 teNormal;
out float fogDist;
smooth out float gmix; //mixture of gravel
//out vec3 tePatchDistance;
//constant projection matrix
uniform mat4 ProjectionMatrix;
uniform mat4 ViewMatrix;
uniform mat3 NormalMatrix;

void main()
{
    //NORMAL
    vec3 n0 = gl_TessCoord.x * tcNormal[0];
    vec3 n1 = gl_TessCoord.y * tcNormal[1];
    vec3 n2 = gl_TessCoord.z * tcNormal[2];
    vec3 tessNormal = normalize(n0 + n1 + n2);
    teNormal = NormalMatrix * tessNormal;

    //POSITION
    vec3 p0 = gl_TessCoord.x * tcPosition[0];
    vec3 p1 = gl_TessCoord.y * tcPosition[1];
    vec3 p2 = gl_TessCoord.z * tcPosition[2];
    tePosition = p0 + p1 + p2;

    //sin(a,b) = length(cross(a,b))
    float i0 = (1-gl_TessCoord.x)*gl_TessCoord.x * length(cross(tcNormal[0],tessNormal));
    float i1 = (1-gl_TessCoord.y)*gl_TessCoord.y * length(cross(tcNormal[1],tessNormal));
    float i2 = (1-gl_TessCoord.z)*gl_TessCoord.z * length(cross(tcNormal[2],tessNormal));
    float standout = i0+i1+i2;
    tePosition = tePosition+tessNormal*standout;
    gl_Position = ProjectionMatrix * ViewMatrix * vec4(tePosition, 1);
    fogDist = gl_Position.z;

    //COLOR-BLENDING
    vec4 c0 = sqrt(gl_TessCoord.x) * tcColor[0];
    vec4 c1 = sqrt(gl_TessCoord.y) * tcColor[1];
    vec4 c2 = sqrt(gl_TessCoord.z) * tcColor[2];
    teColor = (c0 + c1 + c2)/(sqrt(gl_TessCoord.x)+sqrt(gl_TessCoord.y)+sqrt(gl_TessCoord.z));

    //mix gravel based on incline (sin (normal,up))
    gmix = length(cross(tessNormal, vec3(0,1,0)));

}