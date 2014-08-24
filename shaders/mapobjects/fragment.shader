#version 330

in vec3 vPosition;
in vec3 vNormal;

out vec4 fgColor;

uniform mat4 ViewMatrix;

void main () {
    //heliospheric lighting
    vec4 light = vec4(1.0,1.0,1.0,1.0);
    vec4 dark  = vec4(0.0,0.0,0.0,1.0);
    //direction to sun from origin
    vec3 lightDir = normalize(ViewMatrix * vec4(5.0,5.0,1.0,0.0)).xyz;

    float costheta = dot(vNormal, lightDir);
    float a = costheta * 0.5 + 0.5;

    fgColor = vec4(0.5,0.5,0.5,1)*mix(dark,light,a);
}
