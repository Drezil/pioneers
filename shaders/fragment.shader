#version 400

smooth in vec3 teNormal;
in vec4 teColor;

out vec4 fgColor;

uniform mat4 ViewMatrix;

void main(void)
{
    //heliospheric lighting
    vec4 light = vec4(1.0,1.0,1.0,1.0);
    vec4 dark  = vec4(0.0,0.0,0.0,1.0);
    //direction to sun from origin
    vec3 lightDir = normalize(ViewMatrix * vec4(5.0,5.0,1.0,0.0)).xyz;

    float costheta = dot(teNormal, lightDir);
    float a = costheta * 0.5 + 0.5;


    fgColor = teColor * mix(dark, light, a);
}