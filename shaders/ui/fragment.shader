#version 110

uniform sampler2D tex;
varying vec2 texcoord;

void main()
{

    gl_FragColor = texture2D(tex, texcoord);
}