#version 110

uniform sampler2D tex[2];
varying vec2 texcoord;

void main()
{
    vec4 map = texture2D(tex[0], texcoord);
    vec4 hud = texture2D(tex[1], vec2(texcoord.x,-texcoord.y));
    gl_FragColor = vec4(mix(map.rgb,hud.rgb,hud.a),1.0);
}
