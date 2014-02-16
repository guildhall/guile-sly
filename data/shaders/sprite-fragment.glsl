#version 120

uniform sampler2D color_texture;
uniform vec4 color;

void main (void) {
    gl_FragColor = texture2D(color_texture, gl_TexCoord[0].st) * color;
}
