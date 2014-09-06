#version 130

in vec2 frag_tex;
uniform vec4 color;
uniform sampler2D color_texture;

void main (void) {
    gl_FragColor = texture2D(color_texture, frag_tex) * color;
}
