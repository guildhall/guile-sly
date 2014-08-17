#version 130

in vec2 frag_tex;
uniform sampler2D color_texture;

void main (void) {
    gl_FragColor = texture2D(color_texture, frag_tex);
}
