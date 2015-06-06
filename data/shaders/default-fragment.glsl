#version 130

in vec2 frag_tex;
uniform vec4 color;
uniform sampler2D color_texture;
uniform bool use_texture;

void main (void) {
  if(use_texture) {
    gl_FragColor = texture2D(color_texture, frag_tex) * color;
  } else {
    gl_FragColor = color;
  }
}
