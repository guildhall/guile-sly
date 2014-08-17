#version 130

in vec3 position;
in vec2 tex;
out vec2 frag_tex;
uniform mat4 mvp;

void main(void) {
    frag_tex = tex;
    gl_Position = mvp * vec4(position.xyz, 1.0);
}
