#version 120

uniform mat4 projection;
uniform vec2 position;
uniform vec2 anchor;

void main(void) {
    mat4 translation = mat4(1.0, 0.0, 0.0, position.x - anchor.x,
                            0.0, 1.0, 0.0, position.y - anchor.y,
                            0.0, 0.0, 1.0, 0.0,
                            0.0, 0.0, 0.0, 1.0);

    gl_Position = projection * (gl_Vertex * translation);
    gl_TexCoord[0] = gl_MultiTexCoord0;
}
