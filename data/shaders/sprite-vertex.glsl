#version 120

uniform mat4 projection;
uniform vec2 position;
uniform vec2 anchor;
uniform vec2 scale;
uniform float rotation;

void main(void) {
    mat4 rotationMatrix = mat4(cos(rotation), -sin(rotation), 0.0, 0.0,
                               sin(rotation), cos(rotation),  0.0, 0.0,
                               0.0,           0.0,            1.0, 0.0,
                               0.0,           0.0,            0.0, 1.0);
    mat4 translationMatrix = mat4(1.0, 0.0, 0.0, position.x - anchor.x,
                                  0.0, 1.0, 0.0, position.y - anchor.y,
                                  0.0, 0.0, 1.0, 0.0,
                                  0.0, 0.0, 0.0, 1.0);
    mat4 scaleMatrix = mat4(scale.x, 0.0,     0.0, 0.0,
                            0.0,     scale.y, 0.0, 0.0,
                            0.0,     0.0,     1.0, 0.0,
                            0.0,     0.0,     0.0, 1.0);

    gl_Position = projection * (gl_Vertex * scaleMatrix *
                                rotationMatrix * translationMatrix);
    gl_TexCoord[0] = gl_MultiTexCoord0;
}
