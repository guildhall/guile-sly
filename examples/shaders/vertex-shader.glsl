#version 120

uniform float angle;

void main(void) {
    mat4 RotationMatrix = mat4(cos(angle), -sin(angle), 0.0, 0.0,
                               sin(angle), cos(angle),  0.0, 0.0,
                               0.0,        0.0,         1.0, 0.0,
                               0.0,        0.0,         0.0, 1.0);

    gl_Position =  gl_ModelViewProjectionMatrix * RotationMatrix * gl_Vertex;
    gl_TexCoord[0] = gl_MultiTexCoord0;
}
