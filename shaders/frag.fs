#version 430

in vec2 TexCoords;
out vec4 colour;

void main() {
     colour = vec4(TexCoords.x, TexCoords.y, 0, 1);
}
