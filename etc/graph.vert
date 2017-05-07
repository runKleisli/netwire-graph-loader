#version 150
in vec2 vertexPos;
in vec3 vertexColor;
uniform vec2 offset;
out vec3 diffuse;

void main() {
  gl_Position = vec4(offset + 0.1*vertexPos, 1.0, 1.0);
  diffuse = vertexColor;
}
