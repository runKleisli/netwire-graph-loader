#version 150
in vec3 diffuse;
out vec4 fragColor;

void main() {
  fragColor = vec4(diffuse, 1.0);
}
