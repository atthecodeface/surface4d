#version 330 core

out vec4 color;

in vec3 f_color;
in vec3 f_normal;
uniform vec3 L;
void main() {
  float brightness = (1.0 + dot ( L, normalize(f_normal) ))/3.0;
  color.rgb = f_color;
  color.a = 1.0;
 }