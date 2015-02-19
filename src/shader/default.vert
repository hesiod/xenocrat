#version 440 core

in vec3 position;

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;
uniform float zoom;

void main()
{
  gl_Position = proj * view * model * vec4(position / zoom, 1.0);
}
