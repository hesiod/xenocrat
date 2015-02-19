#version 440 core

layout(points) in;
layout(line_strip, max_vertices = 6) out;

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;

void main() {
  mat4 m = proj * view * model;

  gl_Position = m * (gl_in[0].gl_Position + vec4(-1.0, 0.0, 0.0, 0.0));
  EmitVertex();
  gl_Position = m * (gl_in[0].gl_Position + vec4(1.0, 0.0, 0.0, 0.0));
  EmitVertex();
  EndPrimitive();

  gl_Position = m * (gl_in[0].gl_Position + vec4(0.0, -1.0, 0.0, 0.0));
  EmitVertex();
  gl_Position = m * (gl_in[0].gl_Position + vec4(0.0, 1.0, 0.0, 0.0));
  EmitVertex();
  EndPrimitive();

  gl_Position = m * (gl_in[0].gl_Position + vec4(0.0, 0.0, -1.0, 0.0));
  EmitVertex();
  gl_Position = m * (gl_in[0].gl_Position + vec4(0.0, 0.0, 1.0, 0.0));
  EmitVertex();
  EndPrimitive();
}
