#version 450 core

layout(points) in;
layout(line_strip, max_vertices = 6) out;

uniform mat4 transform;

void main() {
  gl_Position = transform * (gl_in[0].gl_Position + vec4(-1.0, 0.0, 0.0, 0.0));
  EmitVertex();
  gl_Position = transform * (gl_in[0].gl_Position + vec4(1.0, 0.0, 0.0, 0.0));
  EmitVertex();
  EndPrimitive();

  gl_Position = transform * (gl_in[0].gl_Position + vec4(0.0, -1.0, 0.0, 0.0));
  EmitVertex();
  gl_Position = transform * (gl_in[0].gl_Position + vec4(0.0, 1.0, 0.0, 0.0));
  EmitVertex();
  EndPrimitive();

  gl_Position = transform * (gl_in[0].gl_Position + vec4(0.0, 0.0, -1.0, 0.0));
  EmitVertex();
  gl_Position = transform * (gl_in[0].gl_Position + vec4(0.0, 0.0, 1.0, 0.0));
  EmitVertex();
  EndPrimitive();
}
