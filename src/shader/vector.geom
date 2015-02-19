#version 440 core

layout(lines) in;
layout(line_strip, max_vertices = 6) out;

uniform mat4 arrowrot;
uniform float arrowscale;

void main() {
  gl_Position = gl_in[0].gl_Position;
  EmitVertex();
  gl_Position = gl_in[1].gl_Position;
  EmitVertex();
  EndPrimitive();

  vec4 diff = arrowscale * (gl_in[0].gl_Position - gl_in[1].gl_Position);
  gl_Position = gl_in[1].gl_Position + arrowrot * diff;
  EmitVertex();
  gl_Position = gl_in[1].gl_Position;
  EmitVertex();
  EndPrimitive();
  gl_Position = gl_in[1].gl_Position + transpose(arrowrot) * diff;
  EmitVertex();
  gl_Position = gl_in[1].gl_Position;
  EmitVertex();
  EndPrimitive();
}
