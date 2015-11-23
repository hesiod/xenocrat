#version 450 core

layout(points) in;
layout(line_strip, max_vertices = 6) out;

const float sz = 0.3;

uniform mat4 transform;

vec4 arr(int which) {
  vec4 v = vec4(0.0);
  v[abs(which) - 1] = sign(which) * sz;
  return gl_in[0].gl_Position + v;
}

void both(int which) {
  gl_Position = transform * arr(-which);
  EmitVertex();
  gl_Position = transform * arr(which);
  EmitVertex();
  EndPrimitive();
}

void main() {
  both(1);
  both(2);
  both(3);
}
