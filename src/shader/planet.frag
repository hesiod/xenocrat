#version 450 core

in vec3 teval_position;
in vec3 teval_normal;
out vec4 outColor;

uniform mat4 transform;

void main()
{
  vec3 light = vec3(1.0, 1.0, 1.0);
  vec3 ambient = vec3(0.2, 0.2, 0.2);
  vec3 diffuse = vec3(0.3, 0.0, 0.0);
  vec3 specular = vec3(0.5, 0.5, 0.5);
  //vec3 teval_normal = normalize(cross(dFdxFine(teval_position), dFdyFine(teval_position)));
  vec3 light_world = vec3(transform * vec4(light, 1.0));
  vec3 light_dir = normalize(light_world - teval_position);
  float l = max(dot(light_dir, teval_normal), 0.0);
  float spec = 0.0;
  if (l > 0.0) {
    vec3 view_dir = normalize(-teval_position);
    vec3 half_dir = normalize(light_dir + view_dir);
    float spec_ang = max(dot(half_dir, teval_normal), 0.0);
    spec = pow(spec_ang, 16.0);
  }
  outColor = vec4(ambient + l * diffuse + spec * specular, 1.0);
}
