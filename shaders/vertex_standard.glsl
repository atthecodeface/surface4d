#version 330 core

layout(location = 0) in vec3 vertex;
layout(location = 1) in vec3 normal;
layout(location = 2) in vec3 color;
layout(location = 3) in vec2 uv;
uniform mat4 M; // Model matrix - map object to where it sits in model space
uniform mat4 V; // View matrix - map model in to camera-centred, Z towards camera, X right, Y up
uniform mat4 G; // GUI matrix - map camera view XYZ1 to GUI space (translate, scale, possibly rotate - widget dependent)
uniform mat4 P; // Projection matrix - map GUI view to projection and where it is on the OpenGL 'page'
out vec3 f_color;
out vec3 f_normal;
out vec2 f_uv;

void main()
{
    mat4 VM;
    mat4 PG;
    vec4 v_w;
    vec4 v_v;
    vec4 v_s;

    vec4 n_w;

    PG = P * G;

    v_w = M*vec4(vertex,1.0);
    v_v = V*v_w;
    v_v.w = 1.0;

    v_s = PG*v_v;

    n_w = M*vec4(normal,0.0);

    f_normal = n_w.xyz;
    f_uv     = uv;
    f_color  = color;

    gl_Position = vec4(v_s.xyz, 1.2+v_s.z/2.0);
}
