#version 330 core

layout(location = 0) in vec4 vertex;
// layout(location = 1) in vec3 normal; Skip normals for now 
layout(location = 1) in vec3 color;
layout(location = 2) in vec2 uv;
uniform mat4 Mm; // Model matrix 
uniform vec4 Mt; // Model translation
uniform mat4 Vm; // View matrix
uniform vec4 Vt; // View translation
uniform mat4 G; // GUI matrix - map camera view XYZ1 to GUI space (translate, scale, possibly rotate - widget dependent)
uniform mat4 P; // Projection matrix - map GUI view to projection and where it is on the OpenGL 'page'

out vec3 f_color;
out vec3 f_normal;
out vec2 f_uv;
// gl_Position is the 3D position in projected OpenGL camera space

void main()
{
    mat4 PG;
    vec4 v_w;
    vec4 v_v;
    vec4 v_s;

    vec4 n_w;

    PG = P * G;

    v_w = Mt + Mm *vertex;
    v_v = Vm * (Vt + v_w); // Maps into 4D space - xyzw - where w is dropped

    v_v = vec4( v_v.y, v_v.x, v_v.z, v_v.w);
    v_v.w = 1.0;

    v_s = PG*v_v;

    //f_normal = n_w.xyz;
    f_normal = vec3 (0.,1.,0.);
    f_uv     = uv;
    f_color  = color;

    gl_Position = vec4(v_s.xyz, 1.0 + v_s.z/2);
}
