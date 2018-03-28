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


// Coordinate systems
//
// The input vertex is in 4D model space, vertex
// It is transformed by rotation/skew/etc Mm and moved to its correct position (and orientation) in 4D world space
// This is then Mt + Mm * vertex = v_w
//
// A 4D world coordinate is transformed in to a 4D camera coordinate by translation by the camera position Vt,
// and orientation of the camera Vm
//
// Hence the 4D vertex view point v_v is Vm * (v_v + Vt)
//
// In this coordinates system v_v is relative to the camera, with the axes of 'forward/backward', 'left/right', 'visible up/down', 'other'
//
// This must be mapped to the 3D view space for the GUI, which has left/right, up/down, out/in, perspective divide as its four axes. This must be mapped by PG. We want 4D l/r to be 3D l/r; 4D u/d to be 3D vis u/d; 4D f/b to be 3D o/i
// 
// Hence v_s = PG * (v_v.lr, v_v.ud, v_v.fb, )
//
// In OpenGL, v_s is clipped to X/Y/Z of range [-1,1] (clipping space coords) giving v_clip
//
// Then this is mapped to normalized device coordinates (3D) by perspective
// v_ndc = v_clip.x / v_clip.w, v_clip.y / v_clip.w, v_clip.z / v_clip.w
//
// These are mapped to window space coordinates (3D) (x/y -1/1 -> left/right top/bottom; z -1/1 -> nearVal/farVal)
// v_window.x = (v_ndc.x+1) * viewport width/2 + viewport center,
// v_window.y = (v_ndc.y+1) * viewport width/2 + viewport center,
// v_window.z = (v_ndc.z+1) * viewport (far val - near val)/2 + viewport near val
//
// Then v_window.z is mapped to the depth buffer use GlDepthRange, so actually
// a v_ndc.z of nearVal will have a depth buffer value of 0, and v_ndc.z of farVal
// has a max depth buffer value


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

    v_v = vec4( v_v.y, v_v.z, v_v.x, v_v.w);
    v_v.w = 1.0;

    v_s = PG*v_v;

    //f_normal = n_w.xyz;
    f_normal = vec3 (0.,1.,0.);
    f_uv     = uv;
    f_color  = color;

    gl_Position = vec4(v_s.xyz, 1.1 + v_s.z/2);
}
