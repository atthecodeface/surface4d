(** Copyright (C) 2018,  Gavin J Stark.  All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * @file     animation.ml
 * @brief    Shared-memory animation viewer using Ogl_gui
 *
 *)
open Sdl_ogl_gui (* replace with cocoa_ogl_gui if you want... *)
open Atcflib
open Tgl4
module Option   = Batteries.Option

(*a Useful functions *)
(*f trace - use with trace __POS__ *)
let trace pos = 
    let (a,b,c,d) = pos in
    Printf.printf "trace:%s:%d:%d:%d\n%!" a b c d

(*f >>= standard monadic function *)
let ( >>= ) x f = match x with Ok v -> f v | Error _ as e -> e

(*f ba creator functions *)
let ba_float_array   len = Bigarray.(Array1.create float32 c_layout len)
let ba_uint16_array  len = Bigarray.(Array1.create int16_unsigned c_layout len)
let ba_uint16s fs = Bigarray.(Array1.of_array int16_unsigned c_layout fs)
let ba_floats  fs = Bigarray.(Array1.of_array float32 c_layout fs)
let ba_uint8_array  len = Bigarray.(Array1.create int8_unsigned c_layout len)
type t_ba_float32s = (float, Bigarray.float32_elt,        Bigarray.c_layout) Bigarray.Array1.t
type t_ba_uint16s  = (int,   Bigarray.int16_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(*f gl_int_val, gl_with_int - to feed ints and get ints back from ctypes Opengl *)
let ba_int32_1    = Bigarray.(Array1.create int32 c_layout 1)
let ba_int32s len = Bigarray.(Array1.create int32 c_layout len)
let gl_int_val  f   = f ba_int32_1 ; Int32.to_int ba_int32_1.{0}
let gl_with_int f i = ba_int32_1.{0} <- Int32.of_int i; f ba_int32_1

(*a Global variables - colors, lights for now *)
(*v colors, lights *)
let light       = ba_floats [| (0.5); (0.5); (0.71)|]
let ambient_col = ba_floats [| (0.7); (0.7); (0.7)|]
let light_col   = ba_floats [| (0.5); (0.5); (0.5)|]

(*a Ogl_obj arrays *)
(*m OOVnc_type - vertex/normal/color type with 11 floats per coord *)
module OOVnc_type =
struct
  let fpc = 11 (* 11 floats per coordinate - vertex, normal, color, tex coord *)
end

(*m OOVnc - extendable arrays of coordinates and indices *)
module OOVnc =
struct
  include Ogl_gui.Obj.Arrays(OOVnc_type)

  let add_xy t x y nx ny nz cr cg cb u v =
    let n = t.num_cs in
    t.num_cs <- t.num_cs + 1;
    t.cs.{11*n+0} <- x;
    t.cs.{11*n+1} <- y;
    t.cs.{11*n+2} <- 0.;
    t.cs.{11*n+3} <- nx;
    t.cs.{11*n+4} <- ny;
    t.cs.{11*n+5} <- nz;
    t.cs.{11*n+6} <- cr;
    t.cs.{11*n+7} <- cg;
    t.cs.{11*n+8} <- cb;
    t.cs.{11*n+9} <- u;
    t.cs.{11*n+10} <- v;
    ()

end

(*a Texture reading *)
(*a Obj class *)
(*c ogl_obj_animation *)
class ogl_obj_animation =
    object (self)
      inherit Ogl_gui.Obj.ogl_obj as super
      val mutable plot_pts = [];
      val mutable plot_strips = [];
      method create_geometry ~offset =
        let oovnc_t = OOVnc.create 1024 1024 10 in
        (*OOVnc.ensure oovnc_t num_vncs num_is;*)
        OOVnc.add_xy oovnc_t (-1.) (-1.)  0. 0. 1.   0. 1. 1.  0. 0.;
        OOVnc.add_xy oovnc_t (-1.) 1.     0. 0. 1.   1. 0. 1.  0. 1.;
        OOVnc.add_xy oovnc_t 1. 1.        0. 0. 1.   1. 1. 0.  1. 1.;
        OOVnc.add_xy oovnc_t 1. (-1.)     0. 0. 1.   1. 1. 1.  1. 0.;
        ignore (OOVnc.add_index oovnc_t 0);
        ignore (OOVnc.add_index oovnc_t 3);
        ignore (OOVnc.add_index oovnc_t 1);
        ignore (OOVnc.add_index oovnc_t 2);
        OOVnc.add_strip oovnc_t 0 4;
        plot_pts    <- OOVnc.points oovnc_t;
        plot_strips <- OOVnc.strips oovnc_t;
        OOVnc.display oovnc_t;
        Printf.printf "Got %d points to plot and %d strips to plot\n" (List.length plot_pts) (List.length plot_strips);
        self # create_vao [ ( [ (0,3,Gl.float,false,(11*4),0);     (* vertices *)
                                (1,3,Gl.float,false,(11*4),(3*4)); (* normals *)
                                (2,3,Gl.float,false,(11*4),(6*4)); (* colors *)
                                (3,2,Gl.float,false,(11*4),(9*4)); (* UVs *)
                                ], OOVnc.cs oovnc_t)
          ] >>= fun _ -> (
        self # add_indices_to_vao (OOVnc.is oovnc_t);
        Ok ())
      method draw view_set other_uids =
        Gl.bind_vertex_array vao_glid;
        List.iter (fun (ofs,num)->Gl.draw_elements Gl.points num Gl.unsigned_short (`Offset (ofs*2))) plot_pts;
        List.iter (fun (ofs,num)->Gl.draw_elements Gl.triangle_strip num Gl.unsigned_short (`Offset (ofs*2))) plot_strips;
        Gl.bind_vertex_array 0;
        ()
    end

(*a Surface stuff *)
(*f vncs_of_surface *)
let vncs_of_surface =
  let vnc_list = ref [] in
  let indices_list = ref [] in
  let numi = 100 in
  let numj = 3 in
  for i=0 to (numi-1) do
    let t = 0.96 *. (float i) /. (float numi) in
    for j=0 to (numj-1) do
      let u = (float j) *. 0.125 +. 0.2 in
      let pt = Surface.Surface.vector t u in
      vnc_list := !vnc_list @ [ pt.(1); pt.(2); pt.(3); 0.;0.;1.; 1.;t;u*.10.; t;t;]
    done;
    if i>0 then (
    let new_triangles = List.init (2*numj) (fun n -> (((i-1)*numj)+n/2)+(if (n mod 2)=1 then numj else 0)) in
    indices_list := !indices_list @ ((List.hd new_triangles) :: new_triangles) @ [i*numj+numj-1]
    );
  done;
  (Array.of_list !vnc_list, Array.of_list !indices_list)

(*a Widget viewer class *)
(*c widget
 *)
(*c ogl_widget_viewer, an OpenGL ogl_obj list viewer  *)
let vector_x_axis = Atcflib.Vector.make3 1. 0. 0.
let vector_y_axis = Atcflib.Vector.make3 0. 1. 0.
let vector_z_axis = Atcflib.Vector.make3 0. 0. 1.
module Ordint = struct type t=int let compare a b = Pervasives.compare a b end
module Intset=Set.Make(Ordint)
(*c ogl_widget_animation_server  - viewer widget *)
let blah = Ogl_gui.Stylesheet.create_desc [("activity_level" , [ ("disable",0); ("enable",1); ("hover",2); ("pressed",3);])] Ogl_gui.widget_base_styles
class ogl_widget_animation_server stylesheet name_values =
  object (self)
    inherit Ogl_gui.Widget.ogl_widget stylesheet blah "4dviewer" name_values  as super
    val location = Array.make 3 0.;    
    val mutable angle=0.;
    val mutable tex_glid = -1;

  val keys_down = ref Intset.empty
  val joystick_axes = Array.make 16 0;
  val direction = Quaternion.make_rijk 1.0 0. 0. 0.
  val scale   = ref 1.
  val center = Vector.make3 0. 0. 0.
  val mutable idler_handle = -1
  val mutable draw_fn = let d a t = () in d
  val rotation = Matrix.make 4 4
  val translation = Matrix.make 4 4
  val view = Matrix.make 4 4
  val tmp = Matrix.make 4 4
  val tmp2 = Matrix.make 4 4
  val q1 = Quaternion.make ()
  val q2 = Quaternion.make ()
  val q3 = Quaternion.make ()
  val mutable opt_material = None
  val mutable objs:Ogl_gui.Obj.ogl_obj list = []

    (*f create *)
    method create app =
      opt_material <- Some (app#get_material "vnc_vertex") ;
      scale := 2.0;
      if (Option.is_none opt_material) then (
          opt_material <- Some (app#get_material "p") ;
      );
      super#create app >>=
        fun _ ->
        (
          self # create_geometry;
          idler_handle <- app#add_idler self#idle ;
          Ok ()
        )

    method get_direction = direction
    method get_center    = center

    (*f create_geometry *)
    method create_geometry =
      Printf.printf "Create geometry\n";
      if (List.length objs)=0 then (
        let surface_obj = new Ogl_gui.Obj.ogl_obj_geometry
                            Gl.triangle_strip
                            (Array.length (snd vncs_of_surface))
                            (snd vncs_of_surface)
                            [ ( [(0,3,Gl.float,false,11*4,0); (1,3,Gl.float,false,11*4,3*4); (2,3,Gl.float,false,11*4,6*4); (3,2,Gl.float,false,11*4,9*4); ],
                                ba_floats (fst vncs_of_surface) (* vertices, normals, colors, uv *)
                            ) ]
        in
        self # set_objs [(surface_obj :> Ogl_gui.Obj.ogl_obj)];
      );
      List.iter (fun o -> ignore (o#create_geometry ~offset:(0.,0.,0.))) objs

    (*f delete_geometry *)
    method delete_geometry =
      List.iter (fun o -> ignore (o#delete_geometry)) objs

    (*f set_objs *)
    method set_objs o = 
      if (self#can_create) then self#delete_geometry;
      objs <- o;
      if (self#can_create) then self#create_geometry

    (*f pitch *)
    method private pitch amount = 
      ignore (Quaternion.assign_of_rotation vector_x_axis (cos amount) (sin amount) q1);
      ignore (Quaternion.(postmultiply q1 direction))

    (*f yaw *)
    method private yaw amount = 
      ignore (Quaternion.assign_of_rotation vector_y_axis (cos amount) (sin amount) q1);
      ignore (Quaternion.(postmultiply q1 direction))

    (*f roll *)
    method private roll amount = 
      ignore (Quaternion.assign_of_rotation vector_z_axis (cos amount) (sin amount) q1);
      ignore (Quaternion.(postmultiply q1 direction))

    (*f move_forward *)
    method private move_forward scale = 
        ignore (Matrix.assign_from_q direction rotation);
        ignore (Matrix.scale scale rotation);
        let z = (Matrix.row_vector rotation 2) in
        ignore (Vector.add z center);
        ()

    (*f move_left *)
    method private move_left scale = 
        ignore (Matrix.assign_from_q direction rotation);
        ignore (Matrix.scale scale rotation);
        let z = (Matrix.row_vector rotation 0) in
        ignore (Vector.add z center);
        ()

    (*f is_key_down *)
    method is_key_down k = Intset.mem (int_of_char k) !keys_down

    (*f joystick_axis_value *)
    method joystick_axis_value a =
      let v = joystick_axes.(a) in
      if (v < -1024) then v else if (v > 1024) then v else 0

    (*f key - handle a keypress along the action vector *)
    method key action k meta vector =
      (match action with
         Key_action_press -> (keys_down := Intset.add k !keys_down)
       | Key_action_release -> (keys_down := Intset.remove k !keys_down)
      );
      None

    method joystick action which axis value options = 
      ( match action with
        | Joystick_action_axis -> (
          joystick_axes.(axis) <- value
        )
        | _ -> ()
      );
      None

    (*f mouse - handle a mouse action along the action vector *)
    method mouse action mouse vector options =
      let (cr,max_d) = vector in
      match (self#intersect_ray cr) with
        None -> None
      | Some k -> Some (k, fun a m v o -> 
    McbSome (fun a m v o -> McbNone)
    )

    (*f draw_content *)
    method draw_content view_set transformation =
      if (Option.is_none opt_material) then () else
      begin    
        let material = (Option.get opt_material) in
        ignore (Matrix.assign_from_q direction rotation);
        ignore (Matrix.identity translation);
        ignore (Matrix.set 0 3 (-. (Atcflib.Vector.get center 0)) translation);
        ignore (Matrix.set 1 3 (-. (Atcflib.Vector.get center 1)) translation);
        ignore (Matrix.set 2 3 (-. (Atcflib.Vector.get center 2)) translation);
        ignore (Matrix.assign_m_m rotation translation view);
        let ar_scale = (min (super#get_content_draw_dims).(0) (super#get_content_draw_dims).(1)) *. 0.35 *. !scale in
        ignore (Matrix.(set 1 1 ar_scale (set 0 0 ar_scale (identity tmp))));  (* Make -1/1 fit the width - but do not scale z *)
        ignore (Matrix.assign_m_m tmp view tmp2);  (* Make -1/1 fit the width - but do not scale z *)
        let other_uids = Ogl_gui.View.Ogl_view.set view_set (Some material) transformation in
        Gl.uniform_matrix4fv other_uids.(0) 1 true (Ogl_gui.Utils.ba_of_matrix4 tmp2); (* 0 -> V *)
        Gl.uniform_matrix4fv other_uids.(1) 1 true Ogl_gui.Utils.identity4; (* 1 -> M *)

        light.{0} <- 0.7 *. (sin angle);
        light.{1} <- 0.7 *. (cos angle);
        Gl.active_texture Gl.texture0 (* + shader *);
        Gl.bind_texture   Gl.texture_2d tex_glid;
        Gl.point_size 4.0;
        Gl.uniform3fv other_uids.(3) 1 ambient_col;
        Gl.uniform3fv other_uids.(4) 1 light;
        Gl.uniform3fv other_uids.(5) 1 light_col;
        Gl.cull_face Gl.back;
        (*Gl.enable Gl.cull_face_enum;*)
        Gl.uniform1i      other_uids.(2) 0 (* T0 = texture sampler 0 *);

        List.iter (fun o -> o#draw view_set other_uids) objs;
        Gl.bind_vertex_array 0;
      end

    (*f idle *)
    method idle _ = 
      if self # is_key_down ',' then self#move_forward ((-0.1) /. !scale);
      if self # is_key_down 'l' then self#move_forward (0.1 /. !scale);
      if self # is_key_down 'q' then self#move_left ((-0.01) /. !scale);
      if self # is_key_down 'w' then self#move_left (0.01 /. !scale);
      if self # is_key_down '.' then self#pitch 0.005;
      if self # is_key_down ';' then self#pitch (-0.005);
      if self # is_key_down 'x' then self#yaw 0.005;
      if self # is_key_down 'z' then self#yaw (-0.005);
      if self # is_key_down 's' then self#roll 0.005;
      if self # is_key_down 'a' then self#roll (-0.005);
      if self # is_key_down '\'' then scale := !scale *. 1.05;
      if self # is_key_down '/' then  scale := !scale /. 1.05;
      let v = self # joystick_axis_value 1 in
      if (v!=0) then self # move_forward ((float (-v)) /. 32768.0 /. 120.);
      let v = self # joystick_axis_value 0 in
      if (v!=0) then self # move_left ((float (-v)) /. 32768.0 /. 120.);
      let v = self # joystick_axis_value 2 in
      if (v!=0) then self # yaw ((float v) /. 32768.0 /. 40.);
      let v = self # joystick_axis_value 3 in
      if (v!=0) then self # pitch ((float v) /. 32768.0 /. 40.);
      if self # is_key_down '=' then None
      else
        (self#request_redraw ; Some 10)

end

