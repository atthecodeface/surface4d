let sfmt = Printf.sprintf
(*m Vector *)
module Vector =
struct
   let epsilon = 1E-10
   let add ?s0:(s0=1.0) ?s1:(s1=1.0) p0 p1 =
    Array.map2 (fun c0 c1 -> (s0 *. c0) +. (s1 *. c1)) p0 p1

  let mod2 p =
    Array.fold_left (fun acc c -> acc +. (c *. c)) 0. p

  let tiny p = mod2 p < 0.01

  let length p0 p1 =
    let l2 = ref 0. in
    Array.iter2 (fun c0 c1 -> let d=(c1 -. c0) in (l2 := !l2 +. (d *. d))) p0 p1;
    sqrt !l2

  let dot p0 p1 =
    let v = ref 0. in
    Array.iter2 (fun c0 c1 -> v := !v +. (c0 *. c1)) p0 p1;
    !v

  let scale p s =
    Array.map (fun c0 -> c0 *. s) p

  let normalize p =
    let l = sqrt (mod2 p) in
    let s = if l < epsilon then 1. else (1. /. l) in
    Array.map (fun c0 -> c0 *. s) p

  let remove v perp =
    (* v = k * perp + v'; hence v.perp = k*(mod2 perp); k = v.perp / (mod2 perp)
      Then v' = v -  * perp
     *)
    let l2 = sqrt (mod2 perp) in
    let k = (dot v perp) /. l2 in
    let r = add ~s1:(-1. *. k) v perp in
    (*let k2 = dot r perp in
    Printf.printf "after remove dot is %g\n" k2;*)
    r
    
  let str p =
    Array.fold_left (fun acc c -> sfmt "%s, %f" acc c) "" p
end

let str_vec = Vector.str

module type Surface_sig =
sig
    val vector : float -> float -> float array
    val grad : float -> float -> (float array * float array)
end

module View(Surface:Surface_sig) =
struct
  type t = {
    mutable t: float;
    mutable u: float;
    mutable theta : float;
    mutable moved : bool;
    mutable pos : float array;
    mutable fwd : float array;
    mutable left : float array;
    mutable up0 : float array;
    mutable up1 : float array;
    }

  let str t =
    sfmt "%f,%f,%f : %s :   %s  %s  %s  %s" t.t t.u t.theta (str_vec t.pos) (str_vec t.fwd) (str_vec t.left) (str_vec t.up0) (str_vec t.up1)

  let update_fwd_left t = 
    let (dt, du) = Surface.grad t.t t.u in
    let st = sin (t.theta) in
    let ct = cos (t.theta) in
    t.fwd  <- Vector.(normalize (add ~s0:ct ~s1:st dt du));
    t.left <- Vector.(normalize (add ~s0:((-1.) *. st) ~s1:ct dt du));
    t.left <- Vector.remove t.left t.fwd;
    ()

  let tidy_up0 t = 
    let up0 = Vector.remove t.up0 t.left in
    let up0 = Vector.remove up0 t.fwd in
    t.up0 <- Vector.normalize up0

  let tidy_up1 t = 
    let up1 = Vector.remove t.up1 t.left in
    let up1 = Vector.remove up1 t.fwd in
    let up1 = Vector.remove up1 t.up0 in
    t.up1 <- Vector.normalize up1

  let calc_pos t u = Surface.vector t u

  let turn t dtheta = t.theta <- t.theta +. dtheta; t.moved <- true

  let resolve t =
    t.pos <- calc_pos t.t t.u;    
    update_fwd_left t;
    tidy_up0 t;
    tidy_up1 t;
    t.moved <- false;
    ()

  let resolve_if_moved t =
    if t.moved then (resolve t)

  let spin t dtheta = 
    resolve_if_moved t;
    let st = sin (dtheta) in
    let ct = cos (dtheta) in
    let up0 = Vector.add ~s0:ct ~s1:(-1. *. st) t.up0 t.up1 in
    let up1 = Vector.add ~s0:ct ~s1:(1. *. st)  t.up1 t.up0 in
    t.up0 <- up0;
    t.up1 <- up1;
    t.moved <- true    

  let v0 = [| 1.;2.;3.;4.; |]
  let v1 = [| 1.;4.;9.;16.; |]
  let v2 = [| 1.;16.;9.;4.; |]
  let v3 = [| 1.;8.;27.;64.; |]
  let create t u theta =
    let t = { t; u; theta; pos=[||]; moved=true; fwd=[||]; left=[||]; up0=v0; up1=v1; } in
    resolve t;
    if (Vector.tiny t.up0) then (t.up0 <- v2; resolve t);
    if (Vector.tiny t.up0) then (t.up0 <- v3; resolve t);
    if (Vector.tiny t.up1) then (t.up1 <- v2; resolve t);
    if (Vector.tiny t.up1) then (t.up1 <- v3; resolve t);
    Printf.printf "created %s" (str t);
    t

  let bound t cb =
    match cb t.t t.u with
    | None -> ()
    | Some (nt,nu) -> (t.t <- nt; t.u <- nu; t.moved <- true;)

  let move t df dl = 
    let st = sin (t.theta) in
    let ct = cos (t.theta) in
    let nt = t.t +. df *. ct -. dl *. st in
    let nu = t.u +. dl *. ct +. df *. st in
    t.t <- nt; 
    t.u <- nu;
    t.moved <- true;
    ()

  (*f ba_world_to_view - matrix/translation to apply to world vector to get view-centric

    Note that a view-centric vector (f l u0 u1) has world position 
      (pos t) + f.(fwd t) + l.(left t) + ...

    And note that f.l=0, etc

    Hence the matrix M with column vectors fwd left up0 up1 has a transpose that is its inverse
    (as M.Mt is equivalent to dot products of the vectors)

    Note that M . (f l u0 u1) + (pos t) gives the world position, hence

    Hence Mt . (w - (pos t)) gives the view-relative position in (f l u0 u1) axes

   *)
  let ba_world_to_view t ba_t ba_m = 
    resolve_if_moved t;
    Array.iteri (fun i v -> ba_t.{i} <- (-1.)*.v) t.pos;
    Array.iteri (fun i v -> ba_m.{i+ 0} <- v) t.fwd;
    Array.iteri (fun i v -> ba_m.{i+ 4} <- v) t.left;
    Array.iteri (fun i v -> ba_m.{i+ 8} <- v) t.up0;
    Array.iteri (fun i v -> ba_m.{i+12} <- v) t.up1;
    ()

  let ba_world_to_view_cm t ba_t ba_m = 
    resolve_if_moved t;
    Array.iteri (fun i v -> ba_t.{i} <- (-1.)*.v) t.pos;
    Array.iteri (fun i v -> ba_m.{4*i+0} <- v) t.fwd;
    Array.iteri (fun i v -> ba_m.{4*i+1} <- v) t.left;
    Array.iteri (fun i v -> ba_m.{4*i+2} <- v) t.up0;
    Array.iteri (fun i v -> ba_m.{4*i+3} <- v) t.up1;
    ()

  let ba_view_to_world t ba_t ba_m = 
    resolve_if_moved t;
    Array.iteri (fun i v -> ba_t.{i} <- v) t.pos;
    Array.iteri (fun i v -> ba_m.{4*i+0} <- v) t.fwd;
    Array.iteri (fun i v -> ba_m.{4*i+1} <- v) t.left;
    Array.iteri (fun i v -> ba_m.{4*i+2} <- v) t.up0;
    Array.iteri (fun i v -> ba_m.{4*i+3} <- v) t.up1;
    ()

end

module Surface_mobius =
struct
  let pi = 3.141592653 
  let pi2 = 2. *. pi
  let pi4 = 4. *. pi
  let vector t u =
    let w = u *. u in
    let w = u in
    let x = cos (t *. pi4) in
    let y = sin (t *. pi2) *. u in
    let z = cos (t *. pi2) *. u in
    [| w; x; y; z; |]

  let grad t u =
    let wxyz = vector t u in
    let dwdt = 0. in
    let dxdt = (-1.) *. pi4 *. sin (pi4 *. t) in
    let dydt = pi2 *. ( 1.) *. cos (t *. pi2) *. u in
    let dzdt = pi2 *. (-1.) *. sin (t *. pi2) *. u in
    let dwdu = 2. *. u in
    let dwdu = 1. in
    let dxdu = 0. in
    let dydu = sin (t *. pi2) in
    let dzdu = cos (t *. pi2) in
    let du = [| dwdu; dxdu; dydu; dzdu |] in
    let dt = [| dwdt; dxdt; dydt; dzdt |] in
    let dt = Vector.remove dt du in
    let dt = if (abs_float (Vector.dot du dt))<1E-8 then dt else (
      Printf.printf "Hack required %f %f   %s   %s\n" t u (Vector.str dt) (Vector.str du); dt
    ) in
    (dt, du)
end

module Surface_loop =
struct
  let pi = 3.141592653 
  let pi2 = 2. *. pi
  let pi4 = 4. *. pi

  let vector t u =
    let w = u in
    let x = u in
    let y = sin (t *. pi2) in
    let z = cos (t *. pi2) in
    [| w; x; y; z; |]

  let grad t u =
    let dwdt = 0. in
    let dxdt = 0. in
    let dydt = cos (t *. pi2) in
    let dzdt = (-1.) *. (sin (t *. pi2)) in
    let dwdu = 1. in
    let dxdu = 1. in
    let dydu = 0. in
    let dzdu = 0. in
    let du = [| dwdu; dxdu; dydu; dzdu |] in
    let dt = [| dwdt; dxdt; dydt; dzdt |] in
    let dt = Vector.remove dt du in
    let dt = if (abs_float (Vector.dot du dt))<1E-8 then dt else (
      Printf.printf "Hack required %f %f   %s   %s\n" t u (Vector.str dt) (Vector.str du); dt
    ) in
    (dt, du)
end

