(*a Stuff *)
let sfmt=Printf.sprintf
(*a Types *)
(*m Obj_sig *)
module type Obj_sig =
sig
  type t
  val add_vertex  : t -> float array -> bool
  val add_normal  : t -> float array -> bool
  val add_texture : t -> float array -> bool
  val add_line    : t -> int array -> bool
  val add_face    : t -> (int*int*int) array -> bool
end

(*a Modules *)
(*m ObjReader *)
module ObjReader(Obj:Obj_sig) = 
struct
  (*t t struct *)
  type t = Obj.t

  (*f is_newline *)
  let is_newline ch    = (ch=='\n')

  (*f is_whitespace *)
  let is_whitespace ch = (ch==' ') || (ch=='\t')

  (*f skip_past_newline *)
  let rec skip_past_newline l ofs len =
    if (ofs>=len) then ofs 
    else if is_newline l.[ofs] then (ofs+1)
    else skip_past_newline l (ofs+1) len

  (*f skip_whitespace *)
  let rec skip_whitespace l ofs len =
    if ofs>=len then ofs
    else if is_whitespace l.[ofs] then (skip_whitespace l (ofs+1) len)
    else ofs

  (*f get_token *)
  let get_token l ofs len =
    let ofs = skip_whitespace l ofs len in
    if ofs>=len then None
    else if is_newline l.[ofs] then None
    else (
      let rec next_char acc ofs =
        if ofs>=len then (acc,ofs) else (
          let ch = l.[ofs] in
          if is_whitespace ch then (acc,ofs)
          else if is_newline ch then (acc,ofs)
          else next_char (ch::acc) (ofs+1)
        )
      in
      let (rev_chars, end_ofs) = next_char [] ofs in
    let l = end_ofs-ofs in
    let b = Bytes.make l '0' in
    let rec from_rev_list l i =
      match l with
      | [] -> (Bytes.unsafe_to_string b)
      | hd::tl -> (
        Bytes.set b i hd;
        from_rev_list tl (i-1)
      )
    in
    Some (from_rev_list rev_chars (l-1), end_ofs)
    )

  (*f parse_tokens *)
  let rec parse_tokens acc l ofs len =
    match get_token l ofs len with
    | None -> (acc, skip_past_newline l ofs len)
    | Some (token, ofs) -> (
      if token.[0]='#' then (acc, skip_past_newline l ofs len)
      else (
        let acc = token :: acc in
        parse_tokens acc l ofs len
      )
    )

  (*f tokenize_line *)
  let tokenize_line l ofs len =
    let (rev_tokens, ofs) = parse_tokens [] l ofs len in
    (List.rev rev_tokens, ofs)

  (*f parse_floats *)
  let parse_floats tokens ofs num =
    let floats = Array.create_float num in
    let rec skip_tokens l n =
      if (n=0) then l else (
        match l with [] -> [] | hd::tl -> (skip_tokens tl (n-1))
      )
    in
    let tokens = skip_tokens tokens ofs in
    let rec parse_next_float tokens i =
      match tokens with
     | [] -> ()
     | hd::tl -> (
      floats.(i) <- float_of_string hd ;
      parse_next_float tl (i+1)
     )
    in
    parse_next_float tokens 0;
    floats

  (*f parse_indices *)
  let parse_indices tokens ofs num =
    let a = Array.create num 0 in
    let rec skip_tokens l n =
      if (n=0) then l else (
        match l with [] -> [] | hd::tl -> (skip_tokens tl (n-1))
      )
    in
    let tokens = skip_tokens tokens ofs in
    let rec parse_next_int tokens i =
      match tokens with
     | [] -> ()
     | hd::tl -> (
      a.(i) <- int_of_string hd ;
      parse_next_int tl (i+1)
     )
    in
    parse_next_int tokens 0;
    a

  (*f parse_line *)
  let parse_line t l ofs len =
    let (tokens, ofs) = tokenize_line l ofs len in
    let num_tokens = List.length tokens in
    if num_tokens=0 then (None,ofs)
    else (
      let cmd_token = List.hd tokens in
      if cmd_token="v" then (
        if (Obj.add_vertex t (parse_floats tokens 1 (num_tokens-1))) then
          (None, ofs)
        else
          (Some "Failed to parse vertex", ofs)
      )
      else if cmd_token="vt" then (
        if (Obj.add_texture t (parse_floats tokens 1 (num_tokens-1))) then
          (None, ofs)
        else
          (Some "Failed to parse texture", ofs)
      )
      else if cmd_token="vn" then (
        if (Obj.add_normal t (parse_floats tokens 1 (num_tokens-1))) then
          (None, ofs)
        else
          (Some "Failed to parse normal", ofs)
      )
      else if cmd_token="l" then (
        if (Obj.add_line t (parse_indices tokens 1 (num_tokens-1))) then
          (None, ofs)
        else
          (Some "Failed to parse line", ofs)
      )
      else (Some (sfmt "Bad token '%s'" cmd_token),ofs)
    )

  (*f parse_file *)
  let parse_file t f =
    let n = ref 0 in
    try (
      while true do
        n := !n + 1;
        let l = input_line f in
        let len = String.length l in
        match (parse_line t l 0 len) with
        | (None, _) -> ()
        | (Some s, _) -> Printf.printf "Error in obj file at line %d: '%s'\n" !n s;
      done
    ) with
    | End_of_file -> ()

  (*f All done *)
end
