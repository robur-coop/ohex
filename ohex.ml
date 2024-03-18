
let string_fold f acc str =
  let st = ref acc in
  String.iter (fun c -> st := f !st c) str;
  !st

let is_space = function
  | ' ' | '\n' | '\r' | '\t' -> true
  | _ -> false

let digit = function
  | '0'..'9' as c -> int_of_char c - 0x30
  | 'A'..'F' as c -> int_of_char c - 0x41 + 10
  | 'a'..'f' as c -> int_of_char c - 0x61 + 10
  | _ -> invalid_arg "bad character"

let required_length ?(skip_whitespace = true) src =
  let req =
    string_fold (fun r c ->
        if skip_whitespace && is_space c then
          r
        else (
          ignore (digit c);
          succ r))
      0 src
  in
  if req mod 2 = 0 then
    req / 2
  else
    invalid_arg "leftover byte in hex string"

let decode_into ?(skip_whitespace = true) src tgt ?(off = 0) () =
  let fold f acc str =
    let st = ref acc in
    String.iter (fun c -> st := f !st c) str;
    !st
  in
  let chars, leftover =
    fold (fun (chars, leftover) c ->
        if skip_whitespace && is_space c then
          chars, leftover
        else
          let c = digit c in
          match leftover with
          | None -> chars, Some (c lsl 4)
          | Some c' -> (c' lor c) :: chars, None)
      ([], None) src
  in
  let chars = List.rev chars in
  if leftover <> None then
    invalid_arg "leftover byte in hex string";
  List.iteri (fun idx c -> Bytes.set_uint8 tgt (off + idx) c) chars

let decode ?(skip_whitespace = true) src =
  let len = required_length ~skip_whitespace src in
  let buf = Bytes.create len in
  decode_into ~skip_whitespace src buf ();
  Bytes.unsafe_to_string buf

let hex_map = "0123456789abcdef"

let encode_into src tgt ?(off = 0) () =
  String.iteri (fun idx c ->
      let hi, lo =
        let i = int_of_char c in
        hex_map.[i lsr 4], hex_map.[i land 0x0F]
      in
      Bytes.set tgt (idx * 2 + off) hi;
      Bytes.set tgt (idx * 2 + off + 1) lo)
    src

let encode src =
  let buf = Bytes.create (String.length src * 2) in
  encode_into src buf ();
  Bytes.unsafe_to_string buf

let printable_ascii c =
  let i = int_of_char c in
  not (i < 0x20 || i >= 0x7f)

let pp ppf s =
  String.iteri (fun idx c ->
      Format.fprintf ppf "%02x" (int_of_char c);
      if idx mod 2 = 1 then
        Format.pp_print_string ppf " ";
      if idx mod 8 = 7 then
        Format.pp_print_string ppf " ")
    s

let pp_hexdump ?(row_numbers = true) ?(chars = true) () ppf s =
  String.iteri (fun idx c ->
      if idx mod 16 = 0 && row_numbers then
        Format.fprintf ppf "%06x  " idx;
      Format.fprintf ppf "%02x" (int_of_char c);
      if idx mod 2 = 1 then
        Format.pp_print_string ppf " ";
      if idx mod 8 = 7 then
        Format.pp_print_string ppf " ";
      if idx mod 16 = 15 && chars then
        String.iter (fun c ->
            Format.pp_print_char ppf (if printable_ascii c then c else '.'))
          (String.sub s (idx - 15) 16);
      if idx mod 16 = 15 then
        Format.pp_print_string ppf "\n")
    s;
  (if chars then
     let last_n, pad =
       let l = String.length s in
       let pad = 16 - (l mod 16) in
       let pad = if pad = 16 then 0 else pad in
       String.sub s (l - (l mod 16)) (l mod 16),
       pad
     in
     if pad > 0 then
       let pad_chars = pad * 2 + (pad + 1) / 2 + (if pad > 8 then 1 else 0) + 1 in
       Format.pp_print_string ppf (String.make pad_chars ' ');
       String.iter (fun c ->
           Format.pp_print_char ppf (if printable_ascii c then c else '.'))
         last_n);
  if String.length s mod 16 <> 0 then
    Format.pp_print_string ppf "\n"
