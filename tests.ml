
let tests = [
  "", 0, "";
  "41", 1, "A";
  "41 41", 2, "AA";
  " 41 41 ", 2, "AA";
  "   414  1", 2, "AA";
]

let len_dec_tests =
  List.mapi (fun i (s, len, v) ->
      string_of_int i ^ " is correct", `Quick,
      (fun () ->
         Alcotest.(check int "required length" len (Ohex.required_length s));
         Alcotest.(check string "decode works fine" v (Ohex.decode s))))
    tests

let bad_char_input = [ "W" ; "AAWW" ; "WWAA" ]

let leftover_input = [ "AAA" ; "A" ]

let bad_input_ws = [ " "; " AA" ; "AA " ; "A A" ]

let bad_len_dec_tests =
  (List.mapi (fun i s ->
       string_of_int i ^ " fails (bad character)", `Quick,
       (fun () ->
          Alcotest.(check_raises "required length raises"
                      (Invalid_argument "bad character")
                      (fun () -> ignore (Ohex.required_length s)));
          Alcotest.(check_raises "decode raises"
                      (Invalid_argument "bad character")
                      (fun () -> ignore (Ohex.decode s)))))
      bad_char_input) @
  (List.mapi (fun i s ->
       string_of_int i ^ " fails (leftover)", `Quick,
       (fun () ->
          Alcotest.(check_raises "required length raises"
                      (Invalid_argument "leftover byte in hex string")
                      (fun () -> ignore (Ohex.required_length ~skip_whitespace:false s)));
          Alcotest.(check_raises "decode raises"
                      (Invalid_argument "leftover byte in hex string")
                      (fun () -> ignore (Ohex.decode ~skip_whitespace:false s)))))
      leftover_input) @
  (List.mapi (fun i s ->
       string_of_int i ^ " fails (skip_whitespace = false)", `Quick,
       (fun () ->
          Alcotest.(check_raises "required length raises"
                      (Invalid_argument "bad character")
                      (fun () -> ignore (Ohex.required_length ~skip_whitespace:false s)));
          Alcotest.(check_raises "decode raises"
                      (Invalid_argument "bad character")
                      (fun () -> ignore (Ohex.decode ~skip_whitespace:false s)))))
      bad_input_ws)

let dec_enc () =
  let random_string () =
    let size = Random.int 128 in
    let buf = Bytes.create size in
    for i = 0 to size - 1 do
      Bytes.set_uint8 buf i (Random.int 256)
    done;
    Bytes.unsafe_to_string buf
  in
  for i = 0 to 10_000 do
    let input = random_string () in
    Alcotest.(check string ("dec (enc s) = s " ^ string_of_int i)
                input Ohex.(decode (encode input)));
    Alcotest.(check string ("dec ~skip_ws:false (enc s) = s " ^ string_of_int i)
                input Ohex.(decode ~skip_whitespace:false (encode input)));
    let buf = Bytes.create (String.length input * 2) in
    Ohex.encode_into input buf ~off:0 ();
    let out = Bytes.create (String.length input) in
    Ohex.decode_into (Bytes.unsafe_to_string buf) out ~off:0 ();
    Alcotest.(check string ("dec_into (enc_into s) = s " ^ string_of_int i)
                input (Bytes.unsafe_to_string out))
  done

let suites = [
  "length and decode pass", len_dec_tests ;
  "bad input", bad_len_dec_tests ;
  "decode encode", [ "decode (encode s) = s", `Quick, dec_enc ];
]

let () = Alcotest.run "hex tests" suites
