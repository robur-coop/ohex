(** Convert from and to hexadecimal representation. *)

val required_length : ?skip_whitespace:bool -> string -> int
(** [required_length ~skip_whitespace s] returns the length needed when the
    hex string [s] would be decoded into a sequence of octets. The argument
    [skip_whitespace] defaults to [true], and skips any whitespace characters
    (' ', '\n', '\r', '\t'). This function is useful for estimating the space
    required for [decode_into].

    @raise Invalid_argument if any character in [s] is not a hex character, or
    an odd amount of characters are present. *)

val decode : ?skip_whitespace:bool -> string -> string
(** [decode ~skip_whitespace s] decodes a hex string [s] into a sequence of
    octets. The argument [skip_whitespace] defaults to [true], and skips any
    whitespace characters in [s] (' ', '\n', '\r', '\t'). An example:
    [decode "4142" = "AB"].

    @raise Invalid_argument if any character in [s] is not a hex character, or
    an odd amount of characters are present. *)

val decode_into : ?skip_whitespace:bool -> string -> bytes -> ?off:int -> unit
  -> unit
(** [decode_into ~skip_whitespace s dst ~off ()] decodes [s] into [dst]
    starting at [off] (defaults to 0). The argument [skip_whitespace] defaults
    to [true] and skips any whitespace characters.

    @raise Invalid_argument if any character in [s] is not a hex character, an
    odd amount of characters are present, or [dst] does not contain enough
    space. *)

val encode : string -> string
(** [encode s] encodes [s] into a freshly allocated string of double size, where
    each character in [s] is encoded as two hex digits in the returned string.
    An example: [encode "AB" = "4142"].
*)

val encode_into : string -> bytes -> ?off:int -> unit -> unit
(** [encode_into s dst ~off ()] encodes [s] into [dst] starting at [off]
    (defaults to 0). Each character is encoded as two hex digits in [dst].

    @raise Invalid_argument if [dst] does not contain enough space. *)

val pp : ?row_numbers:bool -> ?chars:bool -> unit ->
  Format.formatter -> string -> unit
(** [pp ~row_numbers ~chars () ppf s] pretty-prints the string [s] in
    hexadecimal (similar to [hexdump -C]). If [row_numbers] is provided
    (defaults to [true]), each output line is prefixed with the row number.
    If [chars] is provided (defaults to [true]), in the last column the ASCII
    string is printed (non-printable characters are printed as '.'). *)
