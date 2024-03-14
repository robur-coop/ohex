## oHEX

This package with minimal dependency cone provides functionality to decode and
encode strings into hexadecimal representation.

As example, `Ohex.decode "4142" = "AB"`. And `Ohex.encode "AB" = "4142"`.

There's even the property, for all strings s: `Ohex.(decode (encode s)) = s`.

A pretty-printer is provided as well.
