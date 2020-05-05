open! Core_kernel

include S.Cipher_base

let create () =
  let cipher = Array.init 26 ~f:Fn.id in
  let rec self = {
    stringify = stringify "Passthrough" cipher;
    apply = Array.get cipher;
    advance = lazy self;
  }
  in
  self
