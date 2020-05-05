open! Core_kernel


module Cipher_base = struct
  type t = {
    stringify: selected:int option -> string;
    apply: int -> int;
    advance: t Lazy.t;
  }

  let cipher_to_string ?selected cipher =
    Array.fold_right cipher ~init:((Array.length cipher - 1), []) ~f:(fun x (i, acc) ->
      let c = x + 65 |> Char.of_int_exn in
      let next = begin match selected with
      | Some select when i = select -> '('::c::')'::acc
      | None | Some _ -> c::acc
      end
      in
      (pred i), next
    )
    |> snd
    |> String.of_char_list

  let stringify name cipher ~selected =
    sprintf "%s    %s" (cipher_to_string ?selected cipher) name

  let to_string cipher = cipher.stringify ~selected:None
end

module type Cipher = sig
  type t = Cipher_base.t
end

module type Passthrough = sig
  include Cipher

  val create: unit -> t
end

module type Plugboard = sig
  include Cipher

  val create: (char * char) list -> t
end

module type Rotor = sig
  include Cipher
  module I : sig
    val forward: ring:char -> position:int -> t
    val backward: ring:char -> position:int -> t
  end
  module II : sig
    val forward: ring:char -> position:int -> t
    val backward: ring:char -> position:int -> t
  end
  module III : sig
    val forward: ring:char -> position:int -> t
    val backward: ring:char -> position:int -> t
  end
  module IV : sig
    val forward: ring:char -> position:int -> t
    val backward: ring:char -> position:int -> t
  end
  module V : sig
    val forward: ring:char -> position:int -> t
    val backward: ring:char -> position:int -> t
  end
  module VI : sig
    val forward: ring:char -> position:int -> t
    val backward: ring:char -> position:int -> t
  end
  module VII : sig
    val forward: ring:char -> position:int -> t
    val backward: ring:char -> position:int -> t
  end
  module VIII : sig
    val forward: ring:char -> position:int -> t
    val backward: ring:char -> position:int -> t
  end
  module Beta : sig
    val forward: ring:char -> position:int -> t
    val backward: ring:char -> position:int -> t
  end
  module Gamma : sig
    val forward: ring:char -> position:int -> t
    val backward: ring:char -> position:int -> t
  end
end

module type Reflector = sig
  include Cipher
  module A : sig
    val reflector: t
  end
  module B : sig
    val reflector: t
  end
  module C : sig
    val reflector: t
  end
  module B_Thin : sig
    val reflector: t
  end
  module C_Thin : sig
    val reflector: t
  end
end
