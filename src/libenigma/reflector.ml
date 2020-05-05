open! Core_kernel

include S.Cipher_base

let rec make name cipher =
  {
    stringify = stringify name cipher;
    apply = Array.get cipher;
    advance = lazy (make name cipher);
  }

let create name wiring =
  let cipher = wiring |> String.to_array |> Array.map ~f:(fun c -> Char.to_int c - 65) in
  make name cipher

module A = struct
  let reflector = create "Reflector A" "EJMZALYXVBWFCRQUONTSPIKHGD"
end

module B = struct
  let reflector = create "Reflector B" "YRUHQSLDPXNGOKMIEBFZCWVJAT"
end

module C = struct
  let reflector = create "Reflector C" "FVPJIAOYEDRZXWGCTKUQSBNMHL"
end

module B_Thin = struct
  let reflector = create "Reflector B Thin" "ENKQAUYWJICOPBLMDXZVFTHRGS"
end

module C_Thin = struct
  let reflector = create "Reflector C Thin" "RDOBJNTKVEHMLFCWZAXGYIPSUQ"
end
