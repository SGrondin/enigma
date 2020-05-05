open! Core_kernel

include S.Cipher_base

type rotor = {
  name: string;
  cipher: int array;
  notches: int array;
  position: int;
}

let rec make ({ name; cipher; notches = _; position } as rotor) =
  {
    stringify = stringify (sprintf "%s [%c]" name (position + 65 |> Char.of_int_exn)) cipher;
    apply = (fun i ->
      Array.get cipher ((i + position) mod 26)
    );
    advance = lazy (make { rotor with position = position + 1});
  }

let create_one name wiring notches ~ring ~position =
  if not (Char.is_uppercase ring) then failwithf "Invalid ring setting for: %s" name ();
  let ringsetting = Char.to_int ring - 65 in
  let arr = wiring |> String.to_array |> Array.map ~f:(fun c -> Char.to_int c - 65) in
  let cipher = if ringsetting = 0 then arr
    else Array.append (Array.slice arr ringsetting 0) (Array.slice arr 0 ringsetting)
  in
  let rotor = {
    name;
    cipher;
    notches = notches |> Array.map ~f:(fun c -> Char.to_int c - 65);
    position;
  }
  in
  make rotor

let invert wiring =
  let arr = wiring |> String.to_array |> Array.map ~f:(fun c -> Char.to_int c - 65) in
  let inverted = Array.create ~len:26 "A" in
  Array.iteri arr ~f:(fun i c ->
    Array.set inverted c (i + 65 |> Char.of_int_exn |> Char.to_string)
  );
  String.concat_array inverted

let create name wiring notches =
  let forward = create_one (sprintf "<<< %s" name) wiring notches in
  let backward = create_one (sprintf ">>> %s" name) (invert wiring) notches in
  forward, backward

module I = struct
  let forward, backward = create "Rotor I" "EKMFLGDQVZNTOWYHXUSPAIBRCJ" [|'R'|]
end

module II = struct
  let forward, backward = create "Rotor II" "AJDKSIRUXBLHWTMCQGZNPYFVOE" [|'F'|]
end

module III = struct
  let forward, backward = create "Rotor III" "BDFHJLCPRTXVZNYEIWGAKMUSQO" [|'W'|]
end

module IV = struct
  let forward, backward = create "Rotor IV" "ESOVPZJAYQUIRHXLNFTGKDCMWB" [|'K'|]
end

module V = struct
  let forward, backward = create "Rotor V" "VZBRGITYUPSDNHLXAWMJQOFECK" [|'A'|]
end

module VI = struct
  let forward, backward = create "Rotor VI" "JPGVOUMFYQBENHZRDKASXLICTW" [|'A'; 'N'|]
end

module VII = struct
  let forward, backward = create "Rotor VII" "NZJHGRCXMYSWBOUFAIVLPEKQDT" [|'A'; 'N'|]
end

module VIII = struct
  let forward, backward = create "Rotor VIII" "FKQHTLXOCBJSPDZRAMEWNIUYGV" [|'A'; 'N'|]
end

module Beta = struct
  let forward, backward = create "Rotor Beta" "LEYJVCNIXWPBQMDRTAKZGFUHOS" [||]
end

module Gamma = struct
  let forward, backward = create "Rotor Gamma" "FSOKANUERHMBTIYCWLQPZXVGJD" [||]
end
