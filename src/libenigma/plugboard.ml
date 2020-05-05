open! Core_kernel

include S.Cipher_base

let rec make cipher =
  {
    stringify = stringify "Plugboard" cipher;
    apply = Array.get cipher;
    advance = lazy (make cipher);
  }

let create pairs =
  let check key set =
    begin match Char.Map.add set ~key ~data:() with
    | `Ok x -> x
    | `Duplicate -> failwithf "Invalid plugboard: '%c' is duplicated" key ()
    end
  in
  let cipher = Array.init 26 ~f:Fn.id in
  let _acc = List.fold pairs ~init:Char.Map.empty ~f:(fun acc (left, right) ->
      let x = Char.to_int left - 65 in
      let y = Char.to_int right - 65 in
      (try Array.set cipher x y with _ -> failwithf "Invalid plugboard entry: '%c'" left ());
      (try Array.set cipher y x with _ -> failwithf "Invalid plugboard entry: '%c'" right ());
      check left acc |> check right
    )
  in
  make cipher
