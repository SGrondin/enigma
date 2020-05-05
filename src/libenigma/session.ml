open! Core_kernel

type t = {
  ciphers: S.Cipher_base.t array;
}

let create ciphers =
  { ciphers }

let accept session ch =
  if not (Char.is_alpha ch) then None else
  let init = Char.to_int (Char.uppercase ch) - 65 in
  let scrambled = Array.fold session.ciphers ~init ~f:(fun i cipher ->
      let open S.Cipher_base in
      print_endline (sprintf "%c > %s" (i + 65 |> Char.of_int_exn) (cipher.stringify ~selected:(Some i)));
      cipher.apply i
    )
  in
  scrambled + 65 |> Char.of_int_exn |> Option.return

let scramble session input =
  let buf = Buffer.create (String.length input) in
  String.iter input ~f:(fun c ->
    begin match accept session c with
    | None -> ()
    | Some x -> Buffer.add_char buf x
    end
  );
  Buffer.contents buf
