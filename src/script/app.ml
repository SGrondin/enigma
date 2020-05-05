open! Core_kernel

let () = Lwt.async_exception_hook := (fun ex ->
    let open Lwt in
    Lwt_io.eprintlf "💀 UNCAUGHT EXCEPTION: %s" (Libutils.Exception.human ex)
    >>= (fun () -> exit 2)
    |> ignore
  )

(*
  This script reads a file passed by argument and prints it on stdout.
  Use this file as a starting point for projects that require any IO.
*)

let main = function
| [| _; filename |] ->
  let%lwt bytes = Lwt_io.with_file ~flags:Unix.[O_RDONLY; O_NONBLOCK] ~mode:Input filename (fun ic ->
      Lwt_io.read ic
    )
  in
  Lwt_io.printl bytes

| [| _ |] -> failwith "You must pass at least one file argument"
| _ -> failwith "Too many arguments"

let () =
  Lwt_main.run (
    try%lwt
      main Sys.argv
    with
    | (Failure _ as ex) | (Unix.Unix_error _ as ex) | (Exn.Reraised _ as ex) ->
      let message = Libutils.Exception.human ex in
      let%lwt () = Lwt_io.eprintlf "❌ An error occured:\n%s" message in
      exit 1
  )
