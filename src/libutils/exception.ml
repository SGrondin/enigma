open! Core_kernel

let human = function
| Failure msg -> msg
| Lwt.Canceled -> "Timed out."

| Unix.Unix_error (c, n, p) ->
  sprintf {s|System Error "%s" during '%s("%s")'|s} (String.uppercase (Unix.error_message c)) n p

| unknown -> Exn.to_string unknown

let full ex = sprintf "%s\n%s" (human ex) (Backtrace.get () |> Backtrace.to_string)
