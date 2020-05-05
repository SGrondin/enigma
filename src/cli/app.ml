open! Core_kernel

open Libenigma



let () =
  let plugboard = Plugboard.create [
      'A','E';
      'B','F';
      'C','M';
      'D','Q';
      'H','U';
      'J','N';
      'L','X';
      'P','R';
      'S','Z';
      'V','W';
    ]
  in

  let session = Session.create [|
      plugboard;
      Rotor.III.forward ~ring:'A' ~position:0;
      Rotor.II.forward ~ring:'A' ~position:0;
      Rotor.I.forward ~ring:'A' ~position:0;
      (* Rotor.Beta.forward ~ring:'C' ~position:25; *)
      Reflector.B.reflector;
      (* Rotor.Beta.backward ~ring:'C' ~position:25; *)
      Rotor.I.backward ~ring:'A' ~position:0;
      Rotor.II.backward ~ring:'A' ~position:0;
      Rotor.III.backward ~ring:'A' ~position:0;
      plugboard;
    |]
  in

  let scrambled = Session.scramble session "X" in
  print_endline scrambled;
  Session.scramble session scrambled |> print_endline
