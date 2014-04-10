let melody1 =
  ("MI", 2.)::
  ("RE", 2.)::
  ("MI", 2.)::
  ("RE", 2.)::
  ("MI", 2.)::
  ("RE", 2.)::
  ("MI", 2.)::
  ("RE", 2.)::
  []

let melody2 =
  ("SI", 2.)::
  ("LA", 2.)::
  ("SI", 2.)::
  ("SOL", 2.)::
  ("LA", 2.)::
  ("MI", 2.)::
  ("SOL", 2.)::
  ("RE", 2.)::
  []

let generate_wav a l osci =
  WavWriter.print_header (2 * (List.length l) * 44100);
  MelodyMaker.play_melody_poly a melody1 melody2 44100 osci

let _ =
  generate_wav  (float_of_string Sys.argv.(1)) melody1 Oscillators.square
