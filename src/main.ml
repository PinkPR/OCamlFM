let generate_wav a f t =
  WavWriter.print_header (2 * t * 44100);
  NoteMaker.make_data a f (t * 44100)

let _ =
  generate_wav  (float_of_string Sys.argv.(1))
                (float_of_string Sys.argv.(2))
                (int_of_string Sys.argv.(3))
