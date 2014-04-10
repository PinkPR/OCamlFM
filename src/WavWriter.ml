let lil_endian nb =
  let tmp = ref 0
  in
    tmp := (nb land 0xFF000000) / 0x1000000;
    tmp := !tmp lor ((nb land 0x00FF0000) / 0x100);
    tmp := !tmp lor ((nb land 0x0000FF00) * 0x100);
    tmp := !tmp lor ((nb land 0x000000FF) * 0x1000000);
    !tmp

let print_chunk0 chunk2size =
  (* ChunkID *)
  print_string "RIFF";
  (* ChunkSize *)
  print_char (Char.chr ((((36 + chunk2size)) land 0x0000FF)));
  print_char (Char.chr ((((36 + chunk2size)) land 0x0000FF00) / 0x100));
  print_char (Char.chr ((((36 + chunk2size)) land 0x00FF0000) / 0x10000));
  print_char (Char.chr ((((36 + chunk2size)) land 0xFF000000) / 0x1000000));
  (* FORMAT *)
  print_string "WAVE"

let print_chunk1 () =
  (* fmt *)
  print_string "fmt ";
  (* Subchunk1Size *)
  print_char (Char.chr 0x10);
  print_char (Char.chr 0);
  print_char (Char.chr 0);
  print_char (Char.chr 0);
  (* AudioFormat *)
  print_char (Char.chr 1);
  print_char (Char.chr 0);
  (* NumChannels *)
  print_char (Char.chr 1);
  print_char (Char.chr 0);
  (* SampleRate *)
  print_char (Char.chr (((44100) land 0x0000FF)));
  print_char (Char.chr ((((44100)) land 0x0000FF00) / 0x100));
  print_char (Char.chr (((44100 land 0x00FF0000) / 0x10000)));
  print_char (Char.chr ((((44100)) land 0xFF000000) / 0x1000000));
  (* ByteRate *)
  print_char (Char.chr ((((2 * 44100)) land 0x0000FF)));
  print_char (Char.chr ((((2 * 44100)) land 0x0000FF00) / 0x100));
  print_char (Char.chr ((((2 * 44100)) land 0x00FF0000) / 0x10000));
  print_char (Char.chr ((((2 * 44100)) land 0xFF000000) / 0x1000000));
  (* BlockAlign *)
  print_char (Char.chr 2);
  print_char (Char.chr 0);
  (* BitsPerSample *)
  print_char (Char.chr 0x10);
  print_char (Char.chr 0)

let rec print_datas datas ct =
  match datas with
  | [] -> print_string ""
  | data::l ->  (print_char (Char.chr (data land 0x00FF));
                print_char (Char.chr ((data land 0xFF00) / 0x100));
                print_datas l (ct + 1))

let print_chunk2 chunk2size =
  (* data *)
  print_string "data";
  (* SubChunk2Size *)
  print_char (Char.chr ((((chunk2size)) land 0x0000FF)));
  print_char (Char.chr ((((chunk2size)) land 0x0000FF00) / 0x100));
  print_char (Char.chr ((((chunk2size)) land 0x00FF0000) / 0x10000));
  print_char (Char.chr ((((chunk2size)) land 0xFF000000) / 0x1000000))

let print_header chunk2size =
  print_chunk0 chunk2size;
  print_chunk1 ();
  print_chunk2 chunk2size
