let twopi = 8. *. (atan 1.)

let get_freq a p =
  a *. (sin (p))

let make_short nb =
  let tmp = ref 0 and
      nb2 = int_of_float nb
  in
    tmp := (nb2 land 0xFF00) / 0x100;
    tmp := !tmp lor ((nb2 land 0x00FF) * 0x100);
    begin
    if nb2 < 0 then
      tmp := !tmp lor 0x0080
    end;
    !tmp

let make_data a f samples =
  let freq = ref 0 and
      phase = ref 0.
  in
  for i = 1 to samples do
    begin
      freq := make_short (get_freq a !phase);
      phase := mod_float (!phase +. (twopi *. f /. 44100.)) twopi;
      print_char (Char.chr (!freq land 0x00FF));
      print_char (Char.chr ((!freq land 0xFF00) / 0x100));
    end
  done
