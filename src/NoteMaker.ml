let twopi = 8. *. (atan 1.)

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

let make_data a f samples osci =
  let freq = ref 0 and
      phase = ref 0.
  in
  for i = 1 to samples do
    begin
      freq := make_short (osci a !phase);
      phase := mod_float (!phase +. (twopi *. f /. 44100.)) twopi;
      print_char (Char.chr (!freq land 0x00FF));
      print_char (Char.chr ((!freq land 0xFF00) / 0x100));
    end
  done

let rec make_data_list a phase_l osci =
  match phase_l with
  | [] -> []
  | e::l -> (osci a e)::(make_data_list a l osci)

let get_average f_l =
  let rec sub lst =
    match lst with
    | [] -> 0.0
    | e::l -> e +. (sub l)
  in
    (1. /. (sqrt (float_of_int (List.length f_l)))) *. (sub f_l)

let make_mix a phase_l osci =
  get_average (make_data_list a phase_l osci)

let print_data a =
  let aa = make_short a
  in
    print_char (Char.chr (aa land 0xFF));
    print_char (Char.chr ((aa land 0xFF00) / 0x100))
