let pi = 4. *. (atan 1.)

let get_freq a f t =
  a *. (sin (2. *. pi *. f *. t))

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

let rec make_data f ct samples =
  match ct with
  | 44100. -> []
  | t -> (make_short (get_freq 1000. f (ct *. 1. /. 44100.)))::(make_data f (ct +. 1.) samples)
