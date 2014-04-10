let get_freq note oct =
  match note with
  | "DO" -> 65.41 *. (2. ** (oct -. 1.))
  | "DOd" -> 69.30 *. (2. ** (oct -. 1.))
  | "RE" -> 73.42 *. (2. ** (oct -. 1.))
  | "REd" -> 77.78 *. (2. ** (oct -. 1.))
  | "MI" -> 82.41 *. (2. ** (oct -. 1.))
  | "FA" -> 87.31 *. (2. ** (oct -. 1.))
  | "FAd" -> 92.50 *. (2. ** (oct -. 1.))
  | "SOL" -> 98.0 *. (2. ** (oct -. 1.))
  | "SOLd" -> 103.83 *. (2. ** (oct -. 1.))
  | "LA" -> 110.0 *. (2. ** (oct -. 1.))
  | "LADIESE" -> 116.54 *. (2. ** (oct -. 1.))
  | "SI" -> 123.47 *. (2. ** (oct -. 1.))
  | _ -> 0.0

let play_note a cp samples osci =
  match cp with
  | (cp1, cp2) -> NoteMaker.make_data a (get_freq cp1 cp2) samples osci

let rec play_melody a lst samples osci =
  match lst with
  | [] -> ()
  | e::l -> play_note a e samples osci; play_melody a l samples osci

(* inits a phase list to 0; 0; 0 ... *)
let rec init_phase_l f_l =
  match f_l with
  | [] -> []
  | e::l -> 0.::(init_phase_l l)

(* inits the frequency list from the couple list *)
let rec init_freq_l cp_l =
  let get_f cp =
    match cp with
    | (a, b) -> get_freq a b
  in
    match cp_l with
    | [] -> []
    | e::l -> (get_f e)::(init_freq_l l)

(* computes next phases list *)
let rec make_phase_l p_l f_l =
  match (p_l, f_l) with
  | (_, []) -> []
  | (p::l1, f::l2) ->
    (mod_float (p +. (NoteMaker.twopi *. f /. 44100.)) NoteMaker.twopi)::(make_phase_l l1 l2)
  | (_, _) -> []

(* play n samples of notes in cp_l and print them *)
let play_notes a cp_l samples osci =
  let p_l = ref (init_phase_l cp_l) and
      f_l = init_freq_l cp_l
  in
    for i = 1 to samples do
      begin
        NoteMaker.print_data (NoteMaker.get_average (NoteMaker.make_data_list a !p_l osci));
        p_l := make_phase_l !p_l f_l
      end
    done

let rec play_melody_poly a lst1 lst2 samples osci =
  match (lst1, lst2) with
  | ([], []) -> ()
  | (e1::l1, e2::l2) -> play_notes a (e1::e2::[]) samples osci;
                        play_melody_poly a l1 l2 samples osci
  | (_, _) -> ()
