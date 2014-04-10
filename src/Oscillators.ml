let twopi = 8. *. (atan 1.)
let pi = 4. *. (atan 1.)

let square a p =
  if p < pi then
    -1. *. a
  else
    a

let sawtooth a p =
  a *. ((p /. pi) -. 1.)

let triangle a p =
  if p = p then
    a *. (-1. +. 2. *. p /. pi)
  else
    0.0
