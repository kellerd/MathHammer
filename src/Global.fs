module Global

type Page =
  | Home
  | MathHammer
  | GameActions
  | Counter
  | About

let toHash page =
  match page with
  | About -> "#about"
  | Counter -> "#counter"
  | Home -> "#home"
  | MathHammer -> "#mathhammer"
  | GameActions -> "#gameactions"
