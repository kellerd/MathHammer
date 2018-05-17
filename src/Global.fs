module Global

type Page =
    | MathHammer
    | GameActions

let toHash page =
    match page with
    | MathHammer -> "#mathhammer"
    | GameActions -> "#gameactions"
