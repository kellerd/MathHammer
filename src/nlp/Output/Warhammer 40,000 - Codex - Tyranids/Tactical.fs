namespace ``Warhammer 40,000 - Codex - Tyranids`` 
module Tactical = 
    #if INTERACTIVE
    #load @"..\..\..\Check\Check.fs"
    #load @"..\..\..\Probability\Distribution.fs"
    #load @"..\..\..\GameActions\Primitives\Types.fs"
    #load @"..\..\..\Collections\Map.fs"
    #load @"..\..\..\Collections\List.fs"
    #load @"..\..\..\GameActions\Primitives\State.fs"
    #endif
    open GameActions.Primitives.Types
    open GameActions.Primitives.State  
    let _ = ignore
    let ``Swarm`` = 
        Value
          (Str
             "Score 1 victory point if you control more objective markers than your opponent at the end of the turn.")
    let ``Crush`` = 
        Value
          (Str
             "Score 1 victory point if at least one enemy unit was completely destroyed this turn, and the last model in the enemy unit was slain by an attack made by a TYRANIDS MONSTER or a TYRANIDS unit of more than 10 models.")
    let ``Dominate`` = 
        Value
          (Str
             "Score 1 victory point if at least three psychic powers were successfully manifested by friendly TYRANIDS units in your Psychic phase.")
    let ``Decapitate`` = 
        Value
          (Str
             "Score 1 victory point if at least one enemy CHARACTER was destroyed this turn. If two or more enemy CHARACTERS were destroyed, score D3 victory points instead.")
    let ``Terrify`` = 
        Value
          (Str
             "Score 1 victory point if at least one enemy unit failed a Morale test this turn. If three or more enemy units failed Morale tests this turn, score D3 victory points instead.")
    let ``Devour`` = 
        Value
          (Str
             "Score 1 victory point if an enemy unit was destroyed during the Fight phase this turn. If 3 or more enemy units were destroyed during the Fight phase this turn, score D3 victory points instead, and if 6 or more enemy units were destroyed during the Fight phase this turn, score D3+3 victory points instead.")
