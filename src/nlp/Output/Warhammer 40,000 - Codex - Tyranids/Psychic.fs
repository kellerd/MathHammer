namespace ``Warhammer 40,000 - Codex - Tyranids`` 
module Psychic = 
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
    let ``DOMINION`` = 
        Value
          (Str
             "Dominion has a warp charge value of 5. If manifested, select a friendly TYRANIDS unit within 36\" of the psyker that has the Instinctive Behaviour ability. Until the end of your next Psychic phase, that unit ignores its Instinctive Behaviour ability and automatically passes Morale tests.")
    let ``CATALYST`` = 
        Value
          (Str
             "Catalyst has a warp charge value of 6. If manifested, select a friendly TYRANIDS unit within 18\" of the psyker. Until the start of your next Psychic phase, each time that unit loses a wound, roll a D6; on a 5+, the damage is ignored and the unit does not lose that wound.")
    let ``THE HORROR`` = 
        Value
          (Str
             "The Horror has a warp charge value of 6. If manifested, select a unit within 24\" that is visible to the psyker. Until the start of your next Psychic phase, that unit must subtract 1 from their hit rolls and Leadership characteristic.")
    let ``ONSLAUGHT`` = 
        Value
          (Str
             "Onslaught has a warp charge value of 6. If manifested, select a friendly TYRANIDS unit within 18\" of the psyker. That unit can shoot this turn (even if it Advanced) without suffering any penalties to its hit rolls for moving and shooting Heavy weapons, or Advancing and shooting Assault weapons. In addition, that unit can charge this turn even if it Advanced (though not if it Fell Back).")
    let ``PAROXYSM`` = 
        Value
          (Str
             "Paroxysm has a warp charge value of 5. If manifested, choose an enemy unit within 18\" of the psyker. Until your next Psychic phase, that unit cannot fight in the Fight phase until all other units that are able to have done so. If the target unit has an ability that allows it to fight first in the Fight phase, it instead fights as if it didn’t have this ability. If both players have units that cannot fight until all other units have done so, then alternate choosing which of those units to fight with, starting with the player whose turn is taking place.")
    let ``PSYCHIC SCREAM`` = 
        Value
          (Str
             "Psychic Scream has a warp charge value of 5. If manifested, the nearest enemy unit within 18\" suffers D3 mortal wounds. In addition, if that unit is a PSYKER , roll two dice. If the result is higher than their Leadership characteristic, randomly select one of their psychic powers. They can no longer use that psychic power.")
