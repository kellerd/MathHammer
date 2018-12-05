namespace ``Warhammer 40,000 - Codex - Tyranids`` 
module Stratagem = 
    #if INTERACTIVE
    #load @"..\..\..\Check\Check.fs"
    #load @"..\..\..\Probability\Distribution.fs"
    #load @"..\..\..\GameActions\Primitives\Types.fs"
    #load @"..\..\..\GameActions\Primitives\GamePrimitiveOperations.fs"
    #load @"..\..\..\GameActions\Primitives\TypeChecker.fs"
    #load @"..\..\..\Collections\Map.fs"
    #load @"..\..\..\Collections\List.fs"
    #load @"..\..\..\GameActions\Primitives\State.fs"
    #endif
    open GameActions.Primitives.Types
    open GameActions.Primitives.State  
    let _ = ignore
    let ``PSYCHIC BARRAGE`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("PSYCHIC BARRAGE",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem in your Psychic phase if a Zoanthropes unit from your army consisting of at least 3 is within 6 of 2 other such If you do so the Zoanthropes can not take any Psychic this phase instead select a point on the battlefield within 18 of and visible to all three Roll a for each unit friend or foe within 3 of that point Add 1 to the result if the unit being rolled for has 10 or more but subtract 1 if the unit being rolled for is a CHARACTER On a 4 that unit suffers 3D3 mortal"));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "PSYCHIC BARRAGE"])),
                   Value
                     (Str
                        "Use this Stratagem in your Psychic phase if a Zoanthropes unit from your army consisting of at least 3 is within 6 of 2 other such If you do so the Zoanthropes can not take any Psychic this phase instead select a point on the battlefield within 18 of and visible to all three Roll a for each unit friend or foe within 3 of that point Add 1 to the result if the unit being rolled for has 10 or more but subtract 1 if the unit being rolled for is a CHARACTER On a 4 that unit suffers 3D3 mortal"),
                   None))]),None)
    let ``RAPID REGENERATION`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("RAPID REGENERATION",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem at the end of your Movement phase Select a TYRANIDS model from your army It regains D3 lost earlier in the battle"));
               ("2",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "RAPID REGENERATION"])),
                   Value
                     (Str
                        "Use this Stratagem at the end of your Movement phase Select a TYRANIDS model from your army It regains D3 lost earlier in the battle"),
                   None))]),None)
    let ``CAUSTIC BLOOD`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("CAUSTIC BLOOD",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem at the start of a Fight phase Select a TYRANIDS unit from your army a whenever a model in that unit is destroyed in this phase For each roll of 6 the enemy unit that inflicted the final wound on that model suffers a mortal wound after all of their have been resolved"));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "CAUSTIC BLOOD"])),
                   Value
                     (Str
                        "Use this Stratagem at the start of a Fight phase Select a TYRANIDS unit from your army a whenever a model in that unit is destroyed in this phase For each roll of 6 the enemy unit that inflicted the final wound on that model suffers a mortal wound after all of their have been resolved"),
                   None))]),None)
    let ``SCORCH BUGS`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("SCORCH BUGS",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem when a TYRANIDS unit from your army is selected to attack in the Shooting phase You can add 1 to all wound made for that unit 's fleshborer or fleshborer hive in that Shooting phase"));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "SCORCH BUGS"])),
                   Value
                     (Str
                        "Use this Stratagem when a TYRANIDS unit from your army is selected to attack in the Shooting phase You can add 1 to all wound made for that unit 's fleshborer or fleshborer hive in that Shooting phase"),
                   None))]),None)
    let ``IMPLANT ATTACK`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("IMPLANT ATTACK",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem after a TYRANIDS unit from your army in the Fight phase a for each enemy model other than a VEHICLE that was wounded by any of this unit 's and not slain On a 2 the model suffers a mortal wound"));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "IMPLANT ATTACK"])),
                   Value
                     (Str
                        "Use this Stratagem after a TYRANIDS unit from your army in the Fight phase a for each enemy model other than a VEHICLE that was wounded by any of this unit 's and not slain On a 2 the model suffers a mortal wound"),
                   None))]),None)
    let ``BOUNTY OF THE HIVE FLEET`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("BOUNTY OF THE HIVE FLEET",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem before the battle Your army can have one extra Bio-artefact for 1 CP or two extra for 3 CPs All of the Bio-artefacts that you include must be different and be given to different CHARACTERS You can only use this Stratagem once per battle"));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value
                        (ParamArray [Var "Available CP"; Var "BOUNTY OF THE HIVE FLEET"])),
                   Value
                     (Str
                        "Use this Stratagem before the battle Your army can have one extra Bio-artefact for 1 CP or two extra for 3 CPs All of the Bio-artefacts that you include must be different and be given to different CHARACTERS You can only use this Stratagem once per battle"),
                   None));
               ("3",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value
                        (ParamArray [Var "Available CP"; Var "BOUNTY OF THE HIVE FLEET"])),
                   Value
                     (Str
                        "Use this Stratagem before the battle Your army can have one extra Bio-artefact for 1 CP or two extra for 3 CPs All of the Bio-artefacts that you include must be different and be given to different CHARACTERS You can only use this Stratagem once per battle"),
                   None))]),None)
    let ``METABOLIC OVERDRIVE`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("METABOLIC OVERDRIVE",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem in your Movement phase after moving a TYRANIDS unit from your army You can make a second move with that unit including Advancing if you wish but when you do so you must roll a for each model in the unit For each roll of 1 inflict a mortal wound on the unit The unit can not shoot or make a charge move this turn"));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "METABOLIC OVERDRIVE"])),
                   Value
                     (Str
                        "Use this Stratagem in your Movement phase after moving a TYRANIDS unit from your army You can make a second move with that unit including Advancing if you wish but when you do so you must roll a for each model in the unit For each roll of 1 inflict a mortal wound on the unit The unit can not shoot or make a charge move this turn"),
                   None))]),None)
    let ``FEEDER TENDRILS`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("FEEDER TENDRILS",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem when a Genestealer LICTOR Toxicrene or Venomthrope from your army kills a CHARACTER in the Fight phase Gain D3 Command Points"));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "FEEDER TENDRILS"])),
                   Value
                     (Str
                        "Use this Stratagem when a Genestealer LICTOR Toxicrene or Venomthrope from your army kills a CHARACTER in the Fight phase Gain D3 Command Points"),
                   None))]),None)
    let ``PATHOGENIC SLIME`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("PATHOGENIC SLIME",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem in your Shooting phase a TYRANIDS MONSTER from your army Increase the Damage of its by 1 for this phase"));
               ("2",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "PATHOGENIC SLIME"])),
                   Value
                     (Str
                        "Use this Stratagem in your Shooting phase a TYRANIDS MONSTER from your army Increase the Damage of its by 1 for this phase"),
                   None))]),None)
    let ``VORACIOUS APPETITE`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("VORACIOUS APPETITE",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem in the Fight phase when a TYRANIDS MONSTER or CHARACTER from your army is chosen to attack You can re-roll all failed wound for that model until the end of the phase"));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "VORACIOUS APPETITE"])),
                   Value
                     (Str
                        "Use this Stratagem in the Fight phase when a TYRANIDS MONSTER or CHARACTER from your army is chosen to attack You can re-roll all failed wound for that model until the end of the phase"),
                   None))]),None)
    let ``PHEROMONE TRAIL`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("PHEROMONE TRAIL",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem when a TYRANIDS INFANTRY unit from your army is set up on the battlefield as if there is already a LICTOR from your army on the battlefield You can set up the unit wholly within 6 of the LICTOR and more than 9 from any enemy rather than following the normal for setting up the unit"));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "PHEROMONE TRAIL"])),
                   Value
                     (Str
                        "Use this Stratagem when a TYRANIDS INFANTRY unit from your army is set up on the battlefield as if there is already a LICTOR from your army on the battlefield You can set up the unit wholly within 6 of the LICTOR and more than 9 from any enemy rather than following the normal for setting up the unit"),
                   None))]),None)
    let ``SINGLE-MINDED ANNIHILATION`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("SINGLE-MINDED ANNIHILATION",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem at the end of your Shooting phase Select a TYRANIDS INFANTRY unit from your army that unit can immediately shoot again"));
               ("2",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value
                        (ParamArray
                           [Var "Available CP"; Var "SINGLE-MINDED ANNIHILATION"])),
                   Value
                     (Str
                        "Use this Stratagem at the end of your Shooting phase Select a TYRANIDS INFANTRY unit from your army that unit can immediately shoot again"),
                   None))]),None)
    let ``POWER OF THE HIVE MIND`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("POWER OF THE HIVE MIND",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem at the end of your Psychic phase a TYRANIDS PSYKER unit from your army that manifested a psychic power this turn It can immediately attempt to manifest one additional psychic power this turn"));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value
                        (ParamArray [Var "Available CP"; Var "POWER OF THE HIVE MIND"])),
                   Value
                     (Str
                        "Use this Stratagem at the end of your Psychic phase a TYRANIDS PSYKER unit from your army that manifested a psychic power this turn It can immediately attempt to manifest one additional psychic power this turn"),
                   None))]),None)
    let ``DEATH FRENZY`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("DEATH FRENZY",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem when a TYRANIDS CHARACTER from your army is slain the Hive Mind compels it to one final attack and it can immediately either shoot as if it were your Shooting phase or fight as if it were your Fight phase before it is removed from the battlefield"));
               ("2",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "DEATH FRENZY"])),
                   Value
                     (Str
                        "Use this Stratagem when a TYRANIDS CHARACTER from your army is slain the Hive Mind compels it to one final attack and it can immediately either shoot as if it were your Shooting phase or fight as if it were your Fight phase before it is removed from the battlefield"),
                   None))]),None)
    let ``GRISLY FEAST`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("GRISLY FEAST",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem in the Morale phase Select a unit of Ripper Swarms or Haruspex from your army Your opponent must add 1 to any Morale taken for enemy that are within 6 of that unit in this phase"));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "GRISLY FEAST"])),
                   Value
                     (Str
                        "Use this Stratagem in the Morale phase Select a unit of Ripper Swarms or Haruspex from your army Your opponent must add 1 to any Morale taken for enemy that are within 6 of that unit in this phase"),
                   None))]),None)
    let ``OVERRUN`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("OVERRUN",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem when a TYRANIDS unit from your army destroys a unit in the Fight phase and is not within 3 of an enemy unit Instead of consolidating that unit can move and Advance as if it were your Movement phase it can not move within 1 of any enemy"));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "OVERRUN"])),
                   Value
                     (Str
                        "Use this Stratagem when a TYRANIDS unit from your army destroys a unit in the Fight phase and is not within 3 of an enemy unit Instead of consolidating that unit can move and Advance as if it were your Movement phase it can not move within 1 of any enemy"),
                   None))]),None)
    let ``INVISIBLE HUNTER`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("INVISIBLE HUNTER",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem in your Movement phase a LICTOR from your army that is within 1 of an enemy unit That model can Fall Back shoot and charge in this turn"));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "INVISIBLE HUNTER"])),
                   Value
                     (Str
                        "Use this Stratagem in your Movement phase a LICTOR from your army that is within 1 of an enemy unit That model can Fall Back shoot and charge in this turn"),
                   None))]),None)
    let ``SPOREFIELD`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("SPOREFIELD",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem after both have deployed but before the battle begins You can add up to two of Spore Mines to your army as and set them up anywhere on the battlefield that is more than 12 from enemy"));
               ("3",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "SPOREFIELD"])),
                   Value
                     (Str
                        "Use this Stratagem after both have deployed but before the battle begins You can add up to two of Spore Mines to your army as and set them up anywhere on the battlefield that is more than 12 from enemy"),
                   None))]),None)
    let ``WAR ON ALL FRONTS`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "LEVIATHAN")])),
           Choice
             ("WAR ON ALL FRONTS",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem in the Fight phase Select an enemy unit that is within 1 of at least one LEVIATHAN unit from your army that can FLY and at least one that can not You can re-roll hit and wound of 1 in this phase for for LEVIATHAN that target that enemy unit"));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "WAR ON ALL FRONTS"])),
                   Value
                     (Str
                        "Use this Stratagem in the Fight phase Select an enemy unit that is within 1 of at least one LEVIATHAN unit from your army that can FLY and at least one that can not You can re-roll hit and wound of 1 in this phase for for LEVIATHAN that target that enemy unit"),
                   None))]),None)
    let ``HYPER-TOXICITY`` = 
        IfThenElse
          (App (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "GORGON")])),
           Choice
             ("HYPER-TOXICITY",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem in the Fight phase a GORGON unit from your army that has the toxin biomorph For the duration of the phase the toxin biomorph causes 1 additional damage on wound of 5 + rather than 6 + for made by that unit"));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "HYPER-TOXICITY"])),
                   Value
                     (Str
                        "Use this Stratagem in the Fight phase a GORGON unit from your army that has the toxin biomorph For the duration of the phase the toxin biomorph causes 1 additional damage on wound of 5 + rather than 6 + for made by that unit"),
                   None))]),None)
    let ``BRUTE FORCE`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "BEHEMOTH")])),
           Choice
             ("BRUTE FORCE",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem when a BEHEMOTH unit from your army completes a charge move Roll a for each model in the charging unit that is within 1 of an enemy unit For each roll of 6 or 2 + for a MONSTER inflict one mortal wound on an enemy unit within 1"));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "BRUTE FORCE"])),
                   Value
                     (Str
                        "Use this Stratagem when a BEHEMOTH unit from your army completes a charge move Roll a for each model in the charging unit that is within 1 of an enemy unit For each roll of 6 or 2 + for a MONSTER inflict one mortal wound on an enemy unit within 1"),
                   None))]),None)
    let ``ENDLESS SWARM`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("ENDLESS SWARM",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem at the end of your Movement phase a unit of Termagants Hormagaunts or Gargoyles or any HYDRA INFANTRY unit from your army that has been completely destroyed Add an identical unit to your army and set it up as wholly within 6 of any board edge more than 9 from enemy"));
               ("2",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "ENDLESS SWARM"])),
                   Value
                     (Str
                        "Use this Stratagem at the end of your Movement phase a unit of Termagants Hormagaunts or Gargoyles or any HYDRA INFANTRY unit from your army that has been completely destroyed Add an identical unit to your army and set it up as wholly within 6 of any board edge more than 9 from enemy"),
                   None))]),None)
    let ``OPPORTUNISTIC ADVANCE`` = 
        IfThenElse
          (App (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "KRAKEN")])),
           Choice
             ("OPPORTUNISTIC ADVANCE",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem in your Movement phase when you roll the for an Advancing KRAKEN unit other than a unit that can FLY You can double the number you roll and add that total to their Move characteristic for that Movement phase rather than following the normal for Advancing"));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value
                        (ParamArray [Var "Available CP"; Var "OPPORTUNISTIC ADVANCE"])),
                   Value
                     (Str
                        "Use this Stratagem in your Movement phase when you roll the for an Advancing KRAKEN unit other than a unit that can FLY You can double the number you roll and add that total to their Move characteristic for that Movement phase rather than following the normal for Advancing"),
                   None))]),None)
    let ``CALL THE BROOD`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("CALL THE BROOD",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem at the end of your Movement phase Add a new unit of up to 5 to your army and set them up as wholly within 6 of a Broodlord or infestation node from your army and more than 9 from any enemy"));
               ("3",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "CALL THE BROOD"])),
                   Value
                     (Str
                        "Use this Stratagem at the end of your Movement phase Add a new unit of up to 5 to your army and set them up as wholly within 6 of a Broodlord or infestation node from your army and more than 9 from any enemy"),
                   None))]),None)
    let ``THE ENEMY BELOW`` = 
        IfThenElse
          (App
             (Call Contains,
              Value (ParamArray [Var "Keywords"; Value (Str "JORMUNGANDR")])),
           Choice
             ("THE ENEMY BELOW",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem when you set up a JORMUNGANDR INFANTRY unit during deployment It is set up within bored before battle Whenever you set up a unit of Raveners a Mawloc Trygon or a Trygon Prime at the end of your Movement phase a burrowing unit you can also set up any number of you set up within the Set up the unit wholly within 3 of the burrowing unit and more than 9 from any enemy Any you can not set up in this way when you do so are destroyed"));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "THE ENEMY BELOW"])),
                   Value
                     (Str
                        "Use this Stratagem when you set up a JORMUNGANDR INFANTRY unit during deployment It is set up within bored before battle Whenever you set up a unit of Raveners a Mawloc Trygon or a Trygon Prime at the end of your Movement phase a burrowing unit you can also set up any number of you set up within the Set up the unit wholly within 3 of the burrowing unit and more than 9 from any enemy Any you can not set up in this way when you do so are destroyed"),
                   None))]),None)
    let ``THE DEEPEST SHADOW`` = 
        IfThenElse
          (App (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "KRONOS")])),
           Choice
             ("THE DEEPEST SHADOW",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem when an enemy PSYKER attempts to manifest a psychic power within 24 of a KRONOS unit from your army Your opponent can only roll a single for the Psychic test"));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "THE DEEPEST SHADOW"])),
                   Value
                     (Str
                        "Use this Stratagem when an enemy PSYKER attempts to manifest a psychic power within 24 of a KRONOS unit from your army Your opponent can only roll a single for the Psychic test"),
                   None))]),None)
    let ``DIGESTIVE DENIAL`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("DIGESTIVE DENIAL",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem after deployment but before the first battle round begins Choose a piece of terrain other than a Fortification Units fully within or on this piece of terrain do not gain any bonus to their saving throws for being in cover"));
               ("2",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "DIGESTIVE DENIAL"])),
                   Value
                     (Str
                        "Use this Stratagem after deployment but before the first battle round begins Choose a piece of terrain other than a Fortification Units fully within or on this piece of terrain do not gain any bonus to their saving throws for being in cover"),
                   None))]),None)
    let ``ADRENALINE SURGE`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("ADRENALINE SURGE",
              [("<Not selected>",
                Value
                  (Str
                     "Use this Stratagem at the end of the Fight phase Select a TYRANIDS unit from your army that unit can immediately fight again"));
               ("3",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "ADRENALINE SURGE"])),
                   Value
                     (Str
                        "Use this Stratagem at the end of the Fight phase Select a TYRANIDS unit from your army that unit can immediately fight again"),
                   None))]),None)
