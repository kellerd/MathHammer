namespace ``Warhammer 40,000 - Codex - Tyranids`` 
module Rule = 
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
    let ``KEYWORDS`` = 
        Value
          (Str
             "Throughout this section you will come across a keyword that is within angular specifically <HIVE FLEET> This is shorthand for a keyword of your own choosing as described below")
    let ``<HIVE FLEET>`` = 
        Value
          (Str
             "All Tyranids belong to a hive fleet When you include a Tyranids unit in your army you must nominate which hive fleet that unit is from There are many different hive to choose from you can use any of the hive described in this book or make up your own if you prefer You then simply replace the <HIVE FLEET> keyword in every instance on that unit 's datasheet and in any psychic they know with the name of your chosen hive fleet For example if you were to include a Tervigon in your army and you decided it was from Hive Fleet Kraken then its <HIVE FLEET> keyword is changed to KRAKEN and its Brood Progenitor ability would say You can re-roll hit of 1 in the Shooting phase for friendly KRAKEN Termagant within 6 of this model")
    let ``SYNAPSE`` = 
        Value
          (Str
             "<HIVE FLEET> automatically pass Morale tests if they are within 12 of any friendly <HIVE FLEET> with this ability")
    let ``INSTINCTIVE BEHAVIOUR`` = 
        Value
          (Str
             "Unless a <HIVE FLEET> unit with this ability is within 24 of any friendly <HIVE FLEET> SYNAPSE unit you must subtract 1 from any hit made for it when shooting any target other than the nearest visible enemy unit and you must subtract 2 from its charge roll if it declares a charge against any unit other than the nearest enemy unit")
    let ``SHADOW IN THE WARP`` = 
        Value
          (Str
             "Enemy must subtract 1 from any Psychic they make if they are within 18 of any with this ability TYRANID PSYKERS are not affected")
    let _ = 
        Value (Str "TYRANIDS WARGEAR LISTS")
    let ``BASIC BIO-WEAPONS`` = 
        Value (Str "Scything talons Spinefists Deathspitter")
    let ``BASIC BIO-CANNONS`` = 
        Value (Str "Barbed strangler Venom cannon")
    let ``MELEE BIO-WEAPONS`` = 
        Value (Str "Rending claws Boneswords Lash whip and bonesword")
    let ``MONSTROUS BIO-WEAPONS`` = 
        Value (Str "rending Monstrous boneswords Lash whip and monstrous bonesword")
    let ``MONSTROUS BIO-CANNONS`` = 
        Value
          (Str
             "Two with slimer Two with brainleech Stranglethorn cannon * Heavy venom cannon * * A model can not be armed with more than one of these")
    let ``Acid maw`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-3")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")])])
    let ``Biostatic rattle`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-1")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "Each time the bearer fights it can make one and only one attack with this weapon This is in addition to the bearer 's If a unit suffers any unsaved from this weapon add 1 to any Morale they take until the end of the turn")])])
    let ``Blinding venom`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "3")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "0")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "If a unit suffers any unsaved from this weapon your opponent must subtract 1 from hit for that unit until the end of the turn")])])
    let ``Bone mace`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "8")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-1")]);
              Value (ParamArray [Value (Str "D"); Value (Str "D3")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "Each time the bearer fights it can make one and only one attack with this weapon This is in addition to the bearer 's")])])
    let ``Bone sabres`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-3")]);
              Value (ParamArray [Value (Str "D"); Value (Str "3")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "Each time you make a wound roll of 6 for this weapon the target unit suffers a mortal wound in addition to any other damage")])])
    let ``Boneswords`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-2")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "A model armed with can make 1 additional attack with them in the Fight phase")])])
    let ``Claws and teeth`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "0")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")])])
    let ``Crushing claws`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "x2")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-3")]);
              Value (ParamArray [Value (Str "D"); Value (Str "D3")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "When attacking with this weapon you must subtract 1 from the hit roll")])])
    let ``Distensible jaws`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-3")]);
              Value (ParamArray [Value (Str "D"); Value (Str "D6")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "Each time the bearer fights one and only one of its must be made with this weapon")])])
    let ``Grasping talons`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-1")]);
              Value (ParamArray [Value (Str "D"); Value (Str "2")])])
    let ``Lash whip and bonesword`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-2")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "If the bearer is slain in the Fight phase before it has made its leave it where it is When its unit is chosen to fight in that phase the bearer can do so as normal before being removed from the battlefield")])])
    let ``Lash whip and monstrous bonesword`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-2")]);
              Value (ParamArray [Value (Str "D"); Value (Str "3")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "If the bearer is slain in the Fight phase before it has made its leave it where it is When its unit is chosen to fight in that phase the bearer can do so as normal before being removed from the battlefield")])])
    let ``Massive crushing claws`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "x2")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-3")]);
              Value (ParamArray [Value (Str "D"); Value (Str "D6")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "When attacking with this weapon you must subtract 1 from the hit roll")])])
    let ``Massive scything talons`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-3")]);
              Value (ParamArray [Value (Str "D"); Value (Str "D6")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "You can re-roll hit of 1 for this weapon If the bearer has more than one pair of monstrous/massive scything it can make 1 additional attack with this weapon each time it fights")])])
    let ``Massive toxic lashes (melee)`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-2")]);
              Value (ParamArray [Value (Str "D"); Value (Str "D3")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "You can re-roll failed wound for this weapon A model armed with this weapon always fights first in the Fight phase even if it did n't charge If the enemy has that have charged or that have a similar ability then alternate choosing to fight with starting with the player whose turn is taking place")])])
    let ``Monstrous acid maw`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-5")]);
              Value (ParamArray [Value (Str "D"); Value (Str "D3")])])
    let ``Monstrous boneswords`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-2")]);
              Value (ParamArray [Value (Str "D"); Value (Str "3")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "A model armed with monstrous can make 1 additional attack with them in the Fight phase")])])
    let ``Monstrous crushing claws`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "x2")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-3")]);
              Value (ParamArray [Value (Str "D"); Value (Str "3")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "When attacking with this weapon you must subtract 1 from the hit roll")])])
    let ``Monstrous rending claws`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-3")]);
              Value (ParamArray [Value (Str "D"); Value (Str "D3")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "You can re-roll failed wound for this weapon In addition each time you make a wound roll of 6 that hit is resolved with an AP of -6 and Damage of 3")])])
    let ``Monstrous scything talons`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-3")]);
              Value (ParamArray [Value (Str "D"); Value (Str "3")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "You can re-roll hit of 1 for this weapon If the bearer has more than one pair of monstrous/massive scything it can make 1 additional attack with this weapon each time it fights")])])
    let ``Powerful limbs`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-1")]);
              Value (ParamArray [Value (Str "D"); Value (Str "2")])])
    let ``Prehensile pincer tail`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "0")]);
              Value (ParamArray [Value (Str "D"); Value (Str "D3")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "Each time the bearer fights it can make one and only one attack with this weapon This is in addition to the bearer 's")])])
    let ``Ravenous maw`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-1")]);
              Value (ParamArray [Value (Str "D"); Value (Str "D3")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "Make D3 hit for each attack made with this weapon instead of 1")])])
    let ``Rending claws`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-1")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "Each time you make a wound roll of 6 for this weapon that hit is resolved with an AP of -4")])])
    let ``Scything talons`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "0")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "You can re-roll hit of 1 for this weapon If the bearer has more than one pair of scything it can make 1 additional attack with this weapon each time it fights")])])
    let ``Scything wings`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-2")]);
              Value (ParamArray [Value (Str "D"); Value (Str "D3")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value (Str "You can re-roll hit of 1 for this weapon")])])
    let ``Shovelling claws`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "x2")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-3")]);
              Value (ParamArray [Value (Str "D"); Value (Str "D6")])])
    let ``Thresher scythe`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "4")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-1")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "Each time the bearer it can make one and only one attack with this weapon Make D3 hit for this attack instead of one This is in addition to the bearer 's")])])
    let ``Toxic lashes (melee)`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "0")]);
              Value (ParamArray [Value (Str "D"); Value (Str "D3")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "You can re-roll failed wound for this weapon A model armed with this weapon always fights first in the Fight phase even if it did n't charge If the enemy has that have charged or that have a similar ability then alternate choosing to fight with starting with the player whose turn is taking place")])])
    let ``Toxinspike`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "1")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "0")]);
              Value (ParamArray [Value (Str "D"); Value (Str "D3")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "Each time the bearer it can make one and only one attack with this weapon This is in addition to the bearer 's This weapon always wounds other than VEHICLES on a 2")])])
    let ``Wicked spur`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
              Value (ParamArray [Value (Str "S"); Value (Str "8")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-3")]);
              Value (ParamArray [Value (Str "D"); Value (Str "D3")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "Each time the bearer fights it can make one and only one attack with this weapon This is in addition to the bearer 's")])])
    let _ = 
        Value (Str "Spinefist")
    let ``Acid spray`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "18\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Heavy 2D6")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-1")]);
              Value (ParamArray [Value (Str "D"); Value (Str "D3")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value (Str "This weapon automatically hits its target")])])
    let ``Barbed strangler`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "36\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault D6")]);
              Value (ParamArray [Value (Str "S"); Value (Str "5")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-1")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "You can add 1 to hit for this weapon when attacking a unit with 10 or more")])])
    let ``Bio-electric pulse`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "12\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault 6")]);
              Value (ParamArray [Value (Str "S"); Value (Str "5")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "0")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")])])
    let ``Bio-electric pulse with containment spines`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "12\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault 12")]);
              Value (ParamArray [Value (Str "S"); Value (Str "5")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "0")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")])])
    let ``Bio-plasma`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "12\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault D3")]);
              Value (ParamArray [Value (Str "S"); Value (Str "7")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-3")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")])])
    let ``Bio-plasmic cannon`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "36\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Heavy 6")]);
              Value (ParamArray [Value (Str "S"); Value (Str "7")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-3")]);
              Value (ParamArray [Value (Str "D"); Value (Str "2")])])
    let ``Bio-plasmic scream`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "18\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault D6")]);
              Value (ParamArray [Value (Str "S"); Value (Str "7")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-4")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")])])
    let ``Choking spores`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "12\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault D6")]);
              Value (ParamArray [Value (Str "S"); Value (Str "3")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "0")]);
              Value (ParamArray [Value (Str "D"); Value (Str "D3")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "You can re-roll failed wound for this weapon In addition attacked by this weapon do not gain any bonus to their saving throws for being in cover")])])
    let ``Deathspitter`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "24\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault 3")]);
              Value (ParamArray [Value (Str "S"); Value (Str "5")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-1")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")])])
    let ``Deathspitter with slimer maggots`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "24\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault 3")]);
              Value (ParamArray [Value (Str "S"); Value (Str "7")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-1")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")])])
    let ``Devourer`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "18\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault 3")]);
              Value (ParamArray [Value (Str "S"); Value (Str "4")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "0")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")])])
    let ``Devourer with brainleech worms`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "18\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault 6")]);
              Value (ParamArray [Value (Str "S"); Value (Str "6")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "0")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")])])
    let ``Drool cannon`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "8\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault D6")]);
              Value (ParamArray [Value (Str "S"); Value (Str "6")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-1")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value (Str "This weapon automatically hits its target")])])
    let ``Flamespurt`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "10\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault D6")]);
              Value (ParamArray [Value (Str "S"); Value (Str "5")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-1")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value (Str "This weapon automatically hits its target")])])
    let ``Flesh hooks`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "6\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault 2")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "0")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "This weapon can be fired within 1 of an enemy unit and can target enemy within 1 of friendly")])])
    let ``Fleshborer`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "12\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault 1")]);
              Value (ParamArray [Value (Str "S"); Value (Str "4")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "0")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")])])
    let ``Fleshborer hive`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "18\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Heavy 20")]);
              Value (ParamArray [Value (Str "S"); Value (Str "5")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "0")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")])])
    let ``Grasping tongue`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "12\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault 1")]);
              Value (ParamArray [Value (Str "S"); Value (Str "6")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-3")]);
              Value (ParamArray [Value (Str "D"); Value (Str "D3")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "This weapon can be fired within 1 of an enemy unit and can target enemy within 1 of friendly In addition when a model is slain by this weapon the bearer regains 1 lost wound")])])
    let ``Heavy venom cannon`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "36\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault D3")]);
              Value (ParamArray [Value (Str "S"); Value (Str "9")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-2")]);
              Value (ParamArray [Value (Str "D"); Value (Str "3")])])
    let ``Impaler cannon`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "36\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Heavy 2")]);
              Value (ParamArray [Value (Str "S"); Value (Str "8")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-2")]);
              Value (ParamArray [Value (Str "D"); Value (Str "D3")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "This weapon can target that are not visible to the bearer In addition attacked by this weapon do not gain any bonus to their saving throws for being in cover")])])
    let ``Spore mine launcher`` = 
        Value (Str "48 Heavy 1 See Biovore datasheet pg 101")
    let _ = 
        Value
          (Str
             "Attacking from the and from beneath the earth the Tyranids tear apart their prey in a frenzy of slashing")
    let ``Adrenal glands`` = 
        Value
          (ParamArray
             [Value
                (ParamArray
                   [Value (Str "EFFECT");
                    Value
                      (Str
                         "If a unit has adrenal glands, add 1\" to the distance it can move when it Advances or charges.")])])
    let ``Toxin sacs`` = 
        Value
          (ParamArray
             [Value
                (ParamArray
                   [Value (Str "EFFECT");
                    Value
                      (Str
                         "Any wound rolls of 6+ in the Fight phase for a model with toxin sacs cause 1 additional damage.")])])
    let ``Massive toxic lashes (shooting)`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "8\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault D6")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-2")]);
              Value (ParamArray [Value (Str "D"); Value (Str "D3")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "This weapon can be fired within 1 of an enemy unit and can target enemy within 1 of friendly You can re-roll all failed wound for this weapon")])])
    let ``Rupture cannon`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "48\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Heavy 3")]);
              Value (ParamArray [Value (Str "S"); Value (Str "10")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-3")]);
              Value (ParamArray [Value (Str "D"); Value (Str "D6")])])
    let ``Shockcannon`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "24\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault D3")]);
              Value (ParamArray [Value (Str "S"); Value (Str "7")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-1")]);
              Value (ParamArray [Value (Str "D"); Value (Str "D3")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "If the target is a VEHICLE and you make a wound roll of 4 the target suffers 1 mortal wound in addition to any other damage If you make a wound roll of 6 inflict D3 mortal instead")])])
    let ``Spine banks`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "6\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault 4")]);
              Value (ParamArray [Value (Str "S"); Value (Str "5")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "0")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "This weapon can be fired within 1 of an enemy unit and can target enemy within 1 of friendly")])])
    let ``Spinefists`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "12\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Pistol *")]);
              Value (ParamArray [Value (Str "S"); Value (Str "3")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "0")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "When a model fires this weapon it makes a number of equal to its characteristic")])])
    let ``Spinemaws`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "6\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Pistol 4")]);
              Value (ParamArray [Value (Str "S"); Value (Str "2")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "0")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")])])
    let ``Stinger salvo`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "24\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault 4")]);
              Value (ParamArray [Value (Str "S"); Value (Str "5")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-1")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")])])
    let ``Stranglethorn cannon`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "36\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault D6")]);
              Value (ParamArray [Value (Str "S"); Value (Str "7")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-1")]);
              Value (ParamArray [Value (Str "D"); Value (Str "2")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "You can add 1 to hit for this weapon when attacking a unit with 10 or more")])])
    let ``Tentaclids`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "36\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault 4")]);
              Value (ParamArray [Value (Str "S"); Value (Str "5")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "0")]);
              Value (ParamArray [Value (Str "D"); Value (Str "1")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "You may re-roll failed hit for this weapon against that can FLY In addition if the target is a VEHICLE and you make a wound roll of 4 it suffers 1 mortal wound in addition to any other damage If you make a wound roll of 6 inflict D3 mortal instead")])])
    let ``Toxic lashes (shooting)`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "6\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault 2")]);
              Value (ParamArray [Value (Str "S"); Value (Str "User")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "0")]);
              Value (ParamArray [Value (Str "D"); Value (Str "D3")]);
              Value
                (ParamArray
                   [Value (Str "ABILITIES");
                    Value
                      (Str
                         "This weapon can be fired within 1 of an enemy unit and can target enemy within 1 of friendly In addition you can re-roll failed wound for this weapon")])])
    let ``Venom cannon`` = 
        Value
          (ParamArray
             [Value (ParamArray [Value (Str "RANGE"); Value (Str "36\"")]);
              Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault D3")]);
              Value (ParamArray [Value (Str "S"); Value (Str "8")]);
              Value (ParamArray [Value (Str "AP"); Value (Str "-2")]);
              Value (ParamArray [Value (Str "D"); Value (Str "D3")])])
    let ``ALIEN CUNNING`` = 
        Value
          (Str
             "At the start of the first battle round but before the first turn begins you can remove your Warlord from the battlefield and set them up again If both have that can do this roll off The player that wins the roll-off decides who sets up their unit s first")
    let ``HEIGHTENED SENSES`` = 
        Value
          (Str
             "Your Warlord never suffers any to their hit although they still only hit on of 6 when firing Overwatch")
    let ``SYNAPTIC LYNCHPIN`` = 
        Value (Str "Add 6 to the range of the Warlord 's Synapse ability")
    let ``MIND EATER`` = 
        Value
          (Str
             "Each time the Warlord slays an enemy CHARACTER in the Fight phase choose a friendly <HIVE FLEET> unit within 3 At the end of the phase that unit can move and Advance if you wish as if it was your Movement phase")
    let ``INSTINCTIVE KILLER`` = 
        Value
          (Str
             "At the beginning of the battle but before the first turn begins choose an enemy unit You can re-roll failed hit for the Warlord for that target that unit or any unit that has the same datasheet for example all Intercessor Squads or all of Nobz etc.")
    let ``ADAPTIVE BIOLOGY`` = 
        Value
          (Str
             "From the end of the first phase in which this Warlord suffers any for the remainder of the battle when inflicting damage upon the Warlord reduce the damage of the attack by 1 to a minimum of 1")
    let ``The mightiest creatures spawned by the Hive Mind have each been created with a specific purpose. If one of the following named characters is your Warlord, they must be given the associated Warlord Trait shown below.`` = 
        Value
          (Str
             "The Swarmlord Alien Cunning Old One Eye Adaptive Biology Deathleaper Mind Eater The Red Terror Heightened Senses")
    let ``HIVE FLEET WARLORD TRAITS`` = 
        Value
          (Str
             "If you wish you can pick a Hive Fleet Warlord Trait from the list below instead of the Tyranid Warlord Traits to the left but only if your Warlord is from the relevant hive fleet")
    let ``BEHEMOTH: MONSTROUS HUNGER`` = 
        Value
          (Str
             "Each time you make a wound roll of 6 for the Warlord in the Fight phase that attack inflicts 1 additional damage")
    let ``KRAKEN: ONE STEP AHEAD`` = 
        Value
          (Str
             "In each Fight phase you can pick one friendly KRAKEN unit within 6 of your Warlord That unit can fight first in the Fight phase even if it did n't charge If the enemy has that have charged or that have a similar ability then alternate choosing to fight with starting with the player whose turn is taking place")
    let ``LEVIATHAN: PERFECTLY ADAPTED`` = 
        Value
          (Str
             "Once per battle round you can re-roll a single hit roll wound roll damage roll Advance roll charge roll or saving throw made for your Warlord")
    let ``GORGON: LETHAL MIASMA`` = 
        Value
          (Str
             "At the end of the Fight phase roll a D6 for each enemy unit within 1 of the Warlord On a 4 that unit suffers a mortal wound")
    let ``JORMUNGANDR: INSIDIOUS THREAT`` = 
        Value
          (Str
             "Enemy never gain any bonus to their saving throws for being in cover for made by the Warlord or friendly JORMUNGANDR within 3 of the Warlord")
    let ``HYDRA: ENDLESS REGENERATION`` = 
        Value
          (Str
             "At the beginning of each of your roll a for each wound that your Warlord has lost For each roll of 6 your Warlord regains a wound lost earlier in the battle")
    let ``KRONOS: SOUL HUNGER`` = 
        Value
          (Str
             "Whenever an enemy PSYKER fails a psychic test within 18 of your Warlord they suffer D3 mortal")
    let ``EXTENSIONS OF THE HIVE MIND`` = 
        Value
          (Str
             "If your army is Battle-forged all Troops in Tyranids Detachments gain this ability a unit that is within range of an objective marker as specified in the mission controls the objective marker even if there are more enemy within range of that objective marker If an enemy unit within range of the same objective marker has a similar ability then the objective marker is controlled by the player who has the most within range of it as normal")
    let ``HIVE FLEET ADAPTATIONS`` = 
        Value
          (Str
             "If your army is Battle-forged all in Tyranids Detachments gain a Hive Fleet Adaptation so long as every unit in that Detachment is from the same hive fleet The Hive Fleet Adaptation gained depends upon the hive fleet they are from as shown in the table opposite For example a BEHEMOTH unit with the Hive Fleet Adaptation ability the Hyper-aggression adaptation If you are using a splinter fleet rather than a hive fleet use the Hive Fleet Adaptation of its parent hive fleet For example the Court of the Nephilim King is a splinter fleet of Hive Fleet Behemoth so should use the Behemoth Hive Fleet Adaptation If you are unsure of a splinter fleet 's parent hive fleet either consult the background of our or choose an adaptation from the table that best describes its character and fighting style")
    let ``BEHEMOTH: HYPER-AGGRESSION`` = 
        Value (Str "You can re-roll failed charge for with this adaptation")
    let ``KRAKEN: QUESTING TENDRILS`` = 
        Value
          (Str
             "When a unit with this adaptation Advances roll three instead of one and pick the highest to add to the Move characteristic of all in the unit for that Movement phase In addition such can Fall Back and charge in the same turn")
    let ``LEVIATHAN: SYNAPTIC IMPERATIVE`` = 
        Value
          (Str
             "Roll a D6 each time a unit with this adaptation loses a wound whilst it is within 6 of a friendly SYNAPSE unit from the same hive fleet On a 6 the damage is ignored and the unit does not lose a wound Ignore this adaptation on a unit that is currently affected by the Catalyst psychic power")
    let ``GORGON: ADAPTIVE TOXINS`` = 
        Value
          (Str "You can re-roll wound of 1 in the Fight phase for with this adaptation")
    let ``JORMUNGANDR: TUNNEL NETWORKS`` = 
        Value
          (Str
             "A unit with this adaptation other than that can FLY always has the benefit of cover for the of shooting If the unit Advances or charges however it loses the benefit of this adaptation until the start of your next Movement phase")
    let ``HYDRA: SWARMING INSTINCTS`` = 
        Value
          (Str
             "You can re-roll hit in the Fight phase for with this adaptation that target containing fewer than their own")
    let ``KRONOS: BIO-BARRAGE`` = 
        Value
          (Str
             "You can re-roll hit of 1 for with this adaptation in your Shooting phase if they did not move in the preceding Movement phase")
