namespace ``Warhammer 40,000 - Codex - Tyranids`` 
module Relic = 
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
    let ``Slayer Sabres`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "LEVIATHAN model with monstrous boneswords only. The Slayer Sabres replace the model’s monstrous boneswords and have the following profile:");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "ABILITIES");
                          Value
                            (Str
                               "A model armed with the Slayer Sabres can make 1 additional attack with them in the Fight phase. In addition, if an INFANTRY or BIKER model suffers damage from this weapon but is not slain, roll a D3 at the end of the Fight phase. If the result is greater than that model’s remaining number of wounds, it is slain.")]);
                    Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
                    Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
                    Value (ParamArray [Value (Str "S"); Value (Str "User")]);
                    Value (ParamArray [Value (Str "AP"); Value (Str "-2")]);
                    Value (ParamArray [Value (Str "D"); Value (Str "3")])])])
    let ``Slimer Maggot Infestation`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "HYDRA model with two deathspitters with slimer maggots only. The Slimer Maggot Infestation replaces the model’s two deathspitters with slimer maggots and has the following profile:");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "ABILITIES");
                          Value
                            (Str "You can re-roll failed wound rolls for this weapon.")]);
                    Value (ParamArray [Value (Str "RANGE"); Value (Str "24\"")]);
                    Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault 6")]);
                    Value (ParamArray [Value (Str "S"); Value (Str "7")]);
                    Value (ParamArray [Value (Str "AP"); Value (Str "-1")]);
                    Value (ParamArray [Value (Str "D"); Value (Str "1")])])])
    let ``Balethorn Cannon`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "KRONOS model with stranglethorn cannon only. The Balethorn Cannon replaces the model’s stranglethorn cannon and has the following profile:");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "ABILITIES");
                          Value
                            (Str
                               "You can add 1 to hit rolls for this weapon when attacking a unit with 10 or more models. Invulnerable saves cannot be taken against this weapon.")]);
                    Value (ParamArray [Value (Str "RANGE"); Value (Str "36\"")]);
                    Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault D6")]);
                    Value (ParamArray [Value (Str "S"); Value (Str "7")]);
                    Value (ParamArray [Value (Str "AP"); Value (Str "-1")]);
                    Value (ParamArray [Value (Str "D"); Value (Str "2")])])])
    let ``Miasma Cannon`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "Model with a heavy venom cannon only. The Miasma Cannon replaces the model’s heavy venom cannon and has the following profile:");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "ABILITIES");
                          Value
                            (Str
                               "This weapon hits automatically if the target unit is within 8\", and it always wounds targets (other than VEHICLES ) on a 2+.")]);
                    Value (ParamArray [Value (Str "RANGE"); Value (Str "36\"")]);
                    Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault D3")]);
                    Value (ParamArray [Value (Str "S"); Value (Str "9")]);
                    Value (ParamArray [Value (Str "AP"); Value (Str "-2")]);
                    Value (ParamArray [Value (Str "D"); Value (Str "3")])])])
    let ``Scythes of Tyran`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "BEHEMOTH model with monstrous scything talons only. The Scythes of Tyran replaces the model’s monstrous scything talons and has the following profile:");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "ABILITIES");
                          Value
                            (Str
                               "This model can make 1 additional attack with this weapon each time it fights. In addition, each time you make a hit roll of 6+ for this weapon, you can make an additional hit roll. These additional hit rolls cannot generate further additional hit rolls.")]);
                    Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
                    Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
                    Value (ParamArray [Value (Str "S"); Value (Str "+1")]);
                    Value (ParamArray [Value (Str "AP"); Value (Str "-3")]);
                    Value (ParamArray [Value (Str "D"); Value (Str "3")])])])
