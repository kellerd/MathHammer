namespace ``Warhammer 40,000 - Codex - Tyranids`` 
module Relic = 
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
    let ``Slayer Sabres`` = 
        Value
          (ParamArray
             [Value
                (ParamArray
                   [Value
                      (Str
                         "LEVIATHAN model with monstrous boneswords only. The Slayer Sabres replace the model’s monstrous boneswords and have the following profile:");
                    Value (Str "LEVIATHAN model with monstrous only");
                    App
                      (Lam ("obj",Var "obj"),
                       Value (ParamArray [Value (Str "Slayer"); Value (Str "Sabres")]));
                    Value (Str "replace");
                    App
                      (Lam ("obj",Var "obj"),
                       Value (ParamArray [Value (Str "model"); Value (Str "'s")]));
                    Value (Str "monstrous and have");
                    App
                      (Lam ("obj",Var "obj"),
                       Value
                         (ParamArray [Value (Str "following"); Value (Str "profile")]))]);
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "ABILITIES");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     "A model armed with the Slayer Sabres can make 1 additional attack with them in the Fight phase. In addition, if an INFANTRY or BIKER model suffers damage from this weapon but is not slain, roll a D3 at the end of the Fight phase. If the result is greater than that model’s remaining number of wounds, it is slain.");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "model"); Lam ("obj",Var "obj")]));
                                Value (Str "armed with");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "Slayer"); Value (Str "Sabres")]));
                                Value (Str "can make"); Value (Int 1);
                                Value (Str "additional attack with them in");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value
                                           (ParamArray
                                              [Value (Str "Phase"); Value (Str "Fight")]);
                                         Lam ("obj",Var "obj")]));
                                Value (Str "In addition if");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "INFANTRY"); Value (Str "or");
                                         Value (Str "BIKER"); Value (Str "model")]));
                                Call Suffer; Value (Str "damage from");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                                Value (Str "but is not slain roll");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [App (Call Dice,Value (Int 3)); Value (Int 1)]));
                                Value (Str "at");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "end"); Lam ("obj",Var "obj")]));
                                Value (Str "of");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value
                                           (ParamArray
                                              [Value (Str "Phase"); Value (Str "Fight")]);
                                         Lam ("obj",Var "obj")])); Value (Str "If");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "result"); Lam ("obj",Var "obj")]));
                                Value (Str "is greater than");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray [Value (Str "model"); Value (Str "'s")]));
                                Value (Str "remaining number of it is slain")])]);
                    Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
                    Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
                    Value (ParamArray [Value (Str "S"); Value (Str "User")]);
                    Value (ParamArray [Value (Str "AP"); Value (Str "-2")]);
                    Value (ParamArray [Value (Str "D"); Value (Str "3")])])])
    let ``Slimer Maggot Infestation`` = 
        Value
          (ParamArray
             [Value
                (ParamArray
                   [Value
                      (Str
                         "HYDRA model with two deathspitters with slimer maggots only. The Slimer Maggot Infestation replaces the model’s two deathspitters with slimer maggots and has the following profile:");
                    Value (Str "HYDRA model with two deathspitters with slimer only");
                    App
                      (Lam ("obj",Var "obj"),
                       Value
                         (ParamArray
                            [Value (Str "Slimer"); Value (Str "Maggot");
                             Value (Str "Infestation")])); Value (Str "replaces");
                    App
                      (Lam ("obj",Var "obj"),
                       Value (ParamArray [Value (Str "model"); Value (Str "'s")]));
                    Value (Str "two with slimer and has");
                    App
                      (Lam ("obj",Var "obj"),
                       Value
                         (ParamArray [Value (Str "following"); Value (Str "profile")]))]);
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "ABILITIES");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     "You can re-roll failed wound rolls for this weapon.");
                                Value (Str "You can re-roll failed wound for");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "weapon"); Lam ("obj",Var "obj")]))])]);
                    Value (ParamArray [Value (Str "RANGE"); Value (Str "24\"")]);
                    Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault 6")]);
                    Value (ParamArray [Value (Str "S"); Value (Str "7")]);
                    Value (ParamArray [Value (Str "AP"); Value (Str "-1")]);
                    Value (ParamArray [Value (Str "D"); Value (Str "1")])])])
    let ``Balethorn Cannon`` = 
        Value
          (ParamArray
             [Value
                (ParamArray
                   [Value
                      (Str
                         "KRONOS model with stranglethorn cannon only. The Balethorn Cannon replaces the model’s stranglethorn cannon and has the following profile:");
                    Value (Str "KRONOS model with stranglethorn cannon only");
                    App
                      (Lam ("obj",Var "obj"),
                       Value
                         (ParamArray [Value (Str "Balethorn"); Value (Str "Cannon")]));
                    Value (Str "replaces");
                    App
                      (Lam ("obj",Var "obj"),
                       Value (ParamArray [Value (Str "model"); Value (Str "'s")]));
                    Value (Str "stranglethorn cannon and has");
                    App
                      (Lam ("obj",Var "obj"),
                       Value
                         (ParamArray [Value (Str "following"); Value (Str "profile")]))]);
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "ABILITIES");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     "You can add 1 to hit rolls for this weapon when attacking a unit with 10 or more models. Invulnerable saves cannot be taken against this weapon.");
                                Value (Str "You can add"); Value (Int 1);
                                Value (Str "to hit for");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                                Value (Str "when attacking");
                                App
                                  (Call Repeat,
                                   Value (ParamArray [Var "Target"; Value (Int 1)]));
                                Value (Str "with"); Value (Int 10);
                                Value
                                  (Str
                                     "or more Invulnerable saves can not be taken against");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "weapon"); Lam ("obj",Var "obj")]))])]);
                    Value (ParamArray [Value (Str "RANGE"); Value (Str "36\"")]);
                    Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault D6")]);
                    Value (ParamArray [Value (Str "S"); Value (Str "7")]);
                    Value (ParamArray [Value (Str "AP"); Value (Str "-1")]);
                    Value (ParamArray [Value (Str "D"); Value (Str "2")])])])
    let ``Miasma Cannon`` = 
        Value
          (ParamArray
             [Value
                (ParamArray
                   [Value
                      (Str
                         "Model with a heavy venom cannon only. The Miasma Cannon replaces the model’s heavy venom cannon and has the following profile:");
                    Value (Str "Model with");
                    App
                      (Value (Int 1),
                       Value
                         (ParamArray
                            [Value (Str "heavy"); Value (Str "venom");
                             Value (Str "cannon")])); Value (Str "only");
                    App
                      (Lam ("obj",Var "obj"),
                       Value (ParamArray [Value (Str "Miasma"); Value (Str "Cannon")]));
                    Value (Str "replaces");
                    App
                      (Lam ("obj",Var "obj"),
                       Value (ParamArray [Value (Str "model"); Value (Str "'s")]));
                    Value (Str "heavy venom cannon and has");
                    App
                      (Lam ("obj",Var "obj"),
                       Value
                         (ParamArray [Value (Str "following"); Value (Str "profile")]))]);
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "ABILITIES");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     "This weapon hits automatically if the target unit is within 8\", and it always wounds targets (other than VEHICLES ) on a 2+.");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                                Value (Str "hits automatically if");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray [Value (Str "target"); Var "Target"]));
                                Value (Str "is within"); Value (Distance 8);
                                Value (Str "and it always other than VEHICLES on");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Lam
                                           ("roll",
                                            Let
                                              ("gt",
                                               App
                                                 (Call GreaterThan,
                                                  Value
                                                    (ParamArray
                                                       [Var "roll"; Value (Int 2)])),
                                               Let
                                                 ("eq",
                                                  App
                                                    (Call Equals,
                                                     Value
                                                       (ParamArray
                                                          [Var "roll"; Value (Int 2)])),
                                                  App
                                                    (Call Or,
                                                     Value
                                                       (ParamArray [Var "eq"; Var "gt"])))));
                                         Value (Int 1)]))])]);
                    Value (ParamArray [Value (Str "RANGE"); Value (Str "36\"")]);
                    Value (ParamArray [Value (Str "TYPE"); Value (Str "Assault D3")]);
                    Value (ParamArray [Value (Str "S"); Value (Str "9")]);
                    Value (ParamArray [Value (Str "AP"); Value (Str "-2")]);
                    Value (ParamArray [Value (Str "D"); Value (Str "3")])])])
    let ``Scythes of Tyran`` = 
        Value
          (ParamArray
             [Value
                (ParamArray
                   [Value
                      (Str
                         "BEHEMOTH model with monstrous scything talons only. The Scythes of Tyran replaces the model’s monstrous scything talons and has the following profile:");
                    Value (Str "BEHEMOTH model with monstrous scything talons only");
                    App
                      (Call Repeat,
                       Value (ParamArray [Value (Str "Scythes"); Lam ("obj",Var "obj")]));
                    Value (Str "of Tyran replaces");
                    App
                      (Lam ("obj",Var "obj"),
                       Value (ParamArray [Value (Str "model"); Value (Str "'s")]));
                    Value (Str "monstrous scything and has");
                    App
                      (Lam ("obj",Var "obj"),
                       Value
                         (ParamArray [Value (Str "following"); Value (Str "profile")]))]);
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "ABILITIES");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     "This model can make 1 additional attack with this weapon each time it fights. In addition, each time you make a hit roll of 6+ for this weapon, you can make an additional hit roll. These additional hit rolls cannot generate further additional hit rolls.");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "model"); Lam ("obj",Var "obj")]));
                                Value (Str "can make"); Value (Int 1);
                                Value (Str "additional attack with");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                                Lam
                                  ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                                Value (Str "time it fights In addition");
                                Lam
                                  ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                                Value (Str "time you make");
                                App
                                  (Value (Int 1),
                                   Value
                                     (ParamArray [Value (Str "hit"); Value (Str "roll")]));
                                Value (Str "of");
                                Lam
                                  ("roll",
                                   Let
                                     ("gt",
                                      App
                                        (Call GreaterThan,
                                         Value (ParamArray [Var "roll"; Value (Int 6)])),
                                      Let
                                        ("eq",
                                         App
                                           (Call Equals,
                                            Value
                                              (ParamArray [Var "roll"; Value (Int 6)])),
                                         App
                                           (Call Or,
                                            Value (ParamArray [Var "eq"; Var "gt"])))));
                                Value (Str "for");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                                Value (Str "you can make");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "additional"); Value (Str "hit");
                                         Value (Str "roll")]));
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "additional"); Value (Str "hit")]));
                                Value (Str "can not generate further additional hit")])]);
                    Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
                    Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
                    Value (ParamArray [Value (Str "S"); Value (Str "+1")]);
                    Value (ParamArray [Value (Str "AP"); Value (Str "-3")]);
                    Value (ParamArray [Value (Str "D"); Value (Str "3")])])])
