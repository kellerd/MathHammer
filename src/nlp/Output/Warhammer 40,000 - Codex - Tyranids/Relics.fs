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
                   [Value (Str "LEVIATHAN model with monstrous only");
                    Lam ("obj",Var "obj"); Value (Str "Slayer Sabres replace");
                    Lam ("obj",Var "obj"); Value (Str "model 's monstrous and have");
                    Lam ("obj",Var "obj"); Value (Str "following profile")]);
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "ABILITIES");
                          Value
                            (ParamArray
                               [Lam ("obj",Var "obj"); Value (Str "model armed with");
                                Lam ("obj",Var "obj");
                                Value (Str "Slayer Sabres can make"); Value (Int 1);
                                Value (Str "additional attack with them in");
                                Lam ("obj",Var "obj");
                                Value
                                  (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                                Value (Str "In addition if"); Lam ("obj",Var "obj");
                                Value (Str "INFANTRY or BIKER model"); Call Suffer;
                                Value (Str "damage from"); Lam ("obj",Var "obj");
                                Value (Str "weapon but is not slain roll");
                                Value (Int 1); App (Call Dice,Value (Int 3));
                                Value (Str "at"); Lam ("obj",Var "obj");
                                Value (Str "end of"); Lam ("obj",Var "obj");
                                Value
                                  (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                                Value (Str "If"); Lam ("obj",Var "obj");
                                Value (Str "result is greater than");
                                Lam ("obj",Var "obj");
                                Value (Str "model 's remaining number of it is slain")])]);
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
                   [Value (Str "HYDRA model with two with slimer only");
                    Lam ("obj",Var "obj");
                    Value (Str "Slimer Maggot Infestation replaces");
                    Lam ("obj",Var "obj");
                    Value (Str "model 's two with slimer and has");
                    Lam ("obj",Var "obj"); Value (Str "following profile")]);
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "ABILITIES");
                          Value
                            (ParamArray
                               [Value (Str "You can re-roll failed wound for");
                                Lam ("obj",Var "obj"); Value (Str "weapon")])]);
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
                   [Value (Str "model with stranglethorn cannon only");
                    Lam ("obj",Var "obj"); Value (Str "Balethorn Cannon replaces");
                    Lam ("obj",Var "obj");
                    Value (Str "model 's stranglethorn cannon and has");
                    Lam ("obj",Var "obj"); Value (Str "following profile")]);
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "ABILITIES");
                          Value
                            (ParamArray
                               [Value (Str "You can add"); Value (Int 1);
                                Value (Str "to hit for"); Lam ("obj",Var "obj");
                                Value (Str "weapon when attacking"); Value (Int 1);
                                Var "Target"; Value (Str "with"); Value (Int 10);
                                Value
                                  (Str
                                     "or more Invulnerable saves can not be taken against");
                                Lam ("obj",Var "obj"); Value (Str "weapon")])]);
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
                   [Value (Str "Model with"); Value (Int 1);
                    Value (Str "heavy venom cannon only"); Lam ("obj",Var "obj");
                    Value (Str "Miasma Cannon replaces"); Lam ("obj",Var "obj");
                    Value (Str "model 's heavy venom cannon and has");
                    Lam ("obj",Var "obj"); Value (Str "following profile")]);
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "ABILITIES");
                          Value
                            (ParamArray
                               [Lam ("obj",Var "obj");
                                Value (Str "weapon hits automatically if");
                                Lam ("obj",Var "obj"); Value (Str "target");
                                Var "Target"; Value (Str "is within");
                                Value (Distance 8);
                                Value (Str "and it always other than VEHICLES on");
                                Value (Int 1);
                                Lam
                                  ("roll",
                                   Let
                                     ("gt",
                                      App
                                        (Call GreaterThan,
                                         Value (ParamArray [Var "roll"; Value (Int 2)])),
                                      Let
                                        ("eq",
                                         App
                                           (Call Equals,
                                            Value
                                              (ParamArray [Var "roll"; Value (Int 2)])),
                                         App
                                           (Call Or,
                                            Value (ParamArray [Var "eq"; Var "gt"])))))])]);
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
                   [Value (Str "BEHEMOTH model with monstrous scything only");
                    Lam ("obj",Var "obj"); Value (Str "Scythes of Tyran replaces");
                    Lam ("obj",Var "obj");
                    Value (Str "model 's monstrous scything and has");
                    Lam ("obj",Var "obj"); Value (Str "following profile")]);
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "ABILITIES");
                          Value
                            (ParamArray
                               [Lam ("obj",Var "obj"); Value (Str "model can make");
                                Value (Int 1); Value (Str "additional attack with");
                                Lam ("obj",Var "obj"); Value (Str "weapon");
                                Lam
                                  ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                                Value (Str "time it In addition");
                                Lam
                                  ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                                Value (Str "time you make"); Value (Int 1);
                                Value (Str "hit roll of");
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
                                Value (Str "for"); Lam ("obj",Var "obj");
                                Value (Str "weapon you can make"); Lam ("obj",Var "obj");
                                Value (Str "additional hit roll"); Lam ("obj",Var "obj");
                                Value
                                  (Str
                                     "additional hit can not generate further additional hit")])]);
                    Value (ParamArray [Value (Str "RANGE"); Value (Str "Melee")]);
                    Value (ParamArray [Value (Str "TYPE"); Value (Str "Melee")]);
                    Value (ParamArray [Value (Str "S"); Value (Str "+1")]);
                    Value (ParamArray [Value (Str "AP"); Value (Str "-3")]);
                    Value (ParamArray [Value (Str "D"); Value (Str "3")])])])
