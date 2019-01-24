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
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem in your Psychic phase if a Zoanthropes unit from your army consisting of at least 3 models is within 6\" of 2 other such units. If you do so, the Zoanthropes cannot take any Psychic tests this phase – instead, select a point on the battlefield within 18\" of, and visible to, all three units. Roll a dice for each unit (friend or foe) within 3\" of that point. Add 1 to the result if the unit being rolled for has 10 or more models, but subtract 1 if the unit being rolled for is a CHARACTER . On a 4+ that unit suffers 3D3 mortal wounds.");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "in your Psychic phase if");
                      App
                        (Value (Int 1),
                         Value (ParamArray [Value (Str "Zoanthropes"); Var "Target"]));
                      Value (Str "from your army consisting of at least"); Value (Int 3);
                      Value (Str "is within"); Value (Distance 6); Value (Str "of");
                      Value (Int 2); Value (Str "other such If you do so");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray
                              [Value (Str "Zoanthropes"); Lam ("obj",Var "obj")]));
                      Value (Str "can not take");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Psychic"); Lam ("obj",Var "obj")]));
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "phase"); Lam ("obj",Var "obj")]));
                      Value (Str "instead select");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "point"); Value (Int 1)]));
                      Value (Str "on");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray
                              [Value (Str "battlefield"); Lam ("obj",Var "obj")]));
                      Value (Str "within"); Value (Distance 18);
                      Value (Str "of and visible to all three Roll");
                      App (Value (Int 1),Value (ParamArray [])); Value (Str "for");
                      Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                      Var "Target"; Value (Str "friend or foe within");
                      Value (Distance 3); Value (Str "of");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "point"); Lam ("obj",Var "obj")]));
                      Value (Str "Add"); Value (Int 1); Value (Str "to");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "result"); Lam ("obj",Var "obj")]));
                      Value (Str "if");
                      App
                        (Call Repeat,
                         Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                      Value (Str "being rolled for has"); Value (Int 10);
                      Value (Str "or more but subtract"); Value (Int 1);
                      Value (Str "if");
                      App
                        (Call Repeat,
                         Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                      Value (Str "being rolled for is");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "CHARACTER"); Value (Int 1)]));
                      Value (Str "On");
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
                                        Value (ParamArray [Var "roll"; Value (Int 4)])),
                                     Let
                                       ("eq",
                                        App
                                          (Call Equals,
                                           Value
                                             (ParamArray [Var "roll"; Value (Int 4)])),
                                        App
                                          (Call Or,
                                           Value (ParamArray [Var "eq"; Var "gt"])))));
                               Value (Int 1)]));
                      App
                        (Call Repeat,
                         Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                      Call Suffer; Value (Str "3D3 mortal")]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "PSYCHIC BARRAGE"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem in your Psychic phase if a Zoanthropes unit from your army consisting of at least 3 models is within 6\" of 2 other such units. If you do so, the Zoanthropes cannot take any Psychic tests this phase – instead, select a point on the battlefield within 18\" of, and visible to, all three units. Roll a dice for each unit (friend or foe) within 3\" of that point. Add 1 to the result if the unit being rolled for has 10 or more models, but subtract 1 if the unit being rolled for is a CHARACTER . On a 4+ that unit suffers 3D3 mortal wounds.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "in your Psychic phase if");
                         App
                           (Value (Int 1),
                            Value (ParamArray [Value (Str "Zoanthropes"); Var "Target"]));
                         Value (Str "from your army consisting of at least");
                         Value (Int 3); Value (Str "is within"); Value (Distance 6);
                         Value (Str "of"); Value (Int 2);
                         Value (Str "other such If you do so");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Zoanthropes"); Lam ("obj",Var "obj")]));
                         Value (Str "can not take");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "Psychic"); Lam ("obj",Var "obj")]));
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "phase"); Lam ("obj",Var "obj")]));
                         Value (Str "instead select");
                         App
                           (Call Repeat,
                            Value (ParamArray [Value (Str "point"); Value (Int 1)]));
                         Value (Str "on");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "battlefield"); Lam ("obj",Var "obj")]));
                         Value (Str "within"); Value (Distance 18);
                         Value (Str "of and visible to all three Roll");
                         App (Value (Int 1),Value (ParamArray [])); Value (Str "for");
                         Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                         Var "Target"; Value (Str "friend or foe within");
                         Value (Distance 3); Value (Str "of");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "point"); Lam ("obj",Var "obj")]));
                         Value (Str "Add"); Value (Int 1); Value (Str "to");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "result"); Lam ("obj",Var "obj")]));
                         Value (Str "if");
                         App
                           (Call Repeat,
                            Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                         Value (Str "being rolled for has"); Value (Int 10);
                         Value (Str "or more but subtract"); Value (Int 1);
                         Value (Str "if");
                         App
                           (Call Repeat,
                            Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                         Value (Str "being rolled for is");
                         App
                           (Call Repeat,
                            Value (ParamArray [Value (Str "CHARACTER"); Value (Int 1)]));
                         Value (Str "On");
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
                                             (ParamArray [Var "roll"; Value (Int 4)])),
                                        Let
                                          ("eq",
                                           App
                                             (Call Equals,
                                              Value
                                                (ParamArray [Var "roll"; Value (Int 4)])),
                                           App
                                             (Call Or,
                                              Value (ParamArray [Var "eq"; Var "gt"])))));
                                  Value (Int 1)]));
                         App
                           (Call Repeat,
                            Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                         Call Suffer; Value (Str "3D3 mortal")]),None))]),None)
    let ``RAPID REGENERATION`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("RAPID REGENERATION",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem at the end of your Movement phase. Select a TYRANIDS model from your army. It regains D3 wounds lost earlier in the battle.");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "at");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
                      Value (Str "of your Movement phase Select");
                      App
                        (Value (Int 1),
                         Value
                           (ParamArray [Value (Str "TYRANIDS"); Value (Str "model")]));
                      Value (Str "from your army It regains");
                      App (Call Dice,Value (Int 3)); Value (Str "lost earlier in");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "battle"); Lam ("obj",Var "obj")]))]));
               ("2",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "RAPID REGENERATION"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem at the end of your Movement phase. Select a TYRANIDS model from your army. It regains D3 wounds lost earlier in the battle.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "at");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
                         Value (Str "of your Movement phase Select");
                         App
                           (Value (Int 1),
                            Value
                              (ParamArray [Value (Str "TYRANIDS"); Value (Str "model")]));
                         Value (Str "from your army It regains");
                         App (Call Dice,Value (Int 3)); Value (Str "lost earlier in");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "battle"); Lam ("obj",Var "obj")]))]),
                   None))]),None)
    let ``CAUSTIC BLOOD`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("CAUSTIC BLOOD",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem at the start of a Fight phase. Select a TYRANIDS unit from your army. Roll a dice whenever a model in that unit is destroyed in this phase. For each roll of 6, the enemy unit that inflicted the final wound on that model suffers a mortal wound after all of their attacks have been resolved.");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "at");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "start"); Lam ("obj",Var "obj")]));
                      Value (Str "of");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray
                              [Value
                                 (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                               Value (Int 1)])); Value (Str "Select");
                      App
                        (Value (Int 1),
                         Value (ParamArray [Value (Str "TYRANIDS"); Var "Target"]));
                      Value (Str "from your army Roll");
                      App (Value (Int 1),Value (ParamArray [])); Value (Str "whenever");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "model"); Value (Int 1)]));
                      Value (Str "in");
                      App
                        (Call Repeat,
                         Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                      Value (Str "is destroyed in");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "phase"); Lam ("obj",Var "obj")]));
                      Value (Str "For");
                      Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                      Value (Str "roll of"); Value (Int 6);
                      App
                        (Lam ("obj",Var "obj"),
                         Value (ParamArray [Var "Target"; Var "Target"]));
                      Value (Str "that inflicted");
                      App
                        (Lam ("obj",Var "obj"),
                         Value (ParamArray [Value (Str "final"); Value (Str "wound")]));
                      Value (Str "on");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                      Call Suffer;
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "mortal"); Value (Int 1)]));
                      Value (Str "wound after");
                      App (Lam ("obj",Var "obj"),Value (ParamArray []));
                      Value (Str "of their have been resolved")]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "CAUSTIC BLOOD"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem at the start of a Fight phase. Select a TYRANIDS unit from your army. Roll a dice whenever a model in that unit is destroyed in this phase. For each roll of 6, the enemy unit that inflicted the final wound on that model suffers a mortal wound after all of their attacks have been resolved.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "at");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "start"); Lam ("obj",Var "obj")]));
                         Value (Str "of");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value
                                    (ParamArray
                                       [Value (Str "Phase"); Value (Str "Fight")]);
                                  Value (Int 1)])); Value (Str "Select");
                         App
                           (Value (Int 1),
                            Value (ParamArray [Value (Str "TYRANIDS"); Var "Target"]));
                         Value (Str "from your army Roll");
                         App (Value (Int 1),Value (ParamArray []));
                         Value (Str "whenever");
                         App
                           (Call Repeat,
                            Value (ParamArray [Value (Str "model"); Value (Int 1)]));
                         Value (Str "in");
                         App
                           (Call Repeat,
                            Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                         Value (Str "is destroyed in");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "phase"); Lam ("obj",Var "obj")]));
                         Value (Str "For");
                         Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                         Value (Str "roll of"); Value (Int 6);
                         App
                           (Lam ("obj",Var "obj"),
                            Value (ParamArray [Var "Target"; Var "Target"]));
                         Value (Str "that inflicted");
                         App
                           (Lam ("obj",Var "obj"),
                            Value
                              (ParamArray [Value (Str "final"); Value (Str "wound")]));
                         Value (Str "on");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                         Call Suffer;
                         App
                           (Call Repeat,
                            Value (ParamArray [Value (Str "mortal"); Value (Int 1)]));
                         Value (Str "wound after");
                         App (Lam ("obj",Var "obj"),Value (ParamArray []));
                         Value (Str "of their have been resolved")]),None))]),None)
    let ``SCORCH BUGS`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("SCORCH BUGS",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem when a TYRANIDS unit from your army is selected to attack in the Shooting phase. You can add 1 to all wound rolls made for that unit’s fleshborer or fleshborer hive attacks in that Shooting phase.");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "when");
                      App
                        (Value (Int 1),
                         Value (ParamArray [Value (Str "TYRANIDS"); Var "Target"]));
                      Value (Str "from your army is selected to attack in");
                      App
                        (Lam ("obj",Var "obj"),
                         Value
                           (ParamArray [Value (Str "Shooting"); Value (Str "phase")]));
                      Value (Str "You can add"); Value (Int 1); Value (Str "to");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "wound"); Lam ("obj",Var "obj")]));
                      Value (Str "made for");
                      App
                        (Lam ("obj",Var "obj"),
                         Value (ParamArray [Var "Target"; Value (Str "'s")]));
                      Value (Str "fleshborer or fleshborer hive in");
                      App
                        (Lam ("obj",Var "obj"),
                         Value
                           (ParamArray [Value (Str "Shooting"); Value (Str "phase")]))]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "SCORCH BUGS"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem when a TYRANIDS unit from your army is selected to attack in the Shooting phase. You can add 1 to all wound rolls made for that unit’s fleshborer or fleshborer hive attacks in that Shooting phase.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "when");
                         App
                           (Value (Int 1),
                            Value (ParamArray [Value (Str "TYRANIDS"); Var "Target"]));
                         Value (Str "from your army is selected to attack in");
                         App
                           (Lam ("obj",Var "obj"),
                            Value
                              (ParamArray [Value (Str "Shooting"); Value (Str "phase")]));
                         Value (Str "You can add"); Value (Int 1); Value (Str "to");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "wound"); Lam ("obj",Var "obj")]));
                         Value (Str "made for");
                         App
                           (Lam ("obj",Var "obj"),
                            Value (ParamArray [Var "Target"; Value (Str "'s")]));
                         Value (Str "fleshborer or fleshborer hive in");
                         App
                           (Lam ("obj",Var "obj"),
                            Value
                              (ParamArray [Value (Str "Shooting"); Value (Str "phase")]))]),
                   None))]),None)
    let ``IMPLANT ATTACK`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("IMPLANT ATTACK",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem after a TYRANIDS unit from your army fights in the Fight phase. Roll a dice for each enemy model (other than a VEHICLE ) that was wounded by any of this unit’s attacks and not slain. On a 2+ the model suffers a mortal wound.");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "after");
                      App
                        (Value (Int 1),
                         Value (ParamArray [Value (Str "TYRANIDS"); Var "Target"]));
                      Value (Str "from your army in");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray
                              [Value
                                 (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                               Lam ("obj",Var "obj")])); Value (Str "Roll");
                      App (Value (Int 1),Value (ParamArray [])); Value (Str "for");
                      Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                      Var "Target"; Value (Str "model other than");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "VEHICLE"); Value (Int 1)]));
                      Value (Str "that was wounded by");
                      App (Lam ("obj",Var "obj"),Value (ParamArray []));
                      Value (Str "of");
                      App
                        (Lam ("obj",Var "obj"),
                         Value (ParamArray [Var "Target"; Value (Str "'s")]));
                      Value (Str "and not slain On");
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
                                        Value (ParamArray [Var "roll"; Value (Int 2)])),
                                     Let
                                       ("eq",
                                        App
                                          (Call Equals,
                                           Value
                                             (ParamArray [Var "roll"; Value (Int 2)])),
                                        App
                                          (Call Or,
                                           Value (ParamArray [Var "eq"; Var "gt"])))));
                               Value (Int 1)]));
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                      Call Suffer;
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "Mortal Wound"); Value (Int 1)]))]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "IMPLANT ATTACK"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem after a TYRANIDS unit from your army fights in the Fight phase. Roll a dice for each enemy model (other than a VEHICLE ) that was wounded by any of this unit’s attacks and not slain. On a 2+ the model suffers a mortal wound.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "after");
                         App
                           (Value (Int 1),
                            Value (ParamArray [Value (Str "TYRANIDS"); Var "Target"]));
                         Value (Str "from your army in");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value
                                    (ParamArray
                                       [Value (Str "Phase"); Value (Str "Fight")]);
                                  Lam ("obj",Var "obj")])); Value (Str "Roll");
                         App (Value (Int 1),Value (ParamArray [])); Value (Str "for");
                         Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                         Var "Target"; Value (Str "model other than");
                         App
                           (Call Repeat,
                            Value (ParamArray [Value (Str "VEHICLE"); Value (Int 1)]));
                         Value (Str "that was wounded by");
                         App (Lam ("obj",Var "obj"),Value (ParamArray []));
                         Value (Str "of");
                         App
                           (Lam ("obj",Var "obj"),
                            Value (ParamArray [Var "Target"; Value (Str "'s")]));
                         Value (Str "and not slain On");
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
                                             (ParamArray [Var "roll"; Value (Int 2)])),
                                        Let
                                          ("eq",
                                           App
                                             (Call Equals,
                                              Value
                                                (ParamArray [Var "roll"; Value (Int 2)])),
                                           App
                                             (Call Or,
                                              Value (ParamArray [Var "eq"; Var "gt"])))));
                                  Value (Int 1)]));
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                         Call Suffer;
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "Mortal Wound"); Value (Int 1)]))]),
                   None))]),None)
    let ``BOUNTY OF THE HIVE FLEET`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("BOUNTY OF THE HIVE FLEET",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem before the battle. Your army can have one extra Bio-artefact for 1 CP, or two extra Bio-artefacts for 3 CPs. All of the Bio-artefacts that you include must be different and be given to different TYRANIDS CHARACTERS . You can only use this Stratagem once per battle.");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "before");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "battle"); Lam ("obj",Var "obj")]));
                      Value (Str "Your army can have one extra Bio-artefact for");
                      Value (Int 1); Value (Str "CP or two extra for"); Value (Int 3);
                      App (Lam ("obj",Var "obj"),Value (ParamArray []));
                      Value (Str "of");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray
                              [Value (Str "Bio-artefacts"); Lam ("obj",Var "obj")]));
                      Let
                        ("ThatSubject",
                         Value
                           (ParamArray
                              [Var "you";
                               Value
                                 (Str
                                    "must be different and be given to different TYRANIDS You can only use");
                               App
                                 (Call Repeat,
                                  Value
                                    (ParamArray
                                       [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                               Value (Str "once per battle")]),
                         Value
                           (ParamArray
                              [Let
                                 ("ThatObject",
                                  Value
                                    (ParamArray
                                       [Value
                                          (Str
                                             "must be different and be given to different TYRANIDS You can only use");
                                        App
                                          (Call Repeat,
                                           Value
                                             (ParamArray
                                                [Value (Str "Stratagem");
                                                 Lam ("obj",Var "obj")]));
                                        Value (Str "once per battle")]),
                                  Value
                                    (ParamArray
                                       [App
                                          (Value (Str "include"),
                                           Value
                                             (ParamArray
                                                [Var "ThatSubject"; Var "ThatObject"]));
                                        Value
                                          (Str
                                             "must be different and be given to different TYRANIDS You can only use");
                                        App
                                          (Call Repeat,
                                           Value
                                             (ParamArray
                                                [Value (Str "Stratagem");
                                                 Lam ("obj",Var "obj")]));
                                        Value (Str "once per battle")]))]))]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value
                        (ParamArray [Var "Available CP"; Var "BOUNTY OF THE HIVE FLEET"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem before the battle. Your army can have one extra Bio-artefact for 1 CP, or two extra Bio-artefacts for 3 CPs. All of the Bio-artefacts that you include must be different and be given to different TYRANIDS CHARACTERS . You can only use this Stratagem once per battle.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "before");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "battle"); Lam ("obj",Var "obj")]));
                         Value (Str "Your army can have one extra Bio-artefact for");
                         Value (Int 1); Value (Str "CP or two extra for"); Value (Int 3);
                         App (Lam ("obj",Var "obj"),Value (ParamArray []));
                         Value (Str "of");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Bio-artefacts"); Lam ("obj",Var "obj")]));
                         Let
                           ("ThatSubject",
                            Value
                              (ParamArray
                                 [Var "you";
                                  Value
                                    (Str
                                       "must be different and be given to different TYRANIDS You can only use");
                                  App
                                    (Call Repeat,
                                     Value
                                       (ParamArray
                                          [Value (Str "Stratagem");
                                           Lam ("obj",Var "obj")]));
                                  Value (Str "once per battle")]),
                            Value
                              (ParamArray
                                 [Let
                                    ("ThatObject",
                                     Value
                                       (ParamArray
                                          [Value
                                             (Str
                                                "must be different and be given to different TYRANIDS You can only use");
                                           App
                                             (Call Repeat,
                                              Value
                                                (ParamArray
                                                   [Value (Str "Stratagem");
                                                    Lam ("obj",Var "obj")]));
                                           Value (Str "once per battle")]),
                                     Value
                                       (ParamArray
                                          [App
                                             (Value (Str "include"),
                                              Value
                                                (ParamArray
                                                   [Var "ThatSubject"; Var "ThatObject"]));
                                           Value
                                             (Str
                                                "must be different and be given to different TYRANIDS You can only use");
                                           App
                                             (Call Repeat,
                                              Value
                                                (ParamArray
                                                   [Value (Str "Stratagem");
                                                    Lam ("obj",Var "obj")]));
                                           Value (Str "once per battle")]))]))]),None));
               ("3",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value
                        (ParamArray [Var "Available CP"; Var "BOUNTY OF THE HIVE FLEET"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem before the battle. Your army can have one extra Bio-artefact for 1 CP, or two extra Bio-artefacts for 3 CPs. All of the Bio-artefacts that you include must be different and be given to different TYRANIDS CHARACTERS . You can only use this Stratagem once per battle.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "before");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "battle"); Lam ("obj",Var "obj")]));
                         Value (Str "Your army can have one extra Bio-artefact for");
                         Value (Int 1); Value (Str "CP or two extra for"); Value (Int 3);
                         App (Lam ("obj",Var "obj"),Value (ParamArray []));
                         Value (Str "of");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Bio-artefacts"); Lam ("obj",Var "obj")]));
                         Let
                           ("ThatSubject",
                            Value
                              (ParamArray
                                 [Var "you";
                                  Value
                                    (Str
                                       "must be different and be given to different TYRANIDS You can only use");
                                  App
                                    (Call Repeat,
                                     Value
                                       (ParamArray
                                          [Value (Str "Stratagem");
                                           Lam ("obj",Var "obj")]));
                                  Value (Str "once per battle")]),
                            Value
                              (ParamArray
                                 [Let
                                    ("ThatObject",
                                     Value
                                       (ParamArray
                                          [Value
                                             (Str
                                                "must be different and be given to different TYRANIDS You can only use");
                                           App
                                             (Call Repeat,
                                              Value
                                                (ParamArray
                                                   [Value (Str "Stratagem");
                                                    Lam ("obj",Var "obj")]));
                                           Value (Str "once per battle")]),
                                     Value
                                       (ParamArray
                                          [App
                                             (Value (Str "include"),
                                              Value
                                                (ParamArray
                                                   [Var "ThatSubject"; Var "ThatObject"]));
                                           Value
                                             (Str
                                                "must be different and be given to different TYRANIDS You can only use");
                                           App
                                             (Call Repeat,
                                              Value
                                                (ParamArray
                                                   [Value (Str "Stratagem");
                                                    Lam ("obj",Var "obj")]));
                                           Value (Str "once per battle")]))]))]),None))]),
           None)
    let ``METABOLIC OVERDRIVE`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("METABOLIC OVERDRIVE",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem in your Movement phase, after moving a TYRANIDS unit from your army. You can make a second move with that unit (including Advancing, if you wish), but when you do so you must roll a dice for each model in the unit. For each roll of 1, inflict a mortal wound on the unit. The unit cannot shoot or make a charge move this turn.");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "in your Movement phase after moving");
                      App
                        (Value (Int 1),
                         Value (ParamArray [Value (Str "TYRANIDS"); Var "Target"]));
                      Value (Str "from your army You can make");
                      App
                        (Value (Int 1),
                         Value (ParamArray [Value (Str "second"); Value (Str "move")]));
                      Value (Str "with");
                      App
                        (Call Repeat,
                         Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                      Value
                        (Str
                           "including Advancing if you wish but when you do so you must roll");
                      App (Value (Int 1),Value (ParamArray [])); Value (Str "for");
                      Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                      Value (Str "model in");
                      App
                        (Call Repeat,
                         Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                      Value (Str "For");
                      Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                      Value (Str "roll of"); Value (Int 1); Value (Str "inflict");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "Mortal Wound"); Value (Int 1)]));
                      Value (Str "on");
                      App
                        (Call Repeat,
                         Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                      App
                        (Call Repeat,
                         Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                      Value (Str "can not shoot or make");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "charge"); Value (Int 1)]));
                      Value (Str "move");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "turn"); Lam ("obj",Var "obj")]))]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "METABOLIC OVERDRIVE"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem in your Movement phase, after moving a TYRANIDS unit from your army. You can make a second move with that unit (including Advancing, if you wish), but when you do so you must roll a dice for each model in the unit. For each roll of 1, inflict a mortal wound on the unit. The unit cannot shoot or make a charge move this turn.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "in your Movement phase after moving");
                         App
                           (Value (Int 1),
                            Value (ParamArray [Value (Str "TYRANIDS"); Var "Target"]));
                         Value (Str "from your army You can make");
                         App
                           (Value (Int 1),
                            Value
                              (ParamArray [Value (Str "second"); Value (Str "move")]));
                         Value (Str "with");
                         App
                           (Call Repeat,
                            Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                         Value
                           (Str
                              "including Advancing if you wish but when you do so you must roll");
                         App (Value (Int 1),Value (ParamArray [])); Value (Str "for");
                         Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                         Value (Str "model in");
                         App
                           (Call Repeat,
                            Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                         Value (Str "For");
                         Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                         Value (Str "roll of"); Value (Int 1); Value (Str "inflict");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "Mortal Wound"); Value (Int 1)]));
                         Value (Str "on");
                         App
                           (Call Repeat,
                            Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                         App
                           (Call Repeat,
                            Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                         Value (Str "can not shoot or make");
                         App
                           (Call Repeat,
                            Value (ParamArray [Value (Str "charge"); Value (Int 1)]));
                         Value (Str "move");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "turn"); Lam ("obj",Var "obj")]))]),
                   None))]),None)
    let ``FEEDER TENDRILS`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("FEEDER TENDRILS",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem when a Genestealer, LICTOR , Toxicrene or Venomthrope from your army kills a CHARACTER in the Fight phase. Gain D3 Command Points.");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "when");
                      App
                        (Value (Int 1),
                         Value
                           (ParamArray
                              [Value (Str "Genestealer"); Value (Str "LICTOR");
                               Value (Str "Toxicrene"); Value (Str "or");
                               Value (Str "Venomthrope")]));
                      Value (Str "from your army kills");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "CHARACTER"); Value (Int 1)]));
                      Value (Str "in");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray
                              [Value
                                 (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                               Lam ("obj",Var "obj")])); Value (Str "Gain");
                      App (Call Dice,Value (Int 3)); Value (Str "Command Points")]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "FEEDER TENDRILS"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem when a Genestealer, LICTOR , Toxicrene or Venomthrope from your army kills a CHARACTER in the Fight phase. Gain D3 Command Points.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "when");
                         App
                           (Value (Int 1),
                            Value
                              (ParamArray
                                 [Value (Str "Genestealer"); Value (Str "LICTOR");
                                  Value (Str "Toxicrene"); Value (Str "or");
                                  Value (Str "Venomthrope")]));
                         Value (Str "from your army kills");
                         App
                           (Call Repeat,
                            Value (ParamArray [Value (Str "CHARACTER"); Value (Int 1)]));
                         Value (Str "in");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value
                                    (ParamArray
                                       [Value (Str "Phase"); Value (Str "Fight")]);
                                  Lam ("obj",Var "obj")])); Value (Str "Gain");
                         App (Call Dice,Value (Int 3)); Value (Str "Command Points")]),
                   None))]),None)
    let ``PATHOGENIC SLIME`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("PATHOGENIC SLIME",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem in your Shooting phase. Select a TYRANIDS MONSTER from your army. Increase the Damage of its attacks by 1 for this phase.");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "in your Shooting phase Select");
                      App
                        (Value (Int 1),
                         Value
                           (ParamArray [Value (Str "TYRANIDS"); Value (Str "MONSTER")]));
                      Value (Str "from your army Increase");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Damage"); Lam ("obj",Var "obj")]));
                      Value (Str "of its by"); Value (Int 1); Value (Str "for");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "phase"); Lam ("obj",Var "obj")]))]));
               ("2",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "PATHOGENIC SLIME"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem in your Shooting phase. Select a TYRANIDS MONSTER from your army. Increase the Damage of its attacks by 1 for this phase.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "in your Shooting phase Select");
                         App
                           (Value (Int 1),
                            Value
                              (ParamArray
                                 [Value (Str "TYRANIDS"); Value (Str "MONSTER")]));
                         Value (Str "from your army Increase");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "Damage"); Lam ("obj",Var "obj")]));
                         Value (Str "of its by"); Value (Int 1); Value (Str "for");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "phase"); Lam ("obj",Var "obj")]))]),
                   None))]),None)
    let ``VORACIOUS APPETITE`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("VORACIOUS APPETITE",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem in the Fight phase when a TYRANIDS MONSTER or CHARACTER from your army is chosen to attack. You can re-roll all failed wound rolls for that model until the end of the phase.");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "in");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray
                              [Value
                                 (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                               Lam ("obj",Var "obj")])); Value (Str "when");
                      App
                        (Value (Int 1),
                         Value
                           (ParamArray [Value (Str "TYRANIDS"); Value (Str "MONSTER")]));
                      Value
                        (Str
                           "or CHARACTER from your army is chosen to attack You can re-roll");
                      App
                        (Lam ("obj",Var "obj"),
                         Value (ParamArray [Value (Str "failed"); Value (Str "wound")]));
                      Value (Str "for");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                      Value (Str "until");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
                      Value (Str "of");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "phase"); Lam ("obj",Var "obj")]))]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "VORACIOUS APPETITE"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem in the Fight phase when a TYRANIDS MONSTER or CHARACTER from your army is chosen to attack. You can re-roll all failed wound rolls for that model until the end of the phase.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "in");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value
                                    (ParamArray
                                       [Value (Str "Phase"); Value (Str "Fight")]);
                                  Lam ("obj",Var "obj")])); Value (Str "when");
                         App
                           (Value (Int 1),
                            Value
                              (ParamArray
                                 [Value (Str "TYRANIDS"); Value (Str "MONSTER")]));
                         Value
                           (Str
                              "or CHARACTER from your army is chosen to attack You can re-roll");
                         App
                           (Lam ("obj",Var "obj"),
                            Value
                              (ParamArray [Value (Str "failed"); Value (Str "wound")]));
                         Value (Str "for");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                         Value (Str "until");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
                         Value (Str "of");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "phase"); Lam ("obj",Var "obj")]))]),
                   None))]),None)
    let ``PHEROMONE TRAIL`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("PHEROMONE TRAIL",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem when a TYRANIDS INFANTRY unit from your army is set up on the battlefield as reinforcements if there is already a LICTOR from your army on the battlefield. You can set up the unit wholly within 6\" of the LICTOR and more than 9\" from any enemy models, rather than following the normal rules for setting up the unit.");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "when");
                      App
                        (Value (Int 1),
                         Value
                           (ParamArray
                              [Value (Str "TYRANIDS"); Value (Str "INFANTRY");
                               Var "Target"]));
                      Value (Str "from your army is set up on");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray
                              [Value (Str "battlefield"); Lam ("obj",Var "obj")]));
                      Value (Str "as if there is already");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "LICTOR"); Value (Int 1)]));
                      Value (Str "from your army on");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray
                              [Value (Str "battlefield"); Lam ("obj",Var "obj")]));
                      Value (Str "You can set up");
                      App
                        (Call Repeat,
                         Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                      Value (Str "wholly within"); Value (Distance 6); Value (Str "of");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "LICTOR"); Lam ("obj",Var "obj")]));
                      Value (Str "and more than"); Value (Distance 9);
                      Value (Str "from");
                      App
                        (Call Repeat,
                         Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                      Value (Str "rather than following");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "normal"); Lam ("obj",Var "obj")]));
                      Value (Str "for setting up");
                      App
                        (Call Repeat,
                         Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]))]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "PHEROMONE TRAIL"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem when a TYRANIDS INFANTRY unit from your army is set up on the battlefield as reinforcements if there is already a LICTOR from your army on the battlefield. You can set up the unit wholly within 6\" of the LICTOR and more than 9\" from any enemy models, rather than following the normal rules for setting up the unit.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "when");
                         App
                           (Value (Int 1),
                            Value
                              (ParamArray
                                 [Value (Str "TYRANIDS"); Value (Str "INFANTRY");
                                  Var "Target"]));
                         Value (Str "from your army is set up on");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "battlefield"); Lam ("obj",Var "obj")]));
                         Value (Str "as if there is already");
                         App
                           (Call Repeat,
                            Value (ParamArray [Value (Str "LICTOR"); Value (Int 1)]));
                         Value (Str "from your army on");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "battlefield"); Lam ("obj",Var "obj")]));
                         Value (Str "You can set up");
                         App
                           (Call Repeat,
                            Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                         Value (Str "wholly within"); Value (Distance 6);
                         Value (Str "of");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "LICTOR"); Lam ("obj",Var "obj")]));
                         Value (Str "and more than"); Value (Distance 9);
                         Value (Str "from");
                         App
                           (Call Repeat,
                            Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                         Value (Str "rather than following");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "normal"); Lam ("obj",Var "obj")]));
                         Value (Str "for setting up");
                         App
                           (Call Repeat,
                            Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]))]),
                   None))]),None)
    let ``SINGLE-MINDED ANNIHILATION`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("SINGLE-MINDED ANNIHILATION",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem at the end of your Shooting phase. Select a TYRANIDS INFANTRY unit from your army – that unit can immediately shoot again.");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "at");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
                      Value (Str "of your Shooting phase Select");
                      App
                        (Value (Int 1),
                         Value
                           (ParamArray
                              [Value (Str "TYRANIDS"); Value (Str "INFANTRY");
                               Var "Target"])); Value (Str "from your army");
                      App
                        (Call Repeat,
                         Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                      Value (Str "can immediately shoot again")]));
               ("2",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value
                        (ParamArray
                           [Var "Available CP"; Var "SINGLE-MINDED ANNIHILATION"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem at the end of your Shooting phase. Select a TYRANIDS INFANTRY unit from your army – that unit can immediately shoot again.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "at");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
                         Value (Str "of your Shooting phase Select");
                         App
                           (Value (Int 1),
                            Value
                              (ParamArray
                                 [Value (Str "TYRANIDS"); Value (Str "INFANTRY");
                                  Var "Target"])); Value (Str "from your army");
                         App
                           (Call Repeat,
                            Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                         Value (Str "can immediately shoot again")]),None))]),None)
    let ``POWER OF THE HIVE MIND`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("POWER OF THE HIVE MIND",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem at the end of your Psychic phase. Select a TYRANIDS PSYKER unit from your army that manifested a psychic power this turn. It can immediately attempt to manifest one additional psychic power this turn.");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "at");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
                      Value (Str "of your Psychic phase Select");
                      App
                        (Value (Int 1),
                         Value
                           (ParamArray
                              [Value (Str "TYRANIDS"); Value (Str "PSYKER");
                               Var "Target"]));
                      Value (Str "from your army that manifested");
                      App
                        (Value (Int 1),
                         Value (ParamArray [Value (Str "psychic"); Value (Str "power")]));
                      App (Lam ("obj",Var "obj"),Value (ParamArray []));
                      Value
                        (Str
                           "turn It can immediately attempt to manifest one additional psychic power");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "turn"); Lam ("obj",Var "obj")]))]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value
                        (ParamArray [Var "Available CP"; Var "POWER OF THE HIVE MIND"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem at the end of your Psychic phase. Select a TYRANIDS PSYKER unit from your army that manifested a psychic power this turn. It can immediately attempt to manifest one additional psychic power this turn.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "at");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
                         Value (Str "of your Psychic phase Select");
                         App
                           (Value (Int 1),
                            Value
                              (ParamArray
                                 [Value (Str "TYRANIDS"); Value (Str "PSYKER");
                                  Var "Target"]));
                         Value (Str "from your army that manifested");
                         App
                           (Value (Int 1),
                            Value
                              (ParamArray [Value (Str "psychic"); Value (Str "power")]));
                         App (Lam ("obj",Var "obj"),Value (ParamArray []));
                         Value
                           (Str
                              "turn It can immediately attempt to manifest one additional psychic power");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "turn"); Lam ("obj",Var "obj")]))]),
                   None))]),None)
    let ``DEATH FRENZY`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("DEATH FRENZY",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem when a TYRANIDS CHARACTER from your army is slain; the Hive Mind compels it to one final attack, and it can immediately either shoot as if it were your Shooting phase, or fight as if it were your Fight phase before it is removed from the battlefield.");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "when");
                      App
                        (Value (Int 1),
                         Value
                           (ParamArray [Value (Str "TYRANIDS"); Value (Str "CHARACTER")]));
                      Value (Str "from your army is slain");
                      App
                        (Lam ("obj",Var "obj"),
                         Value (ParamArray [Value (Str "Hive"); Value (Str "Mind")]));
                      Value
                        (Str "compels it to one final attack and it can immediately");
                      Lam ("obj",Var "obj");
                      Value
                        (Str
                           "shoot as if it were your Shooting phase or fight as if it were your");
                      Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                      Value (Str "before it is removed from");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray
                              [Value (Str "battlefield"); Lam ("obj",Var "obj")]))]));
               ("2",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "DEATH FRENZY"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem when a TYRANIDS CHARACTER from your army is slain; the Hive Mind compels it to one final attack, and it can immediately either shoot as if it were your Shooting phase, or fight as if it were your Fight phase before it is removed from the battlefield.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "when");
                         App
                           (Value (Int 1),
                            Value
                              (ParamArray
                                 [Value (Str "TYRANIDS"); Value (Str "CHARACTER")]));
                         Value (Str "from your army is slain");
                         App
                           (Lam ("obj",Var "obj"),
                            Value (ParamArray [Value (Str "Hive"); Value (Str "Mind")]));
                         Value
                           (Str "compels it to one final attack and it can immediately");
                         Lam ("obj",Var "obj");
                         Value
                           (Str
                              "shoot as if it were your Shooting phase or fight as if it were your");
                         Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                         Value (Str "before it is removed from");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "battlefield"); Lam ("obj",Var "obj")]))]),
                   None))]),None)
    let ``GRISLY FEAST`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("GRISLY FEAST",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem in the Morale phase. Select a unit of Ripper Swarms or Haruspex from your army. Your opponent must add 1 to any Morale tests taken for enemy units that are within 6\" of that unit in this phase.");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "in");
                      App
                        (Lam ("obj",Var "obj"),
                         Value (ParamArray [Value (Str "Morale"); Value (Str "phase")]));
                      Value (Str "Select");
                      App (Call Repeat,Value (ParamArray [Var "Target"; Value (Int 1)]));
                      Value
                        (Str
                           "of Ripper Swarms or Haruspex from your army Your opponent must add");
                      Value (Int 1); Value (Str "to");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Morale"); Lam ("obj",Var "obj")]));
                      Value (Str "taken for"); Var "Target";
                      Value (Str "that are within"); Value (Distance 6);
                      Value (Str "of");
                      App
                        (Call Repeat,
                         Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                      Value (Str "in");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "phase"); Lam ("obj",Var "obj")]))]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "GRISLY FEAST"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem in the Morale phase. Select a unit of Ripper Swarms or Haruspex from your army. Your opponent must add 1 to any Morale tests taken for enemy units that are within 6\" of that unit in this phase.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "in");
                         App
                           (Lam ("obj",Var "obj"),
                            Value
                              (ParamArray [Value (Str "Morale"); Value (Str "phase")]));
                         Value (Str "Select");
                         App
                           (Call Repeat,Value (ParamArray [Var "Target"; Value (Int 1)]));
                         Value
                           (Str
                              "of Ripper Swarms or Haruspex from your army Your opponent must add");
                         Value (Int 1); Value (Str "to");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "Morale"); Lam ("obj",Var "obj")]));
                         Value (Str "taken for"); Var "Target";
                         Value (Str "that are within"); Value (Distance 6);
                         Value (Str "of");
                         App
                           (Call Repeat,
                            Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                         Value (Str "in");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "phase"); Lam ("obj",Var "obj")]))]),
                   None))]),None)
    let ``OVERRUN`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("OVERRUN",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem when a TYRANIDS unit from your army destroys a unit in the Fight phase, and is not within 3\" of an enemy unit. Instead of consolidating, that unit can move (and Advance) as if it were your Movement phase (it cannot move within 1\" of any enemy models).");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "when");
                      App
                        (Value (Int 1),
                         Value (ParamArray [Value (Str "TYRANIDS"); Var "Target"]));
                      Value (Str "from your army destroys");
                      App (Call Repeat,Value (ParamArray [Var "Target"; Value (Int 1)]));
                      Value (Str "in");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray
                              [Value
                                 (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                               Lam ("obj",Var "obj")])); Value (Str "and is not within");
                      Value (Distance 3); Value (Str "of");
                      App
                        (Lam ("obj",Var "obj"),
                         Value (ParamArray [Var "Target"; Var "Target"]));
                      Value (Str "Instead of consolidating");
                      App
                        (Call Repeat,
                         Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                      Value
                        (Str
                           "can move and Advance as if it were your Movement phase it can not move within");
                      Value (Distance 1); Value (Str "of");
                      App
                        (Call Repeat,
                         Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]))]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "OVERRUN"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem when a TYRANIDS unit from your army destroys a unit in the Fight phase, and is not within 3\" of an enemy unit. Instead of consolidating, that unit can move (and Advance) as if it were your Movement phase (it cannot move within 1\" of any enemy models).");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "when");
                         App
                           (Value (Int 1),
                            Value (ParamArray [Value (Str "TYRANIDS"); Var "Target"]));
                         Value (Str "from your army destroys");
                         App
                           (Call Repeat,Value (ParamArray [Var "Target"; Value (Int 1)]));
                         Value (Str "in");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value
                                    (ParamArray
                                       [Value (Str "Phase"); Value (Str "Fight")]);
                                  Lam ("obj",Var "obj")]));
                         Value (Str "and is not within"); Value (Distance 3);
                         Value (Str "of");
                         App
                           (Lam ("obj",Var "obj"),
                            Value (ParamArray [Var "Target"; Var "Target"]));
                         Value (Str "Instead of consolidating");
                         App
                           (Call Repeat,
                            Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                         Value
                           (Str
                              "can move and Advance as if it were your Movement phase it can not move within");
                         Value (Distance 1); Value (Str "of");
                         App
                           (Call Repeat,
                            Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]))]),
                   None))]),None)
    let ``INVISIBLE HUNTER`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("INVISIBLE HUNTER",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem in your Movement phase. Select a LICTOR from your army that is within 1\" of an enemy unit. That model can Fall Back, shoot and charge in this turn.");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "in your Movement phase Select");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "LICTOR"); Value (Int 1)]));
                      Value (Str "from your army that is within"); Value (Distance 1);
                      Value (Str "of");
                      App
                        (Lam ("obj",Var "obj"),
                         Value (ParamArray [Var "Target"; Var "Target"]));
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                      Value (Str "can Fall Back shoot and charge in");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "turn"); Lam ("obj",Var "obj")]))]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "INVISIBLE HUNTER"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem in your Movement phase. Select a LICTOR from your army that is within 1\" of an enemy unit. That model can Fall Back, shoot and charge in this turn.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "in your Movement phase Select");
                         App
                           (Call Repeat,
                            Value (ParamArray [Value (Str "LICTOR"); Value (Int 1)]));
                         Value (Str "from your army that is within"); Value (Distance 1);
                         Value (Str "of");
                         App
                           (Lam ("obj",Var "obj"),
                            Value (ParamArray [Var "Target"; Var "Target"]));
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                         Value (Str "can Fall Back shoot and charge in");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "turn"); Lam ("obj",Var "obj")]))]),
                   None))]),None)
    let ``SPOREFIELD`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("SPOREFIELD",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem after both armies have deployed but before the battle begins. You can add up to two units of Spore Mines to your army as reinforcements and set them up anywhere on the battlefield that is more than 12\" from enemy models.");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "after");
                      App (Lam ("obj",Var "obj"),Value (ParamArray []));
                      Value (Str "have deployed but before");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "battle"); Lam ("obj",Var "obj")]));
                      Value
                        (Str
                           "begins You can add up to two of Spore Mines to your army as and set them up anywhere on");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray
                              [Value (Str "battlefield"); Lam ("obj",Var "obj")]));
                      Value (Str "that is more than"); Value (Distance 12);
                      Value (Str "from"); Var "Target"]));
               ("3",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "SPOREFIELD"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem after both armies have deployed but before the battle begins. You can add up to two units of Spore Mines to your army as reinforcements and set them up anywhere on the battlefield that is more than 12\" from enemy models.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "after");
                         App (Lam ("obj",Var "obj"),Value (ParamArray []));
                         Value (Str "have deployed but before");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "battle"); Lam ("obj",Var "obj")]));
                         Value
                           (Str
                              "begins You can add up to two of Spore Mines to your army as and set them up anywhere on");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "battlefield"); Lam ("obj",Var "obj")]));
                         Value (Str "that is more than"); Value (Distance 12);
                         Value (Str "from"); Var "Target"]),None))]),None)
    let ``WAR ON ALL FRONTS`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "LEVIATHAN")])),
           Choice
             ("WAR ON ALL FRONTS",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem in the Fight phase. Select an enemy unit that is within 1\" of at least one LEVIATHAN unit from your army that can FLY and at least one that cannot. You can re-roll hit and wound rolls of 1 in this phase for attacks for LEVIATHAN units that target that enemy unit.");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "in");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray
                              [Value
                                 (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                               Lam ("obj",Var "obj")])); Value (Str "Select");
                      App
                        (Lam ("obj",Var "obj"),
                         Value (ParamArray [Var "Target"; Var "Target"]));
                      Value (Str "that is within"); Value (Distance 1);
                      Value (Str "of at least one LEVIATHAN"); Var "Target";
                      Value
                        (Str
                           "from your army that can FLY and at least one that can not You can re-roll hit and wound of");
                      Value (Int 1); Value (Str "in");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "phase"); Lam ("obj",Var "obj")]));
                      Value (Str "for for LEVIATHAN that target");
                      App
                        (Lam ("obj",Var "obj"),
                         Value (ParamArray [Var "Target"; Var "Target"]))]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "WAR ON ALL FRONTS"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem in the Fight phase. Select an enemy unit that is within 1\" of at least one LEVIATHAN unit from your army that can FLY and at least one that cannot. You can re-roll hit and wound rolls of 1 in this phase for attacks for LEVIATHAN units that target that enemy unit.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "in");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value
                                    (ParamArray
                                       [Value (Str "Phase"); Value (Str "Fight")]);
                                  Lam ("obj",Var "obj")])); Value (Str "Select");
                         App
                           (Lam ("obj",Var "obj"),
                            Value (ParamArray [Var "Target"; Var "Target"]));
                         Value (Str "that is within"); Value (Distance 1);
                         Value (Str "of at least one LEVIATHAN"); Var "Target";
                         Value
                           (Str
                              "from your army that can FLY and at least one that can not You can re-roll hit and wound of");
                         Value (Int 1); Value (Str "in");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "phase"); Lam ("obj",Var "obj")]));
                         Value (Str "for for LEVIATHAN that target");
                         App
                           (Lam ("obj",Var "obj"),
                            Value (ParamArray [Var "Target"; Var "Target"]))]),None))]),
           None)
    let ``HYPER-TOXICITY`` = 
        IfThenElse
          (App (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "GORGON")])),
           Choice
             ("HYPER-TOXICITY",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem in the Fight phase. Choose a GORGON unit from your army that has the toxin sacs biomorph. For the duration of the phase, the toxin sacs biomorph causes 1 additional damage on wound rolls of 5+ (rather than 6+) for attacks made by that unit.");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "in");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray
                              [Value
                                 (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                               Lam ("obj",Var "obj")])); Value (Str "Choose");
                      App
                        (Value (Int 1),
                         Value (ParamArray [Value (Str "GORGON"); Var "Target"]));
                      Value (Str "from your army that has");
                      App
                        (Lam ("obj",Var "obj"),
                         Value
                           (ParamArray [Value (Str "toxin"); Value (Str "biomorph")]));
                      Value (Str "For");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "duration"); Lam ("obj",Var "obj")]));
                      Value (Str "of");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "phase"); Lam ("obj",Var "obj")]));
                      App
                        (Lam ("obj",Var "obj"),
                         Value
                           (ParamArray [Value (Str "toxin"); Value (Str "biomorph")]));
                      Value (Str "causes"); Value (Int 1);
                      Value (Str "additional damage on wound of"); Value (Int 5);
                      Value (Str "+ rather than"); Value (Int 6);
                      Value (Str "+ for made by");
                      App
                        (Call Repeat,
                         Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]))]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "HYPER-TOXICITY"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem in the Fight phase. Choose a GORGON unit from your army that has the toxin sacs biomorph. For the duration of the phase, the toxin sacs biomorph causes 1 additional damage on wound rolls of 5+ (rather than 6+) for attacks made by that unit.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "in");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value
                                    (ParamArray
                                       [Value (Str "Phase"); Value (Str "Fight")]);
                                  Lam ("obj",Var "obj")])); Value (Str "Choose");
                         App
                           (Value (Int 1),
                            Value (ParamArray [Value (Str "GORGON"); Var "Target"]));
                         Value (Str "from your army that has");
                         App
                           (Lam ("obj",Var "obj"),
                            Value
                              (ParamArray [Value (Str "toxin"); Value (Str "biomorph")]));
                         Value (Str "For");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "duration"); Lam ("obj",Var "obj")]));
                         Value (Str "of");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "phase"); Lam ("obj",Var "obj")]));
                         App
                           (Lam ("obj",Var "obj"),
                            Value
                              (ParamArray [Value (Str "toxin"); Value (Str "biomorph")]));
                         Value (Str "causes"); Value (Int 1);
                         Value (Str "additional damage on wound of"); Value (Int 5);
                         Value (Str "+ rather than"); Value (Int 6);
                         Value (Str "+ for made by");
                         App
                           (Call Repeat,
                            Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]))]),
                   None))]),None)
    let ``BRUTE FORCE`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "BEHEMOTH")])),
           Choice
             ("BRUTE FORCE",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem when a BEHEMOTH unit from your army completes a charge move. Roll a dice for each model in the charging unit that is within 1\" of an enemy unit. For each roll of 6 (or 2+ for a MONSTER ), inflict one mortal wound on an enemy unit within 1\".");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "when");
                      App
                        (Value (Int 1),
                         Value (ParamArray [Value (Str "BEHEMOTH"); Var "Target"]));
                      Value (Str "from your army completes");
                      App
                        (Value (Int 1),
                         Value (ParamArray [Value (Str "charge"); Value (Str "move")]));
                      Value (Str "Roll"); App (Value (Int 1),Value (ParamArray []));
                      Value (Str "for");
                      Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                      Value (Str "model in");
                      App
                        (Lam ("obj",Var "obj"),
                         Value (ParamArray [Value (Str "charging"); Var "Target"]));
                      Value (Str "that is within"); Value (Distance 1); Value (Str "of");
                      App
                        (Lam ("obj",Var "obj"),
                         Value (ParamArray [Var "Target"; Var "Target"]));
                      Value (Str "For");
                      Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                      Value (Str "roll of"); Value (Int 6); Value (Str "or");
                      Value (Int 2); Value (Str "+ for");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "MONSTER"); Value (Int 1)]));
                      Value (Str "inflict one mortal wound on");
                      App
                        (Lam ("obj",Var "obj"),
                         Value (ParamArray [Var "Target"; Var "Target"]));
                      Value (Str "within"); Value (Distance 1)]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "BRUTE FORCE"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem when a BEHEMOTH unit from your army completes a charge move. Roll a dice for each model in the charging unit that is within 1\" of an enemy unit. For each roll of 6 (or 2+ for a MONSTER ), inflict one mortal wound on an enemy unit within 1\".");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "when");
                         App
                           (Value (Int 1),
                            Value (ParamArray [Value (Str "BEHEMOTH"); Var "Target"]));
                         Value (Str "from your army completes");
                         App
                           (Value (Int 1),
                            Value
                              (ParamArray [Value (Str "charge"); Value (Str "move")]));
                         Value (Str "Roll"); App (Value (Int 1),Value (ParamArray []));
                         Value (Str "for");
                         Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                         Value (Str "model in");
                         App
                           (Lam ("obj",Var "obj"),
                            Value (ParamArray [Value (Str "charging"); Var "Target"]));
                         Value (Str "that is within"); Value (Distance 1);
                         Value (Str "of");
                         App
                           (Lam ("obj",Var "obj"),
                            Value (ParamArray [Var "Target"; Var "Target"]));
                         Value (Str "For");
                         Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                         Value (Str "roll of"); Value (Int 6); Value (Str "or");
                         Value (Int 2); Value (Str "+ for");
                         App
                           (Call Repeat,
                            Value (ParamArray [Value (Str "MONSTER"); Value (Int 1)]));
                         Value (Str "inflict one mortal wound on");
                         App
                           (Lam ("obj",Var "obj"),
                            Value (ParamArray [Var "Target"; Var "Target"]));
                         Value (Str "within"); Value (Distance 1)]),None))]),None)
    let ``ENDLESS SWARM`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("ENDLESS SWARM",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem at the end of your Movement phase. Select a unit of Termagants, Hormagaunts or Gargoyles (or any HYDRA INFANTRY unit) from your army that has been completely destroyed. Add an identical unit to your army, and set it up as reinforcements wholly within 6\" of any board edge, more than 9\" from enemy models.");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "at");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
                      Value (Str "of your Movement phase Select");
                      App (Call Repeat,Value (ParamArray [Var "Target"; Value (Int 1)]));
                      Value (Str "of Termagants Hormagaunts or Gargoyles or");
                      App
                        (Lam ("obj",Var "obj"),
                         Value
                           (ParamArray [Value (Str "HYDRA"); Value (Str "INFANTRY")]));
                      Var "Target";
                      Value
                        (Str "from your army that has been completely destroyed Add");
                      App
                        (Lam ("obj",Var "obj"),
                         Value (ParamArray [Value (Str "identical"); Var "Target"]));
                      Value (Str "to your army and set it up as wholly within");
                      Value (Distance 6); Value (Str "of");
                      App
                        (Lam ("obj",Var "obj"),
                         Value (ParamArray [Value (Str "board"); Value (Str "edge")]));
                      Value (Str "more than"); Value (Distance 9); Value (Str "from");
                      Var "Target"]));
               ("2",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "ENDLESS SWARM"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem at the end of your Movement phase. Select a unit of Termagants, Hormagaunts or Gargoyles (or any HYDRA INFANTRY unit) from your army that has been completely destroyed. Add an identical unit to your army, and set it up as reinforcements wholly within 6\" of any board edge, more than 9\" from enemy models.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "at");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
                         Value (Str "of your Movement phase Select");
                         App
                           (Call Repeat,Value (ParamArray [Var "Target"; Value (Int 1)]));
                         Value (Str "of Termagants Hormagaunts or Gargoyles or");
                         App
                           (Lam ("obj",Var "obj"),
                            Value
                              (ParamArray [Value (Str "HYDRA"); Value (Str "INFANTRY")]));
                         Var "Target";
                         Value
                           (Str "from your army that has been completely destroyed Add");
                         App
                           (Lam ("obj",Var "obj"),
                            Value (ParamArray [Value (Str "identical"); Var "Target"]));
                         Value (Str "to your army and set it up as wholly within");
                         Value (Distance 6); Value (Str "of");
                         App
                           (Lam ("obj",Var "obj"),
                            Value (ParamArray [Value (Str "board"); Value (Str "edge")]));
                         Value (Str "more than"); Value (Distance 9); Value (Str "from");
                         Var "Target"]),None))]),None)
    let ``OPPORTUNISTIC ADVANCE`` = 
        IfThenElse
          (App (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "KRAKEN")])),
           Choice
             ("OPPORTUNISTIC ADVANCE",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem in your Movement phase when you roll the dice for an Advancing KRAKEN unit (other than a unit that can FLY ). You can double the number you roll and add that total to their Move characteristic for that Movement phase, rather than following the normal rules for Advancing.");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "in your Movement phase when you roll");
                      App (Lam ("obj",Var "obj"),Value (ParamArray []));
                      Value (Str "for");
                      App
                        (Lam ("obj",Var "obj"),
                         Value
                           (ParamArray
                              [Value (Str "Advancing"); Value (Str "KRAKEN");
                               Var "Target"])); Value (Str "other than");
                      App (Call Repeat,Value (ParamArray [Var "Target"; Value (Int 1)]));
                      Value (Str "that can FLY You can double");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "number"); Lam ("obj",Var "obj")]));
                      Value (Str "you roll and add");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "total"); Lam ("obj",Var "obj")]));
                      Value (Str "to their Move characteristic for");
                      App
                        (Lam ("obj",Var "obj"),
                         Value
                           (ParamArray [Value (Str "Movement"); Value (Str "phase")]));
                      Value (Str "rather than following");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "normal"); Lam ("obj",Var "obj")]));
                      Value (Str "for Advancing")]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value
                        (ParamArray [Var "Available CP"; Var "OPPORTUNISTIC ADVANCE"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem in your Movement phase when you roll the dice for an Advancing KRAKEN unit (other than a unit that can FLY ). You can double the number you roll and add that total to their Move characteristic for that Movement phase, rather than following the normal rules for Advancing.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "in your Movement phase when you roll");
                         App (Lam ("obj",Var "obj"),Value (ParamArray []));
                         Value (Str "for");
                         App
                           (Lam ("obj",Var "obj"),
                            Value
                              (ParamArray
                                 [Value (Str "Advancing"); Value (Str "KRAKEN");
                                  Var "Target"])); Value (Str "other than");
                         App
                           (Call Repeat,Value (ParamArray [Var "Target"; Value (Int 1)]));
                         Value (Str "that can FLY You can double");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "number"); Lam ("obj",Var "obj")]));
                         Value (Str "you roll and add");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "total"); Lam ("obj",Var "obj")]));
                         Value (Str "to their Move characteristic for");
                         App
                           (Lam ("obj",Var "obj"),
                            Value
                              (ParamArray [Value (Str "Movement"); Value (Str "phase")]));
                         Value (Str "rather than following");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "normal"); Lam ("obj",Var "obj")]));
                         Value (Str "for Advancing")]),None))]),None)
    let ``CALL THE BROOD`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("CALL THE BROOD",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem at the end of your Movement phase. Add a new unit of up to 5 Genestealers to your army and set them up as reinforcements wholly within 6\" of a Broodlord or infestation node from your army and more than 9\" from any enemy models.");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "at");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
                      Value (Str "of your Movement phase Add");
                      App
                        (Value (Int 1),
                         Value (ParamArray [Value (Str "new"); Var "Target"]));
                      Value (Str "of up to"); Value (Int 5);
                      Value (Str "to your army and set them up as wholly within");
                      Value (Distance 6); Value (Str "of");
                      App
                        (Value (Int 1),
                         Value
                           (ParamArray
                              [Value (Str "Broodlord"); Value (Str "or");
                               Value (Str "infestation"); Value (Str "node")]));
                      Value (Str "from your army and more than"); Value (Distance 9);
                      Value (Str "from");
                      App
                        (Call Repeat,
                         Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]))]));
               ("3",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "CALL THE BROOD"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem at the end of your Movement phase. Add a new unit of up to 5 Genestealers to your army and set them up as reinforcements wholly within 6\" of a Broodlord or infestation node from your army and more than 9\" from any enemy models.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "at");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
                         Value (Str "of your Movement phase Add");
                         App
                           (Value (Int 1),
                            Value (ParamArray [Value (Str "new"); Var "Target"]));
                         Value (Str "of up to"); Value (Int 5);
                         Value (Str "to your army and set them up as wholly within");
                         Value (Distance 6); Value (Str "of");
                         App
                           (Value (Int 1),
                            Value
                              (ParamArray
                                 [Value (Str "Broodlord"); Value (Str "or");
                                  Value (Str "infestation"); Value (Str "node")]));
                         Value (Str "from your army and more than"); Value (Distance 9);
                         Value (Str "from");
                         App
                           (Call Repeat,
                            Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]))]),
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
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem when you set up a JORMUNGANDR INFANTRY unit during deployment. It is set up within tunnels bored before battle. Whenever you set up a unit of Raveners, a Mawloc, Trygon or a Trygon Prime at the end of your Movement phase (a burrowing unit), you can also set up any number of units you set up within the tunnels. Set up the unit wholly within 3\" of the burrowing unit and more than 9\" from any enemy units. Any models you cannot set up in this way when you do so are destroyed.");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "when you set up");
                      App
                        (Value (Int 1),
                         Value
                           (ParamArray
                              [Value (Str "JORMUNGANDR"); Value (Str "INFANTRY");
                               Var "Target"]));
                      Value
                        (Str
                           "during deployment It is set up within bored before battle Whenever you set up");
                      App (Call Repeat,Value (ParamArray [Var "Target"; Value (Int 1)]));
                      Value (Str "of Raveners");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "Mawloc"); Value (Int 1)]));
                      Value (Str "Trygon or");
                      App
                        (Value (Int 1),
                         Value (ParamArray [Value (Str "Trygon"); Value (Str "Prime")]));
                      Value (Str "at");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
                      Value (Str "of your Movement phase");
                      App
                        (Value (Int 1),
                         Value (ParamArray [Value (Str "burrowing"); Var "Target"]));
                      Value (Str "you can also set up");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "number"); Lam ("obj",Var "obj")]));
                      Value (Str "of you set up within");
                      App (Lam ("obj",Var "obj"),Value (ParamArray []));
                      Value (Str "Set up");
                      App
                        (Call Repeat,
                         Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                      Value (Str "wholly within"); Value (Distance 3); Value (Str "of");
                      Lam ("obj",Var "obj"); Value (Str "burrowing"); Var "Target";
                      Value (Str "and more than"); Value (Distance 9);
                      Value (Str "from");
                      App
                        (Call Repeat,
                         Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                      App (Lam ("obj",Var "obj"),Value (ParamArray []));
                      Value (Str "you can not set up in");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "way"); Lam ("obj",Var "obj")]));
                      Value (Str "when you do so are destroyed")]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "THE ENEMY BELOW"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem when you set up a JORMUNGANDR INFANTRY unit during deployment. It is set up within tunnels bored before battle. Whenever you set up a unit of Raveners, a Mawloc, Trygon or a Trygon Prime at the end of your Movement phase (a burrowing unit), you can also set up any number of units you set up within the tunnels. Set up the unit wholly within 3\" of the burrowing unit and more than 9\" from any enemy units. Any models you cannot set up in this way when you do so are destroyed.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "when you set up");
                         App
                           (Value (Int 1),
                            Value
                              (ParamArray
                                 [Value (Str "JORMUNGANDR"); Value (Str "INFANTRY");
                                  Var "Target"]));
                         Value
                           (Str
                              "during deployment It is set up within bored before battle Whenever you set up");
                         App
                           (Call Repeat,Value (ParamArray [Var "Target"; Value (Int 1)]));
                         Value (Str "of Raveners");
                         App
                           (Call Repeat,
                            Value (ParamArray [Value (Str "Mawloc"); Value (Int 1)]));
                         Value (Str "Trygon or");
                         App
                           (Value (Int 1),
                            Value
                              (ParamArray [Value (Str "Trygon"); Value (Str "Prime")]));
                         Value (Str "at");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
                         Value (Str "of your Movement phase");
                         App
                           (Value (Int 1),
                            Value (ParamArray [Value (Str "burrowing"); Var "Target"]));
                         Value (Str "you can also set up");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "number"); Lam ("obj",Var "obj")]));
                         Value (Str "of you set up within");
                         App (Lam ("obj",Var "obj"),Value (ParamArray []));
                         Value (Str "Set up");
                         App
                           (Call Repeat,
                            Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                         Value (Str "wholly within"); Value (Distance 3);
                         Value (Str "of"); Lam ("obj",Var "obj");
                         Value (Str "burrowing"); Var "Target";
                         Value (Str "and more than"); Value (Distance 9);
                         Value (Str "from");
                         App
                           (Call Repeat,
                            Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                         App (Lam ("obj",Var "obj"),Value (ParamArray []));
                         Value (Str "you can not set up in");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "way"); Lam ("obj",Var "obj")]));
                         Value (Str "when you do so are destroyed")]),None))]),None)
    let ``THE DEEPEST SHADOW`` = 
        IfThenElse
          (App (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "KRONOS")])),
           Choice
             ("THE DEEPEST SHADOW",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem when an enemy PSYKER attempts to manifest a psychic power within 24\" of a KRONOS unit from your army. Your opponent can only roll a single dice for the Psychic test.");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "when");
                      App
                        (Lam ("obj",Var "obj"),
                         Value (ParamArray [Var "Target"; Value (Str "PSYKER")]));
                      Value (Str "attempts to manifest");
                      App
                        (Value (Int 1),
                         Value (ParamArray [Value (Str "psychic"); Value (Str "power")]));
                      Value (Str "within"); Value (Distance 24); Value (Str "of");
                      App
                        (Value (Int 1),
                         Value (ParamArray [Value (Str "KRONOS"); Var "Target"]));
                      Value (Str "from your army Your opponent can only roll");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "single"); Value (Int 1)]));
                      Value (Str "for");
                      App
                        (Lam ("obj",Var "obj"),
                         Value (ParamArray [Value (Str "Psychic"); Value (Str "test")]))]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "THE DEEPEST SHADOW"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem when an enemy PSYKER attempts to manifest a psychic power within 24\" of a KRONOS unit from your army. Your opponent can only roll a single dice for the Psychic test.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "when");
                         App
                           (Lam ("obj",Var "obj"),
                            Value (ParamArray [Var "Target"; Value (Str "PSYKER")]));
                         Value (Str "attempts to manifest");
                         App
                           (Value (Int 1),
                            Value
                              (ParamArray [Value (Str "psychic"); Value (Str "power")]));
                         Value (Str "within"); Value (Distance 24); Value (Str "of");
                         App
                           (Value (Int 1),
                            Value (ParamArray [Value (Str "KRONOS"); Var "Target"]));
                         Value (Str "from your army Your opponent can only roll");
                         App
                           (Call Repeat,
                            Value (ParamArray [Value (Str "single"); Value (Int 1)]));
                         Value (Str "for");
                         App
                           (Lam ("obj",Var "obj"),
                            Value
                              (ParamArray [Value (Str "Psychic"); Value (Str "test")]))]),
                   None))]),None)
    let ``DIGESTIVE DENIAL`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("DIGESTIVE DENIAL",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem after deployment but before the first battle round begins. Choose a piece of terrain (other than a Fortification). Units fully within or on this piece of terrain do not gain any bonus to their saving throws for being in cover.");
                      Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem after deployment but before");
                      App
                        (Lam ("obj",Var "obj"),
                         Value
                           (ParamArray
                              [Value (Str "first"); Value (Str "battle");
                               Value (Str "round")])); Value (Str "begins Choose");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "piece"); Value (Int 1)]));
                      Value (Str "of terrain other than");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "Fortification"); Value (Int 1)]));
                      Value (Str "fully within or on");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "piece"); Lam ("obj",Var "obj")]));
                      Value (Str "of terrain do not gain");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "bonus"); Lam ("obj",Var "obj")]));
                      Value (Str "to their saving throws for being in cover")]));
               ("2",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "DIGESTIVE DENIAL"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem after deployment but before the first battle round begins. Choose a piece of terrain (other than a Fortification). Units fully within or on this piece of terrain do not gain any bonus to their saving throws for being in cover.");
                         Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem after deployment but before");
                         App
                           (Lam ("obj",Var "obj"),
                            Value
                              (ParamArray
                                 [Value (Str "first"); Value (Str "battle");
                                  Value (Str "round")])); Value (Str "begins Choose");
                         App
                           (Call Repeat,
                            Value (ParamArray [Value (Str "piece"); Value (Int 1)]));
                         Value (Str "of terrain other than");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "Fortification"); Value (Int 1)]));
                         Value (Str "fully within or on");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "piece"); Lam ("obj",Var "obj")]));
                         Value (Str "of terrain do not gain");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "bonus"); Lam ("obj",Var "obj")]));
                         Value (Str "to their saving throws for being in cover")]),None))]),
           None)
    let ``ADRENALINE SURGE`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("ADRENALINE SURGE",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value
                        (Str
                           "Use this Stratagem at the end of the Fight phase. Select a TYRANIDS unit from your army – that unit can immediately fight again.");
                      Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "at");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
                      Value (Str "of");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray
                              [Value
                                 (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                               Lam ("obj",Var "obj")])); Value (Str "Select");
                      App
                        (Value (Int 1),
                         Value (ParamArray [Value (Str "TYRANIDS"); Var "Target"]));
                      Value (Str "from your army");
                      App
                        (Call Repeat,
                         Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                      Value (Str "can immediately fight again")]));
               ("3",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "ADRENALINE SURGE"])),
                   Value
                     (ParamArray
                        [Value
                           (Str
                              "Use this Stratagem at the end of the Fight phase. Select a TYRANIDS unit from your army – that unit can immediately fight again.");
                         Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "at");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
                         Value (Str "of");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value
                                    (ParamArray
                                       [Value (Str "Phase"); Value (Str "Fight")]);
                                  Lam ("obj",Var "obj")])); Value (Str "Select");
                         App
                           (Value (Int 1),
                            Value (ParamArray [Value (Str "TYRANIDS"); Var "Target"]));
                         Value (Str "from your army");
                         App
                           (Call Repeat,
                            Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                         Value (Str "can immediately fight again")]),None))]),None)
