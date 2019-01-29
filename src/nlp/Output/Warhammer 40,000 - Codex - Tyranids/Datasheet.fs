namespace ``Warhammer 40,000 - Codex - Tyranids`` 
module Datasheet = 
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
    let ``RIPPER SWARMS`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value
                            (Str
                               "This unit contains 3 Ripper Swarms. It can include up to 3 additional Ripper Swarms (Power Rating +2)  or up to 6 additional Ripper Swarms (Power Rating +3) . Each model is armed with claws and teeth.");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                          Value (Str "contains"); Value (Int 3);
                          Value (Str "Ripper It include up to"); Value (Int 3);
                          Value (Str "additional Ripper Swarms Power Rating");
                          Value (Int 2); Value (Str "or up to"); Value (Int 6);
                          Value (Str "additional Ripper Swarms Power Rating");
                          Value (Int 3);
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "is armed with and")]);
                    Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour  (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Burrowers:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " During deployment, you can set up a unit of Ripper Swarms underground instead of on the battlefield. At the end of any of your Movement phases, they can tunnel up to the battlefield – set them up anywhere that is more than 9\" from any enemy models.");
                                Value (Str "During deployment set up");
                                App
                                  (Call Repeat,
                                   Value (ParamArray [Var "Target"; Value (Int 1)]));
                                Value (Str "of Ripper Swarms underground instead of on");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battlefield");
                                         Lam ("obj",Var "obj")])); Value (Str "At");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "end"); Lam ("obj",Var "obj")]));
                                Value (Str "of");
                                App (Lam ("obj",Var "obj"),Value (ParamArray []));
                                Value (Str "of your Movement they tunnel up to");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battlefield");
                                         Lam ("obj",Var "obj")]));
                                Value (Str "set them up anywhere that is more than");
                                Value (Distance 9); Value (Str "from");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]))])]);
                    Value
                      (ParamArray
                         [Value (Str "All models in the unit may also take spinemaws.");
                          App (Lam ("obj",Var "obj"),Value (ParamArray []));
                          Value (Str "in");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                          Value (Str "may also take")])])])
    let ``HIVE GUARD`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value
                            (Str
                               "This unit contains 3 Hive Guard. It can include up to 3 additional Hive Guard (Power Rating +6) . Each model is armed with an impaler cannon.");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                          Value (Str "contains 3 Hive Guard It include up to");
                          Value (Int 3);
                          Value (Str "additional Hive Guard Power Rating");
                          Value (Int 6);
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "is armed with");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray [Value (Str "impaler"); Value (Str "cannon")]))]);
                    Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Any model may replace its impaler cannon with a shockcannon.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "may replace its impaler cannon with");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "shockcannon"); Value (Int 1)]))]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "All models in the unit may have toxin sacs and/or adrenal glands (pg 113) .");
                          App (Lam ("obj",Var "obj"),Value (ParamArray []));
                          Value (Str "in");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                          Value (Str "may have toxin and/or adrenal pg");
                          Value (Int 113)])])])
    let ``TYRANT GUARD`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value
                            (Str
                               "This unit contains 3 Tyrant Guard. It can include up to 3 additional Tyrant Guard (Power Rating +6) . Each model is armed with rending claws and scything talons.");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                          Value (Str "contains 3 Tyrant Guard It include up to");
                          Value (Int 3);
                          Value (Str "additional Tyrant Guard Power Rating");
                          Value (Int 6);
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "is armed with rending and scything")]);
                    Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Blind Rampage:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " If a friendly <HIVE FLEET>  HIVE TYRANT  is killed within 6\" of this unit, from the end of that turn increase the Attacks characteristic of each model in this unit by 1 for the rest of the battle.");
                                Value (Str "If");
                                App
                                  (Value (Int 1),
                                   Value
                                     (ParamArray
                                        [Value (Str "friendly");
                                         Value (Str "<HIVE FLEET>"); Value (Str "HIVE");
                                         Value (Str "TYRANT")]));
                                Value (Str "is killed within"); Value (Distance 6);
                                Value (Str "of");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Value (Str "from");
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
                                        [Value (Str "turn"); Lam ("obj",Var "obj")]));
                                Value (Str "increase");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "Attacks");
                                         Value (Str "characteristic")]));
                                Value (Str "of");
                                Lam
                                  ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                                Value (Str "model in");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Value (Str "by"); Value (Int 1); Value (Str "for");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "rest"); Lam ("obj",Var "obj")]));
                                Value (Str "of");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battle"); Lam ("obj",Var "obj")]))])]);
                    Value
                      (ParamArray
                         [Value (Str "Shieldwall:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " Roll a dice each time a friendly <HIVE FLEET>  HIVE TYRANT  loses a wound whilst they are within 3\" of this unit; on a 2+ a model from this unit can intercept that hit – the Hive Tyrant does not lose a wound but this unit suffers a mortal wound.");
                                Value (Str "Roll");
                                App (Value (Int 1),Value (ParamArray []));
                                Lam
                                  ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                                Value (Str "time");
                                App
                                  (Value (Int 1),
                                   Value
                                     (ParamArray
                                        [Value (Str "friendly");
                                         Value (Str "<HIVE FLEET>"); Value (Str "HIVE");
                                         Value (Str "TYRANT")])); Value (Str "loses");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Value (Str "wound"); Value (Int 1)]));
                                Value (Str "whilst they are within"); Value (Distance 3);
                                Value (Str "of");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Value (Str "on");
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
                                         Value (Int 1)]));
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Value (Str "model"); Value (Int 1)]));
                                Value (Str "from");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Value (Str "intercept");
                                App (Lam ("obj",Var "obj"),Value (ParamArray []));
                                Value (Str "hit");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "Hive"); Value (Str "Tyrant")]));
                                Value (Str "does not lose");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Value (Str "wound"); Value (Int 1)]));
                                Value (Str "but");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Call Suffer;
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "Mortal Wound"); Value (Int 1)]))])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Any model may replace its scything talons with crushing claws or a lash whip and bonesword.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "may replace its scything with crushing or");
                          App
                            (Value (Int 1),
                             Value (ParamArray [Value (Str "lash"); Value (Str "whip")]));
                          Value (Str "and bonesword")]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "All models in the unit may have toxin sacs and/or adrenal glands (pg 113).");
                          App (Lam ("obj",Var "obj"),Value (ParamArray []));
                          Value (Str "in");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                          Value (Str "may have toxin and/or adrenal pg");
                          Value (Int 113)])])])
    let ``DEATHLEAPER`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value
                            (Str
                               "Deathleaper is a single model armed with flesh hooks, grasping talons and rending claws. Only one of this model can be included in your army.");
                          Value (Str "Deathleaper is");
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray [Value (Str "single"); Value (Str "model")]));
                          Value
                            (Str "armed with flesh grasping and rending Only one of");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "be included in your army")]);
                    Value
                      (ParamArray
                         [Value (Str "Superior Chameleonic Skin:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " Your opponent must subtract 2 from their hit rolls for attacks that target Deathleaper. In addition, add 2 instead of 1 to saving throws for Deathleaper when it is in cover.");
                                Value (Str "Your opponent must subtract"); Value (Int 2);
                                Value
                                  (Str
                                     "from their Hit Roll for that target Deathleaper In addition add");
                                Value (Int 2); Value (Str "instead of"); Value (Int 1);
                                Value
                                  (Str
                                     "to saving throws for Deathleaper when it is in cover")])]);
                    Value
                      (ParamArray
                         [Value (Str "It’s After Me!:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " At the start of the first battle round but before the first turn begins, pick a CHARACTER  from the opposing army. You can re-roll hit and wound rolls in the Fight phase for any of Deathleaper’s attacks that target that CHARACTER .");
                                Value (Str "At");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "start"); Lam ("obj",Var "obj")]));
                                Value (Str "of");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "first"); Value (Str "battle");
                                         Value (Str "round")]));
                                Value (Str "but before");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "first"); Value (Str "turn")]));
                                Value (Str "begins pick");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "CHARACTER"); Value (Int 1)]));
                                Value (Str "from");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "opposing"); Value (Str "army")]));
                                Value (Str "re-roll hit and wound in");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value
                                           (ParamArray
                                              [Value (Str "Phase"); Value (Str "Fight")]);
                                         Lam ("obj",Var "obj")])); Value (Str "for");
                                App (Lam ("obj",Var "obj"),Value (ParamArray []));
                                Value (Str "of Deathleaper 's that target");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "CHARACTER"); Lam ("obj",Var "obj")]))])]);
                    Value
                      (ParamArray
                         [Value (Str "Hidden Hunter:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " During deployment, you can set up Deathleaper in hiding instead of placing it on the battlefield. At the end of any of your Movement phases, Deathleaper can spring from its hiding place – set it up anywhere on the battlefield that is more than 9\" away from any enemy models. You can re-roll Deathleaper’s charge distance in the turn in which it uses this ability to arrive on the battlefield.");
                                Value
                                  (Str
                                     "During deployment set up Deathleaper in hiding instead of placing it on");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battlefield");
                                         Lam ("obj",Var "obj")])); Value (Str "At");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "end"); Lam ("obj",Var "obj")]));
                                Value (Str "of");
                                App (Lam ("obj",Var "obj"),Value (ParamArray []));
                                Value
                                  (Str
                                     "of your Movement Deathleaper spring from its hiding place set it up anywhere on");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battlefield");
                                         Lam ("obj",Var "obj")]));
                                Value (Str "that is more than"); Value (Distance 9);
                                Value (Str "away from");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Value (Str "re-roll Deathleaper 's charge distance in");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "turn"); Lam ("obj",Var "obj")]));
                                Value (Str "in which it uses"); Lam ("obj",Var "obj");
                                Value (Str "ability to arrive on");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battlefield");
                                         Lam ("obj",Var "obj")]))])])])])
    let ``LICTOR`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value
                            (Str
                               "A Lictor is a single model  armed with flesh hooks, grasping talons and rending claws.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "Lictor"); Lam ("obj",Var "obj")]));
                          Value (Str "is");
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray [Value (Str "single"); Value (Str "model")]));
                          Value (Str "armed with flesh grasping and rending")]);
                    Value
                      (ParamArray
                         [Value (Str "Chameleonic Skin:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " Your opponent must subtract 1 from their hit rolls for attacks that target this model. In addition, add 2 instead of 1 to saving throws for this model when it is in cover.");
                                Value (Str "Your opponent must subtract"); Value (Int 1);
                                Value (Str "from their Hit Roll for that target");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "model"); Lam ("obj",Var "obj")]));
                                Value (Str "In addition add"); Value (Int 2);
                                Value (Str "instead of"); Value (Int 1);
                                Value (Str "to saving throws for");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "model"); Lam ("obj",Var "obj")]));
                                Value (Str "when it is in cover")])]);
                    Value
                      (ParamArray
                         [Value (Str "Hidden Hunter:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " During deployment, you can set up a Lictor in hiding instead of placing it on the battlefield. At the end of any of your Movement phases, the Lictor can spring from its hiding place – set it up anywhere on the battlefield that is more than 9\" away from any enemy models. You can re-roll the Lictor’s charge distance in the turn in which it uses this ability to arrive on the battlefield.");
                                Value (Str "During deployment set up");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Value (Str "Lictor"); Value (Int 1)]));
                                Value (Str "in hiding instead of placing it on");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battlefield");
                                         Lam ("obj",Var "obj")])); Value (Str "At");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "end"); Lam ("obj",Var "obj")]));
                                Value (Str "of");
                                App (Lam ("obj",Var "obj"),Value (ParamArray []));
                                Value (Str "of your Movement");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "Lictor"); Lam ("obj",Var "obj")]));
                                Value
                                  (Str
                                     "spring from its hiding place set it up anywhere on");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battlefield");
                                         Lam ("obj",Var "obj")]));
                                Value (Str "that is more than"); Value (Distance 9);
                                Value (Str "away from");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Value (Str "re-roll");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "Lictor"); Value (Str "'s")]));
                                Value (Str "charge distance in");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "turn"); Lam ("obj",Var "obj")]));
                                Value (Str "in which it uses"); Lam ("obj",Var "obj");
                                Value (Str "ability to arrive on");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battlefield");
                                         Lam ("obj",Var "obj")]))])])])])
    let ``MALECEPTOR`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value
                            (Str
                               "A Maleceptor is a single model armed with massive scything talons.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray
                                  [Value (Str "Maleceptor"); Lam ("obj",Var "obj")]));
                          Value (Str "is");
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray [Value (Str "single"); Value (Str "model")]));
                          Value (Str "armed with massive scything")]);
                    Value
                      (ParamArray
                         [Value (Str "Shadow in the Warp");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Synapse  (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Psychic Overload:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " Instead of manifesting any psychic powers in your Psychic phase, a Maleceptor can unleash brain-bursting psychic tendrils. If it does so, roll a dice for each enemy unit within 6\", to a maximum number of units shown in the damage table above. On a 2+ the Maleceptor deals 1 mortal wound to that unit, but on a 6 it deals 3 mortal wounds to that unit instead.");
                                Value (Str "Instead of manifesting");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "psychic"); Lam ("obj",Var "obj")]));
                                Value (Str "in your Psychic phase");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "Maleceptor"); Value (Int 1)]));
                                Value
                                  (Str
                                     "unleash brain-bursting psychic If it does so roll");
                                App (Value (Int 1),Value (ParamArray []));
                                Value (Str "for");
                                Lam
                                  ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                                Var "Target"; Var "Target"; Value (Str "within");
                                Value (Distance 6); Value (Str "to");
                                App
                                  (Value (Int 1),
                                   Value
                                     (ParamArray
                                        [Value (Str "maximum"); Value (Str "number")]));
                                Value (Str "of shown in");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "damage"); Value (Str "table")]));
                                Value (Str "above On");
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
                                         Value (Int 1)]));
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "Maleceptor"); Lam ("obj",Var "obj")]));
                                Value (Str "deals"); Value (Int 1);
                                Value (Str "Mortal Wound to");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Value (Str "but on");
                                App
                                  (Call Repeat,
                                   Value (ParamArray [Value (Int 6); Value (Int 1)]));
                                Value (Str "it deals"); Value (Int 3);
                                Value (Str "mortal to");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Value (Str "instead")])]);
                    Value
                      (ParamArray
                         [Value (Str "Psychic Barrier:");
                          Value
                            (ParamArray
                               [Value (Str " A Maleceptor has a 4+ invulnerable save.");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "Maleceptor"); Lam ("obj",Var "obj")]));
                                Value (Str "has"); Value (Int 1); Value (Int 4);
                                Value (Str "+ invulnerable save")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "A Maleceptor can attempt to manifest two psychic powers in each friendly Psychic phase, and attempt to deny two psychic powers in each enemy Psychic phase. It knows the Smite  psychic power and one psychic power from the Hive Mind discipline (pg 121). Whenever a Maleceptor attempts to manifest a psychic power, add 1 to its Psychic test.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray
                                  [Value (Str "Maleceptor"); Lam ("obj",Var "obj")]));
                          Value (Str "attempt to manifest two psychic in");
                          Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                          Value
                            (Str
                               "friendly Psychic phase and attempt to deny two psychic in");
                          Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                          Var "Target"; Value (Str "Psychic phase It knows");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray
                                  [Value (Str "Smite"); Value (Str "psychic");
                                   Value (Str "power"); Value (Str "and");
                                   Value (Str "one"); Value (Str "psychic");
                                   Value (Str "power")])); Value (Str "from");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray
                                  [Value (Str "Hive"); Value (Str "Mind");
                                   Value (Str "discipline")])); Value (Str "pg");
                          Value (Int 121); Value (Str "Whenever");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "Maleceptor"); Value (Int 1)]));
                          Value (Str "attempts to manifest");
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray [Value (Str "psychic"); Value (Str "power")]));
                          Value (Str "add"); Value (Int 1);
                          Value (Str "to its Psychic test")])])])
    let ``ZOANTHROPES`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value
                            (Str
                               "This unit contains 3 Zoanthropes. It can include up to 3 additional Zoanthropes (Power Rating +2 per model) . Each model is armed with claws and teeth.");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                          Value (Str "contains"); Value (Int 3);
                          Value (Str "It include up to"); Value (Int 3);
                          Value (Str "additional Power Rating"); Value (Int 2);
                          Value (Str "per model");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "is armed with and")]);
                    Value
                      (ParamArray
                         [Value (Str "Shadow in the Warp");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Synapse  (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Warp Field:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " Models in this unit have a 3+ invulnerable save.");
                                Value (Str "in");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Value (Str "have"); Value (Int 1); Value (Int 3);
                                Value (Str "+ invulnerable save")])]);
                    Value
                      (ParamArray
                         [Value (Str "Warp Blast:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " When this unit manifests the Smite psychic power, it affects the closest visible enemy unit within 24\", instead of within 18\". In addition, it inflicts an additional D3 mortal wounds on that enemy unit if this unit contains 4 or 5 Zoanthropes, or an additional 3 mortal wounds if it contains 6 Zoanthropes.");
                                Value (Str "When");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Value (Str "manifests");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "Smite"); Value (Str "psychic");
                                         Value (Str "power")]));
                                Value (Str "it affects"); Lam ("obj",Var "obj");
                                Value (Str "closest visible"); Var "Target";
                                Var "Target"; Value (Str "within"); Value (Distance 24);
                                Value (Str "instead of within"); Value (Distance 18);
                                Value (Str "In addition it inflicts");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "additional"); Value (Str "D3");
                                         Value (Str "mortal")])); Value (Str "on");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value (ParamArray [Var "Target"; Var "Target"]));
                                Value (Str "if");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Value (Str "contains"); Value (Int 4); Value (Str "or");
                                Value (Int 5); Value (Str "or");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "additional"); Value (Int 3);
                                         Value (Str "mortal")]));
                                Value (Str "if it contains"); Value (Int 6)])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "A unit of Zoanthropes can attempt to manifest one psychic power in each friendly Psychic phase, and attempt to deny one psychic power in each enemy Psychic phase. A Zoanthrope unit of 4 or more models can instead attempt to manifest two psychic powers in each friendly Psychic phase, and attempt to deny one psychic power in each enemy Psychic phase. A Zoanthrope unit knows the Smite  psychic power and one psychic power from the Hive Mind discipline (pg 121).");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                          Value
                            (Str
                               "of Zoanthropes attempt to manifest one psychic power in");
                          Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                          Value
                            (Str
                               "friendly Psychic phase and attempt to deny one psychic power in");
                          Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                          Var "Target"; Value (Str "Psychic phase");
                          App
                            (Lam ("obj",Var "obj"),
                             Value (ParamArray [Value (Str "Zoanthrope"); Var "Target"]));
                          Value (Str "of"); Value (Int 4);
                          Value
                            (Str "or more instead attempt to manifest two psychic in");
                          Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                          Value
                            (Str
                               "friendly Psychic phase and attempt to deny one psychic power in");
                          Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                          Var "Target"; Value (Str "Psychic phase");
                          App
                            (Lam ("obj",Var "obj"),
                             Value (ParamArray [Value (Str "Zoanthrope"); Var "Target"]));
                          Value (Str "knows");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray
                                  [Value (Str "Smite"); Value (Str "psychic");
                                   Value (Str "power"); Value (Str "and");
                                   Value (Str "one"); Value (Str "psychic");
                                   Value (Str "power")])); Value (Str "from");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray
                                  [Value (Str "Hive"); Value (Str "Mind");
                                   Value (Str "discipline")])); Value (Str "pg");
                          Value (Int 121)]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "When manifesting or denying a psychic power with a Zoanthrope unit, first select a model in the unit – measure range, visibility etc. from this model. If this unit suffers Perils of the Warp, it suffers D3 mortal wounds as described in the core rules, but units within 6\" will only suffer damage if the Perils of the Warp causes the last model in the Zoanthrope unit to be slain.");
                          Value (Str "When manifesting or denying");
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray [Value (Str "psychic"); Value (Str "power")]));
                          Value (Str "with");
                          App
                            (Value (Int 1),
                             Value (ParamArray [Value (Str "Zoanthrope"); Var "Target"]));
                          Value (Str "first select");
                          App
                            (Call Repeat,
                             Value (ParamArray [Value (Str "model"); Value (Int 1)]));
                          Value (Str "in");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                          Value (Str "measure range visibility etc. from");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "If");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                          Call Suffer; Value (Str "Perils of");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "Warp"); Lam ("obj",Var "obj")]));
                          Value (Str "it"); Call Suffer;
                          Value (Str "D3 mortal as described in");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "core"); Lam ("obj",Var "obj")]));
                          Value (Str "but within"); Value (Distance 6);
                          Value (Str "will only suffer damage if");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "Perils"); Lam ("obj",Var "obj")]));
                          Value (Str "of");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "Warp"); Lam ("obj",Var "obj")]));
                          Value (Str "causes");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray [Value (Str "last"); Value (Str "model")]));
                          Value (Str "in");
                          App
                            (Lam ("obj",Var "obj"),
                             Value (ParamArray [Value (Str "Zoanthrope"); Var "Target"]));
                          Value (Str "to be slain")])])])
    let ``VENOMTHROPES`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value
                            (Str
                               "This unit contains 3 Venomthropes. It can include up to 3 additional  Venomthropes (Power Rating +4) . Each model is armed with toxic lashes.");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                          Value (Str "contains"); Value (Int 3);
                          Value (Str "It include up to"); Value (Int 3);
                          Value (Str "additional Power Rating"); Value (Int 4);
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "is armed with toxic")]);
                    Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Toxic Miasma:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " At the end of the Fight phase, roll a D6 for each enemy unit within 1\" of any Venomthropes. On a 5+, that unit suffers a mortal wound.");
                                Value (Str "At");
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
                                         Lam ("obj",Var "obj")])); Value (Str "roll");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [App (Call Dice,Value (Int 6)); Value (Int 1)]));
                                Value (Str "for");
                                Lam
                                  ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                                Var "Target"; Var "Target"; Value (Str "within");
                                Value (Distance 1); Value (Str "of");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "Venomthropes");
                                         Lam ("obj",Var "obj")])); Value (Str "On");
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
                                                       [Var "roll"; Value (Int 5)])),
                                               Let
                                                 ("eq",
                                                  App
                                                    (Call Equals,
                                                     Value
                                                       (ParamArray
                                                          [Var "roll"; Value (Int 5)])),
                                                  App
                                                    (Call Or,
                                                     Value
                                                       (ParamArray [Var "eq"; Var "gt"])))));
                                         Value (Int 1)]));
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Call Suffer;
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "Mortal Wound"); Value (Int 1)]))])]);
                    Value
                      (ParamArray
                         [Value (Str "Shrouding Spores:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " Your opponent must subtract 1 from hit rolls made for ranged weapons that target <HIVE FLEET>  units (excluding MONSTERS ) whilst they are within 6\" of any <HIVE FLEET>  Venomthropes. In addition, your opponent must subtract 1 from hit rolls made for ranged weapons that target <HIVE FLEET>  MONSTERS  whilst they are within 6\" of any <HIVE FLEET>  Venomthrope units that contain 3 or more models. Increase the range of both these effects to 9\" whilst this unit contains 6 models.");
                                Value (Str "Your opponent must subtract"); Value (Int 1);
                                Value
                                  (Str
                                     "from Hit Roll made for ranged that target <HIVE FLEET> excluding whilst they are within");
                                Value (Distance 6); Value (Str "of");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "<HIVE FLEET>");
                                         Value (Str "Venomthropes")]));
                                Value (Str "In addition your opponent must subtract");
                                Value (Int 1);
                                Value (Str "from Hit Roll made for ranged");
                                Let
                                  ("ThatSubject",
                                   Value
                                     (ParamArray
                                        [Var "target";
                                         App
                                           (Call Repeat,
                                            Value
                                              (ParamArray
                                                 [Value (Str "range");
                                                  Lam ("obj",Var "obj")]));
                                         Value (Str "of both"); Lam ("obj",Var "obj");
                                         Value (Str "to"); Value (Distance 9);
                                         Value (Str "whilst");
                                         App
                                           (Call Repeat,
                                            Value
                                              (ParamArray
                                                 [Var "Target"; Lam ("obj",Var "obj")]));
                                         Value (Str "contains"); Value (Int 6)]),
                                   Value
                                     (ParamArray
                                        [Let
                                           ("ThatObject",
                                            Value
                                              (ParamArray
                                                 [Value (Str "whilst they are within");
                                                  Value (Distance 6); Value (Str "of");
                                                  App
                                                    (Lam ("obj",Var "obj"),
                                                     Value
                                                       (ParamArray
                                                          [Value (Str "<HIVE FLEET>");
                                                           Value (Str "Venomthrope")]));
                                                  App
                                                    (Lam ("obj",Var "obj"),
                                                     Value (ParamArray []));
                                                  Value (Str "contain"); Value (Int 3);
                                                  Value (Str "or more");
                                                  App
                                                    (Call Repeat,
                                                     Value
                                                       (ParamArray
                                                          [Value (Str "range");
                                                           Lam ("obj",Var "obj")]));
                                                  Value (Str "of both");
                                                  Lam ("obj",Var "obj");
                                                  Value (Str "to"); Value (Distance 9);
                                                  Value (Str "whilst");
                                                  App
                                                    (Call Repeat,
                                                     Value
                                                       (ParamArray
                                                          [Var "Target";
                                                           Lam ("obj",Var "obj")]));
                                                  Value (Str "contains"); Value (Int 6)]),
                                            Value
                                              (ParamArray
                                                 [App
                                                    (Value (Str "MONSTERS"),
                                                     Value
                                                       (ParamArray
                                                          [Var "ThatSubject";
                                                           Var "ThatObject"]));
                                                  App
                                                    (Call Repeat,
                                                     Value
                                                       (ParamArray
                                                          [Value (Str "range");
                                                           Lam ("obj",Var "obj")]));
                                                  Value (Str "of both");
                                                  Lam ("obj",Var "obj");
                                                  Value (Str "to"); Value (Distance 9);
                                                  Value (Str "whilst");
                                                  App
                                                    (Call Repeat,
                                                     Value
                                                       (ParamArray
                                                          [Var "Target";
                                                           Lam ("obj",Var "obj")]));
                                                  Value (Str "contains"); Value (Int 6)]))]))])])])])
    let ``HARUSPEX`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value
                            (Str
                               "A Haruspex is a single model armed with a grasping tongue, a ravenous maw and shovelling claws.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray
                                  [Value (Str "Haruspex"); Lam ("obj",Var "obj")]));
                          Value (Str "is");
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray [Value (Str "single"); Value (Str "model")]));
                          Value (Str "armed with");
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray
                                  [Value (Str "grasping"); Value (Str "tongue")]));
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray [Value (Str "ravenous"); Value (Str "maw")]));
                          Value (Str "and shovelling")]);
                    Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Acid Blood:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " Each time this model loses a wound in the Fight phase, roll a dice; on a 6, the unit that inflicted the damage suffers a mortal wound after all of their attacks have been resolved.");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "time"); Lam ("obj",Var "obj")]));
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "model"); Lam ("obj",Var "obj")]));
                                Value (Str "loses");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Value (Str "wound"); Value (Int 1)]));
                                Value (Str "in");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value
                                           (ParamArray
                                              [Value (Str "Phase"); Value (Str "Fight")]);
                                         Lam ("obj",Var "obj")])); Value (Str "roll");
                                App (Value (Int 1),Value (ParamArray []));
                                Value (Str "on");
                                App
                                  (Call Repeat,
                                   Value (ParamArray [Value (Int 6); Value (Int 1)]));
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Value (Str "that inflicted");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "damage"); Lam ("obj",Var "obj")]));
                                Call Suffer;
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "Mortal Wound"); Value (Int 1)]));
                                Value (Str "after");
                                App (Lam ("obj",Var "obj"),Value (ParamArray []));
                                Value (Str "of their have been resolved")])]);
                    Value
                      (ParamArray
                         [Value (Str "Frenzied Death Throes:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " If a Haruspex is reduced to 0 wounds, roll a dice before removing the model from the battlefield; on a 6, it lashes out in its death throes, and each unit within 3\" suffers 3 mortal wounds.");
                                Value (Str "If");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Value (Str "Haruspex"); Value (Int 1)]));
                                Value (Str "is reduced to"); Value (Int 0);
                                Value (Str "roll");
                                App (Value (Int 1),Value (ParamArray []));
                                Value (Str "before removing");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "model"); Lam ("obj",Var "obj")]));
                                Value (Str "from");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battlefield");
                                         Lam ("obj",Var "obj")])); Value (Str "on");
                                App
                                  (Call Repeat,
                                   Value (ParamArray [Value (Int 6); Value (Int 1)]));
                                Value (Str "it lashes out in its death and");
                                Lam
                                  ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                                Var "Target"; Value (Str "within"); Value (Distance 3);
                                Call Suffer; Value (Int 3); Value (Str "mortal")])]);
                    Value
                      (ParamArray
                         [Value (Str "Rapacious Hunger:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " Each time a Haruspex slays an enemy model with its ravenous maw, it can immediately make one extra attack with its shovelling claws. In addition, at the end of a Fight phase in which a Haruspex slew any models with its ravenous maw, it regains 1 wound lost earlier in the battle.");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "time"); Lam ("obj",Var "obj")]));
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Value (Str "Haruspex"); Value (Int 1)]));
                                Value (Str "slays");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray [Var "Target"; Value (Str "model")]));
                                Value
                                  (Str
                                     "with its ravenous maw it immediately make one extra attack with its shovelling In addition at");
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
                                         Value (Int 1)])); Value (Str "in which");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Value (Str "Haruspex"); Value (Int 1)]));
                                Value (Str "slew");
                                App (Lam ("obj",Var "obj"),Value (ParamArray []));
                                Value (Str "with its ravenous maw it regains");
                                Value (Int 1); Value (Str "wound lost earlier in");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battle"); Lam ("obj",Var "obj")]))])])])])
    let ``PYROVORES`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value
                            (Str
                               "This unit contains 1 Pyrovore. It can include 1 additional Pyrovore (Power Rating +2)  or 2 additional Pyrovores (Power Rating +4) . Each model is armed with a flamespurt and acid maw.");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                          Value (Str "contains"); Value (Int 1);
                          Value (Str "Pyrovore It include"); Value (Int 1);
                          Value (Str "additional Pyrovore Power Rating"); Value (Int 2);
                          Value (Str "or"); Value (Int 2);
                          Value (Str "additional Power Rating"); Value (Int 4);
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "is armed with");
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray
                                  [Value (Str "flamespurt"); Value (Str "and");
                                   Value (Str "acid"); Value (Str "maw")]))]);
                    Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Acid Blood:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " Each time this model loses a wound in the Fight phase, roll a dice; on a 6, the unit that inflicted the damage suffers a mortal wound after all of their attacks have been resolved.");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "time"); Lam ("obj",Var "obj")]));
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "model"); Lam ("obj",Var "obj")]));
                                Value (Str "loses");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Value (Str "wound"); Value (Int 1)]));
                                Value (Str "in");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value
                                           (ParamArray
                                              [Value (Str "Phase"); Value (Str "Fight")]);
                                         Lam ("obj",Var "obj")])); Value (Str "roll");
                                App (Value (Int 1),Value (ParamArray []));
                                Value (Str "on");
                                App
                                  (Call Repeat,
                                   Value (ParamArray [Value (Int 6); Value (Int 1)]));
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Value (Str "that inflicted");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "damage"); Lam ("obj",Var "obj")]));
                                Call Suffer;
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "Mortal Wound"); Value (Int 1)]));
                                Value (Str "after");
                                App (Lam ("obj",Var "obj"),Value (ParamArray []));
                                Value (Str "of their have been resolved")])]);
                    Value
                      (ParamArray
                         [Value (Str "Volatile:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " When a Pyrovore is slain, roll a dice. On a 4+ it bursts in a shower of acid – the nearest enemy unit within 3\" (if any) suffers a mortal wound.");
                                Value (Str "When");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Value (Str "Pyrovore"); Value (Int 1)]));
                                Value (Str "is slain roll");
                                App (Value (Int 1),Value (ParamArray []));
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
                                                    (ParamArray
                                                       [Var "roll"; Value (Int 4)])),
                                               Let
                                                 ("eq",
                                                  App
                                                    (Call Equals,
                                                     Value
                                                       (ParamArray
                                                          [Var "roll"; Value (Int 4)])),
                                                  App
                                                    (Call Or,
                                                     Value
                                                       (ParamArray [Var "eq"; Var "gt"])))));
                                         Value (Int 1)])); Value (Str "it bursts in");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Value (Str "shower"); Value (Int 1)]));
                                Value (Str "of acid");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "nearest"); Var "Target";
                                         Var "Target"])); Value (Str "within");
                                Value (Distance 3); Value (Str "if");
                                App (Lam ("obj",Var "obj"),Value (ParamArray []));
                                Call Suffer;
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "Mortal Wound"); Value (Int 1)]))])])])])
    let ``GARGOYLES`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Hail of Living Ammunition:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " If this unit contains 20 or more models, you can re-roll wound rolls of 1 when it shoots.");
                                Value (Str "If");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Value (Str "contains"); Value (Int 20);
                                Value (Str "or more re-roll wound of"); Value (Int 1);
                                Value (Str "when it shoots")])]);
                    Value
                      (ParamArray
                         [Value (Str "Swooping Assault:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " During deployment you can set this unit up clinging to an airborne Harridan instead of placing them on the battlefield. At the end of any of your Movement phases this unit can swoop down from above – set them up anywhere on the battlefield that is more than 9\" away from any enemy models.");
                                Value (Str "During deployment set");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Value (Str "up clinging to");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "airborne"); Value (Str "Harridan")]));
                                Value (Str "instead of placing them on");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battlefield");
                                         Lam ("obj",Var "obj")])); Value (Str "At");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "end"); Lam ("obj",Var "obj")]));
                                Value (Str "of");
                                App (Lam ("obj",Var "obj"),Value (ParamArray []));
                                Value (Str "of your Movement");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Value
                                  (Str "swoop down from above set them up anywhere on");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battlefield");
                                         Lam ("obj",Var "obj")]));
                                Value (Str "that is more than"); Value (Distance 9);
                                Value (Str "away from");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]))])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "This unit contains 10 Gargoyles. It can include up to 10 additional  Gargoyles (Power Rating + 3)  or up to 20 additional  Gargoyles (Power Rating +6) . Each model is armed with a fleshborer and blinding venom.");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                          Value (Str "contains"); Value (Int 10);
                          Value (Str "It include up to"); Value (Int 10);
                          Value (Str "additional Power Rating + 3 or up to");
                          Value (Int 20); Value (Str "additional Power Rating");
                          Value (Int 6);
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "is armed with");
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray
                                  [Value (Str "fleshborer"); Value (Str "and");
                                   Value (Str "blinding"); Value (Str "venom")]))])])])
    let ``BROODLORD`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "Synapse"); Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Shadow in the Warp  (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Lightning Reflexes:");
                          Value
                            (ParamArray
                               [Value (Str " This model has a 5+ invulnerable save.");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "model"); Lam ("obj",Var "obj")]));
                                Value (Str "has"); Value (Int 1); Value (Int 5);
                                Value (Str "+ invulnerable save")])]);
                    Value
                      (ParamArray
                         [Value (Str "Swift and Deadly:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " This model can charge even if it Advanced during its turn.");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "model"); Lam ("obj",Var "obj")]));
                                Value (Str "charge even if it Advanced during its turn")])]);
                    Value
                      (ParamArray
                         [Value (Str "Brood Telepathy:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " You can add 1 to hit rolls in the Fight phase for <HIVE FLEET> Genestealer units within 6\" of any friendly <HIVE FLEET>  Broodlords.");
                                Value (Str "add"); Value (Int 1);
                                Value (Str "to hit in");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value
                                           (ParamArray
                                              [Value (Str "Phase"); Value (Str "Fight")]);
                                         Lam ("obj",Var "obj")]));
                                Value (Str "for <HIVE FLEET> Genestealer within");
                                Value (Distance 6); Value (Str "of");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "friendly");
                                         Value (Str "<HIVE FLEET>")]))])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "A Broodlord can attempt to manifest one psychic power in each friendly Psychic phase, and attempt to deny one psychic power in each enemy Psychic phase. It knows the Smite  psychic power and one psychic power from the Hive Mind discipline (pg 121).");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray
                                  [Value (Str "Broodlord"); Lam ("obj",Var "obj")]));
                          Value (Str "attempt to manifest one psychic power in");
                          Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                          Value
                            (Str
                               "friendly Psychic phase and attempt to deny one psychic power in");
                          Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                          Var "Target"; Value (Str "Psychic phase It knows");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray
                                  [Value (Str "Smite"); Value (Str "psychic");
                                   Value (Str "power"); Value (Str "and");
                                   Value (Str "one"); Value (Str "psychic");
                                   Value (Str "power")])); Value (Str "from");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray
                                  [Value (Str "Hive"); Value (Str "Mind");
                                   Value (Str "discipline")])); Value (Str "pg");
                          Value (Int 121)]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "A Broodlord is a single model armed with monstrous rending claws.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray
                                  [Value (Str "Broodlord"); Lam ("obj",Var "obj")]));
                          Value (Str "is");
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray [Value (Str "single"); Value (Str "model")]));
                          Value (Str "armed with monstrous rending")])])])
    let ``HIVE TYRANT`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "Shadow in the Warp");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Synapse  (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "The Will of the Hive Mind:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " The range of a Hive Tyrant’s Synapse ability is 18\" rather than 12\".");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "range"); Lam ("obj",Var "obj")]));
                                Value (Str "of");
                                App
                                  (Value (Int 1),
                                   Value
                                     (ParamArray
                                        [Value (Str "Hive"); Value (Str "Tyrant");
                                         Value (Str "'s")]));
                                Value (Str "Synapse ability is"); Value (Distance 18);
                                Value (Str "rather than"); Value (Distance 12)])]);
                    Value
                      (ParamArray
                         [Value (Str "Swooping Assault:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " During deployment, you can set up a Hive Tyrant with wings circling high above instead of placing it on the battlefield. At the end of any of your Movement phases it can swoop down – set it up anywhere on the battlefield that is more than 9\" from any enemy models.");
                                Value (Str "During deployment set up");
                                App
                                  (Value (Int 1),
                                   Value
                                     (ParamArray
                                        [Value (Str "Hive"); Value (Str "Tyrant")]));
                                Value
                                  (Str
                                     "with circling high above instead of placing it on");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battlefield");
                                         Lam ("obj",Var "obj")])); Value (Str "At");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "end"); Lam ("obj",Var "obj")]));
                                Value (Str "of");
                                App (Lam ("obj",Var "obj"),Value (ParamArray []));
                                Value
                                  (Str
                                     "of your Movement it swoop down set it up anywhere on");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battlefield");
                                         Lam ("obj",Var "obj")]));
                                Value (Str "that is more than"); Value (Distance 9);
                                Value (Str "from");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]))])]);
                    Value
                      (ParamArray
                         [Value (Str "Death Throes:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " If this model is reduced to 0 wounds, roll a dice before removing the model from the battlefield; on a 6, it lashes out in its death throes, and each unit within 3\" suffers D3 mortal wounds.");
                                Value (Str "If");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "model"); Lam ("obj",Var "obj")]));
                                Value (Str "is reduced to"); Value (Int 0);
                                Value (Str "roll");
                                App (Value (Int 1),Value (ParamArray []));
                                Value (Str "before removing");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "model"); Lam ("obj",Var "obj")]));
                                Value (Str "from");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battlefield");
                                         Lam ("obj",Var "obj")])); Value (Str "on");
                                App
                                  (Call Repeat,
                                   Value (ParamArray [Value (Int 6); Value (Int 1)]));
                                Value (Str "it lashes out in its death and");
                                Lam
                                  ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                                Var "Target"; Value (Str "within"); Value (Distance 3);
                                Call Suffer; Value (Str "D3 mortal")])]);
                    Value
                      (ParamArray
                         [Value (Str "Psychic Barrier:");
                          Value
                            (ParamArray
                               [Value (Str " A Hive Tyrant has a 4+ invulnerable save.");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "Hive"); Value (Str "Tyrant")]));
                                Value (Str "has"); Value (Int 1); Value (Int 4);
                                Value (Str "+ invulnerable save")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "A Hive Tyrant may replace one pair of monstrous scything talons with one item from the Monstrous Bio-cannons  or Monstrous Bio-weapons  list.");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray [Value (Str "Hive"); Value (Str "Tyrant")]));
                          Value
                            (Str
                               "may replace one pair of monstrous scything with one item from");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray
                                  [Value (Str "Monstrous"); Value (Str "Bio-cannons");
                                   Value (Str "or"); Value (Str "Monstrous");
                                   Value (Str "Bio-weapons"); Value (Str "list")]))]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "A Hive Tyrant may replace both pairs of monstrous scything talons with two items from the Monstrous Bio- cannons  or two items from the Monstrous Bio-weapons  list, or with one item from each list.");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray [Value (Str "Hive"); Value (Str "Tyrant")]));
                          Value (Str "may replace"); Lam ("obj",Var "obj");
                          Value (Str "of monstrous scything with two from");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray [Value (Str "Monstrous"); Value (Str "Bio")]));
                          Value (Str "or two from");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray
                                  [Value (Str "Monstrous"); Value (Str "Bio-weapons");
                                   Value (Str "list")]));
                          Value (Str "or with one item from");
                          Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                          Value (Str "list")]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "This model may have wings (Power Rating +2) . If it does, it uses the second set of Move characteristics in the damage table above, and it gains the FLY  keyword.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "may have Power Rating"); Value (Int 2);
                          Value (Str "If it does it uses");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray [Value (Str "second"); Value (Str "set")]));
                          Value (Str "of Move in");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray [Value (Str "damage"); Value (Str "table")]));
                          Value (Str "above and it gains");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray [Value (Str "FLY"); Value (Str "keyword")]))]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "This model may have toxin sacs and/or adrenal glands (pg 113) .");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "may have toxin and/or adrenal pg");
                          Value (Int 113)]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "A Hive Tyrant can attempt to manifest two psychic powers in each friendly Psychic phase, and attempt to deny one psychic power in each enemy Psychic phase. It knows the Smite  power and two psychic powers from the Hive Mind discipline (pg 121).");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray [Value (Str "Hive"); Value (Str "Tyrant")]));
                          Value (Str "attempt to manifest two psychic in");
                          Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                          Value
                            (Str
                               "friendly Psychic phase and attempt to deny one psychic power in");
                          Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                          Var "Target"; Value (Str "Psychic phase It knows");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray
                                  [Value (Str "Smite"); Value (Str "power");
                                   Value (Str "and"); Value (Str "two");
                                   Value (Str "psychic")])); Value (Str "from");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray
                                  [Value (Str "Hive"); Value (Str "Mind");
                                   Value (Str "discipline")])); Value (Str "pg");
                          Value (Int 121)]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "A Hive Tyrant is a single model armed with two pairs of monstrous scything talons and a prehensile pincer tail.");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray [Value (Str "Hive"); Value (Str "Tyrant")]));
                          Value (Str "is");
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray [Value (Str "single"); Value (Str "model")]));
                          Value (Str "armed with two of monstrous scything and");
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray
                                  [Value (Str "prehensile"); Value (Str "pincer");
                                   Value (Str "tail")]))])])])
    let ``TYRANID PRIME`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "Alpha Warrior:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " You can add 1 to hit rolls for all <HIVE FLEET>  Tyranid Warrior units that are within 6\" of any friendly <HIVE FLEET> Tyranid Primes.");
                                Value (Str "add"); Value (Int 1);
                                Value (Str "to hit for");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "<HIVE FLEET>");
                                         Value (Str "Tyranid"); Value (Str "Warrior")]));
                                Value (Str "that are within"); Value (Distance 6);
                                Value (Str "of");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "friendly");
                                         Value (Str "<HIVE FLEET>");
                                         Value (Str "Tyranid"); Value (Str "Primes")]))])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "This model may replace its devourer with one weapon from the Basic Bio-weapons  or Melee Bio-weapons  list.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "may replace its devourer with one weapon from");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray
                                  [Value (Str "Basic"); Value (Str "Bio-weapons");
                                   Value (Str "or"); Value (Str "Melee");
                                   Value (Str "Bio-weapons"); Value (Str "list")]))]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "This model may replace its scything talons with one weapon from the Melee Bio-weapons  list.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "may replace its scything with one weapon from");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray
                                  [Value (Str "Melee"); Value (Str "Bio-weapons");
                                   Value (Str "list")]))]);
                    Value
                      (ParamArray
                         [Value (Str "This model may have flesh hooks.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "may have flesh")]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "This model may have toxin sacs and/or adrenal glands (pg 113) .");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "may have toxin and/or adrenal pg");
                          Value (Int 113)]);
                    Value
                      (ParamArray
                         [Value (Str "Shadow in the Warp");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Synapse (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "A Tyranid Prime is a single model armed with scything talons and a devourer.");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray [Value (Str "Tyranid"); Value (Str "Prime")]));
                          Value (Str "is");
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray [Value (Str "single"); Value (Str "model")]));
                          Value (Str "armed with scything and");
                          App
                            (Call Repeat,
                             Value (ParamArray [Value (Str "devourer"); Value (Int 1)]))])])])
    let ``THE SWARMLORD`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "Shadow in the Warp");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Synapse  (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Psychic Barrier:");
                          Value
                            (ParamArray
                               [Value (Str " The Swarmlord has a 4+ invulnerable save.");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "Swarmlord"); Lam ("obj",Var "obj")]));
                                Value (Str "has"); Value (Int 1); Value (Int 4);
                                Value (Str "+ invulnerable save")])]);
                    Value
                      (ParamArray
                         [Value (Str "Blade Parry:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " Add 1 to the Swarmlord’s invulnerable saves against wounds caused by Melee weapons.");
                                Value (Str "Add"); Value (Int 1); Value (Str "to");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "Swarmlord"); Value (Str "'s")]));
                                Value (Str "invulnerable saves against caused by Melee")])]);
                    Value
                      (ParamArray
                         [Value (Str "Hive Commander:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " In each of your Shooting phases, you can pick one friendly <HIVE FLEET>  unit within 6\" of the Swarmlord. That unit can move (and Advance, if you wish) as if it were the Movement phase instead of shooting.");
                                Value (Str "In");
                                Lam
                                  ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                                Value
                                  (Str "of your Shooting pick one friendly <HIVE FLEET>");
                                Var "Target"; Value (Str "within"); Value (Distance 6);
                                Value (Str "of");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "Swarmlord"); Lam ("obj",Var "obj")]));
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Value (Str "move and Advance if wish as if it were");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "Movement"); Value (Str "phase")]));
                                Value (Str "instead of shooting")])]);
                    Value
                      (ParamArray
                         [Value (Str "The Will of the Hive Mind:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " The range of the Swarmlord’s Synapse ability is 18\" rather than 12\".");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "range"); Lam ("obj",Var "obj")]));
                                Value (Str "of");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "Swarmlord"); Value (Str "'s")]));
                                Value (Str "Synapse ability is"); Value (Distance 18);
                                Value (Str "rather than"); Value (Distance 12)])]);
                    Value
                      (ParamArray
                         [Value (Str "Death Throes:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " If this model is reduced to 0 wounds, roll a dice before removing the model from the battlefield; on a 6, it lashes out in its death throes, and each unit within 3\" suffers D3 mortal wounds.");
                                Value (Str "If");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "model"); Lam ("obj",Var "obj")]));
                                Value (Str "is reduced to"); Value (Int 0);
                                Value (Str "roll");
                                App (Value (Int 1),Value (ParamArray []));
                                Value (Str "before removing");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "model"); Lam ("obj",Var "obj")]));
                                Value (Str "from");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battlefield");
                                         Lam ("obj",Var "obj")])); Value (Str "on");
                                App
                                  (Call Repeat,
                                   Value (ParamArray [Value (Int 6); Value (Int 1)]));
                                Value (Str "it lashes out in its death and");
                                Lam
                                  ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                                Var "Target"; Value (Str "within"); Value (Distance 3);
                                Call Suffer; Value (Str "D3 mortal")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "The Swarmlord can attempt to manifest two psychic powers in each friendly Psychic phase, and attempt to deny two psychic powers in each enemy Psychic phase. It knows the Smite  power and two psychic powers from the Hive Mind discipline (pg 121).");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray
                                  [Value (Str "Swarmlord"); Lam ("obj",Var "obj")]));
                          Value (Str "attempt to manifest two psychic in");
                          Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                          Value
                            (Str
                               "friendly Psychic phase and attempt to deny two psychic in");
                          Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                          Var "Target"; Value (Str "Psychic phase It knows");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray
                                  [Value (Str "Smite"); Value (Str "power");
                                   Value (Str "and"); Value (Str "two");
                                   Value (Str "psychic")])); Value (Str "from");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray
                                  [Value (Str "Hive"); Value (Str "Mind");
                                   Value (Str "discipline")])); Value (Str "pg");
                          Value (Int 121)]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "The Swarmlord is a single model armed with bone sabres and a prehensile pincer tail. Only one of this model may be included in your army.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray
                                  [Value (Str "Swarmlord"); Lam ("obj",Var "obj")]));
                          Value (Str "is");
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray [Value (Str "single"); Value (Str "model")]));
                          Value (Str "armed with bone and");
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray
                                  [Value (Str "prehensile"); Value (Str "pincer");
                                   Value (Str "tail")])); Value (Str "Only one of");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "may be included in your army")])])])
    let ``TERVIGON`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value
                            (Str
                               "A Tervigon is a single model armed with massive scything talons. It can also fire stinger salvoes.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray
                                  [Value (Str "Tervigon"); Lam ("obj",Var "obj")]));
                          Value (Str "is");
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray [Value (Str "single"); Value (Str "model")]));
                          Value (Str "armed with massive scything It also fire stinger")]);
                    Value
                      (ParamArray
                         [Value (Str "Shadow in the Warp");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Synapse  (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Brood Progenitor:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " You can re-roll hit rolls of 1 in the Shooting phase for friendly <HIVE FLEET>  Termagant units within 6\" of this model.");
                                Let
                                  ("Hit Roll",
                                   Value
                                     (ParamArray
                                        [Lam
                                           ("rollTarget",
                                            Let
                                              ("f",Var "Hit Roll",
                                               Let
                                                 ("x",App (Var "f",Var "rollTarget"),
                                                  IfThenElse
                                                    (App
                                                       (Call NotEquals,
                                                        Value
                                                          (ParamArray
                                                             [Var "x"; Value (Int 1)])),
                                                     Var "x",
                                                     Some
                                                       (App (Var "f",Var "rollTarget"))))))]),
                                   Value
                                     (ParamArray
                                        [Value (Str "in");
                                         App
                                           (Lam ("obj",Var "obj"),
                                            Value
                                              (ParamArray
                                                 [Value (Str "Shooting");
                                                  Value (Str "phase")]));
                                         Value
                                           (Str
                                              "for friendly <HIVE FLEET> Termagant within");
                                         Value (Distance 6); Value (Str "of");
                                         App
                                           (Call Repeat,
                                            Value
                                              (ParamArray
                                                 [Value (Str "model");
                                                  Lam ("obj",Var "obj")]))]))])]);
                    Value
                      (ParamArray
                         [Value (Str "Synaptic Backlash:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " If a Tervigon is reduced to 0 wounds, roll a D6 before removing the model from the battlefield. Each friendly <HIVE FLEET>  Termagant unit within 6\" of the Tervigon immediately suffers a number of mortal wounds equal to the result.");
                                Value (Str "If");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Value (Str "Tervigon"); Value (Int 1)]));
                                Value (Str "is reduced to"); Value (Int 0);
                                Value (Str "roll");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [App (Call Dice,Value (Int 6)); Value (Int 1)]));
                                Value (Str "before removing");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "model"); Lam ("obj",Var "obj")]));
                                Value (Str "from");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battlefield");
                                         Lam ("obj",Var "obj")]));
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "friendly");
                                         Value (Str "<HIVE FLEET>");
                                         Value (Str "Termagant"); Var "Target"]));
                                Value (Str "within"); Value (Distance 6);
                                Value (Str "of");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "Tervigon"); Lam ("obj",Var "obj")]));
                                Value (Str "immediately"); Call Suffer;
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Value (Str "number"); Value (Int 1)]));
                                Value (Str "of mortal equal to");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "result"); Lam ("obj",Var "obj")]))])]);
                    Value
                      (ParamArray
                         [Value (Str "Spawn Termagants:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " At the start of your Movement phase, a Tervigon can spawn Termagants. If it does so, add a new unit of 10 Termagants to your army and set it up on the battlefield so that it is wholly within 6\" of the Tervigon and more than 1\" from the enemy. All of these models are armed with fleshborers. Alternatively, you can replace up to 10 models lost earlier in the battle in an existing unit of Termagants from your army that is within 6\" of the Tervigon. Models placed in this way must be within 6\" of the Tervigon and more than 1\" from the enemy. You can only replace models armed with fleshborers. If you cannot place some of the models the excess is discarded.");
                                Value (Str "At");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "start"); Lam ("obj",Var "obj")]));
                                Value (Str "of your Movement phase");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Value (Str "Tervigon"); Value (Int 1)]));
                                Value (Str "spawn If it does so add");
                                App
                                  (Value (Int 1),
                                   Value (ParamArray [Value (Str "new"); Var "Target"]));
                                Value (Str "of"); Value (Int 10);
                                Value (Str "to your army and set it up on");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battlefield");
                                         Lam ("obj",Var "obj")]));
                                Value (Str "so that it is wholly within");
                                Value (Distance 6); Value (Str "of");
                                Lam ("obj",Var "obj");
                                Value (Str "Tervigon and more than"); Value (Distance 1);
                                Value (Str "from");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                App (Lam ("obj",Var "obj"),Value (ParamArray []));
                                Value (Str "of");
                                App (Lam ("obj",Var "obj"),Value (ParamArray []));
                                Value (Str "are armed with Alternatively replace up to");
                                Value (Int 10); Value (Str "lost earlier in");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battle"); Lam ("obj",Var "obj")]));
                                Value (Str "in");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray [Value (Str "existing"); Var "Target"]));
                                Value
                                  (Str "of Termagants from your army that is within");
                                Value (Distance 6); Value (Str "of");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "Tervigon"); Lam ("obj",Var "obj")]));
                                Value (Str "placed in");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "way"); Lam ("obj",Var "obj")]));
                                Value (Str "must be within"); Value (Distance 6);
                                Value (Str "of"); Lam ("obj",Var "obj");
                                Value (Str "Tervigon and more than"); Value (Distance 1);
                                Value (Str "from");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Value (Str "only replace armed with If not place");
                                App (Lam ("obj",Var "obj"),Value (ParamArray []));
                                Value (Str "of");
                                App (Lam ("obj",Var "obj"),Value (ParamArray []));
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "excess"); Lam ("obj",Var "obj")]));
                                Value (Str "is discarded")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "This model may replace its massive scything talons with massive crushing claws.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value
                            (Str
                               "may replace its massive scything with massive crushing")]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "This model may have toxin sacs and/or adrenal glands (pg 113) .");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "may have toxin and/or adrenal pg");
                          Value (Int 113)]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "A Tervigon can attempt to manifest one psychic power in each friendly Psychic phase, and attempt to deny one psychic power in each enemy Psychic phase. It knows the Smite  power and one psychic power from the Hive Mind discipline (pg 121).");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray
                                  [Value (Str "Tervigon"); Lam ("obj",Var "obj")]));
                          Value (Str "attempt to manifest one psychic power in");
                          Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                          Value
                            (Str
                               "friendly Psychic phase and attempt to deny one psychic power in");
                          Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                          Var "Target"; Value (Str "Psychic phase It knows");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray
                                  [Value (Str "Smite"); Value (Str "power");
                                   Value (Str "and"); Value (Str "one");
                                   Value (Str "psychic"); Value (Str "power")]));
                          Value (Str "from");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray
                                  [Value (Str "Hive"); Value (Str "Mind");
                                   Value (Str "discipline")])); Value (Str "pg");
                          Value (Int 121)])])])
    let ``NEUROTHROPE`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "Spirit Leech:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " Each time a Neurothrope slays a model using the Smite  psychic power, you can heal a wound on a friendly <HIVE FLEET>  ZOANTHROPE  within 6\".");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "time"); Lam ("obj",Var "obj")]));
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "Neurothrope"); Value (Int 1)]));
                                Value (Str "slays");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Value (Str "model"); Value (Int 1)]));
                                Value (Str "using");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "Smite"); Value (Str "psychic");
                                         Value (Str "power")])); Value (Str "heal");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Value (Str "wound"); Value (Int 1)]));
                                Value (Str "on");
                                App
                                  (Value (Int 1),
                                   Value
                                     (ParamArray
                                        [Value (Str "friendly");
                                         Value (Str "<HIVE FLEET>");
                                         Value (Str "ZOANTHROPE")]));
                                Value (Str "within"); Value (Distance 6)])]);
                    Value
                      (ParamArray
                         [Value (Str "Warp Field:");
                          Value
                            (ParamArray
                               [Value (Str " This model has a 3+ invulnerable save.");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "model"); Lam ("obj",Var "obj")]));
                                Value (Str "has"); Value (Int 1); Value (Int 3);
                                Value (Str "+ invulnerable save")])]);
                    Value
                      (ParamArray
                         [Value (Str "Warp Siphon:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " You can re-roll rolls of 1 when taking Psychic tests for friendly <HIVE FLEET>  ZOANTHROPE  units within 6\" of this model.");
                                Value (Str "re-roll of"); Value (Int 1);
                                Value
                                  (Str
                                     "when taking Psychic for friendly <HIVE FLEET> ZOANTHROPE within");
                                Value (Distance 6); Value (Str "of");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "model"); Lam ("obj",Var "obj")]))])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "A Neurothrope can attempt to manifest two psychic powers in each friendly Psychic phase, and attempt to deny one psychic power in each enemy Psychic phase. A Neurothrope knows the Smite  psychic power and one psychic power from the Hive Mind discipline (pg 121).");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray
                                  [Value (Str "Neurothrope"); Lam ("obj",Var "obj")]));
                          Value (Str "attempt to manifest two psychic in");
                          Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                          Value
                            (Str
                               "friendly Psychic phase and attempt to deny one psychic power in");
                          Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                          Var "Target"; Value (Str "Psychic phase");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray
                                  [Value (Str "Neurothrope"); Lam ("obj",Var "obj")]));
                          Value (Str "knows");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray
                                  [Value (Str "Smite"); Value (Str "psychic");
                                   Value (Str "power"); Value (Str "and");
                                   Value (Str "one"); Value (Str "psychic");
                                   Value (Str "power")])); Value (Str "from");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray
                                  [Value (Str "Hive"); Value (Str "Mind");
                                   Value (Str "discipline")])); Value (Str "pg");
                          Value (Int 121)]);
                    Value
                      (ParamArray
                         [Value (Str "Shadow in the Warp");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Synapse (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "A Neurothrope is a single model armed with claws and teeth.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray
                                  [Value (Str "Neurothrope"); Lam ("obj",Var "obj")]));
                          Value (Str "is");
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray [Value (Str "single"); Value (Str "model")]));
                          Value (Str "armed with and")])])])
    let ``OLD ONE EYE`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value
                            (Str
                               "Old One Eye is a single model armed with monstrous crushing claws, monstrous scything talons and a thresher scythe. Only one of this model may be included in your army.");
                          Value (Str "Old One Eye is");
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray [Value (Str "single"); Value (Str "model")]));
                          Value
                            (Str "armed with monstrous crushing monstrous scything and");
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray
                                  [Value (Str "thresher"); Value (Str "scythe")]));
                          Value (Str "Only one of");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "may be included in your army")]);
                    Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour  (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Immortal Battering Ram:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " When Old One Eye finishes a charge move, roll a dice; on a 4+ one enemy unit within 1\" suffers D3 mortal wounds. In addition, add 1 to all hit rolls for Old One Eye in the Fight phase if it charged in the same turn.");
                                Value (Str "When Old One Eye finishes");
                                App
                                  (Value (Int 1),
                                   Value
                                     (ParamArray
                                        [Value (Str "charge"); Value (Str "move")]));
                                Value (Str "roll");
                                App (Value (Int 1),Value (ParamArray []));
                                Value (Str "on");
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
                                                       [Var "roll"; Value (Int 4)])),
                                               Let
                                                 ("eq",
                                                  App
                                                    (Call Equals,
                                                     Value
                                                       (ParamArray
                                                          [Var "roll"; Value (Int 4)])),
                                                  App
                                                    (Call Or,
                                                     Value
                                                       (ParamArray [Var "eq"; Var "gt"])))));
                                         Value (Int 1)])); Value (Str "one");
                                Var "Target"; Var "Target"; Value (Str "within");
                                Value (Distance 1); Call Suffer;
                                Value (Str "D3 mortal In addition add"); Value (Int 1);
                                Value (Str "to");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "hit"); Lam ("obj",Var "obj")]));
                                Value (Str "for Old One Eye in");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value
                                           (ParamArray
                                              [Value (Str "Phase"); Value (Str "Fight")]);
                                         Lam ("obj",Var "obj")]));
                                Value (Str "if it charged in");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "same"); Value (Str "turn")]))])]);
                    Value
                      (ParamArray
                         [Value (Str "Alpha Leader:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " You can add 1 to hit rolls in the Fight phase for friendly <HIVE FLEET>  CARNIFEX  units that are within 6\" of this model.");
                                Value (Str "add"); Value (Int 1);
                                Value (Str "to hit in");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value
                                           (ParamArray
                                              [Value (Str "Phase"); Value (Str "Fight")]);
                                         Lam ("obj",Var "obj")]));
                                Value
                                  (Str
                                     "for friendly <HIVE FLEET> CARNIFEX that are within");
                                Value (Distance 6); Value (Str "of");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "model"); Lam ("obj",Var "obj")]))])]);
                    Value
                      (ParamArray
                         [Value (Str "Berserk Rampage:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " Each time you make a hit roll of 6+ for Old One Eye (except for thresher scythe attacks), you may immediately make 1 additional attack with the same weapon against the same unit. These additional attacks do not confer extra attacks.");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "time"); Lam ("obj",Var "obj")]));
                                Value (Str "make");
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
                                Value
                                  (Str
                                     "for Old One Eye except for thresher scythe may immediately make");
                                Value (Int 1); Value (Str "additional attack with");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "same"); Value (Str "weapon")]));
                                Value (Str "against");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value (ParamArray [Value (Str "same"); Var "Target"]));
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "additional"); Lam ("obj",Var "obj")]));
                                Value (Str "do not confer extra")])]);
                    Value
                      (ParamArray
                         [Value (Str "Regeneration:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " At the beginning of each of your turns, this model heals one wound.");
                                Value (Str "At");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "beginning"); Lam ("obj",Var "obj")]));
                                Value (Str "of");
                                Lam
                                  ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                                Value (Str "of your");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "model"); Lam ("obj",Var "obj")]));
                                Value (Str "heals one wound")])])])])
    let ``GENESTEALERS`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "Flurry of Claws:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " Genestealers have 4 Attacks instead of 3 whilst their unit has 10 or more models.");
                                Value (Str "have"); Value (Int 4);
                                Value (Str "instead of"); Value (Int 3);
                                Value (Str "whilst their"); Var "Target";
                                Value (Str "has"); Value (Int 10); Value (Str "or more")])]);
                    Value
                      (ParamArray
                         [Value (Str "Lightning Reflexes:");
                          Value
                            (ParamArray
                               [Value (Str " Genestealers have a 5+ invulnerable save.");
                                Value (Str "have"); Value (Int 1); Value (Int 5);
                                Value (Str "+ invulnerable save")])]);
                    Value
                      (ParamArray
                         [Value (Str "Swift and Deadly:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " Genestealers can charge even if they Advanced during their turn.");
                                Value
                                  (Str "charge even if they Advanced during their turn")])]);
                    Value
                      (ParamArray
                         [Value (Str "Extended Carapaces:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " Genestealers with extended carapaces have a Save characteristic of 4+ but lose the Swift and Deadly ability.");
                                Value (Str "with extended have");
                                App
                                  (Value (Int 1),
                                   Value
                                     (ParamArray
                                        [Value (Str "Save");
                                         Value (Str "characteristic")]));
                                Value (Str "of");
                                Lam
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
                                Value (Str "but lose"); Lam ("obj",Var "obj");
                                Value (Str "Swift and Deadly ability")])]);
                    Value
                      (ParamArray
                         [Value (Str "Infestation:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " If your army includes any units of Genestealers, you can place up to four infestation nodes anywhere in your deployment zone when your army deploys. You can then set up any units of Genestealers lurking, instead of setting them up on the battlefield. If an enemy model is ever within 9\" of an infestation node, the node is destroyed and removed from the battlefield. Whilst there are any friendly infestation nodes on the battlefield, this unit can stop lurking: at the end of your Movement phase, set it up wholly within 6\" of a friendly infestation node. That infestation node is then removed from the battlefield. If this unit is still lurking when the last friendly infestation node is removed, this unit is destroyed.");
                                Value (Str "If your army includes");
                                App (Lam ("obj",Var "obj"),Value (ParamArray []));
                                Value
                                  (Str
                                     "of Genestealers place up to four infestation anywhere in your deployment zone when your army deploys then set up");
                                App (Lam ("obj",Var "obj"),Value (ParamArray []));
                                Value
                                  (Str
                                     "of Genestealers lurking instead of setting them up on");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battlefield");
                                         Lam ("obj",Var "obj")])); Value (Str "If");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray [Var "Target"; Value (Str "model")]));
                                Value (Str "is ever within"); Value (Distance 9);
                                Value (Str "of");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "infestation"); Value (Str "node")]));
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "node"); Lam ("obj",Var "obj")]));
                                Value (Str "is destroyed and removed from");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battlefield");
                                         Lam ("obj",Var "obj")]));
                                Value (Str "Whilst there are");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "friendly");
                                         Value (Str "infestation")])); Value (Str "on");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battlefield");
                                         Lam ("obj",Var "obj")]));
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Value (Str "stop lurking at");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "end"); Lam ("obj",Var "obj")]));
                                Value
                                  (Str "of your Movement phase set it up wholly within");
                                Value (Distance 6); Value (Str "of");
                                App
                                  (Value (Int 1),
                                   Value
                                     (ParamArray
                                        [Value (Str "friendly");
                                         Value (Str "infestation"); Value (Str "node")]));
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "infestation"); Value (Str "node")]));
                                Value (Str "is then removed from");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battlefield");
                                         Lam ("obj",Var "obj")])); Value (Str "If");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Value (Str "is still lurking when");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "last"); Value (Str "friendly");
                                         Value (Str "infestation"); Value (Str "node")]));
                                Value (Str "is removed");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Value (Str "is destroyed")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str "Any model may also have a pair of scything talons.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "may also have");
                          App
                            (Call Repeat,
                             Value (ParamArray [Value (Str "pair"); Value (Int 1)]));
                          Value (Str "of scything")]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "For every four models in the unit, one model may have flesh hooks and/or one model may have an acid maw.");
                          Value (Str "For");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "four"); Lam ("obj",Var "obj")]));
                          Value (Str "in");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                          Value
                            (Str "one model may have flesh and/or one model may have");
                          App
                            (Lam ("obj",Var "obj"),
                             Value (ParamArray [Value (Str "acid"); Value (Str "maw")]))]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "All models in the unit may have toxin sacs  (pg 113) and/or extended carapaces.");
                          App (Lam ("obj",Var "obj"),Value (ParamArray []));
                          Value (Str "in");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                          Value (Str "may have toxin pg"); Value (Int 113);
                          Value (Str "and/or extended")]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "This unit contains 5 Genestealers. It can include up to 5 additional Genestealers (Power Rating +4) , up to 10 additional Genestealers (Power Rating +8) , or up to 15 additional Genestealers (Power Rating +12) . Each model is armed with rending claws.");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                          Value (Str "contains"); Value (Int 5);
                          Value (Str "It include up to"); Value (Int 5);
                          Value (Str "additional Power Rating"); Value (Int 4);
                          Value (Str "up to"); Value (Int 10);
                          Value (Str "additional Power Rating"); Value (Int 8);
                          Value (Str "or up to"); Value (Int 15);
                          Value (Str "additional Power Rating"); Value (Int 12);
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "is armed with rending")])])])
    let ``TYRANID WARRIORS`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value
                            (Str
                               "This unit contains 3 Tyranid Warriors. It can include up to 3 additional Tyranid Warriors (Power Rating +4)  or up to 6 additional Tyranid Warriors (Power Rating +8) . Each model is armed with a pair of scything talons and a devourer.");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                          Value (Str "contains"); Value (Int 3);
                          Value (Str "Tyranid It include up to"); Value (Int 3);
                          Value (Str "additional Tyranid Warriors Power Rating");
                          Value (Int 4); Value (Str "or up to"); Value (Int 6);
                          Value (Str "additional Tyranid Warriors Power Rating");
                          Value (Int 8);
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "is armed with");
                          App
                            (Call Repeat,
                             Value (ParamArray [Value (Str "pair"); Value (Int 1)]));
                          Value (Str "of scything and");
                          App
                            (Call Repeat,
                             Value (ParamArray [Value (Str "devourer"); Value (Int 1)]))]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Any model may replace its devourer  with  one weapon from the Basic Bio-weapons  or Melee Bio-weapons  list.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "may replace its devourer with one weapon from");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray
                                  [Value (Str "Basic"); Value (Str "Bio-weapons");
                                   Value (Str "or"); Value (Str "Melee");
                                   Value (Str "Bio-weapons"); Value (Str "list")]))]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Any model may replace its scything talons  with  one weapon from the Melee Bio-weapons  list.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "may replace its scything with one weapon from");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray
                                  [Value (Str "Melee"); Value (Str "Bio-weapons");
                                   Value (Str "list")]))]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "For every three models in the unit, one model may replace its devourer with  one weapon from the Basic Bio-cannons  list.");
                          Value (Str "For");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "three"); Lam ("obj",Var "obj")]));
                          Value (Str "in");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                          Value
                            (Str
                               "one model may replace its devourer with one weapon from");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray
                                  [Value (Str "Basic"); Value (Str "Bio-cannons");
                                   Value (Str "list")]))]);
                    Value
                      (ParamArray
                         [Value (Str "All models in the unit may have flesh hooks.");
                          App (Lam ("obj",Var "obj"),Value (ParamArray []));
                          Value (Str "in");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                          Value (Str "may have flesh")]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "All models in the unit may have toxin sacs and/or adrenal glands (pg 113) .");
                          App (Lam ("obj",Var "obj"),Value (ParamArray []));
                          Value (Str "in");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                          Value (Str "may have toxin and/or adrenal pg");
                          Value (Int 113)]);
                    Value
                      (ParamArray
                         [Value (Str "Synapse"); Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Shadow in the Warp  (pg 82)");
                          Value (ParamArray [Value (Str "")])])])])
    let ``HORMAGAUNTS`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value
                            (Str
                               "This unit contains 10 Hormagaunts. It can include up to 10 additional Hormagaunts (Power Rating +3)  or up to 20 additional Hormagaunts (Power Rating +6) . Each model is armed with a pair of scything talons.");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                          Value (Str "contains"); Value (Int 10);
                          Value (Str "It include up to"); Value (Int 10);
                          Value (Str "additional Power Rating"); Value (Int 3);
                          Value (Str "or up to"); Value (Int 20);
                          Value (Str "additional Power Rating"); Value (Int 6);
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "is armed with");
                          App
                            (Call Repeat,
                             Value (ParamArray [Value (Str "pair"); Value (Int 1)]));
                          Value (Str "of scything")]);
                    Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour  (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Bounding Leap:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " Whenever this unit piles in and consolidates, it can move up to 6\".");
                                Value (Str "Whenever");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Value (Str "piles in and consolidates it move up to");
                                Value (Distance 6)])]);
                    Value
                      (ParamArray
                         [Value (Str "Hungering Swarm:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " If this unit contains 20 or more models, you can re-roll wound rolls of 1 when it fights.");
                                Value (Str "If");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Value (Str "contains"); Value (Int 20);
                                Value (Str "or more re-roll wound of"); Value (Int 1);
                                Value (Str "when it fights")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "All models in the unit may take toxin sacs and/or adrenal glands (pg 113) .");
                          App (Lam ("obj",Var "obj"),Value (ParamArray []));
                          Value (Str "in");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                          Value (Str "may take toxin and/or adrenal pg");
                          Value (Int 113)])])])
    let ``TERMAGANTS`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value
                            (Str
                               "This unit contains 10 Termagants. It can include up to 10 additional Termagants (Power Rating +3)  or up to 20 additional Termagants (Power Rating +6) . Each model is armed with a fleshborer.");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                          Value (Str "contains"); Value (Int 10);
                          Value (Str "It include up to"); Value (Int 10);
                          Value (Str "additional Power Rating"); Value (Int 3);
                          Value (Str "or up to"); Value (Int 20);
                          Value (Str "additional Power Rating"); Value (Int 6);
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "is armed with");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "fleshborer"); Value (Int 1)]))]);
                    Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour  (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Hail of Living Ammunition:");
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     " If this unit contains 20 or more models, you can re-roll wound rolls of 1 when it shoots.");
                                Value (Str "If");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                                Value (Str "contains"); Value (Int 20);
                                Value (Str "or more re-roll wound of"); Value (Int 1);
                                Value (Str "when it shoots")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Any model may replace its fleshborer with a devourer or spinefists.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "may replace its fleshborer with");
                          App
                            (Call Repeat,
                             Value (ParamArray [Value (Str "devourer"); Value (Int 1)]));
                          Value (Str "or")]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "All models in the unit may have toxin sacs and/or adrenal glands (pg 113) .");
                          App (Lam ("obj",Var "obj"),Value (ParamArray []));
                          Value (Str "in");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                          Value (Str "may have toxin and/or adrenal pg");
                          Value (Int 113)])])])
    let ``THE RED TERROR`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value
                            (Str
                               "The Red Terror is a single model armed with a prehensile pincer tail and two pairs of scything talons. Only one of this model can be included in your army.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour  (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Death From Below:  During deployment, you can set up the Red Terror underground instead of placing it on the battlefield. At the end of any of your Movement phases, the Red Terror can burrow to the surface – set it up anywhere on the battlefield that is more than 9\" away from any enemy models.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Swallow Whole:  If 4 or more of the Red Terror’s scything talons attacks hit the same unit, instead of causing damage normally it can attempt to swallow a victim whole. Roll a D6, and if the result is equal to or higher than the highest Wounds characteristic of the unit, one model from that unit is slain.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Feeding Frenzy:  You can add 1 to hit rolls in the Fight phase for friendly <HIVE FLEET>  RAVENER  units that are within 6\" of this model.");
                          Value (ParamArray [Value (Str "")])])])])
    let ``RAVENERS`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour  (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Death From Below:  During deployment, you can set up a Ravener unit underground instead of placing it on the battlefield. At the end of any of your Movement phases, the Raveners can burrow to the surface – set them up anywhere on the battlefield that is more than 9\" away from any enemy models.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Any model may replace one of its pairs of scything talons with rending claws.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Any model may have spinefists, a devourer or a deathspitter.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "This unit contains 3 Raveners. It can include up to 3 additional Raveners (Power Rating +4)  or up to 6 additional  Raveners (Power Rating +8) . Each model is armed with two pairs of scything talons.");
                          Value (ParamArray [Value (Str "")])])])])
    let ``TRYGON PRIME`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "Shadow in the Warp , Synapse (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Death Throes:  If this model is reduced to 0 wounds, roll a D6 before removing the model from the battlefield; on a 6, it lashes out in its death throes, and each unit within 3\" suffers D3 mortal wounds.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Subterranean Assault:  During deployment, you can set up a Trygon Prime underground instead of placing it on the battlefield. At the same time, you can set up a <HIVE FLEET>  Troops unit in the Trygon Prime’s tunnel. At the end of any of your Movement phases, set up the Trygon Prime anywhere on the battlefield that is more than 9\" away from any enemy models. If there is another unit in the Trygon Prime’s tunnel, set it up at the same time wholly within 3\" of the Trygon Prime and more than 9\" away from any enemy models. Any models that you cannot place in this way are destroyed.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "This model may  replace its biostatic rattle with a prehensile pincer tail or toxinspike.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "This model may  have toxin sacs and/or adrenal glands  (pg 113) .");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "A Trygon Prime is a single model armed with a bio-electric pulse with containment spines, a biostatic rattle and three pairs of massive scything talons.");
                          Value (ParamArray [Value (Str "")])])])])
    let ``TYRANNOCYTE`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour  (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Invasion Organism:  During deployment, you can set up a Tyrannocyte in its hive ship instead of placing it on the battlefield. If you do so, the hive ship can launch the Tyrannocyte at the end of any of your Movement phases – set it up anywhere on the battlefield that is more than 9\" away from any enemy models.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Any models that are inside the Tyrannocyte (see right) must immediately disembark in the same manner as a unit disembarking from a transport, except that they must be set up more than 9\" away from any enemy models. Any models that cannot be set up in this way are destroyed.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Transport Spore:  When you set up a Tyrannocyte in its hive ship, you can also set up a <HIVE FLEET>  INFANTRY  unit of up to 20 models or a <HIVE FLEET>  MONSTER  with a Wounds characteristic of 14 or less inside it (this cannot be another Tyrannocyte or a Sporocyst).");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Death Throes:  If this model is reduced to 0 wounds, roll a dice before removing the model from the battlefield; on a 6, it lashes out in its death throes, and each unit within 3\" suffers D3 mortal wounds.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "This model may replace all of its deathspitters with either five barbed stranglers or five venom cannons.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "A Tyrannocyte is a single model armed with five deathspitters.");
                          Value (ParamArray [Value (Str "")])])])])
    let ``HIVE CRONE`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour  (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Death Throes:  If this model is reduced to 0 wounds, roll a D6 before removing the model from the battlefield; on a 6, it lashes out in its death throes, and each unit within 3\" suffers D3 mortal wounds.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "A Hive Crone is a single model  armed with a drool cannon, tentaclids, scything wings and a wicked spur. It can also fire stinger salvoes.");
                          Value (ParamArray [Value (Str "")])])])])
    let ``HARPY`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour  (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Death Throes:  If this model is reduced to 0 wounds, roll a D6 before removing the model from the battlefield; on a 6, it lashes out in its death throes, and each unit within 3\" suffers D3 mortal wounds.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Sonic Screech:  When a Harpy successfully charges, until the end of the turn enemy units within 1\" cannot be chosen to Fight until all other eligible units have done so.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Spore Mine Cysts:  A Harpy can drop Spore Mines as it flies over enemy units in its Movement phase. To do so, after the Harpy has moved, pick one enemy unit that it flew over and roll a D6 for each model in that unit, up to a maximum of 3 dice. Each time you roll a 4+ a Spore Mine has hit the target and explodes. Roll a D6 to find out how much damage is inflicted on the unit; on a 1 the Spore Mine fails to inflict any harm, on a 2-5 it inflicts 1 mortal wound, and on a 6 it inflicts D3 mortal wounds.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Each time a Spore Mine misses its target, set up a single Spore Mine anywhere within 6\" of the target unit and more than 3\" from any enemy model (if the Spore Mine cannot be placed it is destroyed). This then follows the rules for Spore Mines (pg 99) that are part of your army, but it cannot move or charge during the turn it was set up.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "This model may replace both its stranglethorn cannons with two heavy venom cannons.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "A Harpy is a single model  armed with two stranglethorn cannons and scything wings. It can also fire stinger salvoes.");
                          Value (ParamArray [Value (Str "")])])])])
    let ``SPOROCYST`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value
                            (Str
                               "A Sporocyst is a single model armed with a spore node and five deathspitters.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Bombardment Organism:  During deployment, you can set up a Sporocyst in its hive ship instead of placing it on the battlefield. If you do so, at the beginning of the first battle round but before the first turn begins, the hive ship can launch the Sporocyst – set it up anywhere on the battlefield that is more than 9\" away from any enemy models.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Bio-fortress:  A Sporocyst can shoot with its weapons even if there are enemies within 1\" of it.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Psychic Resonator:  Whilst a Sporocyst is within 12\" of a friendly <HIVE FLEET>  SYNAPSE  unit, it has the SYNAPSE  keyword and the Synapse ability (pg 82).");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Spawn Spore Mines:  At the end of your Movement phase, a Sporocyst can spawn spore mines. If it does so, add a new unit of 3 Spore Mines or 1 Mucolid Spore to your army and set it up on the battlefield so that it is wholly within 6\" of the Sporocyst and more than 1\" from the enemy.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Spore Node:  Each time a spore node attack hits its target, roll a D6 to find out how much damage is inflicted on the unit; on a 1 the mines fail to inflict any harm, on a 2-5 they inflict D3 mortal wounds, and on a 6 they inflict D6 mortal wounds.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Each time a spore node attack misses its target, set up a single Mucolid Spore or a unit of up to 3 Spore Mines, anywhere within 6\" of the target unit and more than 3\" from any enemy model (any models that cannot be placed are destroyed). These then follow the rules for Mucolid Spores or Spore Mines (pg 99) that are part of your army, but they cannot move or charge during the turn they were set up. This weapon cannot be used to fire Overwatch.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Death Throes:  If this model is reduced to 0 wounds, roll a D6 before removing the model from the battlefield; on a 6, it lashes out in its death throes, and each unit within 3\" suffers D3 mortal wounds.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str "Immobile:  A Sporocyst cannot move for any reason.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "This model may replace all of its deathspitters with either five barbed stranglers or five venom cannons.");
                          Value (ParamArray [Value (Str "")])])])])
    let ``SPORE MINES`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour  (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Float Down:  During deployment, you can set up a Spore Mine unit in the upper atmosphere instead of on the battlefield. At the end of any of your Movement phases, it can float down to the battlefield – set it up anywhere that is more than 12\" from any enemy models.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Floating Death:  A Spore Mine explodes if it is within 3\" of any enemy units at the end of any Charge phase. Each time a Spore Mine explodes, roll a D6; on a 1 it fails to inflict any harm, on a 2-5 it inflicts 1 mortal wound on the nearest enemy unit, and on a 6 it inflicts D3 mortal wounds on that unit. The Spore Mine is then destroyed.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Living Bombs:  Spore Mines automatically pass Morale tests. Furthermore, Spore Mines are discounted for the purposes of any victory conditions – their destruction never awards victory points, they do not count towards the number of models controlling an objective, and they do not count when determining if a player has any models left on the battlefield. If you are playing a matched play game, the creation of new Spore Mines by another unit (e.g. from a Sporocyst’s Spore Node ability, a Biovore’s Spore Mine Launcher ability or a Harpy’s Spore Mine Cysts ability) is free, and the Spore Mines’ points cost does not come out of your pool of reinforcement points.");
                          Value (ParamArray [Value (Str "")])])])])
    let ``MUCOLID SPORES`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour  (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Float Down:  During deployment, you can set up a Mucolid Spore unit in the upper atmosphere instead of on the battlefield. At the end of any of your Movement phases, it can float down to the battlefield – set it up anywhere that is more than 12\" from any enemy models.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Floating Death:  A Mucolid Spore explodes if it is within 3\" of any enemy units at the end of any Charge phase. Each time a Mucolid Spore explodes, roll a D6; on a 1 it fails to inflict any harm, on a 2-5 it inflicts D3 mortal wounds on the nearest enemy unit, and on a 6 it inflicts D6 mortal wounds on that unit. The Mucolid Spore is then destroyed.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Living Bombs:  Mucolid Spores automatically pass Morale tests. Furthermore, Mucolid Spores are discounted for the purposes of any victory conditions – their destruction never awards victory points, they do not count towards the number of models controlling an objective, and they do not count when determining if a player has any models left on the battlefield. If you are playing a matched play game, the creation of new Mucolid Spores by another unit (e.g. from a Sporocyst’s Spore Node ability) is free, and the Mucolid Spores’ points cost does not come out of your pool of reinforcement points.");
                          Value (ParamArray [Value (Str "")])])])])
    let ``TYRANNOFEX`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value
                            (Str
                               "A Tyrannofex is a single model armed with acid spray and powerful limbs. It can also fire stinger salvoes.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Bio-tank:  This model does not suffer the penalty to its hit rolls for moving and firing Heavy weapons.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Weapon Beast:  If this model does not move in your Movement phase, it can shoot all of its weapons twice in your Shooting phase.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Death Throes:  If a Tyrannofex is reduced to 0 wounds, roll a dice before removing the model from the battlefield; on a 6 it lashes out in its death throes, and each unit within 3\" suffers D3 mortal wounds.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "This model may replace its acid spray with a fleshborer hive or rupture cannon.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "This model may have toxin sacs and/or adrenal glands (pg 113).");
                          Value (ParamArray [Value (Str "")])])])])
    let ``EXOCRINE`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Symbiotic Targeting:  If this model does not move in its Movement phase, you can add 1 to its hit rolls in the following Shooting phase. If you do so, it cannot charge in the same turn.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Weapon Beast:  If this model does not move in your Movement phase, it can shoot all of its weapons twice in your Shooting phase.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Death Throes:  If this model is reduced to 0 wounds, roll a dice before removing the model from the battlefield; on a 6, it lashes out in its death throes, and each unit within 3\" suffers D3 mortal wounds.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "An Exocrine is a single model armed with a bio-plasmic cannon and powerful limbs.");
                          Value (ParamArray [Value (Str "")])])])])
    let ``TOXICRENE`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value
                            (Str
                               "A Toxicrene is a single model armed with choking spores and massive toxic lashes.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Acid Blood:  Each time this model loses a wound in the Fight phase, roll a D6; on a 6, the unit that inflicted the damage suffers a mortal wound after all of their attacks have been resolved.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Hypertoxic Miasma:  At the end of the Fight phase, roll a D6 for each enemy model within 1\" of any Toxicrenes. On a 6, that model’s unit suffers a mortal wound.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Frenzied Death Throes:  If this model is reduced to 0 wounds, roll a D6 before removing the model from the battlefield; on a 6, it lashes out in its death throes, and each unit within 3\" suffers 3 mortal wounds.");
                          Value (ParamArray [Value (Str "")])])])])
    let ``BIOVORES`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value
                            (Str
                               "This unit contains 1 Biovore. It can include 1 additional Biovore (Power Rating +2)  or 2 additional Biovores (Power Rating +4) . Each model is armed with a spore mine launcher.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour  (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Spore Mine Launcher:  Each time a spore mine launcher hits the target, roll a D6 to find how much damage is inflicted on the target; on a 1 the Spore Mine fails to inflict any harm, on a 2-5 it inflicts 1 mortal wound, and on a 6 it inflicts D3 mortal wounds.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Each time a spore mine launcher misses its target, set up a single Spore Mine model anywhere within 6\" of the target unit and more than 3\" from any enemy model (if the Spore Mine cannot be placed it is destroyed). This then follows the rules for a Spore Mine (pg 99) that is part of your army, but it cannot move or charge during the turn it was set up. This weapon can target units that are not visible to the firer, but it cannot be used to fire Overwatch.");
                          Value (ParamArray [Value (Str "")])])])])
    let ``CARNIFEXES`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value
                            (Str
                               "This unit contains 1 Carnifex. It can include 1 additional Carnifex (Power Rating +6)  or 2 additional Carnifexes (Power Rating +12) . Each model is armed with two pairs of monstrous scything talons.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour  (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Living Battering Ram:  When a Carnifex finishes a charge move, roll a dice; on a 4+ one enemy unit within 1\" suffers a mortal wound. In addition, add 1 to all hit rolls in the Fight phase for a Carnifex that charged in the same turn.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Monstrous Brood:  The first time this unit is set up on the battlefield, all of its models must be placed within 6\" of at least one other model in their unit. From that point onwards, each operates independently and is treated as a separate unit.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Chitin Thorns:  At the end of the Fight phase, roll a D6 for each enemy unit within 1\" of any models with chitin thorns. On a 6, that unit suffers a mortal wound.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Enhanced Senses:  A Carnifex with enhanced senses has a Ballistic Skill characteristic of 3+.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Spore Cysts:  Your opponent must subtract 1 from their hit rolls for ranged attacks that target a Carnifex with spore cysts. This is not cumulative with the penalties to hit rolls incurred from the Shrouding Spores ability (pg 95).");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Tusks:  You can add 1 to the Attacks characteristic of a Carnifex with tusks in the Fight phase if it charged in the preceding Charge phase.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "• Any model may replace one of its pairs of monstrous scything talons with an item from the Monstrous Bio-cannons  list.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "• Any model may replace both of its pairs of monstrous scything talons with two items from the Monstrous Bio-cannons  list.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "• Any model may replace one of its pairs of monstrous scything talons with monstrous crushing claws.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "• Any model may have toxin sacs and/or adrenal glands  (pg 113) .");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "• Any model may have a thresher scythe or a bone mace.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "• Any model may have spine banks or spore cysts.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "• Any model may have chitin thorns.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "• Any model may have one of the following: bio-plasma, enhanced senses, a monstrous acid maw or tusks.");
                          Value (ParamArray [Value (Str "")])])])])
    let ``SCREAMER-KILLERS`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value
                            (Str
                               "This unit contains 1 Screamer-Killer. It can include 1 additional Screamer-Killer (Power Rating +6)  or 2 additional Screamer-Killers (Power Rating +12) . Each model is armed with a bio-plasmic scream and two pairs of monstrous scything talons.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour  (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Living Battering Ram:  When a Screamer-Killer finishes a charge move, roll a dice; on a 4+ one enemy unit within 1\" suffers a mortal wound. In addition, add 1 to all hit rolls in the Fight phase for a Screamer-Killer that charged in the same turn.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Monstrous Brood:  The first time this unit is set up on the battlefield, all of its models must be placed within 6\" of at least one other model in their unit. From that point onwards, each operates independently and is treated as a separate unit.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Spore Cysts:  Your opponent must subtract 1 from their hit rolls for ranged attacks that target a Screamer-Killer with spore cysts. This is not cumulative with the penalties to hit rolls incurred from the Shrouding Spores ability (pg 95).");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Terrifying:  Your opponent must add 1 to any Morale tests for enemy units within 8\" of one or more Screamer-Killers.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Any model may have toxin sacs and/or adrenal glands  (pg 113) .");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Any model may have spore cysts.");
                          Value (ParamArray [Value (Str "")])])])])
    let ``THORNBACKS`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value
                            (Str
                               "This unit contains 1 Thornback. It can include 1 additional Thornback (Power Rating +6)  or 2 additional Thornbacks (Power Rating +12) . Each model is armed with a pair of monstrous scything talons, two devourers with brainleech worms and chitin thorns.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour  (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Thorned Battering Ram:  When a Thornback finishes a charge move, roll a dice; on a 4+ one enemy unit within 1\" suffers a mortal wound. INFANTRY  units instead suffer D3 mortal wounds. In addition, add 1 to all hit rolls in the Fight phase for a Thornback that charged in the same turn.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Monstrous Brood:  The first time this unit is set up on the battlefield, all of its models must be placed within 6\" of at least one other model in their unit. From that point onwards, each operates independently and is treated as a separate unit.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Chitin Thorns:  At the end of the Fight phase, roll a D6 for each enemy unit within 1\" of any models with chitin thorns. On a 6, that unit suffers a mortal wound.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Enhanced Senses:  A Thornback with enhanced senses has a Ballistic Skill characteristic of 3+.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Vicious Hunter:  Enemy INFANTRY  units never gain any bonus to their saving throws for being in cover against attacks made by a Thornback.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Any model may replace its monstrous scything talons with a stranglethorn cannon.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Any model may replace both of its devourers with two deathspitters with slimer maggots.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Any model may have toxin sacs and/or adrenal glands  (pg 113) .");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Any model may have enhanced senses, spine banks and/or a thresher scythe.");
                          Value (ParamArray [Value (Str "")])])])])
    let ``MAWLOC`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour  (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Terror from the Deep:  During deployment, you can set up a Mawloc underground instead of placing it on the battlefield. At the end of any of your Movement phases, set up the Mawloc anywhere on the battlefield that is more than 1\" away from any enemy models and more than 6\" from any other Mawlocs set up this way this turn, then roll a D6 for each enemy unit within 2\" of it; on a 1 the unit escapes unharmed, on a 2-3 it suffers 1 mortal wound, on a 4-5 it suffers D3 mortal wounds and on a 6 it suffers 3 mortal wounds. The Mawloc cannot charge in the same turn.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Burrow:  At the beginning of any of your Movement phases, any Mawloc that is not within 1\" of an enemy unit can burrow. Remove it from the battlefield – it can return as described in the Terror from the Deep ability. A Mawloc may not burrow and return to the battlefield in the same turn. If the battle ends while the Mawloc is underground, it is considered to be slain.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Death Throes:  If this model is reduced to 0 wounds, roll a D6 before removing the model from the battlefield; on a 6, it lashes out in its death throes, and each unit within 3\" suffers D3 mortal wounds.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "This model may replace its prehensile pincer tail with a biostatic rattle or a toxinspike.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "This model may have toxin sacs and/or adrenal glands  (pg 113) .");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "A Mawloc is a single model armed with distensible jaws, a prehensile pincer tail and three pairs of scything talons.");
                          Value (ParamArray [Value (Str "")])])])])
    let ``TRYGON`` = 
        Value
          (ParamArray
             [Value (Str "ABILITIES");
              Value
                (ParamArray
                   [Value
                      (ParamArray
                         [Value (Str "Instinctive Behaviour (pg 82)");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Death Throes:  If this model is reduced to 0 wounds, roll a D6 before removing the model from the battlefield; on a 6, it lashes out in its death throes, and each unit within 3\" suffers D3 mortal wounds.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "Subterranean Assault:  During deployment, you can set up a Trygon underground instead of placing it on the battlefield. At the same time, you can set up a <HIVE FLEET>  Troops unit in the Trygon’s tunnel. At the end of any of your Movement phases, set up the Trygon anywhere on the battlefield that is more than 9\" away from any enemy models. If there is another unit in the Trygon’s tunnel, set it up at the same time wholly within 3\" of the Trygon and more than 9\" away from any enemy models. Any models that you cannot place in this way are destroyed.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "This model may replace its toxinspike with a biostatic rattle or a prehensile pincer tail.");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "This model may have toxin sacs and/or adrenal glands (pg 113).");
                          Value (ParamArray [Value (Str "")])]);
                    Value
                      (ParamArray
                         [Value
                            (Str
                               "A Trygon is a single  model armed with a bio-electric pulse, three pairs of massive scything talons and a toxinspike.");
                          Value (ParamArray [Value (Str "")])])])])
