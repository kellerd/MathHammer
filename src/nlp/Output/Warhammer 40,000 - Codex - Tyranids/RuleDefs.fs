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
          (ParamArray
             [Value
                (Str
                   "Throughout this section you will come across a keyword that is within angular brackets, specifically <HIVE FLEET> . This is shorthand for a keyword of your own choosing, as described below.");
              Value (Str "Throughout");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "section"); Lam ("obj",Var "obj")]));
              Value (Str "will come across");
              App
                (Call Repeat,Value (ParamArray [Value (Str "keyword"); Value (Int 1)]));
              Value (Str "that is within angular specifically <HIVE FLEET>");
              App (Lam ("obj",Var "obj"),Value (ParamArray []));
              Value (Str "is shorthand for");
              App
                (Call Repeat,Value (ParamArray [Value (Str "keyword"); Value (Int 1)]));
              Value (Str "of your own choosing as described below")])
    let ``<HIVE FLEET>`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "All Tyranids belong to a hive fleet. When you include a Tyranids unit in your army, you must nominate which hive fleet that unit is from. There are many different hive fleets to choose from; you can use any of the hive fleets described in this book, or make up your own if you prefer. You then simply replace the <HIVE FLEET> keyword in every instance on that unit’s datasheet, and in any psychic powers they know, with the name of your chosen hive fleet. For example, if you were to include a Tervigon in your army, and you decided it was from Hive Fleet Kraken, then its <HIVE FLEET> keyword is changed to KRAKEN , and its ‘Brood Progenitor’ ability would say ‘You can re-roll hit rolls of 1 in the Shooting phase for friendly KRAKEN Termagant units within 6\" of this model.’");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "Tyranids"); Lam ("obj",Var "obj")]));
              Value (Str "belong to");
              App
                (Value (Int 1),
                 Value (ParamArray [Value (Str "hive"); Value (Str "fleet")]));
              Value (Str "When include");
              App
                (Value (Int 1),Value (ParamArray [Value (Str "Tyranids"); Var "Target"]));
              Value (Str "in your army must nominate which hive fleet");
              Let
                ("ThatSubject",
                 Value
                   (ParamArray
                      [Var "Target";
                       Value (Str "There are many different hive to choose from use");
                       App (Lam ("obj",Var "obj"),Value (ParamArray []));
                       Value (Str "of");
                       App
                         (Call Repeat,
                          Value (ParamArray [Value (Str "hive"); Lam ("obj",Var "obj")]));
                       Value (Str "described in");
                       App
                         (Call Repeat,
                          Value (ParamArray [Value (Str "book"); Lam ("obj",Var "obj")]));
                       Value (Str "or make up your own if prefer then simply replace");
                       App
                         (Call Repeat,
                          Value
                            (ParamArray
                               [Value (Str "<HIVE FLEET>"); Lam ("obj",Var "obj")]));
                       Value (Str "keyword in");
                       App
                         (Call Repeat,
                          Value
                            (ParamArray [Value (Str "instance"); Lam ("obj",Var "obj")]));
                       Value (Str "on");
                       App
                         (Lam ("obj",Var "obj"),
                          Value (ParamArray [Var "Target"; Value (Str "'s")]));
                       Value (Str "datasheet and in");
                       App
                         (Call Repeat,
                          Value
                            (ParamArray [Value (Str "psychic"); Lam ("obj",Var "obj")]));
                       Value (Str "they know with");
                       App
                         (Call Repeat,
                          Value (ParamArray [Value (Str "name"); Lam ("obj",Var "obj")]));
                       Value
                         (Str "of your chosen hive fleet For example if were to include");
                       App
                         (Call Repeat,
                          Value (ParamArray [Value (Str "Tervigon"); Value (Int 1)]));
                       Value
                         (Str
                            "in your army and decided it was from Hive Fleet Kraken then its <HIVE FLEET> keyword is changed to KRAKEN and its Brood Progenitor");
                       Value (Distance 0); Value (Str "ability would say");
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
                                                 (ParamArray [Var "x"; Value (Int 1)])),
                                            Var "x",
                                            Some (App (Var "f",Var "rollTarget"))))));
                                Value (Distance 0)]),
                          Value
                            (ParamArray
                               [Value (Str "in");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value
                                     (ParamArray
                                        [Value (Str "Shooting"); Value (Str "phase")]));
                                Value (Str "for friendly KRAKEN Termagant within");
                                Value (Distance 6); Value (Str "of");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "model"); Lam ("obj",Var "obj")]));
                                Value (Distance 0)]))]),
                 Value
                   (ParamArray
                      [Let
                         ("ThatObject",
                          Value
                            (ParamArray
                               [Value
                                  (Str
                                     "from There are many different hive to choose from use");
                                App (Lam ("obj",Var "obj"),Value (ParamArray []));
                                Value (Str "of");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "hive"); Lam ("obj",Var "obj")]));
                                Value (Str "described in");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "book"); Lam ("obj",Var "obj")]));
                                Value
                                  (Str
                                     "or make up your own if prefer then simply replace");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "<HIVE FLEET>");
                                         Lam ("obj",Var "obj")]));
                                Value (Str "keyword in");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "instance"); Lam ("obj",Var "obj")]));
                                Value (Str "on");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value (ParamArray [Var "Target"; Value (Str "'s")]));
                                Value (Str "datasheet and in");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "psychic"); Lam ("obj",Var "obj")]));
                                Value (Str "they know with");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "name"); Lam ("obj",Var "obj")]));
                                Value
                                  (Str
                                     "of your chosen hive fleet For example if were to include");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Value (Str "Tervigon"); Value (Int 1)]));
                                Value
                                  (Str
                                     "in your army and decided it was from Hive Fleet Kraken then its <HIVE FLEET> keyword is changed to KRAKEN and its Brood Progenitor");
                                Value (Distance 0); Value (Str "ability would say");
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
                                                       (App (Var "f",Var "rollTarget"))))));
                                         Value (Distance 0)]),
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
                                           (Str "for friendly KRAKEN Termagant within");
                                         Value (Distance 6); Value (Str "of");
                                         App
                                           (Call Repeat,
                                            Value
                                              (ParamArray
                                                 [Value (Str "model");
                                                  Lam ("obj",Var "obj")]));
                                         Value (Distance 0)]))]),
                          Value
                            (ParamArray
                               [App
                                  (Value (Str "is"),
                                   Value
                                     (ParamArray [Var "ThatSubject"; Var "ThatObject"]));
                                Value
                                  (Str
                                     "There are many different hive to choose from use");
                                App (Lam ("obj",Var "obj"),Value (ParamArray []));
                                Value (Str "of");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "hive"); Lam ("obj",Var "obj")]));
                                Value (Str "described in");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "book"); Lam ("obj",Var "obj")]));
                                Value
                                  (Str
                                     "or make up your own if prefer then simply replace");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "<HIVE FLEET>");
                                         Lam ("obj",Var "obj")]));
                                Value (Str "keyword in");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "instance"); Lam ("obj",Var "obj")]));
                                Value (Str "on");
                                App
                                  (Lam ("obj",Var "obj"),
                                   Value (ParamArray [Var "Target"; Value (Str "'s")]));
                                Value (Str "datasheet and in");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "psychic"); Lam ("obj",Var "obj")]));
                                Value (Str "they know with");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "name"); Lam ("obj",Var "obj")]));
                                Value
                                  (Str
                                     "of your chosen hive fleet For example if were to include");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Value (Str "Tervigon"); Value (Int 1)]));
                                Value
                                  (Str
                                     "in your army and decided it was from Hive Fleet Kraken then its <HIVE FLEET> keyword is changed to KRAKEN and its Brood Progenitor");
                                Value (Distance 0); Value (Str "ability would say");
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
                                                       (App (Var "f",Var "rollTarget"))))));
                                         Value (Distance 0)]),
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
                                           (Str "for friendly KRAKEN Termagant within");
                                         Value (Distance 6); Value (Str "of");
                                         App
                                           (Call Repeat,
                                            Value
                                              (ParamArray
                                                 [Value (Str "model");
                                                  Lam ("obj",Var "obj")]));
                                         Value (Distance 0)]))]))]))])
    let ``SYNAPSE`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "<HIVE FLEET> units automatically pass Morale tests if they are within 12\" of any friendly <HIVE FLEET> units with this ability.");
              Value
                (Str "<HIVE FLEET> automatically pass Morale tests if they are within");
              Value (Distance 12); Value (Str "of");
              App
                (Lam ("obj",Var "obj"),
                 Value (ParamArray [Value (Str "friendly"); Value (Str "<HIVE FLEET>")]));
              Value (Str "with");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "ability"); Lam ("obj",Var "obj")]))])
    let ``INSTINCTIVE BEHAVIOUR`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "Unless a <HIVE FLEET> unit with this ability is within 24\" of any friendly <HIVE FLEET> SYNAPSE unit, you must subtract 1 from any hit rolls made for it when shooting any target other than the nearest visible enemy unit, and you must subtract 2 from its charge roll if it declares a charge against any unit other than the nearest enemy unit.");
              Value (Str "Unless");
              App
                (Value (Int 1),
                 Value (ParamArray [Value (Str "<HIVE FLEET>"); Var "Target"]));
              Value (Str "with");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "ability"); Lam ("obj",Var "obj")]));
              Value (Str "is within"); Value (Distance 24); Value (Str "of");
              App
                (Lam ("obj",Var "obj"),
                 Value
                   (ParamArray
                      [Value (Str "friendly"); Value (Str "<HIVE FLEET>");
                       Value (Str "SYNAPSE"); Var "Target"]));
              Value (Str "must subtract"); Value (Int 1); Value (Str "from");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "Hit Roll"); Lam ("obj",Var "obj")]));
              Value (Str "made for it when shooting");
              App
                (Lam ("obj",Var "obj"),
                 Value (ParamArray [Value (Str "target"); Value (Str "other")]));
              Value (Str "than"); Lam ("obj",Var "obj"); Value (Str "nearest visible");
              Var "Target"; Var "Target"; Value (Str "and must subtract"); Value (Int 2);
              Value (Str "from its charge roll if it declares");
              App (Call Repeat,Value (ParamArray [Value (Str "charge"); Value (Int 1)]));
              Value (Str "against");
              App
                (Lam ("obj",Var "obj"),
                 Value (ParamArray [Var "Target"; Value (Str "other")]));
              Value (Str "than");
              App
                (Lam ("obj",Var "obj"),
                 Value (ParamArray [Value (Str "nearest"); Var "Target"; Var "Target"]))])
    let ``SHADOW IN THE WARP`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "Enemy PSYKERS must subtract 1 from any Psychic tests they make if they are within 18\" of any units with this ability. TYRANID PSYKERS are not affected.");
              Value (Str "Enemy must subtract"); Value (Int 1); Value (Str "from");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "Psychic"); Lam ("obj",Var "obj")]));
              Value (Str "they make if they are within"); Value (Distance 18);
              Value (Str "of"); App (Lam ("obj",Var "obj"),Value (ParamArray []));
              Value (Str "with");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "ability"); Lam ("obj",Var "obj")]));
              Value (Str "TYRANID PSYKERS are not affected")])
    let _ = 
        Value
          (ParamArray
             [Value (Str "TYRANIDS WARGEAR LISTS"); Value (Str "TYRANIDS WARGEAR LISTS")])
    let ``BASIC BIO-WEAPONS`` = 
        Value
          (ParamArray
             [Value (Str "Scything talons Spinefists Deathspitter");
              Value (Str "Scything talons Spinefists Deathspitter")])
    let ``BASIC BIO-CANNONS`` = 
        Value
          (ParamArray
             [Value (Str "Barbed strangler Venom cannon");
              Value (Str "Barbed strangler Venom cannon")])
    let ``MELEE BIO-WEAPONS`` = 
        Value
          (ParamArray
             [Value (Str "Rending claws Boneswords Lash whip and bonesword");
              Value (Str "Rending claws Boneswords Lash whip and bonesword")])
    let ``MONSTROUS BIO-WEAPONS`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "Monstrous rending claws Monstrous boneswords Lash whip and monstrous bonesword");
              Value
                (Str "rending Monstrous boneswords Lash whip and monstrous bonesword")])
    let ``MONSTROUS BIO-CANNONS`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "Two deathspitters with slimer maggots Two devourers with brainleech worms Stranglethorn cannon* Heavy venom cannon* *A model cannot be armed with more than one of these weapons.");
              Value
                (Str
                   "Two with slimer Two with brainleech Stranglethorn cannon * Heavy venom cannon * *");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
              Value (Str "not be armed with more than one of");
              App (Lam ("obj",Var "obj"),Value (ParamArray []))])
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
                      (ParamArray
                         [Value
                            (Str
                               "Each time the bearer fights, it can make one (and only one) attack with this weapon. This is in addition to the bearer’s attacks. If a unit suffers any unsaved wounds from this weapon, add 1 to any Morale tests they take until the end of the turn.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "time"); Lam ("obj",Var "obj")]));
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "bearer"); Lam ("obj",Var "obj")]));
                          Value (Str "fights it make one and only one attack with");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          App (Lam ("obj",Var "obj"),Value (ParamArray []));
                          Value (Str "is in addition to");
                          App
                            (Lam ("obj",Var "obj"),
                             Value (ParamArray [Value (Str "bearer"); Value (Str "'s")]));
                          Value (Str "If");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Value (Int 1)]));
                          Call Suffer;
                          App
                            (Call Repeat,
                             Value
                               (ParamArray
                                  [Value (Str "unsaved"); Lam ("obj",Var "obj")]));
                          Value (Str "from");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          Value (Str "add"); Value (Int 1); Value (Str "to");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "Morale"); Lam ("obj",Var "obj")]));
                          Value (Str "they take until");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
                          Value (Str "of");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "turn"); Lam ("obj",Var "obj")]))])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "If a unit suffers any unsaved wounds from this weapon, your opponent must subtract 1 from hit rolls for that unit until the end of the turn.");
                          Value (Str "If");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Value (Int 1)]));
                          Call Suffer;
                          App
                            (Call Repeat,
                             Value
                               (ParamArray
                                  [Value (Str "unsaved"); Lam ("obj",Var "obj")]));
                          Value (Str "from");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          Value (Str "your opponent must subtract"); Value (Int 1);
                          Value (Str "from Hit Roll for");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
                          Value (Str "until");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
                          Value (Str "of");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "turn"); Lam ("obj",Var "obj")]))])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "Each time the bearer fights, it can make one (and only one) attack with this weapon. This is in addition to the bearer’s attacks.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "time"); Lam ("obj",Var "obj")]));
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "bearer"); Lam ("obj",Var "obj")]));
                          Value (Str "fights it make one and only one attack with");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          App (Lam ("obj",Var "obj"),Value (ParamArray []));
                          Value (Str "is in addition to");
                          App
                            (Lam ("obj",Var "obj"),
                             Value (ParamArray [Value (Str "bearer"); Value (Str "'s")]))])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "Each time you make a wound roll of 6+ for this weapon, the target unit suffers a mortal wound in addition to any other damage.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "time"); Lam ("obj",Var "obj")]));
                          Value (Str "make");
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray [Value (Str "wound"); Value (Str "roll")]));
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
                                      Value (ParamArray [Var "roll"; Value (Int 6)])),
                                   App (Call Or,Value (ParamArray [Var "eq"; Var "gt"])))));
                          Value (Str "for");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          App
                            (Lam ("obj",Var "obj"),
                             Value (ParamArray [Value (Str "target"); Var "Target"]));
                          Call Suffer;
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "Mortal Wound"); Value (Int 1)]));
                          Value (Str "in addition to");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray [Value (Str "other"); Value (Str "damage")]))])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "A model armed with boneswords can make 1 additional attack with them in the Fight phase.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "armed with make"); Value (Int 1);
                          Value (Str "additional attack with them in");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray
                                  [Value
                                     (ParamArray
                                        [Value (Str "Phase"); Value (Str "Fight")]);
                                   Lam ("obj",Var "obj")]))])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "When attacking with this weapon, you must subtract 1 from the hit roll.");
                          Value (Str "When attacking with");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          Value (Str "must subtract"); Value (Int 1); Value (Str "from");
                          App
                            (Lam ("obj",Var "obj"),
                             Value (ParamArray [Value (Str "hit"); Value (Str "roll")]))])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "Each time the bearer fights, one (and only one) of its attacks must be made with this weapon.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "time"); Lam ("obj",Var "obj")]));
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "bearer"); Lam ("obj",Var "obj")]));
                          Value (Str "fights one and only one of its must be made with");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]))])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "If the bearer is slain in the Fight phase before it has made its attacks, leave it where it is. When its unit is chosen to fight in that phase, the bearer can do so as normal before being removed from the battlefield.");
                          Value (Str "If");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "bearer"); Lam ("obj",Var "obj")]));
                          Value (Str "is slain in");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray
                                  [Value
                                     (ParamArray
                                        [Value (Str "Phase"); Value (Str "Fight")]);
                                   Lam ("obj",Var "obj")]));
                          Value
                            (Str "before it has made its leave it where it is When its");
                          Var "Target"; Value (Str "is chosen to fight in");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "phase"); Lam ("obj",Var "obj")]));
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "bearer"); Lam ("obj",Var "obj")]));
                          Value (Str "do so as normal before being removed from");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray
                                  [Value (Str "battlefield"); Lam ("obj",Var "obj")]))])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "If the bearer is slain in the Fight phase before it has made its attacks, leave it where it is. When its unit is chosen to fight in that phase, the bearer can do so as normal before being removed from the battlefield.");
                          Value (Str "If");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "bearer"); Lam ("obj",Var "obj")]));
                          Value (Str "is slain in");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray
                                  [Value
                                     (ParamArray
                                        [Value (Str "Phase"); Value (Str "Fight")]);
                                   Lam ("obj",Var "obj")]));
                          Value
                            (Str "before it has made its leave it where it is When its");
                          Var "Target"; Value (Str "is chosen to fight in");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "phase"); Lam ("obj",Var "obj")]));
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "bearer"); Lam ("obj",Var "obj")]));
                          Value (Str "do so as normal before being removed from");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray
                                  [Value (Str "battlefield"); Lam ("obj",Var "obj")]))])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "When attacking with this weapon, you must subtract 1 from the hit roll.");
                          Value (Str "When attacking with");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          Value (Str "must subtract"); Value (Int 1); Value (Str "from");
                          App
                            (Lam ("obj",Var "obj"),
                             Value (ParamArray [Value (Str "hit"); Value (Str "roll")]))])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "You can re-roll hit rolls of 1 for this weapon. If the bearer has more than one pair of monstrous/massive scything talons, it can make 1 additional attack with this weapon each time it fights.");
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
                                                    (ParamArray [Var "x"; Value (Int 1)])),
                                               Var "x",
                                               Some (App (Var "f",Var "rollTarget"))))));
                                   Value (Str "If");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "bearer"); Lam ("obj",Var "obj")]));
                                   Value
                                     (Str
                                        "has more than one pair of monstrous/massive scything it make");
                                   Value (Int 1); Value (Str "additional attack with");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                                   Lam
                                     ("obj",
                                      App (Call Count,Value (ParamArray [Var "obj"])));
                                   Value (Str "time it fights")]),
                             Value
                               (ParamArray
                                  [Value (Str "for");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                                   Value (Str "If");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "bearer"); Lam ("obj",Var "obj")]));
                                   Value
                                     (Str
                                        "has more than one pair of monstrous/massive scything it make");
                                   Value (Int 1); Value (Str "additional attack with");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                                   Lam
                                     ("obj",
                                      App (Call Count,Value (ParamArray [Var "obj"])));
                                   Value (Str "time it fights")]))])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "You can re-roll failed wound rolls for this weapon. A model armed with this weapon always fights first in the Fight phase, even if it didn’t charge. If the enemy has units that have charged, or that have a similar ability, then alternate choosing units to fight with, starting with the player whose turn is taking place.");
                          Let
                            ("Wound Roll",
                             Value
                               (ParamArray
                                  [Lam
                                     ("rollTarget",
                                      Let
                                        ("f",Var "Wound Roll",
                                         Let
                                           ("x",App (Var "f",Var "rollTarget"),
                                            IfThenElse
                                              (Var "x",Var "x",
                                               Some (App (Var "f",Var "rollTarget"))))));
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "model"); Lam ("obj",Var "obj")]));
                                   Value (Str "armed with");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                                   Value (Str "always fights first in");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value
                                              (ParamArray
                                                 [Value (Str "Phase");
                                                  Value (Str "Fight")]);
                                            Lam ("obj",Var "obj")]));
                                   Value (Str "even if it did n't charge If");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Var "Target"; Lam ("obj",Var "obj")]));
                                   Value (Str "has that have charged or that have");
                                   App
                                     (Value (Int 1),
                                      Value
                                        (ParamArray
                                           [Value (Str "similar"); Value (Str "ability")]));
                                   Value
                                     (Str
                                        "then alternate choosing to fight with starting with");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "player"); Lam ("obj",Var "obj")]));
                                   Value (Str "whose turn is taking place")]),
                             Value
                               (ParamArray
                                  [App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "model"); Lam ("obj",Var "obj")]));
                                   Value (Str "armed with");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                                   Value (Str "always fights first in");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value
                                              (ParamArray
                                                 [Value (Str "Phase");
                                                  Value (Str "Fight")]);
                                            Lam ("obj",Var "obj")]));
                                   Value (Str "even if it did n't charge If");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Var "Target"; Lam ("obj",Var "obj")]));
                                   Value (Str "has that have charged or that have");
                                   App
                                     (Value (Int 1),
                                      Value
                                        (ParamArray
                                           [Value (Str "similar"); Value (Str "ability")]));
                                   Value
                                     (Str
                                        "then alternate choosing to fight with starting with");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "player"); Lam ("obj",Var "obj")]));
                                   Value (Str "whose turn is taking place")]))])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "A model armed with monstrous boneswords can make 1 additional attack with them in the Fight phase.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "model"); Lam ("obj",Var "obj")]));
                          Value (Str "armed with monstrous make"); Value (Int 1);
                          Value (Str "additional attack with them in");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray
                                  [Value
                                     (ParamArray
                                        [Value (Str "Phase"); Value (Str "Fight")]);
                                   Lam ("obj",Var "obj")]))])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "When attacking with this weapon, you must subtract 1 from the hit roll.");
                          Value (Str "When attacking with");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          Value (Str "must subtract"); Value (Int 1); Value (Str "from");
                          App
                            (Lam ("obj",Var "obj"),
                             Value (ParamArray [Value (Str "hit"); Value (Str "roll")]))])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "You can re-roll failed wound rolls for this weapon. In addition, each time you make a wound roll of 6+, that hit is resolved with an AP of -6 and Damage of 3.");
                          Let
                            ("Wound Roll",
                             Value
                               (ParamArray
                                  [Lam
                                     ("rollTarget",
                                      Let
                                        ("f",Var "Wound Roll",
                                         Let
                                           ("x",App (Var "f",Var "rollTarget"),
                                            IfThenElse
                                              (Var "x",Var "x",
                                               Some (App (Var "f",Var "rollTarget"))))));
                                   Value (Str "In addition");
                                   Lam
                                     ("obj",
                                      App (Call Count,Value (ParamArray [Var "obj"])));
                                   Value (Str "time make");
                                   App
                                     (Value (Int 1),
                                      Value
                                        (ParamArray
                                           [Value (Str "wound"); Value (Str "roll")]));
                                   Value (Str "of");
                                   Lam
                                     ("roll",
                                      Let
                                        ("gt",
                                         App
                                           (Call GreaterThan,
                                            Value
                                              (ParamArray [Var "roll"; Value (Int 6)])),
                                         Let
                                           ("eq",
                                            App
                                              (Call Equals,
                                               Value
                                                 (ParamArray [Var "roll"; Value (Int 6)])),
                                            App
                                              (Call Or,
                                               Value (ParamArray [Var "eq"; Var "gt"])))));
                                   Value (Str "that hit is resolved with");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "AP"); Lam ("obj",Var "obj")]));
                                   Value (Str "of"); Value (Int -6);
                                   Value (Str "and Damage of"); Value (Int 3)]),
                             Value
                               (ParamArray
                                  [Value (Str "In addition");
                                   Lam
                                     ("obj",
                                      App (Call Count,Value (ParamArray [Var "obj"])));
                                   Value (Str "time make");
                                   App
                                     (Value (Int 1),
                                      Value
                                        (ParamArray
                                           [Value (Str "wound"); Value (Str "roll")]));
                                   Value (Str "of");
                                   Lam
                                     ("roll",
                                      Let
                                        ("gt",
                                         App
                                           (Call GreaterThan,
                                            Value
                                              (ParamArray [Var "roll"; Value (Int 6)])),
                                         Let
                                           ("eq",
                                            App
                                              (Call Equals,
                                               Value
                                                 (ParamArray [Var "roll"; Value (Int 6)])),
                                            App
                                              (Call Or,
                                               Value (ParamArray [Var "eq"; Var "gt"])))));
                                   Value (Str "that hit is resolved with");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "AP"); Lam ("obj",Var "obj")]));
                                   Value (Str "of"); Value (Int -6);
                                   Value (Str "and Damage of"); Value (Int 3)]))])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "You can re-roll hit rolls of 1 for this weapon. If the bearer has more than one pair of monstrous/massive scything talons, it can make 1 additional attack with this weapon each time it fights.");
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
                                                    (ParamArray [Var "x"; Value (Int 1)])),
                                               Var "x",
                                               Some (App (Var "f",Var "rollTarget"))))));
                                   Value (Str "If");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "bearer"); Lam ("obj",Var "obj")]));
                                   Value
                                     (Str
                                        "has more than one pair of monstrous/massive scything it make");
                                   Value (Int 1); Value (Str "additional attack with");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                                   Lam
                                     ("obj",
                                      App (Call Count,Value (ParamArray [Var "obj"])));
                                   Value (Str "time it fights")]),
                             Value
                               (ParamArray
                                  [Value (Str "for");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                                   Value (Str "If");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "bearer"); Lam ("obj",Var "obj")]));
                                   Value
                                     (Str
                                        "has more than one pair of monstrous/massive scything it make");
                                   Value (Int 1); Value (Str "additional attack with");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                                   Lam
                                     ("obj",
                                      App (Call Count,Value (ParamArray [Var "obj"])));
                                   Value (Str "time it fights")]))])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "Each time the bearer fights, it can make one (and only one) attack with this weapon. This is in addition to the bearer’s attacks.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "time"); Lam ("obj",Var "obj")]));
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "bearer"); Lam ("obj",Var "obj")]));
                          Value (Str "fights it make one and only one attack with");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          App (Lam ("obj",Var "obj"),Value (ParamArray []));
                          Value (Str "is in addition to");
                          App
                            (Lam ("obj",Var "obj"),
                             Value (ParamArray [Value (Str "bearer"); Value (Str "'s")]))])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "Make D3 hit rolls for each attack made with this weapon, instead of 1.");
                          Lam
                            ("next",
                             Let
                               ("Hit Rolls",
                                App
                                  (Call Product,
                                   Value
                                     (ParamArray
                                        [Var "A"; App (Call Dice,Value (Int 3))])),
                                Var "next"))])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "Each time you make a wound roll of 6+ for this weapon, that hit is resolved with an AP of -4.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "time"); Lam ("obj",Var "obj")]));
                          Value (Str "make");
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray [Value (Str "wound"); Value (Str "roll")]));
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
                                      Value (ParamArray [Var "roll"; Value (Int 6)])),
                                   App (Call Or,Value (ParamArray [Var "eq"; Var "gt"])))));
                          Value (Str "for");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          Value (Str "that hit is resolved with");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "AP"); Lam ("obj",Var "obj")]));
                          Value (Str "of"); Value (Int -4)])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "You can re-roll hit rolls of 1 for this weapon. If the bearer has more than one pair of scything talons, it can make 1 additional attack with this weapon each time it fights.");
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
                                                    (ParamArray [Var "x"; Value (Int 1)])),
                                               Var "x",
                                               Some (App (Var "f",Var "rollTarget"))))));
                                   Value (Str "If");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "bearer"); Lam ("obj",Var "obj")]));
                                   Value
                                     (Str "has more than one pair of scything it make");
                                   Value (Int 1); Value (Str "additional attack with");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                                   Lam
                                     ("obj",
                                      App (Call Count,Value (ParamArray [Var "obj"])));
                                   Value (Str "time it fights")]),
                             Value
                               (ParamArray
                                  [Value (Str "for");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                                   Value (Str "If");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "bearer"); Lam ("obj",Var "obj")]));
                                   Value
                                     (Str "has more than one pair of scything it make");
                                   Value (Int 1); Value (Str "additional attack with");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                                   Lam
                                     ("obj",
                                      App (Call Count,Value (ParamArray [Var "obj"])));
                                   Value (Str "time it fights")]))])])])
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
                    Value
                      (ParamArray
                         [Value (Str "You can re-roll hit rolls of 1 for this weapon.");
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
                                                    (ParamArray [Var "x"; Value (Int 1)])),
                                               Var "x",
                                               Some (App (Var "f",Var "rollTarget"))))))]),
                             Value
                               (ParamArray
                                  [Value (Str "for");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "weapon"); Lam ("obj",Var "obj")]))]))])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "Each time the bearer fights, it can make one (and only one) attack with this weapon. Make D3 hit rolls for this attack instead of one. This is in addition to the bearer’s attacks.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "time"); Lam ("obj",Var "obj")]));
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "bearer"); Lam ("obj",Var "obj")]));
                          Value (Str "fights it make one and only one attack with");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          Lam
                            ("next",
                             Let
                               ("Hit Rolls",
                                App
                                  (Call Product,
                                   Value
                                     (ParamArray
                                        [Var "A";
                                         Value
                                           (ParamArray
                                              [App (Call Dice,Value (Int 3));
                                               App
                                                 (Lam ("obj",Var "obj"),
                                                  Value (ParamArray []));
                                               Value (Str "is in addition to");
                                               App
                                                 (Lam ("obj",Var "obj"),
                                                  Value
                                                    (ParamArray
                                                       [Value (Str "bearer");
                                                        Value (Str "'s")]))])])),
                                Var "next"))])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "You can re-roll failed wound rolls for this weapon. A model armed with this weapon always fights first in the Fight phase, even if it didn’t charge. If the enemy has units that have charged, or that have a similar ability, then alternate choosing units to fight with, starting with the player whose turn is taking place.");
                          Let
                            ("Wound Roll",
                             Value
                               (ParamArray
                                  [Lam
                                     ("rollTarget",
                                      Let
                                        ("f",Var "Wound Roll",
                                         Let
                                           ("x",App (Var "f",Var "rollTarget"),
                                            IfThenElse
                                              (Var "x",Var "x",
                                               Some (App (Var "f",Var "rollTarget"))))));
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "model"); Lam ("obj",Var "obj")]));
                                   Value (Str "armed with");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                                   Value (Str "always fights first in");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value
                                              (ParamArray
                                                 [Value (Str "Phase");
                                                  Value (Str "Fight")]);
                                            Lam ("obj",Var "obj")]));
                                   Value (Str "even if it did n't charge If");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Var "Target"; Lam ("obj",Var "obj")]));
                                   Value (Str "has that have charged or that have");
                                   App
                                     (Value (Int 1),
                                      Value
                                        (ParamArray
                                           [Value (Str "similar"); Value (Str "ability")]));
                                   Value
                                     (Str
                                        "then alternate choosing to fight with starting with");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "player"); Lam ("obj",Var "obj")]));
                                   Value (Str "whose turn is taking place")]),
                             Value
                               (ParamArray
                                  [App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "model"); Lam ("obj",Var "obj")]));
                                   Value (Str "armed with");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                                   Value (Str "always fights first in");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value
                                              (ParamArray
                                                 [Value (Str "Phase");
                                                  Value (Str "Fight")]);
                                            Lam ("obj",Var "obj")]));
                                   Value (Str "even if it did n't charge If");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Var "Target"; Lam ("obj",Var "obj")]));
                                   Value (Str "has that have charged or that have");
                                   App
                                     (Value (Int 1),
                                      Value
                                        (ParamArray
                                           [Value (Str "similar"); Value (Str "ability")]));
                                   Value
                                     (Str
                                        "then alternate choosing to fight with starting with");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "player"); Lam ("obj",Var "obj")]));
                                   Value (Str "whose turn is taking place")]))])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "Each time the bearer fights, it can make one (and only one) attack with this weapon. This is in addition to the bearer’s attacks. This weapon always wounds targets (other than VEHICLES) on a 2+.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "time"); Lam ("obj",Var "obj")]));
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "bearer"); Lam ("obj",Var "obj")]));
                          Value (Str "fights it make one and only one attack with");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          App (Lam ("obj",Var "obj"),Value (ParamArray []));
                          Value (Str "is in addition to");
                          App
                            (Lam ("obj",Var "obj"),
                             Value (ParamArray [Value (Str "bearer"); Value (Str "'s")]));
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          Value (Str "always wounds other than VEHICLES on");
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
                                   Value (Int 1)]))])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "Each time the bearer fights, it can make one (and only one) attack with this weapon. This is in addition to the bearer’s attacks.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "time"); Lam ("obj",Var "obj")]));
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "bearer"); Lam ("obj",Var "obj")]));
                          Value (Str "fights it make one and only one attack with");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          App (Lam ("obj",Var "obj"),Value (ParamArray []));
                          Value (Str "is in addition to");
                          App
                            (Lam ("obj",Var "obj"),
                             Value (ParamArray [Value (Str "bearer"); Value (Str "'s")]))])])])
    let _ = 
        Value (ParamArray [Value (Str "Spinefist"); Value (Str "Spinefist")])
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
                    Value
                      (ParamArray
                         [Value (Str "This weapon automatically hits its target.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          Value (Str "automatically hits its target")])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "You can add 1 to hit rolls for this weapon when attacking a unit with 10 or more models.");
                          Value (Str "add"); Value (Int 1); Value (Str "to hit for");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          Value (Str "when attacking");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Value (Int 1)]));
                          Value (Str "with"); Value (Int 10); Value (Str "or more")])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "You can re-roll failed wound rolls for this weapon. In addition, units attacked by this weapon do not gain any bonus to their saving throws for being in cover.");
                          Let
                            ("Wound Roll",
                             Value
                               (ParamArray
                                  [Lam
                                     ("rollTarget",
                                      Let
                                        ("f",Var "Wound Roll",
                                         Let
                                           ("x",App (Var "f",Var "rollTarget"),
                                            IfThenElse
                                              (Var "x",Var "x",
                                               Some (App (Var "f",Var "rollTarget"))))));
                                   Value (Str "In addition attacked by");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                                   Value (Str "do not gain");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "bonus"); Lam ("obj",Var "obj")]));
                                   Value
                                     (Str "to their saving throws for being in cover")]),
                             Value
                               (ParamArray
                                  [Value (Str "In addition attacked by");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                                   Value (Str "do not gain");
                                   App
                                     (Call Repeat,
                                      Value
                                        (ParamArray
                                           [Value (Str "bonus"); Lam ("obj",Var "obj")]));
                                   Value
                                     (Str "to their saving throws for being in cover")]))])])])
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
                    Value
                      (ParamArray
                         [Value (Str "This weapon automatically hits its target.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          Value (Str "automatically hits its target")])])])
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
                    Value
                      (ParamArray
                         [Value (Str "This weapon automatically hits its target.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          Value (Str "automatically hits its target")])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "This weapon can be fired within 1\" of an enemy unit, and can target enemy units within 1\" of friendly units.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          Value (Str "be fired within"); Value (Distance 1);
                          Value (Str "of");
                          App
                            (Lam ("obj",Var "obj"),
                             Value (ParamArray [Var "Target"; Var "Target"]));
                          Value (Str "and target"); Var "Target"; Value (Str "within");
                          Value (Distance 1); Value (Str "of friendly")])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "This weapon can be fired within 1\" of an enemy unit, and can target enemy units within 1\" of friendly units. In addition, when a model is slain by this weapon, the bearer regains 1 lost wound.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          Value (Str "be fired within"); Value (Distance 1);
                          Value (Str "of");
                          App
                            (Lam ("obj",Var "obj"),
                             Value (ParamArray [Var "Target"; Var "Target"]));
                          Value (Str "and target"); Var "Target"; Value (Str "within");
                          Value (Distance 1); Value (Str "of friendly In addition when");
                          App
                            (Call Repeat,
                             Value (ParamArray [Value (Str "model"); Value (Int 1)]));
                          Value (Str "is slain by");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "bearer"); Lam ("obj",Var "obj")]));
                          Value (Str "regains"); Value (Int 1); Value (Str "lost wound")])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "This weapon can target units that are not visible to the bearer. In addition, units attacked by this weapon do not gain any bonus to their saving throws for being in cover.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          Value (Str "target that are not visible to");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "bearer"); Lam ("obj",Var "obj")]));
                          Value (Str "In addition attacked by");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          Value (Str "do not gain");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "bonus"); Lam ("obj",Var "obj")]));
                          Value (Str "to their saving throws for being in cover")])])])
    let ``Spore mine launcher`` = 
        Value
          (ParamArray
             [Value (Str "48\" Heavy 1 See Biovore datasheet (pg 101)");
              Value (Str "48"); Value (Distance 0); Value (Str "Heavy"); Value (Int 1);
              Value (Str "See Biovore datasheet pg"); Value (Int 101)])
    let _ = 
        Value
          (ParamArray
             [Value
                (Str
                   "Attacking from the skies and from beneath the earth, the Tyranids tear apart their prey in a frenzy of slashing claws.");
              Value (Str "Attacking from");
              App (Lam ("obj",Var "obj"),Value (ParamArray []));
              Value (Str "and from beneath");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "earth"); Lam ("obj",Var "obj")]));
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "Tyranids"); Lam ("obj",Var "obj")]));
              Value (Str "tear apart their prey in");
              App (Call Repeat,Value (ParamArray [Value (Str "frenzy"); Value (Int 1)]));
              Value (Str "of slashing")])
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
                      (ParamArray
                         [Value
                            (Str
                               "This weapon can be fired within 1\" of an enemy unit, and can target enemy units within 1\" of friendly units. You can re-roll all failed wound rolls for this weapon.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          Value (Str "be fired within"); Value (Distance 1);
                          Value (Str "of");
                          App
                            (Lam ("obj",Var "obj"),
                             Value (ParamArray [Var "Target"; Var "Target"]));
                          Value (Str "and target"); Var "Target"; Value (Str "within");
                          Value (Distance 1); Value (Str "of friendly re-roll");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray [Value (Str "failed"); Value (Str "wound")]));
                          Value (Str "for");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]))])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "If the target is a VEHICLE and you make a wound roll of 4+, the target suffers 1 mortal wound in addition to any other damage. If you make a wound roll of 6+, inflict D3 mortal wounds instead.");
                          Value (Str "If");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "target"); Lam ("obj",Var "obj")]));
                          Value (Str "is");
                          App
                            (Call Repeat,
                             Value (ParamArray [Value (Str "VEHICLE"); Value (Int 1)]));
                          Value (Str "and make");
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray [Value (Str "wound"); Value (Str "roll")]));
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
                                      Value (ParamArray [Var "roll"; Value (Int 4)])),
                                   App (Call Or,Value (ParamArray [Var "eq"; Var "gt"])))));
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "target"); Lam ("obj",Var "obj")]));
                          Call Suffer; Value (Int 1);
                          Value (Str "mortal wound in addition to");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray [Value (Str "other"); Value (Str "damage")]));
                          Value (Str "If make");
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray [Value (Str "wound"); Value (Str "roll")]));
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
                                      Value (ParamArray [Var "roll"; Value (Int 6)])),
                                   App (Call Or,Value (ParamArray [Var "eq"; Var "gt"])))));
                          Value (Str "inflict D3 mortal instead")])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "This weapon can be fired within 1\" of an enemy unit, and can target enemy units within 1\" of friendly units.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          Value (Str "be fired within"); Value (Distance 1);
                          Value (Str "of");
                          App
                            (Lam ("obj",Var "obj"),
                             Value (ParamArray [Var "Target"; Var "Target"]));
                          Value (Str "and target"); Var "Target"; Value (Str "within");
                          Value (Distance 1); Value (Str "of friendly")])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "When a model fires this weapon, it makes a number of shots equal to its Attacks characteristic.");
                          Value (Str "When");
                          App
                            (Call Repeat,
                             Value (ParamArray [Value (Str "model"); Value (Int 1)]));
                          Value (Str "fires");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          Value (Str "it makes");
                          App
                            (Call Repeat,
                             Value (ParamArray [Value (Str "number"); Value (Int 1)]));
                          Value (Str "of equal to its characteristic")])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "You can add 1 to hit rolls for this weapon when attacking a unit with 10 or more models.");
                          Value (Str "add"); Value (Int 1); Value (Str "to hit for");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          Value (Str "when attacking");
                          App
                            (Call Repeat,
                             Value (ParamArray [Var "Target"; Value (Int 1)]));
                          Value (Str "with"); Value (Int 10); Value (Str "or more")])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "You may re-roll failed hit rolls for this weapon against units that can FLY . In addition, if the target is a VEHICLE and you make a wound roll of 4+, it suffers 1 mortal wound in addition to any other damage. If you make a wound roll of 6+, inflict D3 mortal wounds instead.");
                          Value (Str "may re-roll failed Hit Roll for");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          Value (Str "against that FLY In addition if");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "target"); Lam ("obj",Var "obj")]));
                          Value (Str "is");
                          App
                            (Call Repeat,
                             Value (ParamArray [Value (Str "VEHICLE"); Value (Int 1)]));
                          Value (Str "and make");
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray [Value (Str "wound"); Value (Str "roll")]));
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
                                      Value (ParamArray [Var "roll"; Value (Int 4)])),
                                   App (Call Or,Value (ParamArray [Var "eq"; Var "gt"])))));
                          Value (Str "it"); Call Suffer; Value (Int 1);
                          Value (Str "mortal wound in addition to");
                          App
                            (Lam ("obj",Var "obj"),
                             Value
                               (ParamArray [Value (Str "other"); Value (Str "damage")]));
                          Value (Str "If make");
                          App
                            (Value (Int 1),
                             Value
                               (ParamArray [Value (Str "wound"); Value (Str "roll")]));
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
                                      Value (ParamArray [Var "roll"; Value (Int 6)])),
                                   App (Call Or,Value (ParamArray [Var "eq"; Var "gt"])))));
                          Value (Str "inflict D3 mortal instead")])])])
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
                      (ParamArray
                         [Value
                            (Str
                               "This weapon can be fired within 1\" of an enemy unit, and can target enemy units within 1\" of friendly units. In addition, you can re-roll failed wound rolls for this weapon.");
                          App
                            (Call Repeat,
                             Value
                               (ParamArray [Value (Str "weapon"); Lam ("obj",Var "obj")]));
                          Value (Str "be fired within"); Value (Distance 1);
                          Value (Str "of");
                          App
                            (Lam ("obj",Var "obj"),
                             Value (ParamArray [Var "Target"; Var "Target"]));
                          Value (Str "and target"); Var "Target"; Value (Str "within");
                          Value (Distance 1); Value (Str "of friendly In addition");
                          Let
                            ("Wound Roll",
                             Value
                               (ParamArray
                                  [Lam
                                     ("rollTarget",
                                      Let
                                        ("f",Var "Wound Roll",
                                         Let
                                           ("x",App (Var "f",Var "rollTarget"),
                                            IfThenElse
                                              (Var "x",Var "x",
                                               Some (App (Var "f",Var "rollTarget"))))))]),
                             Value (ParamArray []))])])])
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
          (ParamArray
             [Value
                (Str
                   "At the start of the first battle round but before the first turn begins, you can remove your Warlord from the battlefield and set them up again. If both players have units that can do this, roll off. The player that wins the roll-off decides who sets up their unit(s) first.");
              Value (Str "At");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "start"); Lam ("obj",Var "obj")]));
              Value (Str "of");
              App
                (Lam ("obj",Var "obj"),
                 Value
                   (ParamArray
                      [Value (Str "first"); Value (Str "battle"); Value (Str "round")]));
              Value (Str "but before");
              App
                (Lam ("obj",Var "obj"),
                 Value (ParamArray [Value (Str "first"); Value (Str "turn")]));
              Value (Str "begins remove your Warlord from");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "battlefield"); Lam ("obj",Var "obj")]));
              Value (Str "and set them up again If");
              App (Lam ("obj",Var "obj"),Value (ParamArray []));
              Value (Str "have that do");
              App (Lam ("obj",Var "obj"),Value (ParamArray [])); Value (Str "roll off");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "player"); Lam ("obj",Var "obj")]));
              Value (Str "that wins");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "roll-off"); Lam ("obj",Var "obj")]));
              Value (Str "decides who sets up their"); Var "Target";
              Value (Str "s first")])
    let ``HEIGHTENED SENSES`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "Your Warlord never suffers any penalties to their hit rolls (although they still only hit on rolls of 6 when firing Overwatch).");
              Value (Str "Your Warlord never"); Call Suffer;
              App (Lam ("obj",Var "obj"),Value (ParamArray []));
              Value (Str "to their Hit Roll although they still only hit on of");
              Value (Int 6); Value (Str "when firing Overwatch")])
    let ``SYNAPTIC LYNCHPIN`` = 
        Value
          (ParamArray
             [Value (Str "Add 6\" to the range of the Warlord’s Synapse ability.");
              Value (Str "Add"); Value (Distance 6); Value (Str "to");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "range"); Lam ("obj",Var "obj")]));
              Value (Str "of");
              App
                (Lam ("obj",Var "obj"),
                 Value (ParamArray [Value (Str "Warlord"); Value (Str "'s")]));
              Value (Str "Synapse ability")])
    let ``MIND EATER`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "Each time the Warlord slays an enemy CHARACTER in the Fight phase, choose a friendly <HIVE FLEET> unit within 3\". At the end of the phase, that unit can move (and Advance if you wish) as if it was your Movement phase.");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "time"); Lam ("obj",Var "obj")]));
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "Warlord"); Lam ("obj",Var "obj")]));
              Value (Str "slays");
              App
                (Lam ("obj",Var "obj"),
                 Value (ParamArray [Var "Target"; Value (Str "CHARACTER")]));
              Value (Str "in");
              App
                (Call Repeat,
                 Value
                   (ParamArray
                      [Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                       Lam ("obj",Var "obj")])); Value (Str "choose");
              App
                (Value (Int 1),
                 Value
                   (ParamArray
                      [Value (Str "friendly"); Value (Str "<HIVE FLEET>"); Var "Target"]));
              Value (Str "within"); Value (Distance 3); Value (Str "At");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
              Value (Str "of");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "phase"); Lam ("obj",Var "obj")]));
              App (Call Repeat,Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
              Value (Str "move and Advance if wish as if it was your Movement phase")])
    let ``INSTINCTIVE KILLER`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "At the beginning of the battle but before the first turn begins, choose an enemy unit. You can re-roll failed hit rolls for the Warlord for attacks that target that unit or any unit that has the same datasheet (for example, all Intercessor Squads or all units of Nobz etc.).");
              Value (Str "At");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "beginning"); Lam ("obj",Var "obj")]));
              Value (Str "of");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "battle"); Lam ("obj",Var "obj")]));
              Value (Str "but before");
              App
                (Lam ("obj",Var "obj"),
                 Value (ParamArray [Value (Str "first"); Value (Str "turn")]));
              Value (Str "begins choose");
              App
                (Lam ("obj",Var "obj"),Value (ParamArray [Var "Target"; Var "Target"]));
              Value (Str "re-roll failed Hit Roll for");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "Warlord"); Lam ("obj",Var "obj")]));
              Value (Str "for that target");
              App (Call Repeat,Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
              Value (Str "or");
              App (Call Repeat,Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
              Value (Str "that has");
              App
                (Lam ("obj",Var "obj"),
                 Value (ParamArray [Value (Str "same"); Value (Str "datasheet")]));
              Value (Str "for example");
              App
                (Lam ("obj",Var "obj"),
                 Value (ParamArray [Value (Str "Intercessor"); Value (Str "Squads")]));
              Value (Str "or"); App (Lam ("obj",Var "obj"),Value (ParamArray []));
              Value (Str "of Nobz etc.")])
    let ``ADAPTIVE BIOLOGY`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "From the end of the first phase in which this Warlord suffers any wounds, for the remainder of the battle when inflicting damage upon the Warlord, reduce the damage of the attack by 1 to a minimum of 1.");
              Value (Str "From");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
              Value (Str "of");
              App
                (Lam ("obj",Var "obj"),
                 Value (ParamArray [Value (Str "first"); Value (Str "phase")]));
              Value (Str "in which");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "Warlord"); Lam ("obj",Var "obj")]));
              Call Suffer; App (Lam ("obj",Var "obj"),Value (ParamArray []));
              Value (Str "for");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "remainder"); Lam ("obj",Var "obj")]));
              Value (Str "of");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "battle"); Lam ("obj",Var "obj")]));
              Value (Str "when inflicting damage upon");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "Warlord"); Lam ("obj",Var "obj")]));
              Value (Str "reduce");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "damage"); Lam ("obj",Var "obj")]));
              Value (Str "of");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "attack"); Lam ("obj",Var "obj")]));
              Value (Str "by"); Value (Int 1); Value (Str "to");
              App
                (Call Repeat,Value (ParamArray [Value (Str "minimum"); Value (Int 1)]));
              Value (Str "of"); Value (Int 1)])
    let ``The mightiest creatures spawned by the Hive Mind have each been created with a specific purpose. If one of the following named characters is your Warlord, they must be given the associated Warlord Trait shown below.`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "The Swarmlord Alien Cunning Old One Eye Adaptive Biology Deathleaper Mind Eater The Red Terror Heightened Senses");
              Lam ("obj",Var "obj");
              Value
                (Str
                   "Swarmlord Alien Cunning Old One Eye Adaptive Biology Deathleaper Mind Eater The Red Terror Heightened Senses")])
    let ``HIVE FLEET WARLORD TRAITS`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "If you wish, you can pick a Hive Fleet Warlord Trait from the list below instead of the Tyranid Warlord Traits to the left, but only if your Warlord is from the relevant hive fleet.");
              Value (Str "If wish pick");
              App
                (Value (Int 1),
                 Value
                   (ParamArray
                      [Value (Str "Hive"); Value (Str "Fleet"); Value (Str "Warlord");
                       Value (Str "Trait")])); Value (Str "from");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "list"); Lam ("obj",Var "obj")]));
              Value (Str "below instead of");
              App
                (Lam ("obj",Var "obj"),
                 Value
                   (ParamArray
                      [Value (Str "Tyranid"); Value (Str "Warlord");
                       Value (Str "Traits")])); Value (Str "to");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "left"); Lam ("obj",Var "obj")]));
              Value (Str "but only if your Warlord is from");
              App
                (Lam ("obj",Var "obj"),
                 Value
                   (ParamArray
                      [Value (Str "relevant"); Value (Str "hive"); Value (Str "fleet")]))])
    let ``BEHEMOTH: MONSTROUS HUNGER`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "Each time you make a wound roll of 6+ for the Warlord in the Fight phase, that attack inflicts 1 additional damage.");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "time"); Lam ("obj",Var "obj")]));
              Value (Str "make");
              App
                (Value (Int 1),
                 Value (ParamArray [Value (Str "wound"); Value (Str "roll")]));
              Value (Str "of");
              Lam
                ("roll",
                 Let
                   ("gt",
                    App
                      (Call GreaterThan,Value (ParamArray [Var "roll"; Value (Int 6)])),
                    Let
                      ("eq",
                       App (Call Equals,Value (ParamArray [Var "roll"; Value (Int 6)])),
                       App (Call Or,Value (ParamArray [Var "eq"; Var "gt"])))));
              Value (Str "for");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "Warlord"); Lam ("obj",Var "obj")]));
              Value (Str "in");
              App
                (Call Repeat,
                 Value
                   (ParamArray
                      [Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                       Lam ("obj",Var "obj")]));
              Let
                ("ThatSubject",Value (ParamArray [Var "attack"]),
                 Value
                   (ParamArray
                      [Let
                         ("ThatObject",
                          Value
                            (ParamArray [Value (Int 1); Value (Str "additional damage")]),
                          Value
                            (ParamArray
                               [App
                                  (Value (Str "inflicts"),
                                   Value
                                     (ParamArray [Var "ThatSubject"; Var "ThatObject"]))]))]))])
    let ``KRAKEN: ONE STEP AHEAD`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "In each Fight phase, you can pick one friendly KRAKEN unit within 6\" of your Warlord. That unit can fight first in the Fight phase, even if it didn’t charge. If the enemy has units that have charged, or that have a similar ability, then alternate choosing units to fight with, starting with the player whose turn is taking place.");
              Value (Str "In");
              Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
              Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
              Value (Str "pick one friendly KRAKEN"); Var "Target"; Value (Str "within");
              Value (Distance 6); Value (Str "of your Warlord");
              App (Call Repeat,Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
              Value (Str "fight first in");
              App
                (Call Repeat,
                 Value
                   (ParamArray
                      [Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                       Lam ("obj",Var "obj")]));
              Value (Str "even if it did n't charge If");
              App (Call Repeat,Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
              Value (Str "has that have charged or that have");
              App
                (Value (Int 1),
                 Value (ParamArray [Value (Str "similar"); Value (Str "ability")]));
              Value (Str "then alternate choosing to fight with starting with");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "player"); Lam ("obj",Var "obj")]));
              Value (Str "whose turn is taking place")])
    let ``LEVIATHAN: PERFECTLY ADAPTED`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "Once per battle round, you can re-roll a single hit roll, wound roll, damage roll, Advance roll, charge roll or saving throw made for your Warlord.");
              Value (Str "Once per battle round re-roll");
              App
                (Value (Int 1),
                 Value
                   (ParamArray
                      [Value (Str "single"); Value (Str "hit"); Value (Str "roll")]));
              Value
                (Str
                   "wound roll damage roll Advance roll charge roll or saving throw made for your Warlord")])
    let ``GORGON: LETHAL MIASMA`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "At the end of the Fight phase, roll a D6 for each enemy unit within 1\" of the Warlord. On a 4+ that unit suffers a mortal wound.");
              Value (Str "At");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
              Value (Str "of");
              App
                (Call Repeat,
                 Value
                   (ParamArray
                      [Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                       Lam ("obj",Var "obj")])); Value (Str "roll");
              App
                (Call Repeat,
                 Value (ParamArray [App (Call Dice,Value (Int 6)); Value (Int 1)]));
              Value (Str "for");
              Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"]))); Var "Target";
              Var "Target"; Value (Str "within"); Value (Distance 1); Value (Str "of");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "Warlord"); Lam ("obj",Var "obj")]));
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
                                   Value (ParamArray [Var "roll"; Value (Int 4)])),
                                App (Call Or,Value (ParamArray [Var "eq"; Var "gt"])))));
                       Value (Int 1)]));
              App (Call Repeat,Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
              Call Suffer;
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "Mortal Wound"); Value (Int 1)]))])
    let ``JORMUNGANDR: INSIDIOUS THREAT`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "Enemy units never gain any bonus to their saving throws for being in cover for attacks made by the Warlord or friendly JORMUNGANDR units within 3\" of the Warlord.");
              Value (Str "Enemy never gain");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "bonus"); Lam ("obj",Var "obj")]));
              Value (Str "to their saving throws for being in cover for made by");
              App
                (Lam ("obj",Var "obj"),
                 Value
                   (ParamArray
                      [Value (Str "Warlord"); Value (Str "or"); Value (Str "friendly");
                       Value (Str "JORMUNGANDR")])); Value (Str "within");
              Value (Distance 3); Value (Str "of");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "Warlord"); Lam ("obj",Var "obj")]))])
    let ``HYDRA: ENDLESS REGENERATION`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "At the beginning of each of your turns, roll a dice for each wound that your Warlord has lost. For each roll of 6, your Warlord regains a wound lost earlier in the battle.");
              Value (Str "At");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "beginning"); Lam ("obj",Var "obj")]));
              Value (Str "of");
              Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
              Value (Str "of your roll"); App (Value (Int 1),Value (ParamArray []));
              Value (Str "for");
              Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
              Value (Str "wound");
              Let
                ("ThatSubject",
                 Value
                   (ParamArray
                      [Var "your Warlord"; Value (Str "For");
                       Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                       Value (Str "roll of"); Value (Int 6);
                       Value (Str "your Warlord regains");
                       App
                         (Call Repeat,
                          Value (ParamArray [Value (Str "wound"); Value (Int 1)]));
                       Value (Str "lost earlier in");
                       App
                         (Call Repeat,
                          Value
                            (ParamArray [Value (Str "battle"); Lam ("obj",Var "obj")]))]),
                 Value
                   (ParamArray
                      [Let
                         ("ThatObject",
                          Value
                            (ParamArray
                               [Value (Str "lost For");
                                Lam
                                  ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                                Value (Str "roll of"); Value (Int 6);
                                Value (Str "your Warlord regains");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Value (Str "wound"); Value (Int 1)]));
                                Value (Str "lost earlier in");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battle"); Lam ("obj",Var "obj")]))]),
                          Value
                            (ParamArray
                               [App
                                  (Value (Str "has"),
                                   Value
                                     (ParamArray [Var "ThatSubject"; Var "ThatObject"]));
                                Value (Str "For");
                                Lam
                                  ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                                Value (Str "roll of"); Value (Int 6);
                                Value (Str "your Warlord regains");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray [Value (Str "wound"); Value (Int 1)]));
                                Value (Str "lost earlier in");
                                App
                                  (Call Repeat,
                                   Value
                                     (ParamArray
                                        [Value (Str "battle"); Lam ("obj",Var "obj")]))]))]))])
    let ``KRONOS: SOUL HUNGER`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "Whenever an enemy PSYKER fails a psychic test within 18\" of your Warlord, they suffer D3 mortal wounds.");
              Value (Str "Whenever");
              App
                (Lam ("obj",Var "obj"),
                 Value (ParamArray [Var "Target"; Value (Str "PSYKER")]));
              Value (Str "fails");
              App
                (Value (Int 1),
                 Value (ParamArray [Value (Str "psychic"); Value (Str "test")]));
              Value (Str "within"); Value (Distance 18);
              Value (Str "of your Warlord they suffer D3 mortal")])
    let ``EXTENSIONS OF THE HIVE MIND`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "If your army is Battle-forged, all Troops units in Tyranids Detachments gain this ability. Such a unit that is within range of an objective marker (as specified in the mission) controls the objective marker even if there are more enemy models within range of that objective marker. If an enemy unit within range of the same objective marker has a similar ability, then the objective marker is controlled by the player who has the most models within range of it as normal.");
              Value (Str "If your army is Battle-forged");
              App (Lam ("obj",Var "obj"),Value (ParamArray []));
              Value (Str "Troops in Tyranids Detachments gain");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "ability"); Lam ("obj",Var "obj")]));
              Value (Str "Such"); Value (Int 1); Var "Target";
              Value (Str "that is within range of");
              App
                (Lam ("obj",Var "obj"),
                 Value (ParamArray [Value (Str "objective"); Value (Str "marker")]));
              Value (Str "as specified in");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "mission"); Lam ("obj",Var "obj")]));
              Value (Str "controls");
              App
                (Lam ("obj",Var "obj"),
                 Value (ParamArray [Value (Str "objective"); Value (Str "marker")]));
              Value (Str "even if there are more"); Var "Target";
              Value (Str "within range of");
              App
                (Lam ("obj",Var "obj"),
                 Value (ParamArray [Value (Str "objective"); Value (Str "marker")]));
              Value (Str "If");
              App
                (Lam ("obj",Var "obj"),Value (ParamArray [Var "Target"; Var "Target"]));
              Value (Str "within range of");
              App
                (Lam ("obj",Var "obj"),
                 Value
                   (ParamArray
                      [Value (Str "same"); Value (Str "objective"); Value (Str "marker")]));
              Value (Str "has");
              App
                (Value (Int 1),
                 Value (ParamArray [Value (Str "similar"); Value (Str "ability")]));
              Value (Str "then"); Lam ("obj",Var "obj");
              Value (Str "objective marker is controlled by");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "player"); Lam ("obj",Var "obj")]));
              Value (Str "who has");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "most"); Lam ("obj",Var "obj")]));
              Value (Str "within range of it as normal")])
    let ``HIVE FLEET ADAPTATIONS`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "If your army is Battle-forged, all units in Tyranids Detachments gain a Hive Fleet Adaptation, so long as every unit in that Detachment is from the same hive fleet. The Hive Fleet Adaptation gained depends upon the hive fleet they are from, as shown in the table opposite. For example, a BEHEMOTH unit with the Hive Fleet Adaptation ability gains the Hyper-aggression adaptation. If you are using a splinter fleet rather than a hive fleet, use the Hive Fleet Adaptation of its parent hive fleet. For example, the Court of the Nephilim King is a splinter fleet of Hive Fleet Behemoth, so should use the Behemoth Hive Fleet Adaptation. If you are unsure of a splinter fleet’s parent hive fleet, either consult the background sections of our books or choose an adaptation from the table that best describes its character and fighting style.");
              Value (Str "If your army is Battle-forged");
              App (Lam ("obj",Var "obj"),Value (ParamArray []));
              Value (Str "in Tyranids Detachments gain");
              App
                (Value (Int 1),
                 Value
                   (ParamArray
                      [Value (Str "Hive"); Value (Str "Fleet"); Value (Str "Adaptation")]));
              Value (Str "so long as");
              App (Call Repeat,Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
              Value (Str "in that Detachment is from");
              App
                (Lam ("obj",Var "obj"),
                 Value
                   (ParamArray
                      [Value (Str "same"); Value (Str "hive"); Value (Str "fleet")]));
              App
                (Lam ("obj",Var "obj"),
                 Value
                   (ParamArray
                      [Value (Str "Hive"); Value (Str "Fleet"); Value (Str "Adaptation")]));
              Value (Str "gained depends upon");
              App
                (Lam ("obj",Var "obj"),
                 Value (ParamArray [Value (Str "hive"); Value (Str "fleet")]));
              Value (Str "they are from as shown in");
              App
                (Lam ("obj",Var "obj"),
                 Value (ParamArray [Value (Str "table"); Value (Str "opposite")]));
              Value (Str "For example");
              App
                (Value (Int 1),Value (ParamArray [Value (Str "BEHEMOTH"); Var "Target"]));
              Value (Str "with");
              App
                (Lam ("obj",Var "obj"),
                 Value
                   (ParamArray
                      [Value (Str "Hive"); Value (Str "Fleet"); Value (Str "Adaptation");
                       Value (Str "ability")])); Value (Str "gains");
              App
                (Lam ("obj",Var "obj"),
                 Value
                   (ParamArray
                      [Value (Str "Hyper-aggression"); Value (Str "adaptation")]));
              Value (Str "If are using");
              App
                (Value (Int 1),
                 Value (ParamArray [Value (Str "splinter"); Value (Str "fleet")]));
              Value (Str "rather than");
              App
                (Value (Int 1),
                 Value (ParamArray [Value (Str "hive"); Value (Str "fleet")]));
              Value (Str "use");
              App
                (Lam ("obj",Var "obj"),
                 Value
                   (ParamArray
                      [Value (Str "Hive"); Value (Str "Fleet"); Value (Str "Adaptation")]));
              Value (Str "of its parent hive fleet For example");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "Court"); Lam ("obj",Var "obj")]));
              Value (Str "of");
              App
                (Lam ("obj",Var "obj"),
                 Value (ParamArray [Value (Str "Nephilim"); Value (Str "King")]));
              Value (Str "is");
              App
                (Value (Int 1),
                 Value (ParamArray [Value (Str "splinter"); Value (Str "fleet")]));
              Value (Str "of Hive Fleet Behemoth so should use");
              App
                (Lam ("obj",Var "obj"),
                 Value
                   (ParamArray
                      [Value (Str "Behemoth"); Value (Str "Hive"); Value (Str "Fleet");
                       Value (Str "Adaptation")])); Value (Str "If are unsure of");
              App
                (Value (Int 1),
                 Value
                   (ParamArray
                      [Value (Str "splinter"); Value (Str "fleet"); Value (Str "'s")]));
              Value (Str "parent hive fleet either consult");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "background"); Lam ("obj",Var "obj")]));
              Value (Str "of our or choose");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "adaptation"); Lam ("obj",Var "obj")]));
              Value (Str "from");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "table"); Lam ("obj",Var "obj")]));
              Let
                ("ThatSubject",Value (ParamArray [Var "best"]),
                 Value
                   (ParamArray
                      [Let
                         ("ThatObject",
                          Value
                            (ParamArray [Value (Str "its character and fighting style")]),
                          Value
                            (ParamArray
                               [App
                                  (Value (Str "describes"),
                                   Value
                                     (ParamArray [Var "ThatSubject"; Var "ThatObject"]))]))]))])
    let ``BEHEMOTH: HYPER-AGGRESSION`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "You can re-roll failed charge rolls for units with this adaptation.");
              Let
                ("charge",
                 Value
                   (ParamArray
                      [Lam
                         ("rollTarget",
                          Let
                            ("f",Var "charge",
                             Let
                               ("x",App (Var "f",Var "rollTarget"),
                                IfThenElse
                                  (Var "x",Var "x",Some (App (Var "f",Var "rollTarget"))))))]),
                 Value
                   (ParamArray
                      [Value (Str "with");
                       App
                         (Call Repeat,
                          Value
                            (ParamArray
                               [Value (Str "adaptation"); Lam ("obj",Var "obj")]))]))])
    let ``KRAKEN: QUESTING TENDRILS`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "When a unit with this adaptation Advances, roll three dice instead of one and pick the highest to add to the Move characteristic of all models in the unit for that Movement phase. In addition, such units can Fall Back and charge in the same turn.");
              Value (Str "When");
              App (Call Repeat,Value (ParamArray [Var "Target"; Value (Int 1)]));
              Value (Str "with");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "adaptation"); Lam ("obj",Var "obj")]));
              Value (Str "Advances roll three instead of one and pick");
              Lam ("obj",Var "obj"); Value (Str "highest to add to");
              App
                (Lam ("obj",Var "obj"),
                 Value (ParamArray [Value (Str "Move"); Value (Str "characteristic")]));
              Value (Str "of"); App (Lam ("obj",Var "obj"),Value (ParamArray []));
              Value (Str "in");
              App (Call Repeat,Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
              Value (Str "for");
              App
                (Lam ("obj",Var "obj"),
                 Value (ParamArray [Value (Str "Movement"); Value (Str "phase")]));
              Value (Str "In addition such Fall Back and charge in");
              App
                (Lam ("obj",Var "obj"),
                 Value (ParamArray [Value (Str "same"); Value (Str "turn")]))])
    let ``LEVIATHAN: SYNAPTIC IMPERATIVE`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "Roll a D6 each time a unit with this adaptation loses a wound whilst it is within 6\" of a friendly SYNAPSE unit from the same hive fleet. On a 6, the damage is ignored and the unit does not lose a wound. Ignore this adaptation on a unit that is currently affected by the Catalyst psychic power.");
              Value (Str "Roll");
              App
                (Call Repeat,
                 Value (ParamArray [App (Call Dice,Value (Int 6)); Value (Int 1)]));
              Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
              Value (Str "time");
              App (Call Repeat,Value (ParamArray [Var "Target"; Value (Int 1)]));
              Value (Str "with");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "adaptation"); Lam ("obj",Var "obj")]));
              Value (Str "loses");
              App (Call Repeat,Value (ParamArray [Value (Str "wound"); Value (Int 1)]));
              Value (Str "whilst it is within"); Value (Distance 6); Value (Str "of");
              App
                (Value (Int 1),
                 Value
                   (ParamArray
                      [Value (Str "friendly"); Value (Str "SYNAPSE"); Var "Target"]));
              Value (Str "from");
              App
                (Lam ("obj",Var "obj"),
                 Value
                   (ParamArray
                      [Value (Str "same"); Value (Str "hive"); Value (Str "fleet")]));
              Value (Str "On");
              App (Call Repeat,Value (ParamArray [Value (Int 6); Value (Int 1)]));
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "damage"); Lam ("obj",Var "obj")]));
              Value (Str "is ignored and");
              App (Call Repeat,Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
              Value (Str "does not lose");
              App (Call Repeat,Value (ParamArray [Value (Str "wound"); Value (Int 1)]));
              Value (Str "Ignore");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "adaptation"); Lam ("obj",Var "obj")]));
              Value (Str "on");
              App (Call Repeat,Value (ParamArray [Var "Target"; Value (Int 1)]));
              Value (Str "that is currently affected by");
              App
                (Lam ("obj",Var "obj"),
                 Value
                   (ParamArray
                      [Value (Str "Catalyst"); Value (Str "psychic");
                       Value (Str "power")]))])
    let ``GORGON: ADAPTIVE TOXINS`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "You can re-roll wound rolls of 1 in the Fight phase for units with this adaptation.");
              Value (Str "re-roll wound of"); Value (Int 1); Value (Str "in");
              App
                (Call Repeat,
                 Value
                   (ParamArray
                      [Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                       Lam ("obj",Var "obj")])); Value (Str "for with");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "adaptation"); Lam ("obj",Var "obj")]))])
    let ``JORMUNGANDR: TUNNEL NETWORKS`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "A unit with this adaptation (other than units that can FLY ) always has the benefit of cover for the purposes of shooting attacks. If the unit Advances or charges, however, it loses the benefit of this adaptation until the start of your next Movement phase.");
              App (Call Repeat,Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
              Value (Str "with");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "adaptation"); Lam ("obj",Var "obj")]));
              Value (Str "other than that FLY always has");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "benefit"); Lam ("obj",Var "obj")]));
              Value (Str "of cover for");
              App (Lam ("obj",Var "obj"),Value (ParamArray []));
              Value (Str "of shooting If");
              App (Call Repeat,Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
              Value (Str "Advances or charges however it loses");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "benefit"); Lam ("obj",Var "obj")]));
              Value (Str "of");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "adaptation"); Lam ("obj",Var "obj")]));
              Value (Str "until");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "start"); Lam ("obj",Var "obj")]));
              Value (Str "of your next Movement phase")])
    let ``HYDRA: SWARMING INSTINCTS`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "You can re-roll hit rolls in the Fight phase for units with this adaptation that target units containing fewer models than their own.");
              Value (Str "re-roll Hit Roll in");
              App
                (Call Repeat,
                 Value
                   (ParamArray
                      [Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                       Lam ("obj",Var "obj")])); Value (Str "for with");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "adaptation"); Lam ("obj",Var "obj")]));
              Value (Str "that target containing fewer than their own")])
    let ``KRONOS: BIO-BARRAGE`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "You can re-roll hit rolls of 1 for units with this adaptation in your Shooting phase if they did not move in the preceding Movement phase.");
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
                                      Value (ParamArray [Var "x"; Value (Int 1)])),
                                   Var "x",Some (App (Var "f",Var "rollTarget"))))))]),
                 Value
                   (ParamArray
                      [Value (Str "for with");
                       App
                         (Call Repeat,
                          Value
                            (ParamArray
                               [Value (Str "adaptation"); Lam ("obj",Var "obj")]));
                       Value (Str "in your Shooting phase")]))])
