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
                     [Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "in your Psychic phase if");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "Zoanthropes"); Value (Int 1)]));
                      Var "Target"; Value (Str "from your army consisting of at least");
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
                      Value (Str "of and visible to"); Lam ("obj",Var "obj");
                      Value (Str "three Roll");
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
                      Let
                        ("ThatSubject",Value (ParamArray [Var "Target"]),
                         Value
                           (ParamArray
                              [Let
                                 ("ThatObject",
                                  Value (ParamArray [Value (Str "3D3 mortal")]),
                                  Value
                                    (ParamArray
                                       [App
                                          (Call Suffer,
                                           Value
                                             (ParamArray
                                                [Var "ThatSubject"; Var "ThatObject"]))]))]))]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "PSYCHIC BARRAGE"])),
                   Value
                     (ParamArray
                        [Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "in your Psychic phase if");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "Zoanthropes"); Value (Int 1)]));
                         Var "Target";
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
                         Value (Str "of and visible to"); Lam ("obj",Var "obj");
                         Value (Str "three Roll");
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
                         Let
                           ("ThatSubject",Value (ParamArray [Var "Target"]),
                            Value
                              (ParamArray
                                 [Let
                                    ("ThatObject",
                                     Value (ParamArray [Value (Str "3D3 mortal")]),
                                     Value
                                       (ParamArray
                                          [App
                                             (Call Suffer,
                                              Value
                                                (ParamArray
                                                   [Var "ThatSubject"; Var "ThatObject"]))]))]))]),
                   None))]),None)
    let ``RAPID REGENERATION`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("RAPID REGENERATION",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "at");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
                      Value
                        (Str
                           "of your Movement phase Select a TYRANIDS model from your army It regains D3 lost earlier in");
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
                        [Value (Str "Use");
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
                         Value
                           (Str
                              "of your Movement phase Select a TYRANIDS model from your army It regains D3 lost earlier in");
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
                     [Value (Str "Use");
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
                        (Value (Int 1),
                         Value
                           (ParamArray
                              [Value (Str "Fight"); Value (Str "phase");
                               Value (Str "Select")]));
                      App
                        (Value (Int 1),
                         Value (ParamArray [Value (Str "TYRANIDS"); Var "Target"]));
                      Value (Str "from your army");
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
                        [Value (Str "Use");
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
                           (Value (Int 1),
                            Value
                              (ParamArray
                                 [Value (Str "Fight"); Value (Str "phase");
                                  Value (Str "Select")]));
                         App
                           (Value (Int 1),
                            Value (ParamArray [Value (Str "TYRANIDS"); Var "Target"]));
                         Value (Str "from your army");
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
                     [Value (Str "Use");
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
                        [Value (Str "Use");
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
                     [Value (Str "Use");
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
                               Lam ("obj",Var "obj")]));
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
                        [Value (Str "Use");
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
                                  Lam ("obj",Var "obj")]));
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
                     [Value (Str "Use");
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
                      Value (Int 1); Value (Str "CP or two extra for 3 CPs All of");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray
                              [Value (Str "Bio-artefacts"); Lam ("obj",Var "obj")]));
                      Let
                        ("ThatSubject",Value (ParamArray [Var "you"]),
                         Value
                           (ParamArray
                              [Let
                                 ("ThatObject",
                                  Value
                                    (ParamArray
                                       [Value
                                          (Str
                                             "must be different and be given to different CHARACTERS You can only use");
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
                                                [Var "ThatSubject"; Var "ThatObject"]))]))]))]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value
                        (ParamArray [Var "Available CP"; Var "BOUNTY OF THE HIVE FLEET"])),
                   Value
                     (ParamArray
                        [Value (Str "Use");
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
                         Value (Int 1); Value (Str "CP or two extra for 3 CPs All of");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Bio-artefacts"); Lam ("obj",Var "obj")]));
                         Let
                           ("ThatSubject",Value (ParamArray [Var "you"]),
                            Value
                              (ParamArray
                                 [Let
                                    ("ThatObject",
                                     Value
                                       (ParamArray
                                          [Value
                                             (Str
                                                "must be different and be given to different CHARACTERS You can only use");
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
                                                   [Var "ThatSubject"; Var "ThatObject"]))]))]))]),
                   None));
               ("3",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value
                        (ParamArray [Var "Available CP"; Var "BOUNTY OF THE HIVE FLEET"])),
                   Value
                     (ParamArray
                        [Value (Str "Use");
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
                         Value (Int 1); Value (Str "CP or two extra for 3 CPs All of");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Bio-artefacts"); Lam ("obj",Var "obj")]));
                         Let
                           ("ThatSubject",Value (ParamArray [Var "you"]),
                            Value
                              (ParamArray
                                 [Let
                                    ("ThatObject",
                                     Value
                                       (ParamArray
                                          [Value
                                             (Str
                                                "must be different and be given to different CHARACTERS You can only use");
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
                                                   [Var "ThatSubject"; Var "ThatObject"]))]))]))]),
                   None))]),None)
    let ``METABOLIC OVERDRIVE`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("METABOLIC OVERDRIVE",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value (Str "Use");
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
                        [Value (Str "Use");
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
                     [Value (Str "Use");
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
                               Lam ("obj",Var "obj")]));
                      Value (Str "Gain D3 Command Points")]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "FEEDER TENDRILS"])),
                   Value
                     (ParamArray
                        [Value (Str "Use");
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
                                  Lam ("obj",Var "obj")]));
                         Value (Str "Gain D3 Command Points")]),None))]),None)
    let ``PATHOGENIC SLIME`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("PATHOGENIC SLIME",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem in your Shooting phase");
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
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem in your Shooting phase");
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
                     [Value (Str "Use");
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
                        [Value (Str "Use");
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
                     [Value (Str "Use");
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
                        [Value (Str "Use");
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
                     [Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "at"); Lam ("obj",Var "obj");
                      Value (Str "end of your Shooting phase Select");
                      App
                        (Value (Int 1),
                         Value
                           (ParamArray
                              [Value (Str "TYRANIDS"); Value (Str "INFANTRY");
                               Var "Target"])); Value (Str "from your army");
                      Let
                        ("ThatSubject",Value (ParamArray [Var "Target"]),
                         Value
                           (ParamArray
                              [Let
                                 ("ThatObject",
                                  Value
                                    (ParamArray [Value (Str "immediately shoot again")]),
                                  Value
                                    (ParamArray
                                       [App
                                          (Value (Str "can"),
                                           Value
                                             (ParamArray
                                                [Var "ThatSubject"; Var "ThatObject"]))]))]))]));
               ("2",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value
                        (ParamArray
                           [Var "Available CP"; Var "SINGLE-MINDED ANNIHILATION"])),
                   Value
                     (ParamArray
                        [Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "at"); Lam ("obj",Var "obj");
                         Value (Str "end of your Shooting phase Select");
                         App
                           (Value (Int 1),
                            Value
                              (ParamArray
                                 [Value (Str "TYRANIDS"); Value (Str "INFANTRY");
                                  Var "Target"])); Value (Str "from your army");
                         Let
                           ("ThatSubject",Value (ParamArray [Var "Target"]),
                            Value
                              (ParamArray
                                 [Let
                                    ("ThatObject",
                                     Value
                                       (ParamArray
                                          [Value (Str "immediately shoot again")]),
                                     Value
                                       (ParamArray
                                          [App
                                             (Value (Str "can"),
                                              Value
                                                (ParamArray
                                                   [Var "ThatSubject"; Var "ThatObject"]))]))]))]),
                   None))]),None)
    let ``POWER OF THE HIVE MIND`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("POWER OF THE HIVE MIND",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "at");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
                      Value (Str "of your Psychic phase");
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
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "turn"); Lam ("obj",Var "obj")]));
                      Value
                        (Str
                           "It can immediately attempt to manifest one additional psychic power");
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
                        [Value (Str "Use");
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
                         Value (Str "of your Psychic phase");
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
                         App
                           (Call Repeat,
                            Value
                              (ParamArray [Value (Str "turn"); Lam ("obj",Var "obj")]));
                         Value
                           (Str
                              "It can immediately attempt to manifest one additional psychic power");
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
                     [Value (Str "Use");
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
                        [Value (Str "Use");
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
                     [Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "in");
                      App
                        (Lam ("obj",Var "obj"),
                         Value
                           (ParamArray
                              [Value (Str "Morale"); Value (Str "phase");
                               Value (Str "Select")]));
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
                        [Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "in");
                         App
                           (Lam ("obj",Var "obj"),
                            Value
                              (ParamArray
                                 [Value (Str "Morale"); Value (Str "phase");
                                  Value (Str "Select")]));
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
                     [Value (Str "Use");
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
                        [Value (Str "Use");
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
                     [Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "in your Movement phase");
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
                        [Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "in your Movement phase");
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
                     [Value (Str "Use");
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
                        [Value (Str "Use");
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
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem in");
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
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem in");
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
                     [Value (Str "Use");
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
                               Lam ("obj",Var "obj")]));
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
                        [Value (Str "Use");
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
                                  Lam ("obj",Var "obj")]));
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
                     [Value (Str "Use");
                      App (Lam ("obj",Var "obj"),Value (ParamArray []));
                      Value (Str "Stratagem when");
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
                      Value (Str "inflict one Mortal Wound on");
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
                        [Value (Str "Use");
                         App (Lam ("obj",Var "obj"),Value (ParamArray []));
                         Value (Str "Stratagem when");
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
                         Value (Str "inflict one Mortal Wound on");
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
                     [Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "at");
                      App
                        (Call Repeat,
                         Value (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
                      Value (Str "of your Movement phase");
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
                        [Value (Str "Use");
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
                         Value (Str "of your Movement phase");
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
                     [Value (Str "Use");
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
                        [Value (Str "Use");
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
                     [Value (Str "Use");
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
                        [Value (Str "Use");
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
                     [Value (Str "Use");
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
                      App
                        (Lam ("obj",Var "obj"),
                         Value (ParamArray [Value (Str "burrowing"); Var "Target"]));
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
                        [Value (Str "Use");
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
                         Value (Str "of");
                         App
                           (Lam ("obj",Var "obj"),
                            Value (ParamArray [Value (Str "burrowing"); Var "Target"]));
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
                     [Value (Str "Use");
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
                        [Value (Str "Use");
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
                     [Value (Str "Use");
                      App
                        (Call Repeat,
                         Value
                           (ParamArray [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                      Value (Str "after deployment but before");
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
                      Value (Str "Units fully within or on");
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
                        [Value (Str "Use");
                         App
                           (Call Repeat,
                            Value
                              (ParamArray
                                 [Value (Str "Stratagem"); Lam ("obj",Var "obj")]));
                         Value (Str "after deployment but before");
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
                         Value (Str "Units fully within or on");
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
                     [Value (Str "Use");
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
                        [Value (Str "Use");
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
