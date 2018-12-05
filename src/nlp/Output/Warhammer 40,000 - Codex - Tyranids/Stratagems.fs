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
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem in your Psychic phase if"); Value (Int 1);
                      Value (Str "Zoanthropes"); Var "Target";
                      Value (Str "from your army consisting of at least"); Value (Int 3);
                      Value (Str "is within"); Value (Distance 6); Value (Str "of");
                      Value (Int 2); Value (Str "other such If you do so");
                      Lam ("obj",Var "obj"); Value (Str "Zoanthropes can not take");
                      Lam ("obj",Var "obj"); Value (Str "Psychic");
                      Lam ("obj",Var "obj"); Value (Str "phase instead select");
                      Value (Int 1); Value (Str "point on"); Lam ("obj",Var "obj");
                      Value (Str "battlefield within"); Value (Distance 18);
                      Value (Str "of and visible to"); Lam ("obj",Var "obj");
                      Value (Str "three Roll"); Value (Int 1); Value (Str "for");
                      Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                      Var "Target"; Value (Str "friend or foe within");
                      Value (Distance 3); Value (Str "of"); Lam ("obj",Var "obj");
                      Value (Str "point Add"); Value (Int 1); Value (Str "to");
                      Lam ("obj",Var "obj"); Value (Str "result if");
                      Lam ("obj",Var "obj"); Var "Target";
                      Value (Str "being rolled for has"); Value (Int 10);
                      Value (Str "or more but subtract"); Value (Int 1);
                      Value (Str "if"); Lam ("obj",Var "obj"); Var "Target";
                      Value (Str "being rolled for is"); Value (Int 1);
                      Value (Str "CHARACTER On"); Value (Int 1);
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
                      Value (Str "that"); Var "Target"; Call Suffer;
                      Value (Str "3D3 mortal")]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "PSYCHIC BARRAGE"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem in your Psychic phase if"); Value (Int 1);
                         Value (Str "Zoanthropes"); Var "Target";
                         Value (Str "from your army consisting of at least");
                         Value (Int 3); Value (Str "is within"); Value (Distance 6);
                         Value (Str "of"); Value (Int 2);
                         Value (Str "other such If you do so"); Lam ("obj",Var "obj");
                         Value (Str "Zoanthropes can not take"); Lam ("obj",Var "obj");
                         Value (Str "Psychic"); Lam ("obj",Var "obj");
                         Value (Str "phase instead select"); Value (Int 1);
                         Value (Str "point on"); Lam ("obj",Var "obj");
                         Value (Str "battlefield within"); Value (Distance 18);
                         Value (Str "of and visible to"); Lam ("obj",Var "obj");
                         Value (Str "three Roll"); Value (Int 1); Value (Str "for");
                         Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                         Var "Target"; Value (Str "friend or foe within");
                         Value (Distance 3); Value (Str "of"); Lam ("obj",Var "obj");
                         Value (Str "point Add"); Value (Int 1); Value (Str "to");
                         Lam ("obj",Var "obj"); Value (Str "result if");
                         Lam ("obj",Var "obj"); Var "Target";
                         Value (Str "being rolled for has"); Value (Int 10);
                         Value (Str "or more but subtract"); Value (Int 1);
                         Value (Str "if"); Lam ("obj",Var "obj"); Var "Target";
                         Value (Str "being rolled for is"); Value (Int 1);
                         Value (Str "CHARACTER On"); Value (Int 1);
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
                         Value (Str "that"); Var "Target"; Call Suffer;
                         Value (Str "3D3 mortal")]),None))]),None)
    let ``RAPID REGENERATION`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("RAPID REGENERATION",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem at"); Lam ("obj",Var "obj");
                      Value
                        (Str
                           "end of your Movement phase Select a TYRANIDS model from your army It regains D3 lost earlier in");
                      Lam ("obj",Var "obj"); Value (Str "battle")]));
               ("2",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "RAPID REGENERATION"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem at"); Lam ("obj",Var "obj");
                         Value
                           (Str
                              "end of your Movement phase Select a TYRANIDS model from your army It regains D3 lost earlier in");
                         Lam ("obj",Var "obj"); Value (Str "battle")]),None))]),None)
    let ``CAUSTIC BLOOD`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("CAUSTIC BLOOD",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem at"); Lam ("obj",Var "obj");
                      Value (Str "start of"); Value (Int 1);
                      Value (Str "Fight phase Select"); Value (Int 1);
                      Value (Str "TYRANIDS"); Var "Target"; Value (Str "from your army");
                      Value (Int 1); Value (Str "whenever"); Value (Int 1);
                      Value (Str "model in"); Lam ("obj",Var "obj"); Var "Target";
                      Value (Str "is destroyed in"); Lam ("obj",Var "obj");
                      Value (Str "phase For");
                      Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                      Value (Str "roll of"); Value (Int 6); Lam ("obj",Var "obj");
                      Var "Target"; Var "Target"; Value (Str "that inflicted");
                      Lam ("obj",Var "obj"); Value (Str "final wound on");
                      Lam ("obj",Var "obj"); Value (Str "model"); Call Suffer;
                      Value (Int 1); Value (Str "mortal wound after");
                      Lam ("obj",Var "obj"); Value (Str "of their have been resolved")]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "CAUSTIC BLOOD"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem at"); Lam ("obj",Var "obj");
                         Value (Str "start of"); Value (Int 1);
                         Value (Str "Fight phase Select"); Value (Int 1);
                         Value (Str "TYRANIDS"); Var "Target";
                         Value (Str "from your army"); Value (Int 1);
                         Value (Str "whenever"); Value (Int 1); Value (Str "model in");
                         Lam ("obj",Var "obj"); Var "Target";
                         Value (Str "is destroyed in"); Lam ("obj",Var "obj");
                         Value (Str "phase For");
                         Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                         Value (Str "roll of"); Value (Int 6); Lam ("obj",Var "obj");
                         Var "Target"; Var "Target"; Value (Str "that inflicted");
                         Lam ("obj",Var "obj"); Value (Str "final wound on");
                         Lam ("obj",Var "obj"); Value (Str "model"); Call Suffer;
                         Value (Int 1); Value (Str "mortal wound after");
                         Lam ("obj",Var "obj");
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
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem when"); Value (Int 1);
                      Value (Str "TYRANIDS"); Var "Target";
                      Value (Str "from your army is selected to attack in");
                      Lam ("obj",Var "obj"); Value (Str "Shooting phase You can add");
                      Value (Int 1); Value (Str "to"); Lam ("obj",Var "obj");
                      Value (Str "wound made for"); Lam ("obj",Var "obj"); Var "Target";
                      Value (Str "'s fleshborer or fleshborer hive in");
                      Lam ("obj",Var "obj"); Value (Str "Shooting phase")]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "SCORCH BUGS"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem when"); Value (Int 1);
                         Value (Str "TYRANIDS"); Var "Target";
                         Value (Str "from your army is selected to attack in");
                         Lam ("obj",Var "obj"); Value (Str "Shooting phase You can add");
                         Value (Int 1); Value (Str "to"); Lam ("obj",Var "obj");
                         Value (Str "wound made for"); Lam ("obj",Var "obj");
                         Var "Target"; Value (Str "'s fleshborer or fleshborer hive in");
                         Lam ("obj",Var "obj"); Value (Str "Shooting phase")]),None))]),
           None)
    let ``IMPLANT ATTACK`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("IMPLANT ATTACK",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem after"); Value (Int 1);
                      Value (Str "TYRANIDS"); Var "Target";
                      Value (Str "from your army in"); Lam ("obj",Var "obj");
                      Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                      Value (Int 1); Value (Str "for");
                      Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                      Var "Target"; Value (Str "model other than"); Value (Int 1);
                      Value (Str "VEHICLE that was wounded by"); Lam ("obj",Var "obj");
                      Value (Str "of"); Lam ("obj",Var "obj"); Var "Target";
                      Value (Str "'s and not slain On"); Value (Int 1);
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
                                  Value (ParamArray [Var "roll"; Value (Int 2)])),
                               App (Call Or,Value (ParamArray [Var "eq"; Var "gt"])))));
                      Lam ("obj",Var "obj"); Value (Str "model"); Call Suffer;
                      Value (Int 1); Value (Str "Mortal Wound")]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "IMPLANT ATTACK"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem after"); Value (Int 1);
                         Value (Str "TYRANIDS"); Var "Target";
                         Value (Str "from your army in"); Lam ("obj",Var "obj");
                         Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                         Value (Int 1); Value (Str "for");
                         Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                         Var "Target"; Value (Str "model other than"); Value (Int 1);
                         Value (Str "VEHICLE that was wounded by");
                         Lam ("obj",Var "obj"); Value (Str "of"); Lam ("obj",Var "obj");
                         Var "Target"; Value (Str "'s and not slain On"); Value (Int 1);
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
                                     Value (ParamArray [Var "roll"; Value (Int 2)])),
                                  App (Call Or,Value (ParamArray [Var "eq"; Var "gt"])))));
                         Lam ("obj",Var "obj"); Value (Str "model"); Call Suffer;
                         Value (Int 1); Value (Str "Mortal Wound")]),None))]),None)
    let ``BOUNTY OF THE HIVE FLEET`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("BOUNTY OF THE HIVE FLEET",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem before"); Lam ("obj",Var "obj");
                      Value (Str "battle Your army can have one extra Bio-artefact for");
                      Value (Int 1); Value (Str "CP or two extra for 3 CPs All of");
                      Lam ("obj",Var "obj");
                      Value
                        (Str
                           "Bio-artefacts that you include must be different and be given to different CHARACTERS You can only use");
                      Lam ("obj",Var "obj"); Value (Str "Stratagem once per battle")]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value
                        (ParamArray [Var "Available CP"; Var "BOUNTY OF THE HIVE FLEET"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem before"); Lam ("obj",Var "obj");
                         Value
                           (Str "battle Your army can have one extra Bio-artefact for");
                         Value (Int 1); Value (Str "CP or two extra for 3 CPs All of");
                         Lam ("obj",Var "obj");
                         Value
                           (Str
                              "Bio-artefacts that you include must be different and be given to different CHARACTERS You can only use");
                         Lam ("obj",Var "obj"); Value (Str "Stratagem once per battle")]),
                   None));
               ("3",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value
                        (ParamArray [Var "Available CP"; Var "BOUNTY OF THE HIVE FLEET"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem before"); Lam ("obj",Var "obj");
                         Value
                           (Str "battle Your army can have one extra Bio-artefact for");
                         Value (Int 1); Value (Str "CP or two extra for 3 CPs All of");
                         Lam ("obj",Var "obj");
                         Value
                           (Str
                              "Bio-artefacts that you include must be different and be given to different CHARACTERS You can only use");
                         Lam ("obj",Var "obj"); Value (Str "Stratagem once per battle")]),
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
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem in your Movement phase after moving");
                      Value (Int 1); Value (Str "TYRANIDS"); Var "Target";
                      Value (Str "from your army You can make"); Value (Int 1);
                      Value (Str "second move with"); Lam ("obj",Var "obj");
                      Var "Target";
                      Value
                        (Str
                           "including Advancing if you wish but when you do so you must roll");
                      Value (Int 1); Value (Str "for");
                      Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                      Value (Str "model in"); Lam ("obj",Var "obj"); Var "Target";
                      Value (Str "For");
                      Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                      Value (Str "roll of"); Value (Int 1); Value (Str "inflict");
                      Value (Int 1); Value (Str "Mortal Wound on");
                      Lam ("obj",Var "obj"); Var "Target"; Lam ("obj",Var "obj");
                      Var "Target"; Value (Str "can not shoot or make"); Value (Int 1);
                      Value (Str "charge move"); Lam ("obj",Var "obj");
                      Value (Str "turn")]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "METABOLIC OVERDRIVE"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem in your Movement phase after moving");
                         Value (Int 1); Value (Str "TYRANIDS"); Var "Target";
                         Value (Str "from your army You can make"); Value (Int 1);
                         Value (Str "second move with"); Lam ("obj",Var "obj");
                         Var "Target";
                         Value
                           (Str
                              "including Advancing if you wish but when you do so you must roll");
                         Value (Int 1); Value (Str "for");
                         Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                         Value (Str "model in"); Lam ("obj",Var "obj"); Var "Target";
                         Value (Str "For");
                         Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                         Value (Str "roll of"); Value (Int 1); Value (Str "inflict");
                         Value (Int 1); Value (Str "Mortal Wound on");
                         Lam ("obj",Var "obj"); Var "Target"; Lam ("obj",Var "obj");
                         Var "Target"; Value (Str "can not shoot or make");
                         Value (Int 1); Value (Str "charge move"); Lam ("obj",Var "obj");
                         Value (Str "turn")]),None))]),None)
    let ``FEEDER TENDRILS`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("FEEDER TENDRILS",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem when"); Value (Int 1);
                      Value
                        (Str
                           "Genestealer LICTOR Toxicrene or Venomthrope from your army kills");
                      Value (Int 1); Value (Str "CHARACTER in"); Lam ("obj",Var "obj");
                      Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                      Value (Str "Gain D3 Command Points")]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "FEEDER TENDRILS"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem when"); Value (Int 1);
                         Value
                           (Str
                              "Genestealer LICTOR Toxicrene or Venomthrope from your army kills");
                         Value (Int 1); Value (Str "CHARACTER in");
                         Lam ("obj",Var "obj");
                         Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
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
                      Value (Str "Stratagem in your Shooting phase"); Value (Int 1);
                      Value (Str "TYRANIDS MONSTER from your army Increase");
                      Lam ("obj",Var "obj"); Value (Str "Damage of its by");
                      Value (Int 1); Value (Str "for"); Lam ("obj",Var "obj");
                      Value (Str "phase")]));
               ("2",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "PATHOGENIC SLIME"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem in your Shooting phase"); Value (Int 1);
                         Value (Str "TYRANIDS MONSTER from your army Increase");
                         Lam ("obj",Var "obj"); Value (Str "Damage of its by");
                         Value (Int 1); Value (Str "for"); Lam ("obj",Var "obj");
                         Value (Str "phase")]),None))]),None)
    let ``VORACIOUS APPETITE`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("VORACIOUS APPETITE",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem in"); Lam ("obj",Var "obj");
                      Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                      Value (Str "when"); Value (Int 1);
                      Value
                        (Str
                           "TYRANIDS MONSTER or CHARACTER from your army is chosen to attack You can re-roll");
                      Lam ("obj",Var "obj"); Value (Str "failed wound for");
                      Lam ("obj",Var "obj"); Value (Str "model until");
                      Lam ("obj",Var "obj"); Value (Str "end of"); Lam ("obj",Var "obj");
                      Value (Str "phase")]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "VORACIOUS APPETITE"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem in"); Lam ("obj",Var "obj");
                         Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                         Value (Str "when"); Value (Int 1);
                         Value
                           (Str
                              "TYRANIDS MONSTER or CHARACTER from your army is chosen to attack You can re-roll");
                         Lam ("obj",Var "obj"); Value (Str "failed wound for");
                         Lam ("obj",Var "obj"); Value (Str "model until");
                         Lam ("obj",Var "obj"); Value (Str "end of");
                         Lam ("obj",Var "obj"); Value (Str "phase")]),None))]),None)
    let ``PHEROMONE TRAIL`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("PHEROMONE TRAIL",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem when"); Value (Int 1);
                      Value (Str "TYRANIDS INFANTRY"); Var "Target";
                      Value (Str "from your army is set up on"); Lam ("obj",Var "obj");
                      Value (Str "battlefield as if there is already"); Value (Int 1);
                      Value (Str "LICTOR from your army on"); Lam ("obj",Var "obj");
                      Value (Str "battlefield You can set up"); Lam ("obj",Var "obj");
                      Var "Target"; Value (Str "wholly within"); Value (Distance 6);
                      Value (Str "of"); Lam ("obj",Var "obj");
                      Value (Str "LICTOR and more than"); Value (Distance 9);
                      Value (Str "from"); Lam ("obj",Var "obj"); Var "Target";
                      Value (Str "rather than following"); Lam ("obj",Var "obj");
                      Value (Str "normal for setting up"); Lam ("obj",Var "obj");
                      Var "Target"]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "PHEROMONE TRAIL"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem when"); Value (Int 1);
                         Value (Str "TYRANIDS INFANTRY"); Var "Target";
                         Value (Str "from your army is set up on");
                         Lam ("obj",Var "obj");
                         Value (Str "battlefield as if there is already"); Value (Int 1);
                         Value (Str "LICTOR from your army on"); Lam ("obj",Var "obj");
                         Value (Str "battlefield You can set up"); Lam ("obj",Var "obj");
                         Var "Target"; Value (Str "wholly within"); Value (Distance 6);
                         Value (Str "of"); Lam ("obj",Var "obj");
                         Value (Str "LICTOR and more than"); Value (Distance 9);
                         Value (Str "from"); Lam ("obj",Var "obj"); Var "Target";
                         Value (Str "rather than following"); Lam ("obj",Var "obj");
                         Value (Str "normal for setting up"); Lam ("obj",Var "obj");
                         Var "Target"]),None))]),None)
    let ``SINGLE-MINDED ANNIHILATION`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("SINGLE-MINDED ANNIHILATION",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem at"); Lam ("obj",Var "obj");
                      Value (Str "end of your Shooting phase Select"); Value (Int 1);
                      Value (Str "TYRANIDS INFANTRY"); Var "Target";
                      Value (Str "from your army that"); Var "Target";
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
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem at"); Lam ("obj",Var "obj");
                         Value (Str "end of your Shooting phase Select"); Value (Int 1);
                         Value (Str "TYRANIDS INFANTRY"); Var "Target";
                         Value (Str "from your army that"); Var "Target";
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
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem at"); Lam ("obj",Var "obj");
                      Value (Str "end of your Psychic phase"); Value (Int 1);
                      Value (Str "TYRANIDS PSYKER"); Var "Target";
                      Value (Str "from your army that manifested"); Value (Int 1);
                      Value (Str "psychic power"); Lam ("obj",Var "obj");
                      Value
                        (Str
                           "turn It can immediately attempt to manifest one additional psychic power");
                      Lam ("obj",Var "obj"); Value (Str "turn")]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value
                        (ParamArray [Var "Available CP"; Var "POWER OF THE HIVE MIND"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem at"); Lam ("obj",Var "obj");
                         Value (Str "end of your Psychic phase"); Value (Int 1);
                         Value (Str "TYRANIDS PSYKER"); Var "Target";
                         Value (Str "from your army that manifested"); Value (Int 1);
                         Value (Str "psychic power"); Lam ("obj",Var "obj");
                         Value
                           (Str
                              "turn It can immediately attempt to manifest one additional psychic power");
                         Lam ("obj",Var "obj"); Value (Str "turn")]),None))]),None)
    let ``DEATH FRENZY`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("DEATH FRENZY",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem when"); Value (Int 1);
                      Value (Str "TYRANIDS CHARACTER from your army is slain");
                      Lam ("obj",Var "obj");
                      Value
                        (Str
                           "Hive Mind compels it to one final attack and it can immediately");
                      Lam ("obj",Var "obj");
                      Value
                        (Str
                           "shoot as if it were your Shooting phase or fight as if it were your");
                      Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                      Value (Str "before it is removed from"); Lam ("obj",Var "obj");
                      Value (Str "battlefield")]));
               ("2",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "DEATH FRENZY"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem when"); Value (Int 1);
                         Value (Str "TYRANIDS CHARACTER from your army is slain");
                         Lam ("obj",Var "obj");
                         Value
                           (Str
                              "Hive Mind compels it to one final attack and it can immediately");
                         Lam ("obj",Var "obj");
                         Value
                           (Str
                              "shoot as if it were your Shooting phase or fight as if it were your");
                         Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                         Value (Str "before it is removed from"); Lam ("obj",Var "obj");
                         Value (Str "battlefield")]),None))]),None)
    let ``GRISLY FEAST`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("GRISLY FEAST",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem in"); Lam ("obj",Var "obj");
                      Value (Str "Morale phase Select"); Value (Int 1); Var "Target";
                      Value
                        (Str
                           "of Ripper Swarms or Haruspex from your army Your opponent must add");
                      Value (Int 1); Value (Str "to"); Lam ("obj",Var "obj");
                      Value (Str "Morale taken for"); Var "Target";
                      Value (Str "that are within"); Value (Distance 6);
                      Value (Str "of"); Lam ("obj",Var "obj"); Var "Target";
                      Value (Str "in"); Lam ("obj",Var "obj"); Value (Str "phase")]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "GRISLY FEAST"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem in"); Lam ("obj",Var "obj");
                         Value (Str "Morale phase Select"); Value (Int 1); Var "Target";
                         Value
                           (Str
                              "of Ripper Swarms or Haruspex from your army Your opponent must add");
                         Value (Int 1); Value (Str "to"); Lam ("obj",Var "obj");
                         Value (Str "Morale taken for"); Var "Target";
                         Value (Str "that are within"); Value (Distance 6);
                         Value (Str "of"); Lam ("obj",Var "obj"); Var "Target";
                         Value (Str "in"); Lam ("obj",Var "obj"); Value (Str "phase")]),
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
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem when"); Value (Int 1);
                      Value (Str "TYRANIDS"); Var "Target";
                      Value (Str "from your army destroys"); Value (Int 1); Var "Target";
                      Value (Str "in"); Lam ("obj",Var "obj");
                      Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                      Value (Str "and is not within"); Value (Distance 3);
                      Value (Str "of"); Lam ("obj",Var "obj"); Var "Target";
                      Var "Target"; Value (Str "Instead of consolidating");
                      Lam ("obj",Var "obj"); Var "Target";
                      Value
                        (Str
                           "can move and Advance as if it were your Movement phase it can not move within");
                      Value (Distance 1); Value (Str "of"); Lam ("obj",Var "obj");
                      Var "Target"]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "OVERRUN"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem when"); Value (Int 1);
                         Value (Str "TYRANIDS"); Var "Target";
                         Value (Str "from your army destroys"); Value (Int 1);
                         Var "Target"; Value (Str "in"); Lam ("obj",Var "obj");
                         Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                         Value (Str "and is not within"); Value (Distance 3);
                         Value (Str "of"); Lam ("obj",Var "obj"); Var "Target";
                         Var "Target"; Value (Str "Instead of consolidating");
                         Lam ("obj",Var "obj"); Var "Target";
                         Value
                           (Str
                              "can move and Advance as if it were your Movement phase it can not move within");
                         Value (Distance 1); Value (Str "of"); Lam ("obj",Var "obj");
                         Var "Target"]),None))]),None)
    let ``INVISIBLE HUNTER`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("INVISIBLE HUNTER",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem in your Movement phase"); Value (Int 1);
                      Value (Str "LICTOR from your army that is within");
                      Value (Distance 1); Value (Str "of"); Lam ("obj",Var "obj");
                      Var "Target"; Var "Target"; Lam ("obj",Var "obj");
                      Value (Str "model can Fall Back shoot and charge in");
                      Lam ("obj",Var "obj"); Value (Str "turn")]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "INVISIBLE HUNTER"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem in your Movement phase"); Value (Int 1);
                         Value (Str "LICTOR from your army that is within");
                         Value (Distance 1); Value (Str "of"); Lam ("obj",Var "obj");
                         Var "Target"; Var "Target"; Lam ("obj",Var "obj");
                         Value (Str "model can Fall Back shoot and charge in");
                         Lam ("obj",Var "obj"); Value (Str "turn")]),None))]),None)
    let ``SPOREFIELD`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("SPOREFIELD",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem after"); Lam ("obj",Var "obj");
                      Value (Str "have deployed but before"); Lam ("obj",Var "obj");
                      Value
                        (Str
                           "battle begins You can add up to two of Spore Mines to your army as and set them up anywhere on");
                      Lam ("obj",Var "obj"); Value (Str "battlefield that is more than");
                      Value (Distance 12); Value (Str "from"); Var "Target"]));
               ("3",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "SPOREFIELD"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem after"); Lam ("obj",Var "obj");
                         Value (Str "have deployed but before"); Lam ("obj",Var "obj");
                         Value
                           (Str
                              "battle begins You can add up to two of Spore Mines to your army as and set them up anywhere on");
                         Lam ("obj",Var "obj");
                         Value (Str "battlefield that is more than");
                         Value (Distance 12); Value (Str "from"); Var "Target"]),None))]),
           None)
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
                      Value (Str "Stratagem in"); Lam ("obj",Var "obj");
                      Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                      Value (Str "Select"); Lam ("obj",Var "obj"); Var "Target";
                      Var "Target"; Value (Str "that is within"); Value (Distance 1);
                      Value (Str "of at least one LEVIATHAN"); Var "Target";
                      Value
                        (Str
                           "from your army that can FLY and at least one that can not You can re-roll hit and wound of");
                      Value (Int 1); Value (Str "in"); Lam ("obj",Var "obj");
                      Value (Str "phase for for LEVIATHAN that target");
                      Lam ("obj",Var "obj"); Var "Target"; Var "Target"]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "WAR ON ALL FRONTS"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem in"); Lam ("obj",Var "obj");
                         Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                         Value (Str "Select"); Lam ("obj",Var "obj"); Var "Target";
                         Var "Target"; Value (Str "that is within"); Value (Distance 1);
                         Value (Str "of at least one LEVIATHAN"); Var "Target";
                         Value
                           (Str
                              "from your army that can FLY and at least one that can not You can re-roll hit and wound of");
                         Value (Int 1); Value (Str "in"); Lam ("obj",Var "obj");
                         Value (Str "phase for for LEVIATHAN that target");
                         Lam ("obj",Var "obj"); Var "Target"; Var "Target"]),None))]),
           None)
    let ``HYPER-TOXICITY`` = 
        IfThenElse
          (App (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "GORGON")])),
           Choice
             ("HYPER-TOXICITY",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem in"); Lam ("obj",Var "obj");
                      Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                      Value (Int 1); Value (Str "GORGON"); Var "Target";
                      Value (Str "from your army that has"); Lam ("obj",Var "obj");
                      Value (Str "toxin biomorph For"); Lam ("obj",Var "obj");
                      Value (Str "duration of"); Lam ("obj",Var "obj");
                      Value (Str "phase"); Lam ("obj",Var "obj");
                      Value (Str "toxin biomorph causes"); Value (Int 1);
                      Value (Str "additional damage on wound of"); Value (Int 5);
                      Value (Str "+ rather than"); Value (Int 6);
                      Value (Str "+ for made by"); Lam ("obj",Var "obj"); Var "Target"]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "HYPER-TOXICITY"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem in"); Lam ("obj",Var "obj");
                         Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                         Value (Int 1); Value (Str "GORGON"); Var "Target";
                         Value (Str "from your army that has"); Lam ("obj",Var "obj");
                         Value (Str "toxin biomorph For"); Lam ("obj",Var "obj");
                         Value (Str "duration of"); Lam ("obj",Var "obj");
                         Value (Str "phase"); Lam ("obj",Var "obj");
                         Value (Str "toxin biomorph causes"); Value (Int 1);
                         Value (Str "additional damage on wound of"); Value (Int 5);
                         Value (Str "+ rather than"); Value (Int 6);
                         Value (Str "+ for made by"); Lam ("obj",Var "obj");
                         Var "Target"]),None))]),None)
    let ``BRUTE FORCE`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "BEHEMOTH")])),
           Choice
             ("BRUTE FORCE",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem when"); Value (Int 1);
                      Value (Str "BEHEMOTH"); Var "Target";
                      Value (Str "from your army completes"); Value (Int 1);
                      Value (Str "charge move Roll"); Value (Int 1); Value (Str "for");
                      Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                      Value (Str "model in"); Lam ("obj",Var "obj");
                      Value (Str "charging"); Var "Target"; Value (Str "that is within");
                      Value (Distance 1); Value (Str "of"); Lam ("obj",Var "obj");
                      Var "Target"; Var "Target"; Value (Str "For");
                      Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                      Value (Str "roll of"); Value (Int 6); Value (Str "or");
                      Value (Int 2); Value (Str "+ for"); Value (Int 1);
                      Value (Str "MONSTER inflict one Mortal Wound on");
                      Lam ("obj",Var "obj"); Var "Target"; Var "Target";
                      Value (Str "within"); Value (Distance 1)]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "BRUTE FORCE"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem when"); Value (Int 1);
                         Value (Str "BEHEMOTH"); Var "Target";
                         Value (Str "from your army completes"); Value (Int 1);
                         Value (Str "charge move Roll"); Value (Int 1);
                         Value (Str "for");
                         Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                         Value (Str "model in"); Lam ("obj",Var "obj");
                         Value (Str "charging"); Var "Target";
                         Value (Str "that is within"); Value (Distance 1);
                         Value (Str "of"); Lam ("obj",Var "obj"); Var "Target";
                         Var "Target"; Value (Str "For");
                         Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                         Value (Str "roll of"); Value (Int 6); Value (Str "or");
                         Value (Int 2); Value (Str "+ for"); Value (Int 1);
                         Value (Str "MONSTER inflict one Mortal Wound on");
                         Lam ("obj",Var "obj"); Var "Target"; Var "Target";
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
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem at"); Lam ("obj",Var "obj");
                      Value (Str "end of your Movement phase"); Value (Int 1);
                      Var "Target";
                      Value (Str "of Termagants Hormagaunts or Gargoyles or");
                      Lam ("obj",Var "obj"); Value (Str "HYDRA INFANTRY"); Var "Target";
                      Value
                        (Str "from your army that has been completely destroyed Add");
                      Lam ("obj",Var "obj"); Value (Str "identical"); Var "Target";
                      Value (Str "to your army and set it up as wholly within");
                      Value (Distance 6); Value (Str "of"); Lam ("obj",Var "obj");
                      Value (Str "board edge more than"); Value (Distance 9);
                      Value (Str "from"); Var "Target"]));
               ("2",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "ENDLESS SWARM"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem at"); Lam ("obj",Var "obj");
                         Value (Str "end of your Movement phase"); Value (Int 1);
                         Var "Target";
                         Value (Str "of Termagants Hormagaunts or Gargoyles or");
                         Lam ("obj",Var "obj"); Value (Str "HYDRA INFANTRY");
                         Var "Target";
                         Value
                           (Str "from your army that has been completely destroyed Add");
                         Lam ("obj",Var "obj"); Value (Str "identical"); Var "Target";
                         Value (Str "to your army and set it up as wholly within");
                         Value (Distance 6); Value (Str "of"); Lam ("obj",Var "obj");
                         Value (Str "board edge more than"); Value (Distance 9);
                         Value (Str "from"); Var "Target"]),None))]),None)
    let ``OPPORTUNISTIC ADVANCE`` = 
        IfThenElse
          (App (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "KRAKEN")])),
           Choice
             ("OPPORTUNISTIC ADVANCE",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem in your Movement phase when you roll");
                      Lam ("obj",Var "obj"); Value (Str "for"); Lam ("obj",Var "obj");
                      Value (Str "Advancing KRAKEN"); Var "Target";
                      Value (Str "other than"); Value (Int 1); Var "Target";
                      Value (Str "that can FLY You can double"); Lam ("obj",Var "obj");
                      Value (Str "number you roll and add"); Lam ("obj",Var "obj");
                      Value (Str "total to their Move characteristic for");
                      Lam ("obj",Var "obj");
                      Value (Str "Movement phase rather than following");
                      Lam ("obj",Var "obj"); Value (Str "normal for Advancing")]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value
                        (ParamArray [Var "Available CP"; Var "OPPORTUNISTIC ADVANCE"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem in your Movement phase when you roll");
                         Lam ("obj",Var "obj"); Value (Str "for"); Lam ("obj",Var "obj");
                         Value (Str "Advancing KRAKEN"); Var "Target";
                         Value (Str "other than"); Value (Int 1); Var "Target";
                         Value (Str "that can FLY You can double");
                         Lam ("obj",Var "obj"); Value (Str "number you roll and add");
                         Lam ("obj",Var "obj");
                         Value (Str "total to their Move characteristic for");
                         Lam ("obj",Var "obj");
                         Value (Str "Movement phase rather than following");
                         Lam ("obj",Var "obj"); Value (Str "normal for Advancing")]),
                   None))]),None)
    let ``CALL THE BROOD`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("CALL THE BROOD",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem at"); Lam ("obj",Var "obj");
                      Value (Str "end of your Movement phase Add"); Value (Int 1);
                      Value (Str "new"); Var "Target"; Value (Str "of up to");
                      Value (Int 5);
                      Value (Str "to your army and set them up as wholly within");
                      Value (Distance 6); Value (Str "of"); Value (Int 1);
                      Value
                        (Str
                           "Broodlord or infestation node from your army and more than");
                      Value (Distance 9); Value (Str "from"); Lam ("obj",Var "obj");
                      Var "Target"]));
               ("3",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "CALL THE BROOD"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem at"); Lam ("obj",Var "obj");
                         Value (Str "end of your Movement phase Add"); Value (Int 1);
                         Value (Str "new"); Var "Target"; Value (Str "of up to");
                         Value (Int 5);
                         Value (Str "to your army and set them up as wholly within");
                         Value (Distance 6); Value (Str "of"); Value (Int 1);
                         Value
                           (Str
                              "Broodlord or infestation node from your army and more than");
                         Value (Distance 9); Value (Str "from"); Lam ("obj",Var "obj");
                         Var "Target"]),None))]),None)
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
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem when you set up"); Value (Int 1);
                      Value (Str "JORMUNGANDR INFANTRY"); Var "Target";
                      Value
                        (Str
                           "during deployment It is set up within bored before battle Whenever you set up");
                      Value (Int 1); Var "Target"; Value (Str "of Raveners");
                      Value (Int 1); Value (Str "Mawloc Trygon or"); Value (Int 1);
                      Value (Str "Trygon Prime at"); Lam ("obj",Var "obj");
                      Value (Str "end of your Movement phase"); Value (Int 1);
                      Value (Str "burrowing"); Var "Target";
                      Value (Str "you can also set up"); Lam ("obj",Var "obj");
                      Value (Str "number of you set up within"); Lam ("obj",Var "obj");
                      Value (Str "Set up"); Lam ("obj",Var "obj"); Var "Target";
                      Value (Str "wholly within"); Value (Distance 3); Value (Str "of");
                      Lam ("obj",Var "obj"); Value (Str "burrowing"); Var "Target";
                      Value (Str "and more than"); Value (Distance 9);
                      Value (Str "from"); Lam ("obj",Var "obj"); Var "Target";
                      Lam ("obj",Var "obj"); Value (Str "you can not set up in");
                      Lam ("obj",Var "obj");
                      Value (Str "way when you do so are destroyed")]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "THE ENEMY BELOW"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem when you set up"); Value (Int 1);
                         Value (Str "JORMUNGANDR INFANTRY"); Var "Target";
                         Value
                           (Str
                              "during deployment It is set up within bored before battle Whenever you set up");
                         Value (Int 1); Var "Target"; Value (Str "of Raveners");
                         Value (Int 1); Value (Str "Mawloc Trygon or"); Value (Int 1);
                         Value (Str "Trygon Prime at"); Lam ("obj",Var "obj");
                         Value (Str "end of your Movement phase"); Value (Int 1);
                         Value (Str "burrowing"); Var "Target";
                         Value (Str "you can also set up"); Lam ("obj",Var "obj");
                         Value (Str "number of you set up within");
                         Lam ("obj",Var "obj"); Value (Str "Set up");
                         Lam ("obj",Var "obj"); Var "Target";
                         Value (Str "wholly within"); Value (Distance 3);
                         Value (Str "of"); Lam ("obj",Var "obj");
                         Value (Str "burrowing"); Var "Target";
                         Value (Str "and more than"); Value (Distance 9);
                         Value (Str "from"); Lam ("obj",Var "obj"); Var "Target";
                         Lam ("obj",Var "obj"); Value (Str "you can not set up in");
                         Lam ("obj",Var "obj");
                         Value (Str "way when you do so are destroyed")]),None))]),None)
    let ``THE DEEPEST SHADOW`` = 
        IfThenElse
          (App (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "KRONOS")])),
           Choice
             ("THE DEEPEST SHADOW",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem when"); Lam ("obj",Var "obj"); Var "Target";
                      Value (Str "PSYKER attempts to manifest"); Value (Int 1);
                      Value (Str "psychic power within"); Value (Distance 24);
                      Value (Str "of"); Value (Int 1); Value (Str "KRONOS");
                      Var "Target";
                      Value (Str "from your army Your opponent can only roll");
                      Value (Int 1); Value (Str "single for"); Lam ("obj",Var "obj");
                      Value (Str "Psychic test")]));
               ("1",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "THE DEEPEST SHADOW"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem when"); Lam ("obj",Var "obj");
                         Var "Target"; Value (Str "PSYKER attempts to manifest");
                         Value (Int 1); Value (Str "psychic power within");
                         Value (Distance 24); Value (Str "of"); Value (Int 1);
                         Value (Str "KRONOS"); Var "Target";
                         Value (Str "from your army Your opponent can only roll");
                         Value (Int 1); Value (Str "single for"); Lam ("obj",Var "obj");
                         Value (Str "Psychic test")]),None))]),None)
    let ``DIGESTIVE DENIAL`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("DIGESTIVE DENIAL",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem after deployment but before");
                      Lam ("obj",Var "obj");
                      Value (Str "first battle round begins Choose"); Value (Int 1);
                      Value (Str "piece of terrain other than"); Value (Int 1);
                      Value (Str "Fortification Units fully within or on");
                      Lam ("obj",Var "obj"); Value (Str "piece of terrain do not gain");
                      Lam ("obj",Var "obj");
                      Value (Str "bonus to their saving throws for being in cover")]));
               ("2",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "DIGESTIVE DENIAL"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem after deployment but before");
                         Lam ("obj",Var "obj");
                         Value (Str "first battle round begins Choose"); Value (Int 1);
                         Value (Str "piece of terrain other than"); Value (Int 1);
                         Value (Str "Fortification Units fully within or on");
                         Lam ("obj",Var "obj");
                         Value (Str "piece of terrain do not gain");
                         Lam ("obj",Var "obj");
                         Value (Str "bonus to their saving throws for being in cover")]),
                   None))]),None)
    let ``ADRENALINE SURGE`` = 
        IfThenElse
          (App
             (Call Contains,Value (ParamArray [Var "Keywords"; Value (Str "TYRANIDS")])),
           Choice
             ("ADRENALINE SURGE",
              [("<Not selected>",
                Value
                  (ParamArray
                     [Value (Str "Use"); Lam ("obj",Var "obj");
                      Value (Str "Stratagem at"); Lam ("obj",Var "obj");
                      Value (Str "end of"); Lam ("obj",Var "obj");
                      Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                      Value (Str "Select"); Value (Int 1); Value (Str "TYRANIDS");
                      Var "Target"; Value (Str "from your army"); Lam ("obj",Var "obj");
                      Var "Target"; Value (Str "can immediately fight again")]));
               ("3",
                IfThenElse
                  (App
                     (Call GreaterThan,
                      Value (ParamArray [Var "Available CP"; Var "ADRENALINE SURGE"])),
                   Value
                     (ParamArray
                        [Value (Str "Use"); Lam ("obj",Var "obj");
                         Value (Str "Stratagem at"); Lam ("obj",Var "obj");
                         Value (Str "end of"); Lam ("obj",Var "obj");
                         Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                         Value (Str "Select"); Value (Int 1); Value (Str "TYRANIDS");
                         Var "Target"; Value (Str "from your army");
                         Lam ("obj",Var "obj"); Var "Target";
                         Value (Str "can immediately fight again")]),None))]),None)
