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
             [Value (Str "Throughout"); Lam ("obj",Var "obj");
              Value (Str "section you will come across"); Value (Int 1);
              Value (Str "keyword that is within angular specifically <HIVE FLEET>");
              Lam ("obj",Var "obj"); Value (Str "is shorthand for"); Value (Int 1);
              Value (Str "keyword of your own choosing as described below")])
    let ``<HIVE FLEET>`` = 
        Value
          (ParamArray
             [Lam ("obj",Var "obj"); Value (Str "Tyranids belong to"); Value (Int 1);
              Value (Str "hive fleet When you include"); Value (Int 1);
              Value (Str "Tyranids"); Var "Target";
              Value (Str "in your army you must nominate which hive fleet that");
              Var "Target";
              Value
                (Str "is from There are many different hive to choose from you can use");
              Lam ("obj",Var "obj"); Value (Str "of"); Lam ("obj",Var "obj");
              Value (Str "hive described in"); Lam ("obj",Var "obj");
              Value
                (Str "book or make up your own if you prefer You then simply replace");
              Lam ("obj",Var "obj"); Value (Str "<HIVE FLEET> keyword in");
              Lam ("obj",Var "obj"); Value (Str "instance on"); Lam ("obj",Var "obj");
              Var "Target"; Value (Str "'s datasheet and in"); Lam ("obj",Var "obj");
              Value (Str "psychic they know with"); Lam ("obj",Var "obj");
              Value
                (Str "name of your chosen hive fleet For example if you were to include");
              Value (Int 1);
              Value
                (Str
                   "Tervigon in your army and you decided it was from Hive Fleet Kraken then its <HIVE FLEET> keyword is changed to KRAKEN and its Brood Progenitor");
              Value (Distance 0); Value (Str "ability would say You can re-roll hit of");
              Value (Int 1); Value (Str "in"); Lam ("obj",Var "obj");
              Value (Str "Shooting phase for friendly KRAKEN Termagant within");
              Value (Distance 6); Value (Str "of"); Lam ("obj",Var "obj");
              Value (Str "model"); Value (Distance 0)])
    let ``SYNAPSE`` = 
        Value
          (ParamArray
             [Value
                (Str "<HIVE FLEET> automatically pass Morale tests if they are within");
              Value (Distance 12); Value (Str "of"); Lam ("obj",Var "obj");
              Value (Str "friendly <HIVE FLEET> with"); Lam ("obj",Var "obj");
              Value (Str "ability")])
    let ``INSTINCTIVE BEHAVIOUR`` = 
        Value
          (ParamArray
             [Value (Str "Unless"); Value (Int 1); Value (Str "<HIVE FLEET>");
              Var "Target"; Value (Str "with"); Lam ("obj",Var "obj");
              Value (Str "ability is within"); Value (Distance 24); Value (Str "of");
              Lam ("obj",Var "obj"); Value (Str "friendly <HIVE FLEET> SYNAPSE");
              Var "Target"; Value (Str "you must subtract"); Value (Int 1);
              Value (Str "from"); Lam ("obj",Var "obj");
              Value (Str "hit made for it when shooting"); Lam ("obj",Var "obj");
              Value (Str "target other than"); Lam ("obj",Var "obj");
              Value (Str "nearest visible"); Var "Target"; Var "Target";
              Value (Str "and you must subtract"); Value (Int 2);
              Value (Str "from its charge roll if it declares"); Value (Int 1);
              Value (Str "charge against"); Lam ("obj",Var "obj"); Var "Target";
              Value (Str "other than"); Lam ("obj",Var "obj"); Value (Str "nearest");
              Var "Target"; Var "Target"])
    let ``SHADOW IN THE WARP`` = 
        Value
          (ParamArray
             [Value (Str "Enemy must subtract"); Value (Int 1); Value (Str "from");
              Lam ("obj",Var "obj"); Value (Str "Psychic they make if they are within");
              Value (Distance 18); Value (Str "of"); Lam ("obj",Var "obj");
              Value (Str "with"); Lam ("obj",Var "obj");
              Value (Str "ability TYRANID PSYKERS are not affected")])
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
          (ParamArray
             [Value
                (Str
                   "Two with slimer Two with brainleech Stranglethorn cannon * Heavy venom cannon * *");
              Lam ("obj",Var "obj");
              Value (Str "model can not be armed with more than one of");
              Lam ("obj",Var "obj")])
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
                         [Lam ("obj",Var "obj"); Value (Str "time");
                          Lam ("obj",Var "obj");
                          Value
                            (Str
                               "bearer fights it can make one and only one attack with");
                          Lam ("obj",Var "obj"); Value (Str "weapon");
                          Lam ("obj",Var "obj"); Value (Str "is in addition to");
                          Lam ("obj",Var "obj"); Value (Str "bearer 's If");
                          Value (Int 1); Var "Target"; Call Suffer;
                          Lam ("obj",Var "obj"); Value (Str "unsaved from");
                          Lam ("obj",Var "obj"); Value (Str "weapon add"); Value (Int 1);
                          Value (Str "to"); Lam ("obj",Var "obj");
                          Value (Str "Morale they take until"); Lam ("obj",Var "obj");
                          Value (Str "end of"); Lam ("obj",Var "obj");
                          Value (Str "turn")])])])
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
                         [Value (Str "If"); Value (Int 1); Var "Target"; Call Suffer;
                          Lam ("obj",Var "obj"); Value (Str "unsaved from");
                          Lam ("obj",Var "obj");
                          Value (Str "weapon your opponent must subtract");
                          Value (Int 1); Value (Str "from hit for");
                          Lam ("obj",Var "obj"); Var "Target"; Value (Str "until");
                          Lam ("obj",Var "obj"); Value (Str "end of");
                          Lam ("obj",Var "obj"); Value (Str "turn")])])])
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
                         [Lam ("obj",Var "obj"); Value (Str "time");
                          Lam ("obj",Var "obj");
                          Value
                            (Str
                               "bearer fights it can make one and only one attack with");
                          Lam ("obj",Var "obj"); Value (Str "weapon");
                          Lam ("obj",Var "obj"); Value (Str "is in addition to");
                          Lam ("obj",Var "obj"); Value (Str "bearer 's")])])])
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
                         [Lam ("obj",Var "obj"); Value (Str "time you make");
                          Value (Int 1); Value (Str "wound roll of");
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
                          Value (Str "for"); Lam ("obj",Var "obj"); Value (Str "weapon");
                          Lam ("obj",Var "obj"); Value (Str "target"); Var "Target";
                          Call Suffer; Value (Int 1);
                          Value (Str "Mortal Wound in addition to");
                          Lam ("obj",Var "obj"); Value (Str "other damage")])])])
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
                         [Lam ("obj",Var "obj"); Value (Str "model armed with can make");
                          Value (Int 1); Value (Str "additional attack with them in");
                          Lam ("obj",Var "obj");
                          Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")])])])])
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
                         [Value (Str "When attacking with"); Lam ("obj",Var "obj");
                          Value (Str "weapon you must subtract"); Value (Int 1);
                          Value (Str "from"); Lam ("obj",Var "obj");
                          Value (Str "hit roll")])])])
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
                         [Lam ("obj",Var "obj"); Value (Str "time");
                          Lam ("obj",Var "obj");
                          Value
                            (Str
                               "bearer fights one and only one of its must be made with");
                          Lam ("obj",Var "obj"); Value (Str "weapon")])])])
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
                         [Value (Str "If"); Lam ("obj",Var "obj");
                          Value (Str "bearer is slain in"); Lam ("obj",Var "obj");
                          Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                          Value
                            (Str "before it has made its leave it where it is When its");
                          Var "Target"; Value (Str "is chosen to fight in");
                          Lam ("obj",Var "obj"); Value (Str "phase");
                          Lam ("obj",Var "obj");
                          Value
                            (Str "bearer can do so as normal before being removed from");
                          Lam ("obj",Var "obj"); Value (Str "battlefield")])])])
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
                         [Value (Str "If"); Lam ("obj",Var "obj");
                          Value (Str "bearer is slain in"); Lam ("obj",Var "obj");
                          Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                          Value
                            (Str "before it has made its leave it where it is When its");
                          Var "Target"; Value (Str "is chosen to fight in");
                          Lam ("obj",Var "obj"); Value (Str "phase");
                          Lam ("obj",Var "obj");
                          Value
                            (Str "bearer can do so as normal before being removed from");
                          Lam ("obj",Var "obj"); Value (Str "battlefield")])])])
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
                         [Value (Str "When attacking with"); Lam ("obj",Var "obj");
                          Value (Str "weapon you must subtract"); Value (Int 1);
                          Value (Str "from"); Lam ("obj",Var "obj");
                          Value (Str "hit roll")])])])
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
                         [Value (Str "You can re-roll hit of"); Value (Int 1);
                          Value (Str "for"); Lam ("obj",Var "obj");
                          Value (Str "weapon If"); Lam ("obj",Var "obj");
                          Value
                            (Str
                               "bearer has more than one pair of monstrous/massive scything it can make");
                          Value (Int 1); Value (Str "additional attack with");
                          Lam ("obj",Var "obj"); Value (Str "weapon");
                          Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                          Value (Str "time it fights")])])])
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
                         [Value (Str "You can re-roll failed wound for");
                          Lam ("obj",Var "obj"); Value (Str "weapon");
                          Lam ("obj",Var "obj"); Value (Str "model armed with");
                          Lam ("obj",Var "obj");
                          Value (Str "weapon always fights first in");
                          Lam ("obj",Var "obj");
                          Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                          Value (Str "even if it did n't charge If");
                          Lam ("obj",Var "obj"); Var "Target";
                          Value (Str "has that have charged or that have");
                          Value (Int 1);
                          Value
                            (Str
                               "similar ability then alternate choosing to fight with starting with");
                          Lam ("obj",Var "obj");
                          Value (Str "player whose turn is taking place")])])])
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
                         [Lam ("obj",Var "obj");
                          Value (Str "model armed with monstrous can make");
                          Value (Int 1); Value (Str "additional attack with them in");
                          Lam ("obj",Var "obj");
                          Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")])])])])
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
                         [Value (Str "When attacking with"); Lam ("obj",Var "obj");
                          Value (Str "weapon you must subtract"); Value (Int 1);
                          Value (Str "from"); Lam ("obj",Var "obj");
                          Value (Str "hit roll")])])])
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
                         [Value (Str "You can re-roll failed wound for");
                          Lam ("obj",Var "obj"); Value (Str "weapon In addition");
                          Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                          Value (Str "time you make"); Value (Int 1);
                          Value (Str "wound roll of");
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
                          Value (Str "that hit is resolved with"); Lam ("obj",Var "obj");
                          Value (Str "AP of"); Value (Int -6);
                          Value (Str "and Damage of"); Value (Int 3)])])])
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
                         [Value (Str "You can re-roll hit of"); Value (Int 1);
                          Value (Str "for"); Lam ("obj",Var "obj");
                          Value (Str "weapon If"); Lam ("obj",Var "obj");
                          Value
                            (Str
                               "bearer has more than one pair of monstrous/massive scything it can make");
                          Value (Int 1); Value (Str "additional attack with");
                          Lam ("obj",Var "obj"); Value (Str "weapon");
                          Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                          Value (Str "time it fights")])])])
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
                         [Lam ("obj",Var "obj"); Value (Str "time");
                          Lam ("obj",Var "obj");
                          Value
                            (Str
                               "bearer fights it can make one and only one attack with");
                          Lam ("obj",Var "obj"); Value (Str "weapon");
                          Lam ("obj",Var "obj"); Value (Str "is in addition to");
                          Lam ("obj",Var "obj"); Value (Str "bearer 's")])])])
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
                         [Value (Str "Make D3 hit for");
                          Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                          Value (Str "attack made with"); Lam ("obj",Var "obj");
                          Value (Str "weapon instead of"); Value (Int 1)])])])
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
                         [Lam ("obj",Var "obj"); Value (Str "time you make");
                          Value (Int 1); Value (Str "wound roll of");
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
                          Value (Str "for"); Lam ("obj",Var "obj");
                          Value (Str "weapon that hit is resolved with");
                          Lam ("obj",Var "obj"); Value (Str "AP of"); Value (Int -4)])])])
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
                         [Value (Str "You can re-roll hit of"); Value (Int 1);
                          Value (Str "for"); Lam ("obj",Var "obj");
                          Value (Str "weapon If"); Lam ("obj",Var "obj");
                          Value
                            (Str "bearer has more than one pair of scything it can make");
                          Value (Int 1); Value (Str "additional attack with");
                          Lam ("obj",Var "obj"); Value (Str "weapon");
                          Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
                          Value (Str "time it fights")])])])
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
                         [Value (Str "You can re-roll hit of"); Value (Int 1);
                          Value (Str "for"); Lam ("obj",Var "obj"); Value (Str "weapon")])])])
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
                         [Lam ("obj",Var "obj"); Value (Str "time");
                          Lam ("obj",Var "obj");
                          Value (Str "bearer it can make one and only one attack with");
                          Lam ("obj",Var "obj"); Value (Str "weapon Make D3 hit for");
                          Lam ("obj",Var "obj"); Value (Str "attack instead of one");
                          Lam ("obj",Var "obj"); Value (Str "is in addition to");
                          Lam ("obj",Var "obj"); Value (Str "bearer 's")])])])
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
                         [Value (Str "You can re-roll failed wound for");
                          Lam ("obj",Var "obj"); Value (Str "weapon");
                          Lam ("obj",Var "obj"); Value (Str "model armed with");
                          Lam ("obj",Var "obj");
                          Value (Str "weapon always fights first in");
                          Lam ("obj",Var "obj");
                          Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                          Value (Str "even if it did n't charge If");
                          Lam ("obj",Var "obj"); Var "Target";
                          Value (Str "has that have charged or that have");
                          Value (Int 1);
                          Value
                            (Str
                               "similar ability then alternate choosing to fight with starting with");
                          Lam ("obj",Var "obj");
                          Value (Str "player whose turn is taking place")])])])
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
                         [Lam ("obj",Var "obj"); Value (Str "time");
                          Lam ("obj",Var "obj");
                          Value (Str "bearer it can make one and only one attack with");
                          Lam ("obj",Var "obj"); Value (Str "weapon");
                          Lam ("obj",Var "obj"); Value (Str "is in addition to");
                          Lam ("obj",Var "obj"); Value (Str "bearer 's");
                          Lam ("obj",Var "obj");
                          Value (Str "weapon always wounds other than VEHICLES on");
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
                                      Value (ParamArray [Var "roll"; Value (Int 2)])),
                                   App (Call Or,Value (ParamArray [Var "eq"; Var "gt"])))))])])])
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
                         [Lam ("obj",Var "obj"); Value (Str "time");
                          Lam ("obj",Var "obj");
                          Value
                            (Str
                               "bearer fights it can make one and only one attack with");
                          Lam ("obj",Var "obj"); Value (Str "weapon");
                          Lam ("obj",Var "obj"); Value (Str "is in addition to");
                          Lam ("obj",Var "obj"); Value (Str "bearer 's")])])])
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
                    Value
                      (ParamArray
                         [Lam ("obj",Var "obj");
                          Value (Str "weapon automatically hits its target")])])])
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
                         [Value (Str "You can add"); Value (Int 1);
                          Value (Str "to hit for"); Lam ("obj",Var "obj");
                          Value (Str "weapon when attacking"); Value (Int 1);
                          Var "Target"; Value (Str "with"); Value (Int 10);
                          Value (Str "or more")])])])
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
                         [Value (Str "You can re-roll failed wound for");
                          Lam ("obj",Var "obj");
                          Value (Str "weapon In addition attacked by");
                          Lam ("obj",Var "obj"); Value (Str "weapon do not gain");
                          Lam ("obj",Var "obj");
                          Value (Str "bonus to their saving throws for being in cover")])])])
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
                         [Lam ("obj",Var "obj");
                          Value (Str "weapon automatically hits its target")])])])
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
                         [Lam ("obj",Var "obj");
                          Value (Str "weapon automatically hits its target")])])])
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
                         [Lam ("obj",Var "obj");
                          Value (Str "weapon can be fired within"); Value (Distance 1);
                          Value (Str "of"); Lam ("obj",Var "obj"); Var "Target";
                          Var "Target"; Value (Str "and can target"); Var "Target";
                          Value (Str "within"); Value (Distance 1);
                          Value (Str "of friendly")])])])
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
                         [Lam ("obj",Var "obj");
                          Value (Str "weapon can be fired within"); Value (Distance 1);
                          Value (Str "of"); Lam ("obj",Var "obj"); Var "Target";
                          Var "Target"; Value (Str "and can target"); Var "Target";
                          Value (Str "within"); Value (Distance 1);
                          Value (Str "of friendly In addition when"); Value (Int 1);
                          Value (Str "model is slain by"); Lam ("obj",Var "obj");
                          Value (Str "weapon"); Lam ("obj",Var "obj");
                          Value (Str "bearer regains"); Value (Int 1);
                          Value (Str "lost wound")])])])
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
                         [Lam ("obj",Var "obj");
                          Value (Str "weapon can target that are not visible to");
                          Lam ("obj",Var "obj");
                          Value (Str "bearer In addition attacked by");
                          Lam ("obj",Var "obj"); Value (Str "weapon do not gain");
                          Lam ("obj",Var "obj");
                          Value (Str "bonus to their saving throws for being in cover")])])])
    let ``Spore mine launcher`` = 
        Value
          (ParamArray
             [Value (Str "48"); Value (Distance 0); Value (Str "Heavy"); Value (Int 1);
              Value (Str "See Biovore datasheet pg"); Value (Int 101)])
    let _ = 
        Value
          (ParamArray
             [Value (Str "Attacking from"); Lam ("obj",Var "obj");
              Value (Str "and from beneath"); Lam ("obj",Var "obj"); Value (Str "earth");
              Lam ("obj",Var "obj"); Value (Str "Tyranids tear apart their prey in");
              Value (Int 1); Value (Str "frenzy of slashing")])
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
                         [Lam ("obj",Var "obj");
                          Value (Str "weapon can be fired within"); Value (Distance 1);
                          Value (Str "of"); Lam ("obj",Var "obj"); Var "Target";
                          Var "Target"; Value (Str "and can target"); Var "Target";
                          Value (Str "within"); Value (Distance 1);
                          Value (Str "of friendly You can re-roll");
                          Lam ("obj",Var "obj"); Value (Str "failed wound for");
                          Lam ("obj",Var "obj"); Value (Str "weapon")])])])
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
                         [Value (Str "If"); Lam ("obj",Var "obj");
                          Value (Str "target is"); Value (Int 1);
                          Value (Str "VEHICLE and you make"); Value (Int 1);
                          Value (Str "wound roll of");
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
                          Lam ("obj",Var "obj"); Value (Str "target"); Call Suffer;
                          Value (Int 1); Value (Str "mortal wound in addition to");
                          Lam ("obj",Var "obj"); Value (Str "other damage If you make");
                          Value (Int 1); Value (Str "wound roll of");
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
                         [Lam ("obj",Var "obj");
                          Value (Str "weapon can be fired within"); Value (Distance 1);
                          Value (Str "of"); Lam ("obj",Var "obj"); Var "Target";
                          Var "Target"; Value (Str "and can target"); Var "Target";
                          Value (Str "within"); Value (Distance 1);
                          Value (Str "of friendly")])])])
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
                         [Value (Str "When"); Value (Int 1); Value (Str "model fires");
                          Lam ("obj",Var "obj"); Value (Str "weapon it makes");
                          Value (Int 1);
                          Value (Str "number of equal to its characteristic")])])])
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
                         [Value (Str "You can add"); Value (Int 1);
                          Value (Str "to hit for"); Lam ("obj",Var "obj");
                          Value (Str "weapon when attacking"); Value (Int 1);
                          Var "Target"; Value (Str "with"); Value (Int 10);
                          Value (Str "or more")])])])
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
                         [Value (Str "You may re-roll failed hit for");
                          Lam ("obj",Var "obj");
                          Value (Str "weapon against that can FLY In addition if");
                          Lam ("obj",Var "obj"); Value (Str "target is"); Value (Int 1);
                          Value (Str "VEHICLE and you make"); Value (Int 1);
                          Value (Str "wound roll of");
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
                          Lam ("obj",Var "obj"); Value (Str "other damage If you make");
                          Value (Int 1); Value (Str "wound roll of");
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
                         [Lam ("obj",Var "obj");
                          Value (Str "weapon can be fired within"); Value (Distance 1);
                          Value (Str "of"); Lam ("obj",Var "obj"); Var "Target";
                          Var "Target"; Value (Str "and can target"); Var "Target";
                          Value (Str "within"); Value (Distance 1);
                          Value
                            (Str
                               "of friendly In addition you can re-roll failed wound for");
                          Lam ("obj",Var "obj"); Value (Str "weapon")])])])
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
             [Value (Str "At"); Lam ("obj",Var "obj"); Value (Str "start of");
              Lam ("obj",Var "obj"); Value (Str "first battle round but before");
              Lam ("obj",Var "obj");
              Value (Str "first turn begins you can remove your Warlord from");
              Lam ("obj",Var "obj"); Value (Str "battlefield and set them up again If");
              Lam ("obj",Var "obj"); Value (Str "have that can do");
              Lam ("obj",Var "obj"); Value (Str "roll off"); Lam ("obj",Var "obj");
              Value (Str "player that wins"); Lam ("obj",Var "obj");
              Value (Str "roll-off decides who sets up their"); Var "Target";
              Value (Str "s first")])
    let ``HEIGHTENED SENSES`` = 
        Value
          (ParamArray
             [Value (Str "Your Warlord never"); Call Suffer; Lam ("obj",Var "obj");
              Value (Str "to their hit although they still only hit on of");
              Value (Int 6); Value (Str "when firing Overwatch")])
    let ``SYNAPTIC LYNCHPIN`` = 
        Value
          (ParamArray
             [Value (Str "Add"); Value (Distance 6); Value (Str "to");
              Lam ("obj",Var "obj"); Value (Str "range of"); Lam ("obj",Var "obj");
              Value (Str "Warlord 's Synapse ability")])
    let ``MIND EATER`` = 
        Value
          (ParamArray
             [Lam ("obj",Var "obj"); Value (Str "time"); Lam ("obj",Var "obj");
              Value (Str "Warlord slays"); Lam ("obj",Var "obj"); Var "Target";
              Value (Str "CHARACTER in"); Lam ("obj",Var "obj");
              Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
              Value (Str "choose"); Value (Int 1); Value (Str "friendly <HIVE FLEET>");
              Var "Target"; Value (Str "within"); Value (Distance 3); Value (Str "At");
              Lam ("obj",Var "obj"); Value (Str "end of"); Lam ("obj",Var "obj");
              Value (Str "phase"); Lam ("obj",Var "obj"); Var "Target";
              Value
                (Str "can move and Advance if you wish as if it was your Movement phase")])
    let ``INSTINCTIVE KILLER`` = 
        Value
          (ParamArray
             [Value (Str "At"); Lam ("obj",Var "obj"); Value (Str "beginning of");
              Lam ("obj",Var "obj"); Value (Str "battle but before");
              Lam ("obj",Var "obj"); Value (Str "first turn begins choose");
              Lam ("obj",Var "obj"); Var "Target"; Var "Target";
              Value (Str "You can re-roll failed hit for"); Lam ("obj",Var "obj");
              Value (Str "Warlord for that target"); Lam ("obj",Var "obj"); Var "Target";
              Value (Str "or"); Lam ("obj",Var "obj"); Var "Target";
              Value (Str "that has"); Lam ("obj",Var "obj");
              Value (Str "same datasheet for example"); Lam ("obj",Var "obj");
              Value (Str "Intercessor Squads or"); Lam ("obj",Var "obj");
              Value (Str "of Nobz etc.")])
    let ``ADAPTIVE BIOLOGY`` = 
        Value
          (ParamArray
             [Value (Str "From"); Lam ("obj",Var "obj"); Value (Str "end of");
              Lam ("obj",Var "obj"); Value (Str "first phase in which");
              Lam ("obj",Var "obj"); Value (Str "Warlord"); Call Suffer;
              Lam ("obj",Var "obj"); Value (Str "for"); Lam ("obj",Var "obj");
              Value (Str "remainder of"); Lam ("obj",Var "obj");
              Value (Str "battle when inflicting damage upon"); Lam ("obj",Var "obj");
              Value (Str "Warlord reduce"); Lam ("obj",Var "obj");
              Value (Str "damage of"); Lam ("obj",Var "obj"); Value (Str "attack by");
              Value (Int 1); Value (Str "to"); Value (Int 1); Value (Str "minimum of");
              Value (Int 1)])
    let ``The mightiest creatures spawned by the Hive Mind have each been created with a specific purpose. If one of the following named characters is your Warlord, they must be given the associated Warlord Trait shown below.`` = 
        Value
          (ParamArray
             [Lam ("obj",Var "obj");
              Value
                (Str
                   "Swarmlord Alien Cunning Old One Eye Adaptive Biology Deathleaper Mind Eater The Red Terror Heightened Senses")])
    let ``HIVE FLEET WARLORD TRAITS`` = 
        Value
          (ParamArray
             [Value (Str "If you wish you can pick"); Value (Int 1);
              Value (Str "Hive Fleet Warlord Trait from"); Lam ("obj",Var "obj");
              Value (Str "list below instead of"); Lam ("obj",Var "obj");
              Value (Str "Tyranid Warlord Traits to"); Lam ("obj",Var "obj");
              Value (Str "left but only if your Warlord is from"); Lam ("obj",Var "obj");
              Value (Str "relevant hive fleet")])
    let ``BEHEMOTH: MONSTROUS HUNGER`` = 
        Value
          (ParamArray
             [Lam ("obj",Var "obj"); Value (Str "time you make"); Value (Int 1);
              Value (Str "wound roll of");
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
              Value (Str "for"); Lam ("obj",Var "obj"); Value (Str "Warlord in");
              Lam ("obj",Var "obj");
              Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
              Value (Str "that attack inflicts"); Value (Int 1);
              Value (Str "additional damage")])
    let ``KRAKEN: ONE STEP AHEAD`` = 
        Value
          (ParamArray
             [Value (Str "In");
              Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
              Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
              Value (Str "you can pick one friendly KRAKEN"); Var "Target";
              Value (Str "within"); Value (Distance 6); Value (Str "of your Warlord");
              Lam ("obj",Var "obj"); Var "Target"; Value (Str "can fight first in");
              Lam ("obj",Var "obj");
              Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
              Value (Str "even if it did n't charge If"); Lam ("obj",Var "obj");
              Var "Target"; Value (Str "has that have charged or that have");
              Value (Int 1);
              Value
                (Str
                   "similar ability then alternate choosing to fight with starting with");
              Lam ("obj",Var "obj"); Value (Str "player whose turn is taking place")])
    let ``LEVIATHAN: PERFECTLY ADAPTED`` = 
        Value
          (ParamArray
             [Value (Str "Once per battle round you can re-roll"); Value (Int 1);
              Value
                (Str
                   "single hit roll wound roll damage roll Advance roll charge roll or saving throw made for your Warlord")])
    let ``GORGON: LETHAL MIASMA`` = 
        Value
          (ParamArray
             [Value (Str "At"); Lam ("obj",Var "obj"); Value (Str "end of");
              Lam ("obj",Var "obj");
              Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
              Value (Str "roll"); Value (Int 1); App (Call Dice,Value (Int 6));
              Value (Str "for");
              Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"]))); Var "Target";
              Var "Target"; Value (Str "within"); Value (Distance 1); Value (Str "of");
              Lam ("obj",Var "obj"); Value (Str "Warlord On"); Value (Int 1);
              Lam
                ("roll",
                 Let
                   ("gt",
                    App
                      (Call GreaterThan,Value (ParamArray [Var "roll"; Value (Int 4)])),
                    Let
                      ("eq",
                       App (Call Equals,Value (ParamArray [Var "roll"; Value (Int 4)])),
                       App (Call Or,Value (ParamArray [Var "eq"; Var "gt"])))));
              Value (Str "that"); Var "Target"; Call Suffer; Value (Int 1);
              Value (Str "Mortal Wound")])
    let ``JORMUNGANDR: INSIDIOUS THREAT`` = 
        Value
          (ParamArray
             [Value (Str "Enemy never gain"); Lam ("obj",Var "obj");
              Value (Str "bonus to their saving throws for being in cover for made by");
              Lam ("obj",Var "obj");
              Value (Str "Warlord or friendly JORMUNGANDR within"); Value (Distance 3);
              Value (Str "of"); Lam ("obj",Var "obj"); Value (Str "Warlord")])
    let ``HYDRA: ENDLESS REGENERATION`` = 
        Value
          (ParamArray
             [Value (Str "At"); Lam ("obj",Var "obj"); Value (Str "beginning of");
              Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
              Value (Str "of your roll"); Value (Int 1); Value (Str "for");
              Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
              Value (Str "wound that your Warlord has lost For");
              Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
              Value (Str "roll of"); Value (Int 6); Value (Str "your Warlord regains");
              Value (Int 1); Value (Str "wound lost earlier in"); Lam ("obj",Var "obj");
              Value (Str "battle")])
    let ``KRONOS: SOUL HUNGER`` = 
        Value
          (ParamArray
             [Value (Str "Whenever"); Lam ("obj",Var "obj"); Var "Target";
              Value (Str "PSYKER fails"); Value (Int 1);
              Value (Str "psychic test within"); Value (Distance 18);
              Value (Str "of your Warlord they suffer D3 mortal")])
    let ``EXTENSIONS OF THE HIVE MIND`` = 
        Value
          (ParamArray
             [Value (Str "If your army is Battle-forged"); Lam ("obj",Var "obj");
              Value (Str "Troops in Tyranids Detachments gain"); Lam ("obj",Var "obj");
              Value (Str "ability"); Value (Int 1); Var "Target";
              Value (Str "that is within range of"); Lam ("obj",Var "obj");
              Value (Str "objective marker as specified in"); Lam ("obj",Var "obj");
              Value (Str "mission controls"); Lam ("obj",Var "obj");
              Value (Str "objective marker even if there are more"); Var "Target";
              Value (Str "within range of"); Lam ("obj",Var "obj");
              Value (Str "objective marker If"); Lam ("obj",Var "obj"); Var "Target";
              Var "Target"; Value (Str "within range of"); Lam ("obj",Var "obj");
              Value (Str "same objective marker has"); Value (Int 1);
              Value (Str "similar ability then"); Lam ("obj",Var "obj");
              Value (Str "objective marker is controlled by"); Lam ("obj",Var "obj");
              Value (Str "player who has"); Lam ("obj",Var "obj");
              Value (Str "most within range of it as normal")])
    let ``HIVE FLEET ADAPTATIONS`` = 
        Value
          (ParamArray
             [Value (Str "If your army is Battle-forged"); Lam ("obj",Var "obj");
              Value (Str "in Tyranids Detachments gain"); Value (Int 1);
              Value (Str "Hive Fleet Adaptation so long as"); Lam ("obj",Var "obj");
              Var "Target"; Value (Str "in that Detachment is from");
              Lam ("obj",Var "obj"); Value (Str "same hive fleet");
              Lam ("obj",Var "obj");
              Value (Str "Hive Fleet Adaptation gained depends upon");
              Lam ("obj",Var "obj"); Value (Str "hive fleet they are from as shown in");
              Lam ("obj",Var "obj"); Value (Str "table opposite For example");
              Value (Int 1); Value (Str "BEHEMOTH"); Var "Target"; Value (Str "with");
              Lam ("obj",Var "obj"); Value (Str "Hive Fleet Adaptation ability");
              Lam ("obj",Var "obj");
              Value (Str "Hyper-aggression adaptation If you are using"); Value (Int 1);
              Value (Str "splinter fleet rather than"); Value (Int 1);
              Value (Str "hive fleet use"); Lam ("obj",Var "obj");
              Value (Str "Hive Fleet Adaptation of its parent hive fleet For example");
              Lam ("obj",Var "obj"); Value (Str "Court of"); Lam ("obj",Var "obj");
              Value (Str "Nephilim King is"); Value (Int 1);
              Value (Str "splinter fleet of Hive Fleet Behemoth so should use");
              Lam ("obj",Var "obj");
              Value (Str "Behemoth Hive Fleet Adaptation If you are unsure of");
              Value (Int 1);
              Value (Str "splinter fleet 's parent hive fleet either consult");
              Lam ("obj",Var "obj"); Value (Str "background of our or choose");
              Lam ("obj",Var "obj"); Value (Str "adaptation from");
              Lam ("obj",Var "obj");
              Value (Str "table that best describes its character and fighting style")])
    let ``BEHEMOTH: HYPER-AGGRESSION`` = 
        Value
          (ParamArray
             [Value (Str "You can re-roll failed charge for with");
              Lam ("obj",Var "obj"); Value (Str "adaptation")])
    let ``KRAKEN: QUESTING TENDRILS`` = 
        Value
          (ParamArray
             [Value (Str "When"); Value (Int 1); Var "Target"; Value (Str "with");
              Lam ("obj",Var "obj");
              Value (Str "adaptation Advances roll three instead of one and pick");
              Lam ("obj",Var "obj"); Value (Str "highest to add to");
              Lam ("obj",Var "obj"); Value (Str "Move characteristic of");
              Lam ("obj",Var "obj"); Value (Str "in"); Lam ("obj",Var "obj");
              Var "Target";
              Value
                (Str
                   "for that Movement phase In addition such can Fall Back and charge in");
              Lam ("obj",Var "obj"); Value (Str "same turn")])
    let ``LEVIATHAN: SYNAPTIC IMPERATIVE`` = 
        Value
          (ParamArray
             [Value (Str "Roll"); Value (Int 1); App (Call Dice,Value (Int 6));
              Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
              Value (Str "time"); Value (Int 1); Var "Target"; Value (Str "with");
              Lam ("obj",Var "obj"); Value (Str "adaptation loses"); Value (Int 1);
              Value (Str "wound whilst it is within"); Value (Distance 6);
              Value (Str "of"); Value (Int 1); Value (Str "friendly SYNAPSE");
              Var "Target"; Value (Str "from"); Lam ("obj",Var "obj");
              Value (Str "same hive fleet On"); Value (Int 1); Value (Int 6);
              Lam ("obj",Var "obj"); Value (Str "damage is ignored and");
              Lam ("obj",Var "obj"); Var "Target"; Value (Str "does not lose");
              Value (Int 1); Value (Str "wound Ignore"); Lam ("obj",Var "obj");
              Value (Str "adaptation on"); Value (Int 1); Var "Target";
              Value (Str "that is currently affected by"); Lam ("obj",Var "obj");
              Value (Str "Catalyst psychic power")])
    let ``GORGON: ADAPTIVE TOXINS`` = 
        Value
          (ParamArray
             [Value (Str "You can re-roll wound of"); Value (Int 1); Value (Str "in");
              Lam ("obj",Var "obj");
              Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
              Value (Str "for with"); Lam ("obj",Var "obj"); Value (Str "adaptation")])
    let ``JORMUNGANDR: TUNNEL NETWORKS`` = 
        Value
          (ParamArray
             [Lam ("obj",Var "obj"); Var "Target"; Value (Str "with");
              Lam ("obj",Var "obj");
              Value (Str "adaptation other than that can FLY always has");
              Lam ("obj",Var "obj"); Value (Str "benefit of cover for");
              Lam ("obj",Var "obj"); Value (Str "of shooting If"); Lam ("obj",Var "obj");
              Var "Target"; Value (Str "Advances or charges however it loses");
              Lam ("obj",Var "obj"); Value (Str "benefit of"); Lam ("obj",Var "obj");
              Value (Str "adaptation until"); Lam ("obj",Var "obj");
              Value (Str "start of your next Movement phase")])
    let ``HYDRA: SWARMING INSTINCTS`` = 
        Value
          (ParamArray
             [Value (Str "You can re-roll hit in"); Lam ("obj",Var "obj");
              Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
              Value (Str "for with"); Lam ("obj",Var "obj");
              Value (Str "adaptation that target containing fewer than their own")])
    let ``KRONOS: BIO-BARRAGE`` = 
        Value
          (ParamArray
             [Value (Str "You can re-roll hit of"); Value (Int 1);
              Value (Str "for with"); Lam ("obj",Var "obj");
              Value (Str "adaptation in your Shooting phase if they did not move in");
              Lam ("obj",Var "obj"); Value (Str "preceding Movement phase")])
