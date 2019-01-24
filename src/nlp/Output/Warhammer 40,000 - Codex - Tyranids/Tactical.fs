namespace ``Warhammer 40,000 - Codex - Tyranids`` 
module Tactical = 
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
    let ``Swarm`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "Score 1 victory point if you control more objective markers than your opponent at the end of the turn.");
              Value (Str "Score"); Value (Int 1);
              Value
                (Str
                   "victory point if you control more objective markers than your opponent at");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
              Value (Str "of");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "turn"); Lam ("obj",Var "obj")]))])
    let ``Crush`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "Score 1 victory point if at least one enemy unit was completely destroyed this turn, and the last model in the enemy unit was slain by an attack made by a TYRANIDS MONSTER or a TYRANIDS unit of more than 10 models.");
              Value (Str "Score"); Value (Int 1);
              Value (Str "victory point if at least one"); Var "Target"; Var "Target";
              Value (Str "was completely destroyed");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "turn"); Lam ("obj",Var "obj")]));
              Value (Str "and");
              App
                (Lam ("obj",Var "obj"),
                 Value (ParamArray [Value (Str "last"); Value (Str "model")]));
              Value (Str "in");
              App
                (Lam ("obj",Var "obj"),Value (ParamArray [Var "Target"; Var "Target"]));
              Value (Str "was slain by");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "attack"); Lam ("obj",Var "obj")]));
              Value (Str "made by");
              App
                (Value (Int 1),
                 Value (ParamArray [Value (Str "TYRANIDS"); Value (Str "MONSTER")]));
              Value (Str "or");
              App
                (Value (Int 1),Value (ParamArray [Value (Str "TYRANIDS"); Var "Target"]));
              Value (Str "of more than"); Value (Int 10)])
    let ``Dominate`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "Score 1 victory point if at least three psychic powers were successfully manifested by friendly TYRANIDS units in your Psychic phase.");
              Value (Str "Score"); Value (Int 1);
              Value
                (Str
                   "victory point if at least three psychic were successfully manifested by friendly TYRANIDS in your Psychic phase")])
    let ``Decapitate`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "Score 1 victory point if at least one enemy CHARACTER was destroyed this turn. If two or more enemy CHARACTERS were destroyed, score D3 victory points instead.");
              Value (Str "Score"); Value (Int 1);
              Value (Str "victory point if at least one"); Var "Target";
              Value (Str "CHARACTER was destroyed");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "turn"); Lam ("obj",Var "obj")]));
              Value (Str "If two or more"); Var "Target";
              Value (Str "were destroyed score"); App (Call Dice,Value (Int 3));
              Value (Str "victory points instead")])
    let ``Terrify`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "Score 1 victory point if at least one enemy unit failed a Morale test this turn. If three or more enemy units failed Morale tests this turn, score D3 victory points instead.");
              Value (Str "Score"); Value (Int 1);
              Value (Str "victory point if at least one"); Var "Target"; Var "Target";
              Value (Str "failed");
              App (Call Repeat,Value (ParamArray [Value (Str "Morale"); Value (Int 1)]));
              Value (Str "test");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "turn"); Lam ("obj",Var "obj")]));
              Value (Str "If three or more"); Var "Target";
              Value (Str "failed Morale tests");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "turn"); Lam ("obj",Var "obj")]));
              Value (Str "score"); App (Call Dice,Value (Int 3));
              Value (Str "victory points instead")])
    let ``Devour`` = 
        Value
          (ParamArray
             [Value
                (Str
                   "Score 1 victory point if an enemy unit was destroyed during the Fight phase this turn. If 3 or more enemy units were destroyed during the Fight phase this turn, score D3 victory points instead, and if 6 or more enemy units were destroyed during the Fight phase this turn, score D3+3 victory points instead.");
              Value (Str "Score"); Value (Int 1); Value (Str "victory point if");
              App
                (Lam ("obj",Var "obj"),Value (ParamArray [Var "Target"; Var "Target"]));
              Value (Str "was destroyed during");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "Fight"); Lam ("obj",Var "obj")]));
              Value (Str "phase");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "turn"); Lam ("obj",Var "obj")]));
              Value (Str "If"); Value (Int 3); Value (Str "or more"); Var "Target";
              Value (Str "were destroyed during");
              App
                (Call Repeat,
                 Value
                   (ParamArray
                      [Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                       Lam ("obj",Var "obj")]));
              App
                (Lam ("obj",Var "obj"),
                 Value
                   (ParamArray
                      [Value (Str "turn"); Value (Str "score");
                       App (Call Dice,Value (Int 3)); Value (Str "victory")]));
              Value (Str "points instead and if"); Value (Int 6); Value (Str "or more");
              Var "Target"; Value (Str "were destroyed during");
              App
                (Call Repeat,
                 Value
                   (ParamArray
                      [Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                       Lam ("obj",Var "obj")]));
              App
                (Lam ("obj",Var "obj"),
                 Value
                   (ParamArray
                      [Value (Str "turn"); Value (Str "score");
                       App (Call Dice,Value (Int 3)); Value (Int 3);
                       Value (Str "victory")])); Value (Str "points instead")])
