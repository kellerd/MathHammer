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
             [Value (Str "Score"); Value (Int 1);
              Value
                (Str
                   "victory point if you control more objective markers than your opponent at");
              Lam ("obj",Var "obj"); Value (Str "end of"); Lam ("obj",Var "obj");
              Value (Str "turn")])
    let ``Crush`` = 
        Value
          (ParamArray
             [Value (Str "Score"); Value (Int 1);
              Value (Str "victory point if at least one"); Var "Target"; Var "Target";
              Value (Str "was completely destroyed"); Lam ("obj",Var "obj");
              Value (Str "turn and"); Lam ("obj",Var "obj"); Value (Str "last model in");
              Lam ("obj",Var "obj"); Var "Target"; Var "Target";
              Value (Str "was slain by"); Lam ("obj",Var "obj");
              Value (Str "attack made by"); Value (Int 1);
              Value (Str "TYRANIDS MONSTER or"); Value (Int 1); Value (Str "TYRANIDS");
              Var "Target"; Value (Str "of more than"); Value (Int 10)])
    let ``Dominate`` = 
        Value
          (ParamArray
             [Value (Str "Score"); Value (Int 1);
              Value
                (Str
                   "victory point if at least three psychic were successfully manifested by friendly TYRANIDS in your Psychic phase")])
    let ``Decapitate`` = 
        Value
          (ParamArray
             [Value (Str "Score"); Value (Int 1);
              Value (Str "victory point if at least one"); Var "Target";
              Value (Str "CHARACTER was destroyed"); Lam ("obj",Var "obj");
              Value (Str "turn If two or more"); Var "Target";
              Value (Str "were destroyed score D3 victory instead")])
    let ``Terrify`` = 
        Value
          (ParamArray
             [Value (Str "Score"); Value (Int 1);
              Value (Str "victory point if at least one"); Var "Target"; Var "Target";
              Value (Str "failed"); Value (Int 1); Value (Str "Morale test");
              Lam ("obj",Var "obj"); Value (Str "turn If three or more"); Var "Target";
              Value (Str "failed Morale tests"); Lam ("obj",Var "obj");
              Value (Str "turn score D3 victory instead")])
    let ``Devour`` = 
        Value
          (ParamArray
             [Value (Str "Score"); Value (Int 1); Value (Str "victory point if");
              Lam ("obj",Var "obj"); Var "Target"; Var "Target";
              Value (Str "was destroyed during"); Lam ("obj",Var "obj");
              Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
              Lam ("obj",Var "obj"); Value (Str "turn If"); Value (Int 3);
              Value (Str "or more"); Var "Target"; Value (Str "were destroyed during");
              Lam ("obj",Var "obj");
              Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
              Lam ("obj",Var "obj"); Value (Str "turn score");
              App (Call Dice,Value (Int 3)); Value (Str "victory instead and if");
              Value (Int 6); Value (Str "or more"); Var "Target";
              Value (Str "were destroyed during"); Lam ("obj",Var "obj");
              Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
              Lam ("obj",Var "obj"); Value (Str "turn score D3"); Value (Int 3);
              Value (Str "victory instead")])
