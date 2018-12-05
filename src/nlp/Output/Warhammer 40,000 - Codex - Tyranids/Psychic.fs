namespace ``Warhammer 40,000 - Codex - Tyranids`` 
module Psychic = 
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
    let ``DOMINION`` = 
        Value
          (ParamArray
             [Value (Str "Dominion has"); Value (Int 1);
              Value (Str "warp charge value of"); Value (Int 5);
              Value (Str "If manifested"); Value (Int 1);
              Value (Str "friendly TYRANIDS"); Var "Target"; Value (Str "within");
              Value (Distance 36); Value (Str "of"); Lam ("obj",Var "obj");
              Value (Str "psyker that has"); Lam ("obj",Var "obj");
              Value (Str "Instinctive Behaviour ability Until"); Lam ("obj",Var "obj");
              Value (Str "end of your next Psychic phase"); Lam ("obj",Var "obj");
              Var "Target";
              Value
                (Str
                   "ignores its Instinctive Behaviour ability and automatically passes Morale")])
    let ``CATALYST`` = 
        Value
          (ParamArray
             [Value (Str "Catalyst has"); Value (Int 1);
              Value (Str "warp charge value of"); Value (Int 6);
              Value (Str "If manifested select"); Value (Int 1);
              Value (Str "friendly TYRANIDS"); Var "Target"; Value (Str "within");
              Value (Distance 18); Value (Str "of"); Lam ("obj",Var "obj");
              Value (Str "psyker Until"); Lam ("obj",Var "obj");
              Value (Str "start of your next Psychic phase");
              Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
              Value (Str "time that"); Var "Target"; Value (Str "loses"); Value (Int 1);
              Value (Str "wound roll"); Value (Int 1); App (Call Dice,Value (Int 6));
              Value (Str "on"); Value (Int 1);
              Lam
                ("roll",
                 Let
                   ("gt",
                    App
                      (Call GreaterThan,Value (ParamArray [Var "roll"; Value (Int 5)])),
                    Let
                      ("eq",
                       App (Call Equals,Value (ParamArray [Var "roll"; Value (Int 5)])),
                       App (Call Or,Value (ParamArray [Var "eq"; Var "gt"])))));
              Lam ("obj",Var "obj"); Value (Str "damage is ignored and");
              Lam ("obj",Var "obj"); Var "Target"; Value (Str "does not lose");
              Lam ("obj",Var "obj"); Value (Str "wound")])
    let ``THE HORROR`` = 
        Value
          (ParamArray
             [Lam ("obj",Var "obj"); Value (Str "Horror has"); Value (Int 1);
              Value (Str "warp charge value of"); Value (Int 6);
              Value (Str "If manifested select"); Value (Int 1); Var "Target";
              Value (Str "within"); Value (Distance 24);
              Value (Str "that is visible to"); Lam ("obj",Var "obj");
              Value (Str "psyker Until"); Lam ("obj",Var "obj");
              Value (Str "start of your next Psychic phase"); Lam ("obj",Var "obj");
              Var "Target"; Value (Str "must subtract"); Value (Int 1);
              Value (Str "from their hit and Leadership characteristic")])
    let ``ONSLAUGHT`` = 
        Value
          (ParamArray
             [Value (Str "Onslaught has"); Value (Int 1);
              Value (Str "warp charge value of"); Value (Int 6);
              Value (Str "If manifested select"); Value (Int 1); Value (Str "friendly");
              Var "Target"; Value (Str "within"); Value (Distance 18); Value (Str "of");
              Lam ("obj",Var "obj"); Value (Str "psyker"); Lam ("obj",Var "obj");
              Var "Target"; Value (Str "can shoot"); Lam ("obj",Var "obj");
              Value (Str "turn even if it Advanced without suffering");
              Lam ("obj",Var "obj");
              Value
                (Str
                   "to its hit for moving and shooting Heavy or Advancing and shooting Assault weapons In addition");
              Lam ("obj",Var "obj"); Var "Target"; Value (Str "can charge");
              Lam ("obj",Var "obj");
              Value (Str "turn even if it Advanced though not if it Fell Back")])
    let ``PAROXYSM`` = 
        Value
          (ParamArray
             [Value (Str "Paroxysm has"); Value (Int 1);
              Value (Str "warp charge value of"); Value (Int 5);
              Value (Str "If manifested choose"); Lam ("obj",Var "obj"); Var "Target";
              Var "Target"; Value (Str "within"); Value (Distance 18); Value (Str "of");
              Lam ("obj",Var "obj"); Value (Str "psyker Until your next Psychic phase");
              Lam ("obj",Var "obj"); Var "Target"; Value (Str "can not fight in");
              Lam ("obj",Var "obj");
              Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
              Value (Str "until"); Lam ("obj",Var "obj");
              Value (Str "other that are able to have done so If");
              Lam ("obj",Var "obj"); Value (Str "target"); Var "Target";
              Value (Str "has"); Lam ("obj",Var "obj");
              Value (Str "ability that allows it to fight first in");
              Lam ("obj",Var "obj");
              Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
              Value (Str "it instead fights as if it did n't have");
              Lam ("obj",Var "obj"); Value (Str "ability If"); Lam ("obj",Var "obj");
              Value (Str "have that can not fight until"); Lam ("obj",Var "obj");
              Value (Str "other have done so then alternate choosing which of");
              Lam ("obj",Var "obj"); Value (Str "to fight with starting with");
              Lam ("obj",Var "obj"); Value (Str "player whose turn is taking place")])
    let ``PSYCHIC SCREAM`` = 
        Value
          (ParamArray
             [Value (Str "Psychic Scream has"); Value (Int 1);
              Value (Str "warp charge value of"); Value (Int 5);
              Value (Str "If manifested"); Lam ("obj",Var "obj"); Value (Str "nearest");
              Var "Target"; Var "Target"; Value (Str "within"); Value (Distance 18);
              Call Suffer; Value (Str "D3 mortal wounds In addition if");
              Lam ("obj",Var "obj"); Var "Target"; Value (Str "is"); Value (Int 1);
              Value (Str "PSYKER roll two dice If"); Lam ("obj",Var "obj");
              Value
                (Str
                   "result is higher than their Leadership characteristic randomly select one of their psychic They can no longer use");
              Lam ("obj",Var "obj"); Value (Str "psychic power")])
