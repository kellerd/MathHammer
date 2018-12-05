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
             [Value (Str "Dominion has");
              App
                (Value (Int 1),
                 Value
                   (ParamArray
                      [Value (Str "warp"); Value (Str "charge"); Value (Str "value")]));
              Value (Str "of"); Value (Int 5); Value (Str "If manifested");
              App
                (Value (Int 1),
                 Value
                   (ParamArray
                      [Value (Str "friendly"); Value (Str "TYRANIDS"); Var "Target"]));
              Value (Str "within"); Value (Distance 36); Value (Str "of");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "psyker"); Lam ("obj",Var "obj")]));
              Value (Str "that has");
              App
                (Lam ("obj",Var "obj"),
                 Value
                   (ParamArray
                      [Value (Str "Instinctive"); Value (Str "Behaviour");
                       Value (Str "ability")])); Value (Str "Until");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "end"); Lam ("obj",Var "obj")]));
              Value (Str "of your next Psychic phase");
              App (Call Repeat,Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
              Value
                (Str
                   "ignores its Instinctive Behaviour ability and automatically passes Morale")])
    let ``CATALYST`` = 
        Value
          (ParamArray
             [Value (Str "Catalyst has");
              App
                (Value (Int 1),
                 Value
                   (ParamArray
                      [Value (Str "warp"); Value (Str "charge"); Value (Str "value")]));
              Value (Str "of"); Value (Int 6); Value (Str "If manifested select");
              App
                (Value (Int 1),
                 Value
                   (ParamArray
                      [Value (Str "friendly"); Value (Str "TYRANIDS"); Var "Target"]));
              Value (Str "within"); Value (Distance 18); Value (Str "of");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "psyker"); Lam ("obj",Var "obj")]));
              Value (Str "Until");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "start"); Lam ("obj",Var "obj")]));
              Value (Str "of your next Psychic phase");
              Lam ("obj",App (Call Count,Value (ParamArray [Var "obj"])));
              Value (Str "time that"); Var "Target"; Value (Str "loses");
              App (Call Repeat,Value (ParamArray [Value (Str "wound"); Value (Int 1)]));
              Value (Str "roll");
              App
                (Call Repeat,
                 Value (ParamArray [App (Call Dice,Value (Int 6)); Value (Int 1)]));
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
                                Value (ParamArray [Var "roll"; Value (Int 5)])),
                             Let
                               ("eq",
                                App
                                  (Call Equals,
                                   Value (ParamArray [Var "roll"; Value (Int 5)])),
                                App (Call Or,Value (ParamArray [Var "eq"; Var "gt"])))));
                       Value (Int 1)]));
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "damage"); Lam ("obj",Var "obj")]));
              Value (Str "is ignored and");
              App (Call Repeat,Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
              Value (Str "does not lose");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "wound"); Lam ("obj",Var "obj")]))])
    let ``THE HORROR`` = 
        Value
          (ParamArray
             [App
                (Call Repeat,
                 Value (ParamArray [Value (Str "Horror"); Lam ("obj",Var "obj")]));
              Value (Str "has");
              App
                (Value (Int 1),
                 Value
                   (ParamArray
                      [Value (Str "warp"); Value (Str "charge"); Value (Str "value")]));
              Value (Str "of"); Value (Int 6); Value (Str "If manifested select");
              App (Call Repeat,Value (ParamArray [Var "Target"; Value (Int 1)]));
              Value (Str "within"); Value (Distance 24);
              Value (Str "that is visible to");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "psyker"); Lam ("obj",Var "obj")]));
              Value (Str "Until");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "start"); Lam ("obj",Var "obj")]));
              Value (Str "of your next Psychic phase");
              App (Call Repeat,Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
              Value (Str "must subtract"); Value (Int 1);
              Value (Str "from their hit and Leadership characteristic")])
    let ``ONSLAUGHT`` = 
        Value
          (ParamArray
             [Value (Str "Onslaught has");
              App
                (Value (Int 1),
                 Value
                   (ParamArray
                      [Value (Str "warp"); Value (Str "charge"); Value (Str "value")]));
              Value (Str "of"); Value (Int 6); Value (Str "If manifested select");
              App
                (Call Repeat,Value (ParamArray [Value (Str "friendly"); Value (Int 1)]));
              Var "Target"; Value (Str "within"); Value (Distance 18); Value (Str "of");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "psyker"); Lam ("obj",Var "obj")]));
              App (Call Repeat,Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
              Value (Str "can shoot");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "turn"); Lam ("obj",Var "obj")]));
              Value (Str "even if it Advanced without suffering");
              App (Lam ("obj",Var "obj"),Value (ParamArray []));
              Value
                (Str
                   "to its hit for moving and shooting Heavy or Advancing and shooting Assault weapons In addition");
              App (Call Repeat,Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
              Value (Str "can charge");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "turn"); Lam ("obj",Var "obj")]));
              Value (Str "even if it Advanced though not if it Fell Back")])
    let ``PAROXYSM`` = 
        Value
          (ParamArray
             [Value (Str "Paroxysm has");
              App (Call Repeat,Value (ParamArray [Value (Str "warp"); Value (Int 1)]));
              Value (Str "charge value of"); Value (Int 5);
              Value (Str "If manifested choose");
              App
                (Lam ("obj",Var "obj"),Value (ParamArray [Var "Target"; Var "Target"]));
              Value (Str "within"); Value (Distance 18); Value (Str "of");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "psyker"); Lam ("obj",Var "obj")]));
              Value (Str "Until your next Psychic phase");
              App (Call Repeat,Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
              Value (Str "can not fight in");
              App
                (Call Repeat,
                 Value
                   (ParamArray
                      [Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                       Lam ("obj",Var "obj")])); Value (Str "until");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "other"); Lam ("obj",Var "obj")]));
              Value (Str "that are able to have done so If");
              App
                (Lam ("obj",Var "obj"),
                 Value (ParamArray [Value (Str "target"); Var "Target"]));
              Value (Str "has");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "ability"); Lam ("obj",Var "obj")]));
              Value (Str "that allows it to fight first in");
              App
                (Call Repeat,
                 Value
                   (ParamArray
                      [Value (ParamArray [Value (Str "Phase"); Value (Str "Fight")]);
                       Lam ("obj",Var "obj")]));
              Value (Str "it instead fights as if it did n't have");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "ability"); Lam ("obj",Var "obj")]));
              Value (Str "If"); App (Lam ("obj",Var "obj"),Value (ParamArray []));
              Value (Str "have that can not fight until");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "other"); Lam ("obj",Var "obj")]));
              Value (Str "have done so then alternate choosing which of");
              App (Lam ("obj",Var "obj"),Value (ParamArray []));
              Value (Str "to fight with starting with");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "player"); Lam ("obj",Var "obj")]));
              Value (Str "whose turn is taking place")])
    let ``PSYCHIC SCREAM`` = 
        Value
          (ParamArray
             [Value (Str "Psychic Scream has");
              App
                (Value (Int 1),
                 Value
                   (ParamArray
                      [Value (Str "warp"); Value (Str "charge"); Value (Str "value")]));
              Value (Str "of"); Value (Int 5); Value (Str "If manifested");
              App
                (Lam ("obj",Var "obj"),
                 Value (ParamArray [Value (Str "nearest"); Var "Target"; Var "Target"]));
              Value (Str "within"); Value (Distance 18); Call Suffer;
              Value (Str "D3 mortal wounds In addition if");
              App (Call Repeat,Value (ParamArray [Var "Target"; Lam ("obj",Var "obj")]));
              Value (Str "is");
              App (Call Repeat,Value (ParamArray [Value (Str "PSYKER"); Value (Int 1)]));
              Value (Str "roll two dice If");
              App
                (Call Repeat,
                 Value (ParamArray [Value (Str "result"); Lam ("obj",Var "obj")]));
              Value
                (Str
                   "is higher than their Leadership characteristic randomly select one of their psychic They can no longer use");
              App
                (Lam ("obj",Var "obj"),
                 Value (ParamArray [Value (Str "psychic"); Value (Str "power")]))])
