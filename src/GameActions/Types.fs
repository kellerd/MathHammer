module GameActions.Types
type Model = {
    Actions : GameActions.GameActionsList.Types.Model
}
//     Actions : MathHammer.UnitList.Types.Model
//     Defender : MathHammer.UnitList.Types.Model
//     Selected : Option<MathHammer.Models.Types.Model>
//     StoredActions : Map<string,Probability.Distribution<Result>>
// }

type Msg = 
    | GameActionListMsg of GameActions.GameActionsList.Types.Msg
    // | Swap
