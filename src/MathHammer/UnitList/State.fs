module MathHammer.UnitList.State
open GameActions.Primitives.Types
open Elmish
open Types

let init name () : Model * Cmd<Msg> =
    {
        Location = { Label = name
                     Fill = "#000000"
                     Dimensions = {
                                    Top    = 0<mm>
                                    Left   = 0<mm>
                                    Width  = ft.ToMM 6<ft>
                                    Height = ft.ToMM 2<ft> } }
        Models=Map.empty<_,_>
        ElementFill="#FFFFFF"
        ElementStroke="#000000"
        DeploymentFill="#CCFFCC"
        Deployment=inch.ToMM 12<inch>
    }, Cmd.ofMsg Distribute


let distribute width deployment (models:(string*MathHammer.Models.Types.Model) list) =
    let spacing = models |> List.maxBy (fun m -> (snd m).Size) |> fun m -> (snd m).Size + inch.ToMM(2<inch>)
    let rowSize = (width / spacing)
    let rowWidth = spacing
    [for i in 0 .. rowSize .. List.length models - 1 do
            let maxPage = min (i + rowSize - 1) (List.length models - 1)
            let columns = maxPage - i + 1
            let columnWidth = float width / (float columns + 1.)
            yield! 
                [for j in i .. maxPage do
                        let offsetX' = float (j - i + 1) * columnWidth
                        let offsetY' = float( deployment - (snd models.[j]).Size / 2) - (float (i / rowSize)) * float rowWidth 
                        yield models.[j],offsetX',offsetY']
    ]

let update msg model : Model * Cmd<Msg> =
    //Don't do this every time please
    
    match msg with
    | Distribute -> 
            let (newModels, modelsCmds) =
                model.Models
                |> Map.toList
                |> distribute model.Location.Dimensions.Width model.Deployment
                |> List.map(fun((_,m),x,y) -> MathHammer.Models.State.update (MathHammer.Models.Types.Msg.ChangePosition(x,y)) m)
                |> List.fold(fun (map,cmds) (m,cmd) -> (Map.add m.Name m map), (Cmd.map (fun msg -> ModelMsg(msg,m.Name)) cmd)::cmds) (model.Models,[])
            {model with Models = newModels}, Cmd.batch (modelsCmds)
    | ModelMsg(msg,key) -> 
            let (newModel, modelCmds) = model.Models.Item(key) |> MathHammer.Models.State.update msg
            {model with Models = Map.add key newModel model.Models}, Cmd.map (fun msg -> ModelMsg(msg,key)) modelCmds
