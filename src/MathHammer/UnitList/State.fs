module MathHammer.UnitList.State
open GameActions.Primitives.Types
open Elmish
open Types

let init name () : Model * Cmd<Msg> =
    {
        Name = ""
        Models=Map.empty<_,_>
        OffsetY=0<mm>
        BoxFill="#000000"
        ElementFill="#FFFFFF"
        ElementStroke="#000000"
        DeploymentFill="#CCFFCC"
        Width=ft.ToMM 6<ft>
        Height=ft.ToMM 2<ft>
        Deployment=inch.ToMM 12<inch>
        Scale = "scale(1,1)"
    }, Cmd.ofMsg Distribute


let distribute width height deployment (models:(string*MathHammer.Models.Types.Model) list) =
    let width = 1830<mm>
    let height = 1220<mm>
    let deployment = 12<inch> |> inch.ToMM
    let spacing = models |> List.maxBy (fun m -> (snd m).Size) |> fun m -> (snd m).Size + inch.ToMM(2<inch>)
    let rowSize = (width / spacing)
    let rows = (List.length models / rowSize) + 1
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
                |> distribute model.Width model.Height model.Deployment
                |> List.map(fun((_,m),x,y) -> MathHammer.Models.State.update (MathHammer.Models.Types.Msg.ChangePosition(x,y,model.Scale)) m)
                |> List.fold(fun (map,cmds) (m,cmd) -> (Map.add m.Name m map), cmd::cmds) (model.Models,[])
            {model with Models = newModels}, Cmd.batch (modelsCmds)
    | ModelMsg(msg,key) -> 
            let (newModel, modelCmds) = model.Models.Item(key) |> MathHammer.Models.State.update msg
            {model with Models = Map.add key newModel model.Models}, Cmd.map ModelMsg modelCmds
