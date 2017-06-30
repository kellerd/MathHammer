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
    DeploymentFill="#EEFFEE"
    Width=ft.ToMM 6<ft>
    Height=ft.ToMM 2<ft>
    Deployment=inch.ToMM 12<inch>
  }, Cmd.ofMsg Distribute


let distribute deployment offsetX offsetY (models:(string*MathHammer.Models.Types.Model) list) =
  let rows = (List.length models / 10) + 1
  let rowWidth = 50. / (float rows + 1.)
  [for i in 0 .. 10 .. List.length models - 1 do
      let maxPage = min (i + 9) (List.length models - 1)
      let columns = maxPage - i + 1
      let columnWidth = 100. / (float columns + 1.)
      yield! 
        [for j in i .. maxPage do
            let offsetX' = float (j - i + 1) * columnWidth + offsetX
            let offsetY' = (float (i / 10) + 1.) * rowWidth + offsetY + deployment - (float (snd models.[j]).size / 2.)
            yield models.[j],offsetX',offsetY']
  ]

let update msg model : Model * Cmd<Msg> =
  //Don't do this every time please
  
  match msg with
  | Distribute -> 
      let (newModels, modelsCmds) =
        model.Models
        |> Map.toList
        |> distribute (float model.Deployment) 0. (float model.OffsetY) 
        |> List.map(fun((_,m),x,y) -> MathHammer.Models.State.update (MathHammer.Models.Types.Msg.ChangePosition(x,y)) m)
        |> List.fold(fun (map,cmds) (m,cmd) -> (Map.add m.name m map), cmd::cmds) (model.Models,[])
      {model with Models = newModels}, Cmd.batch (modelsCmds)
  | ModelMsg(msg,key) -> 
      let (newModel, modelCmds) = model.Models.Item(key) |> MathHammer.Models.State.update msg
      {model with Models = Map.add key newModel model.Models}, Cmd.map ModelMsg modelCmds
