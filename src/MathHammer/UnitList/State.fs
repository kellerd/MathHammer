module MathHammer.UnitList.State

open Elmish
open Types

let init () : Model * Cmd<Msg> =
  {
    Models=[]
    OffsetY=0.
    BoxFill="#000000"
    ElementFill="#FFFFFF"
    ElementStroke="#000000"
  }, []


let distribute offsetX offsetY models =
  let rows = (List.length models / 10) + 1
  let rowWidth = 50. / (float rows + 1.)
  [for i in 0 .. 10 .. List.length models - 1 do
      let maxPage = min (i + 9) (List.length models - 1)
      let columns = maxPage - i + 1
      let columnWidth = 100. / (float columns + 1.)
      yield! 
        [for j in i .. maxPage do
            let offsetX' = float (j - i + 1) * columnWidth + offsetX
            let offsetY' = (float (i / 10) + 1.) * rowWidth + offsetY
            yield models.[j],offsetX',offsetY']
  ]

let update msg model : Model * Cmd<Msg> =
  //Don't do this every time please
  let newModel = 
    {
      model with Models = distribute 0. model.OffsetY model.Models
                         |> List.map (fun (m,x,y) -> {m with posX = x; posY = y}) 
    }
  match msg with
  | () -> newModel, []
