module MathHammer.UnitList.State
open GameActions.Primitives.Types
open MathHammer.Models.Types
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
        Deployment= { Label = (Some "Deployment Zone")
                      Fill = "#CCFFCC"
                      Dimensions ={ Top    = 0<mm>
                                    Left   = 0<mm>
                                    Width  = ft.ToMM 6<ft>
                                    Height = ft.ToMM 1<ft> } }
    }, Cmd.none

//Switch to hausdorff distance? http://cgm.cs.mcgill.ca/~godfried/teaching/cg-projects/98/normand/main.html
let points min spacing deployment =
    let spacing' = inch.ToMM spacing |> int
    match deployment with 
    | { Top    = t 
        Left   = l 
        Width  = w 
        Height = h } -> 
            [for x in (int l) + (int min / 2 + 1) .. spacing' .. (int l) + (int w) - (int min / 2 - 1) do 
                for y in (int t) + (int min / 2 + 1).. spacing' .. (int t) + (int h) - (int min / 2 - 1) do 
                     yield (float x, float y) ]

let rec cartesian lstlst =
    match lstlst with
    | [h] ->
        List.fold (fun acc elem -> [elem]::acc) [] h
    | h::t ->
        List.fold (fun cacc celem ->
            (List.fold (fun acc elem -> (elem::celem)::acc) [] h) @ cacc
            ) [] (cartesian t)
    | _ -> []

let distance (xa,ya) (xb,yb : float) = sqrt (pown (xb - xa) 2 + pown (yb - ya) 2)

let midpoint area = (float area.Left + float area.Width / 2.0, float area.Top + float area.Height / 2.0)

let distributeClosestToEnemy deploymentArea enemy models =
    let spacing = 2<inch>
    let minMargin = (models |> List.minBy (fun (_,m) -> m.Size) |> snd).Size
    let (enemyPoints, middle) = 
        match enemy with 
        | Area area -> points minMargin spacing area, midpoint area
        | Enemy models -> 
            let points = 
                models 
                |> Map.toList 
                |> List.map(fun (_,e) -> e.PosX, e.PosY)
            let (xs,ys) = points |> List.unzip 
            points, (List.average xs, List.average ys)
    let armyPoints = points minMargin spacing deploymentArea
    let bothPoints = seq { 
        for a in armyPoints do
            for b in enemyPoints do
                yield a, distance a b }
    let minPointDistances = 
        bothPoints
        |> Seq.groupBy fst 
        |> Seq.map (fun (a,dst) -> a, dst |> Seq.minBy snd |> snd) 
        |> Seq.distinctBy fst    
        |> Seq.sortBy snd
    Seq.zip (Seq.ofList models) minPointDistances
    |> Seq.map(fun (m,((offSetX,offSetY),_)) -> m,offSetX, offSetY )
    


let update msg model : Model * Cmd<Msg> =
    //Don't do this every time please
    
    match msg with
    | Distribute t -> 
            let (newModels, modelsCmds) =
                model.Models
                |> Map.toList 
                |> distributeClosestToEnemy model.Deployment.Dimensions t
                |> Seq.map (fun((_,m),x,y) -> MathHammer.Models.State.update (MathHammer.Models.Types.Msg.ChangePosition(x,y)) m)
                |> Seq.fold(fun (map,cmds) (m,cmd) -> (Map.add m.Name m map), (Cmd.map (fun msg -> ModelMsg(msg,m.Name)) cmd)::cmds) (model.Models,[])
            {model with Models = newModels}, Cmd.batch (modelsCmds)
    | ModelMsg(msg,key) -> 
            let (newModel, modelCmds) = model.Models.Item(key) |> MathHammer.Models.State.update msg
            {model with Models = Map.add key newModel model.Models}, Cmd.map (fun msg -> ModelMsg(msg,key)) modelCmds
