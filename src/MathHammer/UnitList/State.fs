module MathHammer.UnitList.State

open GameActions.Primitives.Types
open Elmish
open Types

let init name () : Model * Cmd<Msg> =
    { Location =
          { Label = name
            Fill = "#000000"
            Dimensions =
                { Top = 0<mm>
                  Left = 0<mm>
                  Width = ft.ToMM 6<ft>
                  Height = ft.ToMM 2<ft> } }
      Models = Map.empty<_, _>
      ElementFill = "#FFFFFF"
      ElementStroke = "#000000"
      Deployment =
          { Label = (Some "Deployment Zone")
            Fill = "#CCFFCC"
            Dimensions =
                { Top = 0<mm>
                  Left = 0<mm>
                  Width = ft.ToMM 6<ft>
                  Height = ft.ToMM 1<ft> } } }, Cmd.none

//Switch to hausdorff distance? http://cgm.cs.mcgill.ca/~godfried/teaching/cg-projects/98/normand/main.html
let distance (xa, ya) (xb, yb : float) =
    sqrt (pown (xb - xa) 2 + pown (yb - ya) 2)
let midpoint (x1, y1) (x2, y2) =
    List.average [ x1; x2 ], List.average [ y1; y2 ]
let midpointArea area =
    midpoint (float area.Left, float area.Top) 
        (float area.Left + float area.Width, float area.Top + float area.Height)

let slope (x1 : float, y1) (x2, y2) =
    let run = x2 - x1
    let rise = y2 - y1
    rise / run

let offsetY (x, y) m = y + (-1. * m * x)

let distributeClosestToEnemyHex (spacing : int<mm>) deploymentArea enemy models =
    let enemyMiddle =
        match enemy with
        | Area enemyArea -> midpointArea enemyArea
        | Enemy models -> 
            let (xs, ys) =
                models
                |> Map.toList
                |> List.map (fun (_, e) -> e.PosX, e.PosY)
                |> List.unzip
            List.average xs, List.average ys
    
    let armyMiddle = midpointArea deploymentArea
    
    let intersectionPoints =
        let m = slope enemyMiddle armyMiddle
        let b = offsetY armyMiddle m
        if m = 0.0 then 
            [ float deploymentArea.Left, (fst armyMiddle)
              
              float deploymentArea.Left + float deploymentArea.Width, 
              (fst armyMiddle)
              armyMiddle ]
        elif m = nan then //same deployment area
            [ armyMiddle ]
        elif m = infinity || m = -infinity then 
            [ (fst armyMiddle), float deploymentArea.Top
              
              (fst armyMiddle), 
              float deploymentArea.Top + float deploymentArea.Height
              armyMiddle ]
        else 
            [ (//X Intercepts
               float deploymentArea.Top - b) / m, float deploymentArea.Top
              
              (float deploymentArea.Top + float deploymentArea.Height - b) / m, 
              float deploymentArea.Top + float deploymentArea.Height
              //Y Intercepts
              float deploymentArea.Left, m * float deploymentArea.Left + b
              
              float deploymentArea.Left + float deploymentArea.Width, 
              m * (float deploymentArea.Left + float deploymentArea.Width) + b
              //Deployment inside itself
              armyMiddle ]
    
    let canDeploy (size : int<mm>) (x, y) =
        x - (float size / 2.000000000001) > float deploymentArea.Left 
        && y - (float size / 2.000000000001) > float deploymentArea.Top 
        && x + (float size / 2.000000000001) < float deploymentArea.Left 
                                               + float deploymentArea.Width 
        && y + (float size / 2.000000000001) < float deploymentArea.Top 
                                               + float deploymentArea.Height
    
    let origin size =
        intersectionPoints
        |> List.collect (fun (x, y) -> 
               [ x + float size / 2.0, y
                 x - float size / 2.0, y
                 x, y + float size / 2.0
                 x, y - float size / 2.0 ])
        |> List.filter (canDeploy size)
        |> List.minBy (distance enemyMiddle)
    
    let results =
        match models with
        | [] -> Seq.empty
        | (_, h : MathHammer.Models.Types.Model) :: _ -> 
            let size = h.Size
            let originPoint = origin size
            let (x, y) = originPoint
            let diameter = (size + (spacing * 2) |> float)
            let canDeploy = canDeploy size
            Seq.initInfinite (fun i -> i, diameter * float i)
            |> Seq.takeWhile 
                   (fun (_, r) -> x - 0.5 * r > float deploymentArea.Left)
            |> Seq.map (fun (i, r) -> 
                   seq { 
                       if i = 0 then yield (x, y)
                       else 
                           let h = (sqrt (3.0) / 2.0) * r
                           let hstep = h / float i
                           let dstep = diameter / 2.0
                           let a = (x - 0.5 * r, y + h)
                           let b = (x + 0.5 * r, y + h)
                           let c = (x + r, y)
                           let d = (x + 0.5 * r, y - h)
                           let e = (x - 0.5 * r, y - h)
                           let f = (x - r, y)
                           //Draw a hexagon
                           yield! List.map (fun x' -> x', snd a) 
                                      [ fst a..diameter..fst b - 0.00005 ]
                           yield! List.zip [ fst b..dstep..fst c - 0.00005 ] 
                                      [ snd b.. -hstep..snd c + 0.00005 ]
                           yield! List.zip [ fst c.. -dstep..fst d + 0.00005 ] 
                                      [ snd c.. -hstep..snd d + 0.00005 ]
                           yield! List.map (fun x' -> x', snd d) 
                                      [ fst d.. -diameter..fst e + 0.00005 ]
                           yield! List.zip [ fst e.. -dstep..fst f + 0.00005 ] 
                                      [ snd e..hstep..snd f - 0.00005 ]
                           yield! List.zip [ fst f..dstep..fst a - 0.00005 ] 
                                      [ snd f..hstep..snd a - 0.00005 ]
                   })
            |> Seq.collect (Seq.filter canDeploy) //Filter out points outside the zone
            |> Seq.truncate (max models.Length 60) //Say max 30 models in squad 
            |> Seq.sortBy (distance enemyMiddle) //Radiates from center
    
    Seq.zip models results

let update msg model : Model * Cmd<Msg> =
    //Don't do this every time please
    match msg with
    | Distribute enemyArea -> 
        let (newModels, modelsCmds) =
            let deploymentArea = model.Deployment.Dimensions
            let spacing = 1<inch> |> inch.ToMM
            model.Models
            |> Map.toList
            |> distributeClosestToEnemyHex spacing deploymentArea enemyArea
            |> Seq.map 
                   (fun ((_, m), (x, y)) -> 
                   MathHammer.Models.State.update 
                       (MathHammer.Models.Types.Msg.ChangePosition(x, y)) m)
            |> Seq.fold 
                   (fun (map, cmds) (m, cmd) -> 
                   (Map.add m.Name m map), 
                   (Cmd.map (fun msg -> ModelMsg(msg, m.Name)) cmd) :: cmds) 
                   (model.Models, [])
        { model with Models = newModels }, Cmd.batch (modelsCmds)
    | ModelMsg(msg, key) -> 
        let (newModel, modelCmds) =
            model.Models.Item(key) |> MathHammer.Models.State.update msg
        { model with Models = Map.add key newModel model.Models }, 
        Cmd.map (fun msg -> ModelMsg(msg, key)) modelCmds
