module MathHammer.UnitList.State
open GameActions.Primitives.Types
open MathHammer.Models.Types
open Elmish
open Types
open Fable.AST.Fable
open Distribution.Example
open Distribution.Example

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
    

let distance (xa,ya) (xb,yb : float) = sqrt (pown (xb - xa) 2 + pown (yb - ya) 2)
let midpoint (x1,y1) (x2,y2) = List.average [x1;x2], List.average [y1;y2]
let midpointArea area =  midpoint (float area.Left,float area.Top) (float area.Left + float area.Width,float area.Top + float area.Height)
let nums = 2
let points { Left = l; Top = t; Width = w; Height = h } nums =
    let rec split (p1:float*float) p2 nums : seq<float*float>= seq {
        if nums > 0 then 
            let mid = midpoint p1 p2
            yield mid
            if mid <> p1 then yield! split p1 mid (nums - 1)
            if mid <> p2 then yield! split p2 mid (nums - 1)
    }
    let p  = float l          , float t
    let p2 = float l          , float t + float h
    let p3 = float l + float w, float t + float h
    let p4 = float l + float w, float t
    seq {
        yield p
        yield p2
        yield p3
        yield p4
        yield! split p  p2 nums
        yield! split p2 p3 nums
        yield! split p3 p4 nums
        yield! split p4 p nums
    }
let points { Left = l; Top = t; Width = w; Height = h }  =
    [ for x in (int l) .. (int l) + (int w) do 
          for y in (int t) .. (int t) + (int h) do 
            yield (float x, float y)  ]    

let area = { Top = 0<mm>; Left = 0<mm>; Width = 500<mm>; Height = 1000<mm> }
let enemy = Area (area)
let deploymentArea = { Left = 0<mm>; Top = 600<mm>; Width = 500<mm>; Height = 1000<mm> }
let  { Left = l; Top = t; Width = w; Height = h } = { Top = 0<mm>; Left = 0<mm>; Width = 500<mm>; Height = 1000<mm> }

let slope (x1:float,y1) (x2,y2) = 
    let run = x2 - x1
    let rise = y2 - y1
    rise/run
    
let offsetY (x,y) m = y + (-1. * m * x)

let points (size:float) r midpoint : seq<float*float> = 
    let (x,y) = midpoint
    seq {
        if r < size / 2. then yield midpoint 
        else
            for x' in [-r .. size .. r] do 
                let y' = sqrt (abs (r * r - x' * x'))  
                yield x+x', y+y'
                yield x+x', y-y' }      
let distributeClosestToEnemyHex spacing deploymentArea enemy models =
    let enemyMiddle = 
        match enemy with 
        | Area area -> midpointArea area
        | Enemy models -> 
            let (xs,ys)  = 
                models 
                |> Map.toList 
                |> List.map(fun (_,e) -> e.PosX, e.PosY)
                |> List.unzip 
            List.average xs, List.average ys

    let armyMiddle = midpointArea deploymentArea
    
    let intersectionPoints = 
        let m = slope enemyMiddle armyMiddle 
        let b = offsetY armyMiddle m 
        if m = 0.0 then 
            [float deploymentArea.Left, (fst armyMiddle)
             float deploymentArea.Left + float deploymentArea.Width, (fst armyMiddle)
             armyMiddle ]
        elif m = nan then //same deployment area
            [armyMiddle ]
        elif m = infinity then 
            [(fst armyMiddle), float deploymentArea.Top
             (fst armyMiddle), float deploymentArea.Top + float deploymentArea.Height
             armyMiddle ]
        else 
            [   //X Intercepts
                (float deploymentArea.Top - b) / m, float deploymentArea.Top  
                (float deploymentArea.Top + float deploymentArea.Height - b) / m, float deploymentArea.Top + float deploymentArea.Height                
                //Y Intercepts
                float deploymentArea.Left, m * float deploymentArea.Left + b
                float deploymentArea.Left + float deploymentArea.Width, m * (float deploymentArea.Left + float deploymentArea.Width) + b
                //Deployment inside itself
                armyMiddle ]

    
    let canDeploy (size:int<mm>) (x, y) = 
        x - (float size / 2.0) > float deploymentArea.Left &&
        y - (float size / 2.0) > float deploymentArea.Top  &&
        x + (float size / 2.0) < float deploymentArea.Left + float deploymentArea.Width &&
        y + (float size / 2.0) < float deploymentArea.Top  + float deploymentArea.Height

    let origin size = 
        intersectionPoints
        |> List.collect (fun (x,y) -> [x + float size / 2.0,y 
                                       x - float size/ 2.0,y
                                       x, y + float size/ 2.0
                                       x, y - float size/ 2.0])
        |> List.filter (canDeploy size)
        |> List.minBy (distance enemyMiddle)

    let models = [
            "A", MathHammer.Models.State.init "A"
            "B", MathHammer.Models.State.init "B"
            "C", MathHammer.Models.State.init "C"
            "D", MathHammer.Models.State.init "D"
            "E", MathHammer.Models.State.init "E"
    ]    
    match models with 
    | [ ] -> Seq.empty
    | (_,h)::tail -> 
        let size = h.Size
        let (x,y) = origin size 
        Seq.initInfinite (fun i -> 
            seq {
                if i = 0 then yield (x,y) 
                else 
                    let r = float (i + 1) * (size + spacing)
                    let A = (x+0.5*r, (sqrt(3) / 2.0) * r + y)
                    let C = (x+r,y)
                    let F = (x-r,y)

            }
        )

    
    ()
    // let results = findAvailablePoints models minPointDistances |> Seq.toList 
    // if results.Length <> models.Length then failwith "Had a hard time deploying all of the models"

    // results
// let models = 
//     ['A'..'z']
//     |> List.map (string >> fun n -> n, MathHammer.Models.State.init n)
// models |> distributeClosestToEnemy deploymentArea enemy 

let update msg model : Model * Cmd<Msg> =
    //Don't do this every time please
    
    match msg with
    | Distribute t -> 
            let (newModels, modelsCmds) =
                model.Models
                |> Map.toList 
                |> distributeClosestToEnemyHex model.Deployment.Dimensions t
                |> Seq.map (fun(m,x,y) -> MathHammer.Models.State.update (MathHammer.Models.Types.Msg.ChangePosition(x,y)) m)
                |> Seq.fold(fun (map,cmds) (m,cmd) -> (Map.add m.Name m map), (Cmd.map (fun msg -> ModelMsg(msg,m.Name)) cmd)::cmds) (model.Models,[])
            {model with Models = newModels}, Cmd.batch (modelsCmds)
    | ModelMsg(msg,key) -> 
            let (newModel, modelCmds) = model.Models.Item(key) |> MathHammer.Models.State.update msg
            {model with Models = Map.add key newModel model.Models}, Cmd.map (fun msg -> ModelMsg(msg,key)) modelCmds
