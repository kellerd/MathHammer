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
    

let distance (xa,ya) (xb,yb : float) = sqrt (pown (xb - xa) 2 + pown (yb - ya) 2)
let midpoint (x1,y1) (x2,y2) = List.average [x1;x2], List.average [y1;y2]
let midpointArea area =  midpoint (float area.Left,float area.Top) (float area.Left + float area.Width,float area.Top + float area.Height)

let enemyPoints { Left = l; Top = t; Width = w; Height = h } nums =
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
    // [ for x in (float l) .. 50. .. (float l) + (float w) do 
    //     yield (float x, float t)
    //     yield (float x, float t + float h)
    //   for y in (int t) .. 50 .. (int t) + (int h) do 
    //     yield (float l, float y) 
    //     yield (float l + float w, float y) ]
let points { Left = l; Top = t; Width = w; Height = h }  =
    [ for x in (int l) .. (int l) + (int w) do 
          for y in (int t) .. (int t) + (int h) do 
            yield (float x, float y)  ]    

let enemy = Area ({ Top = 0<mm>; Left = 0<mm>; Width = 500<mm>; Height = 1000<mm> })
let deploymentArea = { Left = 0<mm>; Top = 600<mm>; Width = 500<mm>; Height = 1000<mm> }
let  { Left = l; Top = t; Width = w; Height = h } = { Top = 0<mm>; Left = 0<mm>; Width = 500<mm>; Height = 1000<mm> }
let distributeClosestToEnemy deploymentArea enemy models =
    let (enemyPoints, middle) = 
        match enemy with 
        | Area area -> enemyPoints area 1, midpointArea area
        | Enemy models -> 
            let points = 
                models 
                |> Map.toList 
                |> List.map(fun (_,e) -> e.PosX, e.PosY)
            let (xs,ys) = points |> List.unzip 
            Seq.ofList points, (List.average xs, List.average ys)
    let armyPoints = points deploymentArea
    let bothPoints = 
        seq {
            for a in armyPoints do
                        for b in enemyPoints do
                            yield a, distance a b 
        }
    let minPointDistances = 
        bothPoints
        |> Seq.groupBy fst 
        |> Seq.map (fun (a,dst) -> a, dst |> Seq.minBy snd |> snd) 
        |> Seq.sortBy (fun (a,dst) -> dst,distance a middle)
        |> Seq.map fst
    
    let canDeploy (x, y) (size:int<mm>) = 
        x - (float size / 2.0) > float deploymentArea.Left &&
        y - (float size / 2.0) > float deploymentArea.Top  &&
        x + (float size / 2.0) < float deploymentArea.Left + float deploymentArea.Width &&
        y + (float size / 2.0) < float deploymentArea.Top  + float deploymentArea.Height

    let ``Distance between two circle centers`` (x1:float,y1) (x2,y2) = pown (x2 - x1) 2 + pown (y2 - y1) 2 |> sqrt
    let ``Circles intersect`` (size:int<mm>) (size2:int<mm>) (distance:float) = float size + float size2 > distance && distance > abs (float size - float size2)
    let ``Distance between of two circles`` (size:int<mm>) (size2:int<mm>)  (x1,y1) (x2,y2) = ``Distance between two circle centers`` (x1,y1) (x2,y2) - float size - float size2

    let notTouchingOthers m = 
        List.forall (fun pick -> 
            ``Distance between two circle centers`` (pick.PosX,pick.PosY) (m.PosX,m.PosY)
            |> ``Circles intersect`` pick.Size m.Size
            |> not ) 

    let findAvailablePoints models points  = 
        Seq.unfold (fun (models, points, picked) -> 
            match models,Seq.tryHead points with 
            | [], _ | _, None -> None
            | (_,h)::tail, Some (px,py) ->
                let newModel = { h with PosX = px; PosY = py }
                let withinSpace = canDeploy (px,py) newModel.Size
                let notTouching = notTouchingOthers newModel picked 
                if withinSpace && notTouching then 
                    Some (Some (h,px,py), (tail,Seq.tail points,newModel::picked))
                else 
                    Some (None, (models,Seq.tail points,picked))
        ) (models,points,[])
        |> Seq.choose id

    let results = findAvailablePoints models minPointDistances |> Seq.toList 
    if results.Length <> models.Length then failwith "Had a hard time deploying all of the models"

    results

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
                |> distributeClosestToEnemy model.Deployment.Dimensions t
                |> Seq.map (fun(m,x,y) -> MathHammer.Models.State.update (MathHammer.Models.Types.Msg.ChangePosition(x,y)) m)
                |> Seq.fold(fun (map,cmds) (m,cmd) -> (Map.add m.Name m map), (Cmd.map (fun msg -> ModelMsg(msg,m.Name)) cmd)::cmds) (model.Models,[])
            {model with Models = newModels}, Cmd.batch (modelsCmds)
    | ModelMsg(msg,key) -> 
            let (newModel, modelCmds) = model.Models.Item(key) |> MathHammer.Models.State.update msg
            {model with Models = Map.add key newModel model.Models}, Cmd.map (fun msg -> ModelMsg(msg,key)) modelCmds
