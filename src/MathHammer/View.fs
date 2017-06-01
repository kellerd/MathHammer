module MathHammer.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types

[<Emit("$0")>] 
let svgRaw (text:string) : Fable.Import.React.ReactElement = jsNative
let root model dispatch =
  let top = 0.
  let bottom = 50.
  let displayModel model posX posY  = 
      g []
       [ circle [ Cx (!^ posX); Cy (!^ posY); R (!^ "3");  ] []
         text [ TextAnchor "middle"
                X (!^ posX)
                Y (!^ (posY + 7.))
                StrokeWidth (!^ ".1")
                Fill "#000000"
                Stroke "#000000"
                FontSize !^ "4"] 
              [svgRaw model] 
       ]

  let distribute offsetX offsetY f models =
    let rows = (List.length models / 10) + 1
    let rowWidth = 50. / (float rows + 1.)
    printfn "%d rows by %f" rows rowWidth
    [for i in 0 .. 10 .. List.length models - 1 do
        let maxPage = min (i + 9) (List.length models - 1)
        let columns = maxPage - i + 1
        let columnWidth = 100. / (float columns + 1.)
        printfn "columns %d: by %f" columns columnWidth
        yield! 
          [for j in i .. maxPage do
              let offsetX' = float (j - i + 1) * columnWidth + offsetX
              let offsetY' = (float (i / 10) + 1.) * rowWidth + offsetY
              yield f models.[j] offsetX' offsetY']
    ]

  let attackers models = 
    models 
    |> distribute 0. top displayModel
    |> g [Fill "#79CE0B" ; Stroke "#396302"; StrokeWidth (!^ "1")]
    

  let defenders models = 
    models 
    |> distribute 0. bottom displayModel
    |> g [Fill "#0B79CE" ; Stroke  "#023963"; StrokeWidth (!^ "1")]
    
  svg 
      [ ViewBox "0 0 100 100"; unbox ("width", "100%") ]
      (rect 
         [ SVGAttr.Y (!^ top)
           unbox ("width", "100%")
           unbox ("height", "50%")
           Fill "#FFEEEE"] []
      :: attackers model.Attacker
      :: rect 
         [ SVGAttr.Y (!^ bottom)
           unbox ("width", "100%")
           unbox ("height", "50%")
           Fill "#EEEEFF"] []
      :: [defenders model.Defender])
      
