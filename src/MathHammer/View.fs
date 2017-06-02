module MathHammer.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types

[<Emit("$0")>] 
let svgRaw (text:string) : Fable.Import.React.ReactElement = jsNative
let root model dispatch =


  let drawing =   
    svg 
        [ ViewBox "0 0 100 100"; unbox ("width", "100%") ]
        [ UnitList.View.root model.Attacker dispatch
          UnitList.View.root model.Defender dispatch]
  div [] 
      [ i [ClassName "column fa fa-exchange"
           OnClick (fun _ -> Swap |> dispatch) ] [];
        div [] [drawing] ] 
