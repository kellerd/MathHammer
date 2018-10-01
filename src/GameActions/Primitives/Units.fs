module GameActions.Primitives.Units 

[<Measure>]
type ft

and [<Measure>] inch

and [<Measure>] mm

type ft with
    static member ToInch(a : int<ft>) : int<inch> = a * 12<inch/ft>
    static member ToMM(a : int<ft>) : int<mm> = a * 305<mm/ft>
    static member FromInch(a : int<inch>) : int<ft> = a / 12<inch/ft>
    static member FromMM(a : int<mm>) : int<ft> = a / 305<mm/ft>
    static member ToInchf(a : float<ft>) : float<inch> = a * 12.<inch/ft>
    static member ToMMf(a : float<ft>) : float<mm> = a * 305.<mm/ft>
    static member FromInchf(a : float<inch>) : float<ft> = a / 12.<inch/ft>
    static member FromMMf(a : float<mm>) : float<ft> = a / 305.<mm/ft>

type inch with
    static member ToFt a = a
    static member ToMM(a : int<inch>) : int<mm> = a * 25<mm/inch>
    static member FromMM(a : int<mm>) : int<inch> = a / 25<mm/inch>
    static member FromFt a = ft.ToInch a
    static member ToFtf a = ft.FromInchf a
    static member ToMMf(a : float<inch>) : float<mm> = a * 25.<mm/inch>
    static member FromMMf(a : float<mm>) : float<inch> = a / 25.<mm/inch>
    static member FromFtf a = ft.ToInchf a

type mm with
    static member ToInch a = inch.FromMM a
    static member ToFt a = ft.FromMM a
    static member FromInch a = inch.ToMM a
    static member FromFt a = ft.ToMM a
    static member ToInchf a = inch.FromMMf a
    static member ToFtf a = ft.FromMMf a
    static member FromInchf a = inch.ToMMf a
    static member FromFtf a = ft.ToMMf a