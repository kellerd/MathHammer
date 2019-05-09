(*
ParserLibrary.fsx
Final version of a parser library.
Related blog post: http://fsharpforfunandprofit.com/posts/understanding-parser-combinators-3/
*)

module TextInput =
    open System

    type Position = {
        line : int
        column : int
    }
    
    /// define an initial position
    let initialPos = {line=0; column=0}

    /// increment the column number
    let incrCol pos = 
        {pos with column=pos.column + 1}

    /// increment the line number and set the column to 0
    let incrLine pos = 
        {line=pos.line + 1; column=0}

    /// Define the current input state
    type InputState = {
        lines : string[]
        position : Position 
    }

    // return the current line
    let currentLine inputState = 
        let linePos = inputState.position.line
        if linePos < inputState.lines.Length then
            inputState.lines.[linePos]
        else
            "end of file"

    /// Create a new InputState from a string
    let fromStr str = 
        if String.IsNullOrEmpty(str) then
            {lines=[||]; position=initialPos}
        else
            let separators = [| "\r\n"; "\n" |]
            let lines = str.Split(separators, StringSplitOptions.None)
            {lines=lines; position=initialPos}


    /// Get the next character from the input, if any
    /// else return None. Also return the updated InputState
    /// Signature: InputState -> InputState * char option 
    let nextChar input =
        let linePos = input.position.line
        let colPos = input.position.column
        // three cases
        // 1) if line >= maxLine -> 
        //       return EOF
        // 2) if col less than line length -> 
        //       return char at colPos, increment colPos
        // 3) if col at line length -> 
        //       return NewLine, increment linePos

        if linePos >= input.lines.Length then
            input, None
        else
            let currentLine = currentLine input
            if colPos < currentLine.Length then
                let char = currentLine.[colPos]
                let newPos = incrCol input.position 
                let newState = {input with position=newPos}
                newState, Some char
            else 
                // end of line, so return LF and move to next line
                let char = '\n'
                let newPos = incrLine input.position 
                let newState = {input with position=newPos}
                newState, Some char

// ===========================================
// Parser code
// ===========================================

open System

// Aliases for input, etc
type ParserLabel = string
type ParserError = string

/// Stores information about the parser position for error messages
type ParserPosition<'b> = {
    node: 'b
    // currentLine : string
    // line : int
    // column : int
    }

// // Result type
// type Result<'a,'b> =
//     | Ok of 'a
//     | Error of ParserLabel * ParserError * ParserPosition<'b> 

/// A Parser structure has a parsing function & label
type Parser<'a,'b> = {
    parseFn : ('b -> Result<'a,ParserLabel * ParserError * ParserPosition<'b> >)     //Result<'a,'b>
    label:  ParserLabel 
    }


/// Run the parser on a InputState
let runOnInput parser input = 
    // call inner function with input
    parser.parseFn input

/// Run the parser on a string
let run parser inputStr = 
    // call inner function with input
    runOnInput parser inputStr

// =============================================
// Error messages
// =============================================

let parserPositionFromInputState (inputState) = {
    node = inputState
    }
        
let printResult debug result =
    match result with
    | Ok (value,input) -> 
        printfn "%A" value
    | Error (label : ParserLabel,error:ParserError,parserPos) -> 
        let errorLine = debug parserPos.node
        // let colPos = parserPos.column
        // let linePos = parserPos.line
        // let ErrorCaret = sprintf "%*s^%s" colPos "" error
        // printfn "Line:%i Col:%i Error parsing %s\n%s\n%s" linePos colPos label errorLine ErrorCaret 
        printfn "Error parsing %s\n%s\n" label errorLine 


// =============================================
// Label related
// =============================================

/// get the label from a parser
let getLabel parser = 
    // get label
    parser.label

/// update the label in the parser
let setLabel parser newLabel = 
    // change the inner function to use the new label
    let newInnerFn input = 
        let result = parser.parseFn input
        match result with
        | Ok s ->
            // if Ok, do nothing
            Ok s 
        | Error (oldLabel,err,pos) -> 
            // if Error, return new label
            Error (newLabel,err,pos) 
    // return the Parser
    {parseFn=newInnerFn; label=newLabel}

/// infix version of setLabel
let ( <?> ) = setLabel


// =============================================
// Standard combinators
// =============================================

/// Match an input token if the predicate is satisfied
let satisfy next predicate label =
    let innerFn input =
        let remainingInput,charOpt = next input 
        match charOpt with
        | None -> 
            let err = "No more input"
            let pos = parserPositionFromInputState input
            Error (label,err,pos)
        | Some first -> 
            if predicate first then
                Ok (first,remainingInput)
            else
                let err = sprintf "Unexpected '%A'" first
                let pos = parserPositionFromInputState input
                Error (label,err,pos)
    // return the parser
    {parseFn=innerFn;label=label}

/// "bindP" takes a parser-producing function f, and a parser p
/// and passes the output of p into f, to create a new parser
let bindP f p =
    let label = "unknown"
    let innerFn input =
        let result1 = runOnInput p input 
        match result1 with
        | Error (label,err,pos) -> 
            // return error from parser1
            Error (label,err,pos)  
        | Ok (value1,remainingInput) ->
            // apply f to get a new parser
            let p2 = f value1
            // run parser with remaining input
            runOnInput p2 remainingInput
    {parseFn=innerFn; label=label}

/// Infix version of bindP
let ( >>= ) p f = bindP f p

/// Lift a value to a Parser
let returnP x = 
    let label = sprintf "%A" x
    let innerFn input =
        // ignore the input and return x
        Ok (x,input)
    // return the inner function
    {parseFn=innerFn; label=label}

/// apply a function to the value inside a parser
let mapP f p = 
    let label = getLabel p 
    bindP (f >> returnP) p <?> label

/// infix version of mapP
let ( <!> ) = mapP

/// "piping" version of mapP
let ( |>> ) x f = mapP f x

/// apply a wrapped function to a wrapped value
let applyP fP xP =         
    fP >>= (fun f -> 
    xP >>= (f >> returnP))

/// infix version of apply
let ( <*> ) = applyP

/// lift a two parameter function to Parser World
let lift2 f xP yP =
    returnP f <*> xP <*> yP

/// Combine two parsers as "A andThen B"
let andThen p1 p2 =         
    let label = sprintf "%s andThen %s" (getLabel p1) (getLabel p2)
    p1 >>= (fun p1Result -> 
    p2 >>= (fun p2Result -> 
        returnP (p1Result,p2Result) ))
    <?> label

/// Infix version of andThen
let ( .>>. ) = andThen

/// Combine two parsers as "A orElse B"
let orElse p1 p2 =
    let label = sprintf "%s orElse %s" (getLabel p1) (getLabel p2)
    let innerFn input =
        // run parser1 with the input
        let result1 = runOnInput p1 input

        // test the result for Error/Ok
        match result1 with
        | Ok result -> 
            // if Ok, return the original result
            result1

        | Error _ -> 
            // if failed, run parser2 with the input
            let result2 = runOnInput p2 input

            // return parser2's result
            result2 

    // return the inner function
    {parseFn=innerFn; label=label}

/// Infix version of orElse
let ( <|> ) = orElse

/// Choose any of a list of parsers
let choice listOfParsers = 
    List.reduce ( <|> ) listOfParsers 
let anyOf p tests = 
    List.map p tests
    |> choice
let rec sequence parserList =
    // define the "cons" function, which is a two parameter function
    let cons head tail = head::tail

    // lift it to Parser World
    let consP = lift2 cons

    // process the list of parsers recursively
    match parserList with
    | [] -> 
        returnP []
    | head::tail ->
        consP head (sequence tail)

/// (helper) match zero or more occurences of the specified parser
let rec parseZeroOrMore parser input =
    // run parser with the input
    let firstResult = runOnInput parser input 
    // test the result for Error/Ok
    match firstResult with
    | Error _ -> 
        // if parse fails, return empty list
        ([],input)  
    | Ok (firstValue,inputAfterFirstParse) -> 
        // if parse succeeds, call recursively
        // to get the subsequent values
        let (subsequentValues,remainingInput) = 
            parseZeroOrMore parser inputAfterFirstParse
        let values = firstValue::subsequentValues
        (values,remainingInput)  
let parseZeroOrMoreUntil  until parser input = 
    let rec innerFn input =
        match runOnInput until input with
        | Ok(untilValue,inputAfterUntil) ->
            Ok([untilValue], inputAfterUntil)
        | Error _ -> 
            match runOnInput parser input with 
            | Error(a,b,c) -> 
                // Both failed return empty match
                Error(a,b,c)
            | Ok(firstValue, inputAfterFirstParse) ->
                match innerFn inputAfterFirstParse with
                | Ok  (subsequentValues,remainingInput) ->
                    let values = firstValue::subsequentValues
                    Ok(values, remainingInput)  
                | Error(a,b,c) -> 
                    // Both failed return empty match
                    Error(a, b, c)
    match innerFn input with 
    | Error(a,b,c) -> 
                    // Both failed return empty match
                    Error(a, b, c)
    | Ok (a, b) -> Ok(a |> List.rev, b)             
/// matches zero or more occurences of the specified parser
let many parser = 
    let label = sprintf "many %s" (getLabel parser)
    let innerFn input =
        // parse the input -- wrap in Ok as it always succeeds
        Ok (parseZeroOrMore parser input)
    {parseFn=innerFn; label=label}
let manyUntil until parser = 
    let label = sprintf "many %s until %s" (getLabel parser) (getLabel until) 
    {parseFn = parseZeroOrMoreUntil until parser; label = label}


/// matches one or more occurences of the specified parser
let many1 p =         
    let label = sprintf "many1 %s" (getLabel p)

    p      >>= (fun head -> 
    many p >>= (fun tail -> 
        returnP (head::tail) ))
    <?> label

/// Parses an optional occurrence of p and returns an option value.
let opt p = 
    let label = sprintf "opt %s" (getLabel p)
    let some = p |>> Some
    let none = returnP None
    (some <|> none) <?> label

/// Keep only the result of the left side parser
let (.>>) p1 p2 = 
    let label = sprintf "%s andThen %s" (getLabel p1) (getLabel p2)
    // create a pair
    p1 .>>. p2 
    // then only keep the first value
    |> mapP (fun (a,b) -> a) 
    <?> label

/// Keep only the result of the right side parser
let (>>.) p1 p2 = 
    let label = sprintf "%s andThen %s" (getLabel p1) (getLabel p2)
    // create a pair
    p1 .>>. p2 
    // then only keep the second value
    |> mapP (fun (a,b) -> b) 
    <?> label
    
/// Keep only the result of the middle parser
let between p1 p2 p3 = 
    p1 >>. p2 .>> p3 

/// Parses one or more occurrences of p separated by sep
let sepBy1 p sep =
    let sepThenP = sep >>. p            
    p .>>. many sepThenP 
    |>> fun (p,pList) -> p::pList

/// Parses zero or more occurrences of p separated by sep
let sepBy p sep =
    sepBy1 p sep <|> returnP []
