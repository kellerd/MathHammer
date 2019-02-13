open ParseLibrary
#load "ParseLibrary.fsx"
open ParseLibrary
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


let satisfy = satisfy nextChar
let pchar charToMatch = 
    let predicate ch = (ch = charToMatch) 
    let label = sprintf "%c" charToMatch 
    satisfy predicate label 

let charListToStr (str:char list) = new String(str|>List.toArray)
let matchString str = 
    // label is just the string
    let label = str 

    str
    // convert to list of char
    |> List.ofSeq
    // map each char to a pchar
    |> List.map pchar 
    // convert to Parser<char list>
    |> sequence
    // convert Parser<char list> to Parser<string>
    |> mapP charListToStr 
    <?> label

let anyChar = satisfy (fun _ -> true) "any"
let space = 
    let predicate = Char.IsWhiteSpace 
    let label = "whitespace"
    satisfy predicate label 

let matchElizabethIsAweomse =
    matchString "Elizabeth" .>> (many space) .>>. matchString "is" .>> (many space) .>>. matchString "awesome"

fromStr  "Elizabeth is awesome" |> run matchElizabethIsAweomse











let pmanyuntil = manyUntil (pchar 'c') (anyChar) 
fromStr "aaaaaccccd" |> run pmanyuntil 










