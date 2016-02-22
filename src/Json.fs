module Cthom06.Json

open Cthom06.Au
open Cthom06.Au.Operators

let private cons a b = a::b
let private fcons a b = b::a

type Json =
    | Null
    | Bool of bool
    | String of string
    | Number of double
    | List of Json list
    | Object of Map<string,Json>

let private parseStringLit =
    let rec readHex h = // the strcat is lazy
        if String.length h = 4 then
            Done (System.Convert.ToInt32 (h, 16) |> char)
        else
            Parser.range
                ['a';'b';'c';'d';'e';'f';'A';'B';'C';'D';'E';'F';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9']
                (fun c -> readHex (sprintf "%s%c" h c))
    let backSlash =
        Parser.exactly 'n' (Done '\n')
        <|> Parser.exactly 'r' (Done '\r')
        <|> Parser.exactly 'b' (Done '\b')
        <|> Parser.exactly 'f' (Done '\f')
        <|> Parser.exactly 't' (Done '\t')
        <|> Parser.exactly '\\' (Done '\\')
        <|> Parser.exactly '/' (Done '/')
        <|> Parser.exactly '"' (Done '"')
        <|> Parser.exactly 'u' (readHex "")
    let rec readChars l =
        Parser.exactly '\\' backSlash
        <|> Parser.except ['\\'; '"'] Done
    Parser.exactly '"' (Done ())
    *> Parser.repeat (readChars [])
    >>= (Done >> Parser.exactly '"')
    >>= (Array.ofSeq >> System.String >> Done)

let private parseNum =
    // BUG: doesn't care about leading zeros
    let range = ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9']
    let digit = Parser.range range Done
    let digits = digit >>= (fun c -> Parser.repeat digit <*> cons c)
    let eSign = Parser.range ['+'; '-'] (fun c -> Done [c]) <|> Done []
    let e =
        Parser.range ['e'; 'E'] (fun c ->
            eSign >>= (fun sgn ->
                digits <*> (fun num -> [c]@sgn@num)))
    let dec = digits >>= (fun d -> Parser.exactly '.' (digits <*> (fun d2 -> d@('.'::d2))))
    let minus = Parser.exactly '-' (Done ['-'])
    (minus <|> Done []) >>= (fun sgn ->
        (dec <|> digits) >>= (fun bse ->
            printfn "read a base %A" bse
            (e <|> Done []) <*> (fun ev ->
                Seq.concat [sgn; bse; ev])))
    <*> (Array.ofSeq >> System.String >> System.Double.Parse) <*> Number

let private parseString = parseStringLit <*> String
let private parseTrue = Parser.sequence "true" (Bool true)
let private parseFalse = Parser.sequence "false" (Bool false)
let private parseBool = parseTrue <|> parseFalse
let private parseNull = Parser.sequence "null" Null

let private wsc c = Parser.eatWhite *> Parser.exactly c (Done ())

let private csv p =
    p >>= (fun v -> Parser.repeat (wsc ',' *> p) <*> cons v)

let rec private parseValue () =
    parseNum
    <|> parseString
    <|> parseBool
    <|> parseNull
    <~> lazy (parseArray ())
    <~> lazy (parseObject ())

and private parseArray () =
    let oneValue = Parser.eatWhite >>= parseValue
    let endBracket l = wsc ']' *> Done l
    Parser.exactly '[' (Done ())
    *> (endBracket [] <|> (csv oneValue >>= endBracket))
    <*> List

and private parseObject () =
    let key = Parser.eatWhite *> parseStringLit
    let value = Parser.eatWhite >>= parseValue
    let pair = key >>= (fun k -> wsc ':' *> value <*> (fun v -> k, v))
    let endCurl l = wsc '}' *> Done l
    Parser.exactly '{' (Done ())
    *> (endCurl [] <|> (csv pair >>= endCurl))
    <*> (Map.ofSeq >> Object)

