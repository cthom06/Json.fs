module Cthom06.Json

open Cthom06.Au
open Cthom06.Au.Parser.Operators

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
            Range ( ['a';'b';'c';'d';'e';'f';'A';'B';'C';'D';'E';'F';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9'],
                    fun c -> readHex (sprintf "%s%c" h c) )
    let backSlash =
        Parser.orOf
            [ lazy(Parser.char 'n' (Done '\n'))
              lazy(Parser.char 'r' (Done '\r'))
              lazy(Parser.char 'b' (Done '\b'))
              lazy(Parser.char 'f' (Done '\f'))
              lazy(Parser.char 't' (Done '\t'))
              lazy(Parser.char '\\' (Done '\\'))
              lazy(Parser.char '/' (Done '/'))
              lazy(Parser.char '"' (Done '"'))
              lazy(Parser.char 'u' (readHex "")) ]
    let rec readChars l =
        Parser.orOf
            [ lazy(Parser.char '"' (Done (List.rev l)))
              lazy(Parser.char '\\' (backSlash >>= (fcons l >> readChars)))
              lazy(Except ([], fcons l >> readChars)) ]
    Parser.char '"' (Done ()) >>= (fun _ -> readChars []) >>= (Array.ofSeq >> System.String >> Done)

let private parseNum =
    // BUG: doesn't care about leading zeros
    let digits =
        let range = ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9']
        // List.rev gets called every time here, maybe need to add laziness at least to or
        let rec moreDigits d = Or ( lazy(Range (range, fcons d >> moreDigits)),
                                    lazy(Done (List.rev d)) )
        Range (range, fcons [] >> moreDigits)
    let eSign = Or ( lazy(Range (['+'; '-'], fun c -> digits >>= (fun l -> Done (c::l)))),
                     lazy(digits) )
    let e = Or ( lazy(digits >>= (fun d -> Range (['e'; 'E'], fun c -> eSign <*> (cons c >> ((@)d))))),
                 lazy(digits) )
    let orDot = Or ( lazy(digits >>= (fun d ->
                        Parser.char '.' (e >>= (fun d2 -> Done (d@('.'::d2)))))),
                     lazy (e) )
    let minus = Or ( lazy(Parser.char '-' (Done ()) *> (orDot >>= (cons '-' >> Done))),
                     lazy(orDot) )
    minus <*> (Array.ofSeq >> System.String >> System.Double.Parse) <*> Number

let private parseString = parseStringLit <*> String
let private parseTrue = Parser.word "true" (Bool true)
let private parseFalse = Parser.word "false" (Bool false)
let private parseBool = Or (lazy (parseFalse), lazy (parseTrue))
let private parseNull = Parser.word "null" Null

let rec private eatWs p = Or ( lazy (Range ([' '; '\t'; '\r'; '\n'], fun _ -> eatWs p)),
                               lazy (p) )

let rec private parseValue () =
    Parser.orOf [ lazy(parseNum)
                  lazy(parseString)
                  lazy(parseBool)
                  lazy(parseNull)
                  lazy(parseArray ())
                  lazy(parseObject ()) ]

and private parseArray () =
    // BUG: allows trailing comma
    let rec getItems d =
        eatWs ( Or ( lazy(parseValue () >>= fun v -> anotherItem (v::d)),
                     lazy(Done (List.rev d)) ) )
    and anotherItem d =
        eatWs ( Or ( lazy(Parser.char ',' (getItems d)),
                     lazy(Done d) ) )
    
    Parser.char '[' (getItems []) >>= (fun d -> eatWs (Parser.char ']' (Done d))) <*> List

and private parseObject () =
    // BUG: allows trailing comma
    let rec getItems m =
        eatWs ( Or ( lazy(parseStringLit >>= (fun k ->
                        eatWs (Parser.char ':' (eatWs (parseValue ()) >>= (fun v -> anotherItem (Map.add k v m)))))),
                     lazy(Done m)))
    and anotherItem m =
        eatWs ( Or ( lazy(Parser.char ',' (getItems m)),
                     lazy(Done m) ) )
    Parser.char '{' (getItems Map.empty) >>= (fun d -> eatWs (Parser.char '}' (Done d))) <*> Object

