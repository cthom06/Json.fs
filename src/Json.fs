module Cthom06.Json

type private ParseStep<'a> =
    | Done of 'a
    | NoChars of 'a
    | AnyChar of (char -> ParseStep<'a>)
    | Char of char * ParseStep<'a>
    | CharRange of char list * (char -> ParseStep<'a>)
    | Or of ParseStep<'a> Lazy * ParseStep<'a> Lazy
    with
        override this.ToString () =
            match this with
            | Done v -> "[Completed Computation]"
            | NoChars _ -> "[End of Input]"
            | AnyChar _ -> "[Any Character]"
            | CharRange (cs, _) -> "[Any of " + System.String.Join(" ", (cs |> Seq.map string)) + "]"
            | Char (c, _) -> "'" + (c.ToString ()) + "'"
            | Or (p1, p2) -> p1.ToString () + " or " + p2.ToString ()

type private ParseError<'a> =
    | NotError of 'a
    | Error of ('a ParseStep * char list)

module private ParseStep =
    let rec bind p f =
        match p with
        | Done a
        | NoChars a -> f a
        | AnyChar g -> AnyChar (fun c -> bind (g c) f)
        | Char (c, g) -> Char (c, bind g f)
        | CharRange (cs, g) -> CharRange (cs, fun c -> bind (g c) f)
        | Or (g, h) -> Or (lazy (bind g.Value f), lazy (bind h.Value f))

    let rec map p f =
        match p with
        | Done a -> Done (f a)
        | NoChars a -> NoChars (f a)
        | Char (c, g) -> Char (c, map g f)
        | AnyChar g -> AnyChar (fun c -> map (g c) f)
        | CharRange (cs, g) -> CharRange (cs, fun c -> map (g c) f)
        | Or (g, h) -> Or (lazy(map g.Value f), lazy (map h.Value f))

    let rec orOf steps =
        match steps with
        | a::b::[] -> Or (a, b)
        | x::xs -> Or (x, lazy(orOf xs))
        | [] -> raise (new System.InvalidOperationException ())

    let rec eval p l =
        match p, l with
        | Done v, xs -> NotError v, xs
        | NoChars v, [] -> NotError v, []
        | AnyChar f, x::xs -> eval (f x) xs
        | Char (c, v), x::xs when c = x -> eval v xs
        | CharRange (cs, f), x::xs when List.contains x cs ->
            eval (f x) xs
        | Or (p1, p2), l ->
            match eval p1.Value l with
            | Error (e1,r1), _ ->
                match eval p2.Value l with
                | Error (e2, r2), _ ->
                    if List.length r2 <= List.length r1 then
                        Error (e2,r2), l
                    else
                        Error (e1, r1), l
                | NotError v, xs -> NotError v, xs
            | NotError v, xs -> NotError v, xs
        | p, xs -> Error (p, xs), l

    let word s v =
        bind (Seq.fold (fun d c -> bind d  (fun _ -> Char (c, Done ()))) (Done ()) s) (fun _ -> Done v)
        

let private (>>=) a b = ParseStep.bind a b
let private (>>>) a b = a >>= fun _ -> b
let private (<*>) a b = ParseStep.map a b
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
            CharRange ( ['a';'b';'c';'d';'e';'f';'A';'B';'C';'D';'E';'F';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9'],
                        fun c -> readHex (sprintf "%s%c" h c) )
    let backSlash =
        ParseStep.orOf
            [ lazy(Char ('n', Done '\n'))
              lazy(Char ('r', Done '\r'))
              lazy(Char ('b', Done '\b'))
              lazy(Char ('f', Done '\f'))
              lazy(Char ('t', Done '\t'))
              lazy(Char ('\\',Done '\\'))
              lazy(Char ('/', Done '/'))
              lazy(Char ('"', Done '"'))
              lazy(Char ('u', readHex "")) ]
    let rec readChars l =
        ParseStep.orOf
            [ lazy(Char ('"', Done (List.rev l)))
              lazy(Char ('\\', backSlash >>= (fcons l >> readChars)))
              lazy(AnyChar (fcons l >> readChars)) ]
    Char ('"', Done ()) >>= (fun _ -> readChars []) >>= (Array.ofSeq >> System.String >> Done)

let private parseNum =
    // BUG: doesn't care about leading zeros
    let digits =
        let range = ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9']
        // List.rev gets called every time here, maybe need to add laziness at least to or
        let rec moreDigits d = Or ( lazy(CharRange (range, fcons d >> moreDigits)),
                                    lazy(Done (List.rev d)) )
        CharRange (range, fcons [] >> moreDigits)
    let eSign = Or ( lazy(CharRange (['+'; '-'], fun c -> digits >>= (fun l -> Done (c::l)))),
                     lazy(digits) )
    let e = Or ( lazy(digits >>= (fun d -> CharRange (['e'; 'E'], fun c -> eSign <*> (cons c >> ((@)d))))),
                 lazy(digits) )
    let orDot = Or ( lazy(digits >>= (fun d ->
                        Char ('.', e >>= (fun d2 -> Done (d@('.'::d2)))))),
                     lazy (e) )
    let minus = Or ( lazy(Char ('-', Done ()) >>> (orDot >>= (cons '-' >> Done))),
                     lazy(orDot) )
    minus <*> (Array.ofSeq >> System.String >> System.Double.Parse) <*> Number

let private parseString = parseStringLit <*> String
let private parseTrue = ParseStep.word "true" (Bool true)
let private parseFalse = ParseStep.word "false" (Bool false)
let private parseBool = Or (lazy (parseFalse), lazy (parseTrue))
let private parseNull = ParseStep.word "null" Null

let rec private eatWs p = Or ( lazy (CharRange ([' '; '\t'; '\r'; '\n'], fun _ -> eatWs p)),
                               lazy (p) )

let rec private parseValue () =
    ParseStep.orOf [ lazy(parseNum)
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
        eatWs ( Or ( lazy(Char (',', getItems d)),
                     lazy(Done d) ) )
    
    Char ('[', getItems []) >>= (fun d -> eatWs (Char (']', Done d))) <*> List

and private parseObject () =
    // BUG: allows trailing comma
    let rec getItems m =
        eatWs ( Or ( lazy(parseStringLit >>= (fun k ->
                        eatWs (Char (':', eatWs (parseValue ()) >>= (fun v -> anotherItem (Map.add k v m)))))),
                     lazy(Done m)))
    and anotherItem m =
        eatWs ( Or ( lazy(Char (',', getItems m)),
                     lazy(Done m) ) )
    Char ('{', getItems Map.empty) >>= (fun d -> eatWs (Char ('}', Done d))) <*> Object

