//taken from the assignment
type aExp =
| N of int // Integer value
| V of string // Variable
| WL // Length of the word
| PV of aExp // Point value of character at specific word index
| Add of aExp * aExp // Addition
| Sub of aExp * aExp // Subtraction
| Mul of aExp * aExp // Multiplication

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)

let a1 = N 42;;
let a2 = N 4 .+. (N 5 .-. N 6);;
let a3 = N 4 .*. N 2 .+. N 34;;
let a4 = (N 4 .+. N 2) .*. N 34;;
let a5 = N 4 .+. (N 2 .*. N 34);;

// 3.1
let rec arithEvalSimple : aExp -> int =
    function
    | N i -> i
    | Add (ex1,ex2) -> arithEvalSimple(ex1) + arithEvalSimple(ex2)
    | Sub (ex1,ex2) -> arithEvalSimple(ex1) - arithEvalSimple(ex2)
    | Mul (ex1,ex2) -> arithEvalSimple(ex1) * arithEvalSimple(ex2)

// 3.2

let rec arithEvalState (exp:aExp) (map:Map<string, int>) =
    match exp with
    | V s -> match map.TryFind(s) with
        | None -> 0
        | Some x -> x 
    | N i -> i
    | Add (exp,exp2) -> arithEvalState exp map + arithEvalState exp2 map
    | Sub (exp,exp2) -> arithEvalState exp map - arithEvalState exp2 map
    | Mul (exp,exp2) -> arithEvalState exp map * arithEvalState exp2 map

// 3.3
//taken from assignment
type word = (char * int) list

let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_");;
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithDoubleWordScore = N 2 .*. V "_acc_";;
let arithTripleWordScore = N 3 .*. V "_acc_";;

let getPointValue (char,int) = int

//let hello = ['H';'E';'L';'L';'O']

let hello = [('H',4);('E',1);('L',1);('L',1);('O',1)]

let rec arithEval (exp:aExp) (w:word) (map:Map<string, int>) =
    match exp with
    | V s -> match map.TryFind(s) with
        | None -> 0
        | Some x -> x 
    | N i -> i
    | WL -> w.Length
    | PV x -> w.[arithEval x w map] |> getPointValue
    | Add (exp,exp2) -> arithEval exp w map + arithEval exp2 w map
    | Sub (exp,exp2) -> arithEval exp w map - arithEval exp2 w map
    | Mul (exp,exp2) -> arithEval exp w map * arithEval exp2 w map



// 3.4

type cExp =
| C of char (* Character value *)
| ToUpper of cExp (* Converts lower case to upper case character, non-letters are unchanged *)
| ToLower of cExp (* Converts upper case to lower case character, non-letters are unchanged *)
| CV of aExp (* Character lookup at word index *)

let getChar (char,int) = char

let rec charEval (exp:cExp) (w:word) (map:Map<string, int>) =
    match exp with
    | C ch -> ch
    | ToUpper ex -> charEval ex w map |> System.Char.ToUpper
    | ToLower ex -> charEval ex w map |> System.Char.ToLower
    | CV cv -> w.[arithEval cv w map] |> getChar


// 3.5

type bExp =
| TT (* true *)
| FF (* false *)
| AEq of aExp * aExp (* numeric equality *)
| ALt of aExp * aExp (* numeric less than *)
| Not of bExp (* boolean not *)
| Conj of bExp * bExp (* boolean conjunction *)
| IsDigit of cExp (* check for digit *)
| IsLetter of cExp (* check for letter *)
| IsVowel of cExp (* check for vowel *)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)

let (.=.) a b = AEq (a, b)
let (.<.) a b = ALt (a, b)
let (.<>.) a b = ~~(a .=. b) (* numeric inequality *)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b) (* numeric less than or equal to *)
let (.>=.) a b = ~~(a .<. b) (* numeric greater than or equal to *)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

let isDigit c =
    match c with
    | int -> true
    | _ -> false

let isLetter c =
    match c with
    | char -> true
    | _ -> false

let isVowel c =
    match c with
    | 'A' -> true
    | 'E' -> true
    | 'I' -> true
    | 'O' -> true
    | 'U' -> true
    | 'Y' -> true
    | 'a' -> true
    | 'e' -> true
    | 'i' -> true
    | 'o' -> true
    | 'u' -> true
    | 'y' -> true
    | _ -> false

let rec boolEval (exp:bExp) (w:word) (map:Map<string, int>) =
    match exp with
    | TT -> true
    | FF -> false
    | AEq (a1,a2) -> arithEval a1 w map = arithEval a2 w map
    | ALt (a1,a2) -> arithEval a1 w map < arithEval a2 w map
    | Not b -> not (boolEval b w map)
    | Conj (b1,b2) -> (boolEval b1 w map) && (boolEval b2 w map)
    | IsDigit c -> charEval c w map |> System.Char.IsDigit
    | IsLetter c -> charEval c w map |> System.Char.IsLetter
    | IsVowel c -> charEval c w map |> isVowel

