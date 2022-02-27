//open System

// GREEN

// 2.1

let rec downto1 x =
    if x > 0 then x :: downto1(x-1)
    else []

let rec downto2 x =
    match x with 
    | x when x > 0 -> x :: downto2(x-1)
    | _ -> []

// 2.2

//Checks if a number is odd
//let isOdd x = x % 2 <> 0

let rec removeOddIdx xs =
    match xs with
    | x::y::xs -> x :: removeOddIdx(xs) //take the pair at the front of the list, but only add the first element
    | x -> x

// 2.3

let rec combinePair xs =
    match xs with
    | x::y::xs -> (x,y) :: combinePair(xs) //take the pair at the front of the list, curry them together
    | x -> []

// 2.4

type complex = float * float;;

let mkComplex x y = complex(x,y);;

let complexToPair ((a,b):complex) = (a,b);;

let (|+|) ((a,b):complex) ((c,d):complex) = ((a+c,b+d):complex)

let (|*|) ((a,b):complex) ((c,d):complex) = ((a*c - b*d,b*c+a*d):complex)

let (|-|) ((a,b):complex) ((c,d):complex) = ((a-c,b-d):complex)

let (|/|) ((a,b):complex) ((c,d):complex) = (|*|) (a,b) ((c)/(c*c+d*d),(-d)/(c*c+d*d))


// 2.5

let explode1 str = 
    match str with
    | "" -> []
    | _ -> List.ofArray(str.ToCharArray())

let rec explode2 str =
    match str with
    | "" -> []
    | _ -> str.[0] :: explode2(str.Remove(0,1))

// 2.6

(* //Done without foldback
let rec implode strList =
    match strList with
    | [] -> ""
    | x :: xs -> x + implode(xs)
*)

//with foldback
let implode (clist:char list) = 
    List.foldBack(fun x acc -> x.ToString() + acc ) clist ""

//with fold
let implodeRev (clist:char list) = 
    List.fold(fun acc x -> x.ToString() + acc.ToString() ) "" clist

// 2.7

//let toUpper s = List.fold(fun (xs:char list) -> System.Char.ToUpper(x.[0]) :: xs) explode1(s) x::xs :> implode

let toUpper s = 
    let slist = explode2(s)
    let slist = List.map(fun x -> System.Char.ToUpper(x)) slist
    implode(slist)

let toUpper2 s = 
    explode2(s) |> List.map(fun x -> System.Char.ToUpper(x)) |> implode

//2.8

let rec ack (m,n) = 
    match (m,n) with
    | _ when m < 0 -> 0
    | _ when n < 0 -> 0
    | (0,n) -> n + 1
    | (m,0) when m > 0 -> ack(m-1, 1)
    | (m,n) -> ack(m-1, ack(m, n-1))





System.Console.WriteLine("GREEN tests")

System.Console.WriteLine(downto1(10))
System.Console.WriteLine(downto2(10))
System.Console.WriteLine(downto2(-5))

System.Console.WriteLine(removeOddIdx([0;1;2;3;4;5]))

System.Console.WriteLine(combinePair([0;1;2;3;4;5]))
System.Console.WriteLine(combinePair([0;1;2;3;4;5;6]))

System.Console.WriteLine(explode1 "Din mor")
System.Console.WriteLine(explode2 "Din mor")

System.Console.WriteLine(implode ['a'; 'b'; 'c'])

System.Console.WriteLine(implodeRev ['H'; 'e'; 'l'; 'l'; 'o'])




// YELLOW



