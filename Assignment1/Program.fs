//open System

// GREEN

// 1.1
let sqr x = x * x

// 1.2
let pow x n = System.Math.Pow(x, n)

// 1.3
let rec sum =
    function 
    | 0 -> 0
    | x -> x + sum(x - 1);;

// 1.4
let rec fib =
    function
    | 0 -> 0
    | 1 -> 1
    | x -> fib(x-1) + fib(x-2);;


// 1.5

//let dup x = x + x

//make it handle no input

let rec dup =
    function 
    | "" -> ""
    | x -> x + x;;

//printfn "%s" (dup "Hej ")

// 1.6
let rec dupn x n = 
    match n with
    | 0 -> ""
    | 1 -> x
    | n -> x + (dupn x (n-1));;

// 1.7
let rec bin =
    function
    | n,k when k = 0 -> 1
    | n,k when n = k -> 1
    | n,k when k > n -> 0
    | (n, k) -> (bin(n-1,k-1)) + (bin(n-1, k));;

(*
Console.WriteLine("Green tests")
Console.WriteLine(sqr 3)
Console.WriteLine(pow 3 3)
Console.WriteLine(sum 5)
Console.WriteLine(fib 7)
Console.WriteLine(dup "hi ")
Console.WriteLine(dupn "hi " 4)
Console.WriteLine(bin (4,2))
*)

// YELLOW

// 1.8
let timediff (h1, m1) (h2, m2) = (h2-h1)*60 + m2-m1

// 1.9

let minutes (hh, mm) = timediff (00, 00) (hh, mm) 

// 1.10
let curry func x y = func (x,y);;

let uncurry func (x,y) = func x y;;

// 1.11
let empty (letter, pointValue) =  fun pos -> (letter, pointValue)

(*
System.Console.WriteLine("Yellow tests")
System.Console.WriteLine("timediff: "); 
    System.Console.WriteLine(timediff (10,20) (23,40));
    System.Console.WriteLine(timediff (23,40) (10,20));
    System.Console.WriteLine(timediff (10,40) (10,50));
    System.Console.WriteLine(timediff (12,34) (11,35));
    System.Console.WriteLine(timediff (12,34) (13,35));
System.Console.WriteLine("minutes:");
    System.Console.WriteLine(minutes (23,00)); 
    System.Console.WriteLine(minutes (00,00)); 
    System.Console.WriteLine(minutes (12,00)); 
    System.Console.WriteLine(minutes (23,58)); //best iron maiden song :)))) 
    System.Console.WriteLine(minutes (11,35)); 
*)

