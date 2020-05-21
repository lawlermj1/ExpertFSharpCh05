
open System 
open System.IO
open System.Net
open System.Runtime.Serialization.Formatters.Binary

// Get the contents of the URL via a web request 
let http (url: string) =
    let req = WebRequest.Create(url) 
    let resp = req.GetResponse()
    let stream = resp.GetResponseStream()
    let reader = new StreamReader(stream)
    let html = reader.ReadToEnd()
    resp.Close()
    html

//  timer performance function
let time f = 
//  need full module path 
    let start = System.DateTime.Now 
//  using () makes it strict - otherwise it will be lazy and not execute 
    let res = f()
    let finish = System.DateTime.Now 
    (res, finish - start)  

let delimiters = [| ' '; '\n'; '\t'; '<'; '>'; '=' |] 
let getWords (s: string) = s.Split delimiters  

// site statistics 
type PageStats = 
    { Site : string 
      Time : System.TimeSpan
      Length : int
      NumWords : int
      NumRefs : int }

let stats site = 
    let url = "http://" + site  
    let html, t = time (fun () -> http url) 
    let words = html |> getWords 
    let hrefs = html |> getWords |> Array.filter (fun s -> s = "href")  
    { Site = site
      Time = t
      Length = html.Length 
      NumWords = words.Length 
      NumRefs = hrefs.Length }  

// person type example 
type Person = 
    { Name : string 
      DateOfBirth : DateTime} 

// dots and points types and functions 
type Dot = { X : int ; Y : int } 
type Point = { X : float ; Y : float } 

let coords1 (p : Point) = (p.X, p.Y)
let coords2 (p : Dot) = (p.X, p.Y)
let dist p = sqrt (p.X * p.X + p.Y * p.Y)

// Transport 
type Route = int 
type Make = string 
type Model = string 
type Transport = 
    | Car of Make * Model 
    | Bicycle 
    | Bus of Route 

let ian = Car("BMW", "360")
let don = [Bicycle; Bus 8]
let peter = [Car("Ford", "Fiesta"); Bicycle]
let averageSpeed (tr : Transport) = 
    match tr with 
    | Car _ -> 35 
    | Bicycle -> 16 
    | Bus _ -> 24 

//  reading and writing dotnet style using box and unbox from type <-> obj 
let writeValue outputStream x = 
    let formatter = new BinaryFormatter() 
    formatter.Serialize(outputStream, box x)

let readValue inputStream = 
    let formatter = new BinaryFormatter() 
    let res = formatter.Deserialize(inputStream)   
    unbox res 

// Generic Discrimated unions - haskell Monads 
// Using type variables like 'T or 'Key 
// type 'T option = 
//    | None 
//    | Some of 'T 

// this example has caused a few problems - not really useful either 
// type 'T list = 
//    | ( [] )
//    | ( :: ) of 'T * 'T list 

// from haskell Binary Tree 
// data Tree = RoseLeaf | Node Int Tree Tree deriving Show 
type Tree1<'T> = 
    | Tree of 'T * Tree1<'T> * Tree1<'T> 
    | Tip of 'T 

// Tree 
let cities = ["Munich"; "Rome"; "Florence"; "Berlin"; "Paris"; "Marseille"]

type Tree<'A> =
| Node of Tree<'A> * 'A * Tree<'A>
| RoseLeaf

let rec insert tree element = 
    match element, tree with
    | x, RoseLeaf                    -> Node(RoseLeaf, x, RoseLeaf)
    | x, Node(l,y,r) when x <= y -> Node((insert l x), y, r)
//    | x, Node(l,y,r) when x >  y -> Node(l, y, (insert r x))
//  ala Tomas Petricek 
    | x, Node(l,y,r) (*when x >  y*) -> Node(l, y, (insert r x))

let rec flatten = function
| RoseLeaf        -> []
| Node(l,x,r) -> flatten l @ [x] @ flatten r

let sort xs = xs |> List.fold insert RoseLeaf
                 |> flatten

let cityTree     = List.fold insert RoseLeaf cities
let sortedCities = sort cities

// from haskell Rose Tree 
// data Tree a = Node {
//        rootLabel :: a,         -- ^ label value
//        subForest :: Forest a   -- ^ zero or more child trees }
// type Forest a = [Tree a] 
// This is a multiple silmultaneous type example, which enables mutual recusion. 
type RoseTree<'T> = 
    | Knob of 'T * RoseTuple<'T> 
and  RoseTuple<'T> = 
    | NoRose 
    | Rose of RoseTree<'T> * RoseTuple<'T>  

// Now build a rose tree with list instead of tuple 
type RoseForest<'T> = 
// this works, as the List is explicitly the outer 'monad' which contains RoseForest 
    | RoseBranch of 'T * List<RoseForest<'T>> 
// this also works, but putting list last is less clear    
//    | RoseBranch of 'T * RoseForest<'T> list 
    | RoseLeaf of 'T

// not sure how  to use this ??? - it compiles, so what the hell 
let rec checkstuff tree =
    match tree with
    | RoseLeaf _ -> true
    | RoseBranch (node, children) ->
        List.fold ( || ) false (List.map checkstuff children)

//  tests if Upper case 
let isUpper (x : char) = (System.Char.ToUpper x) = x 
// vowel boolean 
let vowels = [ 'a'; 'e'; 'i'; 'o'; 'u' ]  
let isVowel (x : char) = List.contains x vowels 

//  this is a key type where 'Key = string, and 'Value = 'T 
type StringMap<'T> = Map<string, 'T> 

//  this defines a type as a tuple with a function and its inverse 
//  haskell (a -> b) -> (b -> a) - no hoogle instance  
//  not sure what it is for, but may be useful 
//  if the function is a bijection, then composing them should be an identity function  
type Projections<'T, 'U> = ('T -> 'U) * ('U -> 'T)

// redefintion of map 
let rec map2<'T, 'U> (f : 'T -> 'U) (l : 'T list) = 
    match l with 
    | h :: t -> f h :: map2 f t 
//  now it works! after removing redefined list above 
    | [] -> [] 

//  extract from a triple
let fst3 (a, _, _) = a
let snd3 (_, b, _) = b
let thd3 (_, _, c) = c

let mapPair f g (x, y) = (f x, g y)

// hcf highest common factor 
// works only with integers, by default, because 0, - and <  
let rec hcf a b = 
    if a = 0 then b 
    elif a < b then hcf a (b - a) 
    else hcf (a - b) b  

// hcf highest common factor - more generic 
// first define a type like the Num type class 
// called a dictionary of operations or a vtable in OOP 
type Numeric<'T> = 
    {Zero : 'T 
     Subtract : ('T -> 'T -> 'T)
     LessThan : ('T -> 'T -> bool) }

// define instances for different int types 
let intOps = {Zero = 0 ; Subtract = (-) ; LessThan = (<) }
let bigintOps = {Zero = 0I ; Subtract = (-) ; LessThan = (<) }
let int64Ops = {Zero = 0L ; Subtract = (-) ; LessThan = (<) }

// define a generic function across all int types 
let hcfGeneric (ops : Numeric<'T>) = 
    let rec hcf a b = 
        if a = ops.Zero then b 
        elif ops.LessThan a b then hcf a (ops.Subtract b a) 
        else hcf (ops.Subtract a b) b  
    hcf 

// convenient wrappers 
let hcfInt = hcfGeneric intOps 
let hcfbigInt = hcfGeneric bigintOps 

// checking types
let checkObject (x : obj) = 
    match x with 
    | :? string -> printfn "The object is a string"
    | :? int -> printfn "The object is an integer"
    | _ -> printfn "The object is something else"

let reportObject (x : obj) = 
    match x with 
    | :? string as s -> printfn "The object is the string '%s'" s
    | :? int as d -> printfn "The object is an integer '%d'" d
    | _  -> printfn "The object is something else " 
//    | _ as A -> printfn "The object is something else '%A'" A

//////////////////////////////////////////////////////////
// example data or function calls 
{ Name = "Bill" ; DateOfBirth = DateTime(1962, 09, 02) } 

stats "www.google.com"  

isVowel 'f' 

// Generic Monadic Tree - let's try RoseTree with int 
let n1 = Knob (10, NoRose) 
let n2 = Knob (20, NoRose) 
let n3 = Knob (20, Rose (n1, NoRose)) 

// let's try RoseTree with Transport 
let t1 = Knob (ian , NoRose)   
let t2 = Knob ((List.head don) , NoRose)  
let t3 = Knob ((List.head peter) , NoRose)  
let t4 = Knob ((List.head (List.tail don)) , NoRose)  
let t5 = Knob ((List.head (List.tail peter)) , NoRose)  
let t6 = Rose (t1, Rose (t2, Rose (t3, Rose (t4, NoRose))))

// let's try RoseForest with char   
let rt0 = RoseBranch ('c', [RoseBranch ('b', [])])
let rt1 = RoseBranch ('a', [RoseBranch ('b', [RoseLeaf 'c'; RoseLeaf 'd']); RoseBranch ('e', [RoseLeaf 'f'; RoseLeaf 'g'])])

// comparison examples 
('a','b') < ('a','z')  
compare ('a','b') ('a','z')  
compare [10; 30] [10; 20]  
compare [|10; 30|] [|10; 20|]  
compare [|10; 30|] [|10; 40|]  

// hash examples 
hash 100 ;; 
hash "abc" ;; 
hash (100, "abc") ;; 

// generic boxing and unboxing 
// this is used to convert data to obj type used by dotnet C# 
box 1 ;; 
box "abc" ;; 
let stringObj = box "abc" ;; 
// must tell unbox what the underlying type really is ... 
(unbox<string> stringObj) ;; 
(unbox stringObj : string) ;;  
// this fails with System.InvalidCastException
// (unbox stringObj : int) ;; 

// serialize example 
let addresses = 
    Map.ofList ["Jeff", "123 Main st Red WA 98052"
                "Fred", "987 Pind rd Phila PA 19116"
                "Mary", "PO Box 123, Palo Alto CA 94301" ]

let fsOut = new FileStream("Data.dat", FileMode.Create)
writeValue fsOut  addresses 
fsOut.Close() 

let fsIn = new FileStream("Data.dat", FileMode.Open)
let res : Map<string, string> = readValue fsIn 
fsIn.Close() 

// hcf 
hcf 18 12 ;; 
hcf 32 24 ;; 

hcfInt 18 12 ;; 
hcfbigInt 1810287116I 1239056I ;; 

// sub-typing 
let xobj = (1 :> obj) ;; 
let sobj = ("abc" :> obj) ;; 
let boxedObject = box "abc" ;; 
let downcastString = (boxedObject :? string) ;; 

let xobj2 = box 1 ;; 
let x2 = (xobj2 :? string) ;; 
// System.InvalidCastException: 
// let x3 = (xobj2 :?> string) ;; 

checkObject (box 1) ;; 
checkObject (box "abc") ;; 
checkObject (box 10.5) ;; 

reportObject (box 1) ;; 
reportObject (box "abc") ;; 
reportObject (box 10.5) ;; 
