// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open System.Numerics

let digits = 20000

let arctanInv (x:int):BigInteger =
    let bigDigits = BigInteger.Pow ((BigInteger 10), digits)
    let bigX = BigInteger x
    let rec iter (ret:BigInteger) (flag:int) (a:BigInteger) (b:BigInteger) =
        let ab_prod = BigInteger.Multiply(a,b)
        if bigDigits.CompareTo( ab_prod ) < 0 
        then 
            ret
        else
            let dv = if flag > 0 then  BigInteger.Divide (bigDigits, ab_prod) 
                     else BigInteger.Negate (BigInteger.Divide (bigDigits, ab_prod))
            iter (BigInteger.Add (ret, dv)) -flag  (BigInteger.Add(a, BigInteger(2))) (BigInteger.Multiply (BigInteger.Multiply (b, bigX), bigX))
    iter (BigInteger 0) 1 (BigInteger 1) (BigInteger x)


[<EntryPoint>]
let main argv = 
    printfn "start:%s" (DateTime.Now.ToString())
    let atan5 = arctanInv 5 
    printfn "atan5:%s" (DateTime.Now.ToString())
    let atan239 = arctanInv 239
    printfn "atan239:%s" (DateTime.Now.ToString())
    let pi = BigInteger.Multiply(BigInteger 4, BigInteger.Add( BigInteger.Multiply(BigInteger 4, atan5), BigInteger.Negate(atan239)))
    printfn "pi:%s" (DateTime.Now.ToString())
    printfn "%s" (pi.ToString());
    printfn "end:%s" (DateTime.Now.ToString())
//    let ans = BigInteger.Multiply ((BigInteger 4), (arctanInv 1))
//    printfn "%s\n" (ans.ToString())
    0 // return an integer exit code

