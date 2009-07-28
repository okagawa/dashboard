// Learn more about F# at http://fsharp.net
#light
open System
open System.IO

type gb_prime () =
    let word_bits = 5                           (* 1word = 32bits *)
    let word_mask = (1L <<< word_bits) - 1L
    let max_val  = 8192L * 1024L * 16L// * 1024L
    let num_bits = max_val / 2L                  (* number of bits required *)
    let array_size = (int) (num_bits / (int64)(1 <<< word_bits))
    let subarray_bits = 21
    let subarray_size = (int) (1 <<< subarray_bits) / (1 <<< word_bits)   (* 2Mb *)
    let subarray_mask = (int64) (1 <<< subarray_bits) - 1L
    let num_subarray = array_size / subarray_size
    (* let max_sieve = 507640L *)
    
    let gb_array = Array.init num_subarray (fun i -> 
        Array.init subarray_size (fun j -> 
            match ((i * subarray_size + j) % 3) with
                | 0 -> 0x92492492u
                | 1 -> 0x24924924u
                | _ -> 0x49249249u))

    do gb_array.[0].[0] <- 0x92492491u
    let sieve_start = 5L

    let sieve_subarray i (subarray:uint32 array) = 
        let max_v = (((int64)(i + 1) <<< subarray_bits) * 2L) - 1L
        let max_v2 = max_v >>> 1
        let max_sieve = max_v |> float |> sqrt |> ceil |> int64
        let min_v = ((int64)i <<< subarray_bits) * 2L
        let rec next_prime n =
            let m = n + 2L
            if m > max_sieve then m else
                let i = (int) ((m >>> 1) >>> subarray_bits)
                let x = (int) (((m >>> 1) &&& subarray_mask) >>> word_bits)
                let b = (int) ( (m >>> 1) &&& word_mask)
                if ((~~~gb_array.[i].[x]) &&& (1u <<< b)) = 0u then
                    next_prime m
                else m
        let rec sieve_iter n =
            let n2 = (n * 2L) >>> 1
            if n > max_sieve then () else
                (* Console.WriteLine(n) *)
                (* n_tmp : nの奇数倍でmin_vを越える最小値 *)
                let n_tmp = (min_v / n + 1L) + if (min_v / n + 1L) % 2L = 0L then 1L else 0L
                (* m : n^2以上の数。n_tmpは奇数なので必ずnの奇数倍となる。 *)
                let m = ref ((n * if n_tmp > n then n_tmp else n) >>> 1)
                while !m < max_v2 do
                    (* we can ommit following oddity check, as !m must be odd *)
                    (* if !m % 2L <> 0L then *)
                    let x = (int) ((!m &&& subarray_mask) >>> word_bits)
                    let b = (int) ( !m &&& word_mask)
                    subarray.[x] <- subarray.[x] ||| (1u <<< b)
                    m := !m + n2
                done
                sieve_iter (next_prime n)
        sieve_iter sieve_start
                
    let sieve () =
        Array.iteri sieve_subarray gb_array

    let gba_count () =
        fst (Array.fold (fun (c,i) subarray ->
            let new_c = c + fst (Array.fold (fun (sc,j) (arry:uint32) ->
                    let r_arry = ~~~arry
                    let rec iter ret k =
                        if k > 31 then ret else
                            if (r_arry &&& (1u <<< k)) > 0u then
                              begin
                                Printf.printf "%s\n" (((((int64)k*2L+1L) + 64L*((int64)j))+(1L <<< 22)*((int64)i)).ToString())
                                iter (ret + 1L) (k + 1)
                              end
                            else iter ret (k + 1)
                    (sc + iter 0L 0, j + 1)
                                            ) (0L, 0) subarray);
            (new_c, i+1)
            ) (1L,0) gb_array)


    let save_gba fname =
        use fs = new FileStream(fname, FileMode.Create)
        use bw = new BinaryWriter(fs)
        Array.iter (fun a -> 
            Array.iter (fun (x:uint32) -> bw.Write(x)) a) gb_array;
        bw.Close()
        fs.Close()

    member this.sieve_n = sieve
    member this.count = gba_count
    member this.save = save_gba


let _ = 
  let tstart = DateTime.Now
  Console.WriteLine(tstart.ToString())
  let g = new gb_prime()
  Console.WriteLine("Initialize Done")
  g.sieve_n()
  Console.WriteLine("Sieve Done")
  let tsieve = DateTime.Now
  Console.WriteLine(tsieve.ToString())
  Console.WriteLine("timespan = {0}", tsieve.Subtract(tstart).TotalSeconds)
  (* g.save("plist.bin") *)
  Console.WriteLine(g.count())
  let tend = DateTime.Now
  Console.WriteLine(tend.ToString())
  let tspan:TimeSpan = tend.Subtract(tstart)
  Console.WriteLine("timespan = {0}", tspan.TotalSeconds)
