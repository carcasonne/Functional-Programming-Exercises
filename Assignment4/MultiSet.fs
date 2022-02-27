module MultiSet
    type MultiSet<'a when 'a : comparison> = MS of Map<'a, uint32>

    let empty = MS Map.empty<'a, uint32>

    let isEmpty (MS set) = 
        Map.isEmpty set

    let size (MS set) = 
        Map.fold (fun state key value -> state + value) 0u set

    let contains a (MS set) = 
        Map.containsKey a set

    let exists (o:option<uint32>) =
        match o with
        | Some x -> x
        | None -> 0u

    let numItems a (MS set) = 
        Map.tryFind a set |> exists

    let add a n (MS set) =
        MS (Map.add a n set)

    let addSingle a (MS set) =
        MS (Map.add a 1u set)

    let remove a (n:uint32) (MS set) = 
        let newValue = 
            if ((numItems a (MS set)) > n) then ((numItems a (MS set)) - n)
            else 0u

        MS (Map.add a newValue set);;