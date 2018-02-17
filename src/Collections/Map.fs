module Map 
    let inline mergeSet map key value  = 
        let newSet = value :: (Map.tryFind key map |> Option.toList ) |> Set.unionMany        
        Map.add key newSet map
    let inline mergeSets map1 map2 = 
        Map.fold mergeSet map1 map2
