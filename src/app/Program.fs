open System
open Football.Units
open Football

open FsEx

let main() = 

    printfn "Start programm"
    printfn " "


    // Get Data from namesfile
    let getNameData() = 

        // Extract und Remove noise from data
        let extractNameData filepath= 

            let readlines (_filepath: string) = System.IO.File.ReadLines(_filepath)

            let numberindex (inputString:string) = 
                inputString 
                |> Seq.map (fun u -> System.Char.IsNumber(u)) 
                |> Seq.findIndex ((=) true)

            let checkStringForNumber (inputString': string) =

                let checkseq = 
                    inputString' 
                    |> Seq.map(fun u -> System.Char.IsNumber u)
                
                Seq.contains true checkseq

    
            let inputData_Surnames: Collections.Generic.IEnumerable<String> = readlines filepath  

            let removeIndexes: seq<int> = 
                inputData_Surnames 
                |> Seq.map(fun u -> numberindex u) //|> Seq.map (fun u -> u.Remove(5))


            let removeIndexesAsList = List.ofSeq removeIndexes  //Sequqnce need to be initiatet, so the right values for removel come out


            let removeNoiseFromString (stringSeq:seq<string>, intIndexSeq: List<int>) = 

                let firstStringRemoval = 
                    Seq.zip stringSeq intIndexSeq  
                    |> Seq.map (fun (u, z) -> u.Remove(z, u.Length - z)) 
                
                firstStringRemoval 
                    |> Seq.map (fun u -> u.Replace("   ", "")) 
                    |> Seq.map (fun u -> u.Replace(" ",""))
                    |> List.ofSeq


            removeNoiseFromString (inputData_Surnames, removeIndexesAsList) 

        (extractNameData "../lib/data/surnames.txt",extractNameData "../lib/data/names.txt") // retuns Surenames and names





    let germanTeamNames = System.IO.File.ReadLines("../lib/data/teamnames.txt")
    let (germanSurenames, germanNames) = getNameData()

    let Teamcount = 18
    let PlayerPerTeam = 22
    let PlayerCount =  Teamcount * PlayerPerTeam
    let MatchAmount = Teamcount - 2 // Starts with negative two, because array index starts at 0 and goes to Teamcount-2, because you have to subtract -1 for a match where a team normaly would play itself! Eg. Teamcount 18 the listindex would be 0..16 for 17 matches


    let createRandomTeam() = 
        let germanTeamNamesList = List.ofSeq(germanTeamNames)
        let randIndexForName = randintB(0, List.length germanTeamNamesList - 1)
        let randomName = List.item randIndexForName  germanTeamNamesList
        Agents.Team(randomName)

    let createRandomPlayer() = 

        let randage = randintB(16,40) * 1<years>

        let randIndexForName = randintB(0, List.length(germanNames) - 1) 
        let randomName = List.item randIndexForName  germanNames

        let randIndexForSurename = randintB(0, List.length(germanSurenames) - 1) 
        let randomSurename = List.item randIndexForSurename  germanSurenames

        Agents.FootballPlayer( randomSurename + " " + randomName, randage , None, None, None) 


    // Create Leage data
    let teams = [for i in 1..Teamcount do createRandomTeam()] 
    let players = [for i in 1..PlayerCount do createRandomPlayer()] 
    let leage = Agents.Leage(teams ,players ) 
  



    //_------------Utility functino-------------------
    //
    // Slice a list into smaller pieces, which sizes are determined by a stepsize
    //
    // testcase for sliceToPieces:  let list1 = [1;3;4;6;7;4;4;6;7]
    // let cheklist = sliceToPieces(list1, 0, 2)
    // should give: [[1; 3]; [4; 6]; [7; 4]; [4; 6]; [7]]
    let rec sliceToPieces(listx, currentindex, stepsize) = 

        let nextindex:int = currentindex + stepsize 
        let list1ength:int = List.length listx

        // For Debugging
        //printfn "current : %i" currentindex
        //printfn "length : %i" list1ength

        match list1ength with
        | list1ength when (list1ength > currentindex) -> listx[currentindex..nextindex-1] :: (sliceToPieces(listx, nextindex, stepsize))
        | _ -> []   //    it should be this, but this doesnt work, but [] does:  list1ength when list1ength <= stepsize -> listx[currentindex::nextindex]



    // Add players to Teams
    let slicedplayerlist = sliceToPieces(players, 0, PlayerPerTeam)
    [0..Teamcount-1] |> Seq.iter(fun u -> teams.[u].Players <- slicedplayerlist.[u])



    //_------------Utility functino-------------------
    //
    // Shuffel tuple in a array
    let shuffle (arrayx) =
        let rnd = System.Random()
        arrayx
        |> Array.sortBy(fun _ -> rnd.Next(0,Teamcount-1))





    printfn "---"

    // Helper functions 
    let getTupleVals arraytuple1 =

        //let listOfFirst = [for a, b in arraytuple1 do a]
        let listOfFirst = [ for (t , c) in arraytuple1 do yield t ]
        let listOfSeconds = [ for (t , c) in arraytuple1 do yield c ]

        List.sum listOfFirst + List.sum listOfSeconds


    let checkVals val1 val2 val3 = 

        let sum1 = val1 + val2

        match sum1 with
            | sum1 when sum1 = val3 -> true
            | _ -> false




    let checkAllRowsForSumOfRow(targetValueOfRow:int, matchAmount : int, targetNestedArray : (int * int) [,]) = 
        [for i in 0..matchAmount-1 do
            let internarr = targetNestedArray[*,i] |> getTupleVals
            printfn "%A"(internarr)
            match internarr with
            | internarr when internarr = targetValueOfRow -> false
            | _ -> false]


    let evalMatchDay(matchArr, matchAmount) = 

        let checkseries = [for i in 0..matchAmount do i + 1]
        let checkValue = List.sum checkseries * 2
        let test = checkAllRowsForSumOfRow(checkValue, matchAmount, matchArr)
        printfn "\n %A" (test)
        List.contains true test 



    // Create matchplan
    let createMatchplan ()= 
        let matchPairArray = array2D [for j in 0..Teamcount-1 do [for i in 0..Teamcount-1 do if j <> i then (j,i)]]     // Matrix with tuple pairs of index of a team
        array2D [| for i in 0..MatchAmount do shuffle matchPairArray[i,*] |]


    let test() =
        let a = createMatchplan()
        printfn "%A" (evalMatchDay(a, MatchAmount))


    let mutable continueLoop = true   
    let mutable a = createMatchplan()

    while continueLoop do
        let eval = evalMatchDay(a, MatchAmount)
        
        match eval with
        | eval when eval = true -> continueLoop <-false
        | _ -> continueLoop <-true
        a <- createMatchplan()
    

    //printfn "%A" (matchPairArray[*,17])
    //printfn "%A" (matchPairArray[*, 0] |> shuffle )



    //Check Team players
    //let testplayer = List.item 0 leage.players
    //let testteam = List.item 0 teams
    //testteam.Players |> Seq.iter(fun u -> printfn "%s" u.Name; printfn "%A" (u.PlayerAttributes) ; printfn "___________-")


    // TODO .> Programmierung von einem Pool an matches, aus den man pro spieltag ziehen kann. Random optimierung ist zu zeitaufwändig
    printfn "End programm"
    
    


main()
