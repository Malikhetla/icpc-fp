module ICPC
open System

let commaSprinkler input =
    let thing = input.ToString()
    match thing.Length < 2 with 
    | true -> None 
    | false -> 
            let newTing = List.ofSeq(thing)
            let rec checkForNonValid lst = 
               match lst with 
               | [] -> None
               | a::rest -> 
                       match a = ' ' || a = '.' || a = ',' || System.Char.IsUpper(a) = false with //not happy with System.Char.IsUpper......
                       | true -> checkForNonValid rest 
                       | _ -> None 
            checkForNonValid newTing
    //let dhjfjd = Seq.toList(input)
    
    //let sjfjkd = List.ofSeq("please sit spot. sit spot, sit. spot here now here.")
    let thing = "please sit spot. sit spot, sit. spot here now here."
    let this = thing.Split()
    let hey = Array.toList(this)
    let changeString xs = 
        let rec putCommas xs a = 
            match xs with 
            | [] -> a 
            | b::rest -> putCommas rest (b::a)
        putCommas hey []
    changeString hey
    
    //let iKnow = List.rev(changeString hey)
   // List.rev(changeString hey)|>  List.filter(fun p -> p.Contains(",")) 

   
    
      

    //failwith "Not implemented"
 
let rivers input =
    
    //let listInput = List.ofSeq(input)
    let makeWords orgList = 
       let rec onlyValidCharacters xs lst = 
           match xs with 
           | [] ->  orgList
           | a::rest -> 
                match System.Char.IsUpper(a) ||  System.Char.IsLower(a) || System.Char.IsSeparator(a) with 
                | true -> 
                        onlyValidCharacters rest (a::lst)
                | _ -> []
       onlyValidCharacters orgList []


    let countWords xs num = 
        let rec number xs count lst = 
            match xs with 
            | [] -> List.rev(lst)
            | a::rest -> 
                       match System.Char.IsSeparator(a) with 
                       | true -> 
                            let word = count 
                            number (rest) (num) (word::lst)
                       | _ -> number (rest) (count+1) lst
                     
        number xs num []

    let  confirmString xs =
         let newList = makeWords xs
         match newList with 
         | [] -> None
         | _ -> 
                let listWords = countWords newList 0 //|>  List.exists(fun k -> k <= 80)
                let wordCount = List.exists(fun k -> k <= 80) listWords
                match wordCount with
                | true -> 
                       match listWords.Length >= 2 with 
                       | true -> None //Some newList
                       | false -> None 
                | _ -> None
    confirmString (List.ofSeq("The Yangtze is the third longest river in Asia and the longest in the world to flow entirely in one country"))
   // let listInput = List.ofSeq(input)
                
               
    

    //YOU MAY WANT TO CONSIDER LESS NON-SENSICAL VARIABLE NAMES BEFORE THIS PART OF THE CODE!!!!
    let fixSize alist desiredLength currentLength = 
        let rec repeat blist outputListLen =
            match outputListLen = desiredLength with
            | false -> 
               
                let newList = ' '::blist
                repeat newList (outputListLen+1)
            | true -> List.rev(blist)
        repeat (List.rev(alist)) currentLength
   
    let ppp list b = 
        let rec jjj ah c ls r =
            match c = b with 
            | true -> ls,r
            | false ->
                match ah with 
                | [] -> ls, r
                | a::rest -> jjj (rest) (c+1) (a::ls) (rest)
        jjj list 0 [] []

    let findSpaceWidth lstA lstB number = 
         let rec searchForWords listWidth listRef n = 
             match System.Char.IsSeparator(List.head(listWidth)) with //To make sure that the first letter of our 'word' is not a space
             | true ->
                    let rest = List.tail(listWidth)
                    searchForWords rest listRef n 
             | false -> 
                 match System.Char.IsSeparator(List.head(List.rev(listWidth))) with
                 | false -> 
                       match System.Char.IsSeparator(List.head(listRef)) with
                       | true -> listWidth, listRef //We are fine the list had correct width
                       | false -> 
                               [],[]
                            
                 | true ->  //check that spacing is correct, else add trailing separators till you have the width required
                        listWidth, listRef
         searchForWords (lstA) (lstB) number

    
    let checkString primiteString len tupleOne tupleTwo = 
        let rec repeat orgList z shortList longList = 
            match shortList, longList with 
            | [], [] -> 
                    let sL, lL = ppp orgList (z-1)
                    let strOne, strTwo  = findSpaceWidth (List.rev(sL)) lL z

                    repeat orgList (z-1) strOne strTwo 
            | _ -> shortList, longList //we are good, continue
        repeat primiteString len tupleOne tupleTwo

    let seeSpaces (alist:char List) a = 
        let rec record xs count ls = 
          match xs with 
          | [] -> ls
          | b::rem -> 
                  match System.Char.IsSeparator(b) with 
                  | true -> record rem (alist.Length-rem.Length) ((alist.Length - rem.Length)::ls)
                  | false -> record rem (alist.Length-rem.Length) ls
        record alist a [] 

    

    let separateString aStringList increment =
           let rec getStrings (lst:char List) number indexList =
                match lst.Length <= number with 
                | true  -> 
                //I OFFICIALLY BROKE MY CODE, SOMEHOW THE FIXSIZE FUNTION NO LONGER WORKS WITH SEPARATE STRING 
                // ANYWAY, REMOVE THE FIRST SPACE CHAR ON THE LAST WORD THEN RECORD NUMBER OF SPACES IN EACH LIST
                //THEN COMPARE THEM AND RETUREN AN OPTION, YOU CAN ONLY WORRY ABOUT DIFFFERENT LIBNE WIDTHS AFTER THE OPTION FUNCTION WORKS
                      //let check = System.Char.IsSeparator(List.head(lst))
                      match lst.Length = number with 
                      | true ->
                             //let check = System.Char.IsSeparator(List.head(lst))    //List.find(fun a -> System.Char.IsSeparator(a) = true ) lst
                             let nextElement = List.rev(seeSpaces lst 0)
                             printf "List is: %A " lst
                             nextElement::indexList
                      | false -> 
                           let fixedList = fixSize lst number (lst.Length)
                           let nextElement = List.rev(seeSpaces fixedList 0)                      
                           printf "List is: %A " lst
                           nextElement::indexList
                        
                | _ -> 
                     let stringA, stringB = ppp lst increment
                     let wordList, longList  = findSpaceWidth (List.rev(stringA)) stringB number
                     let stringToModify,stringToUseAgain = checkString lst number wordList longList
                     let xs = fixSize stringToModify increment (stringToModify.Length)
                     let nextElement = List.rev(seeSpaces xs 0) 
                     printf "List is: %A " stringToModify
                     getStrings stringToUseAgain number (nextElement::indexList)
                     
           getStrings aStringList increment []
    //separateString  ['T'; 'h'; 'e'; ' '; 'Y'; 'a'; 'n'; 'g'; 't'; 'z'; 'e'; ' '; 'i'; 's'; ' '] 15;;

    
   //let k p = List.find(fun v -> v = firstOccurance || v = firstOccurance+1) //lineBefore


    let lookForRivers xs firstValue num = 
         let rec backAndForth combinedList recentList lastValue counter = 
                 match combinedList with
                 | (a:int List)::tail ->
                            match counter = 1 with //means you start over becuase the last string did not match with the previous ones
                            | true -> 
                                   //let firstOccurance = a.Head
                                   let currentLine = a 
                                  // let nextLine = b 
                                   let check = List.exists(fun p -> p = lastValue || p = lastValue+1) currentLine
                                   
                                   let exact = List.exists(fun p -> p = lastValue) currentLine
                                   let similar =  List.exists(fun p -> p = lastValue+1) currentLine
                                   match check with
                                   | true -> 
                                           match check = exact with 
                                           | true ->backAndForth tail currentLine lastValue (counter+1)
                                           | false -> backAndForth tail currentLine (lastValue+1) (counter+1)
                                       
                                   | false -> backAndForth tail currentLine (currentLine.Head)  num 
                            | false -> 
                                    let prevLine = recentList
                                    //let hi = k prevLine |> (fun j -> j = lastValue)     //List.exists(fun p -> p = lastValue || p = lastValue+1) prevLine
                                    let check = List.exists(fun p -> p = lastValue || p = lastValue+1) prevLine
                                    let exact = List.exists(fun p -> p = lastValue) prevLine
                                    let similar =  List.exists(fun p -> p = lastValue+1) prevLine
                                    match check with
                                    | true -> 
                                          match check = exact with 
                                               | true -> backAndForth tail prevLine lastValue (counter+1)
                                               | false -> backAndForth tail prevLine (lastValue+1) (counter+1) 
                                                                     
                                    | false -> backAndForth tail prevLine (prevLine.Head) num 
                                    // counter helps us pick the first space in a new string should our last attempt fail 
                                     //one and two worked but third line is useless, start with it and compare to the fourth
                 | [] ->  counter
         backAndForth xs [] firstValue num  
  
    let searchLineWidth lst numero = 
        let rec lines ls n cnt = 
            match numero = 25 with 
            | true -> List.rev( cnt )
            | _ -> 
                let p = separateString ls numero
                let q = lookForRivers (List.rev(p)) 1
                lines (n+1) ((numero,q)::cnt
        lines lst numero []
    
    let finn xs = 
        let v = searchLineWidth (List.ofSeq(input)) 10 
        let a,b = v.head 
        let k = List.find(fun c,d -> c,d > a,b) v
        match k with 
        | l, m -> Some (l, m)
        | _ -> None
    //failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
