namespace Football

    open System

    module Units =

        [<Measure>] type years //  years

        [<Measure>] type days

        type Health = 
            | fit = 0
            | injured = 1
            | dead = 2
        
        type Injury = 
            | none = 0
            | muscular = 1
            | traumatic = 2 // ACL/PCL, Knee injuries
            | concussion = 3
            | overtrain = 4 // pain from training
            | minor = 5 // flu, cold, etc
            | severe = 6 // heart problem

        // create constrained int type for attributes

        type Attribute = private Attribute of int

        // Defines a constructor for creation. Implementation see footballplayer
        module Attribute = 

            let create i =

                if i > 25 then  
                    Attribute 25
                elif i< 1 then
                    Attribute 1
                else
                    Attribute i

            let value (Attribute i) = i //Return function


        type playerAttributes = 
            {
                Dribbling: Attribute
                Shooting :  Attribute
                Passing: Attribute
                Tackling: Attribute
                Marking: Attribute
            }

        type physicalAttributes = 
            {
                Speed: Attribute
                Strength: Attribute
            }

        type MentalAttributes = 
            {
                Concentration: Attribute
            }

        let convertYear2Days ( year : int<years> ) = 365<days> * year 

        let convert2Int (year: int<years>) = int year


        let randint() = 
            let r = System.Random()
            r.Next()

        let randintB(lower: int, upper : int) = 
            let r = System.Random()
            r.Next(lower , upper)
            


