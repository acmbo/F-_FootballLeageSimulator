namespace Football

    open Football.Units

    module BasicsClasses = 

        [<AbstractClass>]
        type ACFootballplayer( _name: string,
                                _age: int<years>, 
                                PlayerAttributesMap: playerAttributes option,
                                physicalAttributes: physicalAttributes option,
                                mentalAttributes: MentalAttributes option) =

            abstract Name : string 
            abstract Age : int<years> with set, get
            abstract Print : unit -> unit

            abstract PerformanceForm : float 
            abstract Health : Health
            abstract Injury : Injury
            abstract PlayerAttributes : playerAttributes
            abstract PhysicalAttributes : physicalAttributes
            abstract mentalAttributes : MentalAttributes
        
        [<AbstractClass>]
        type ACFootballTeam( _name: string) = 
            abstract Name : string 
            abstract Print : unit -> unit
            

    
    module Agents = 



        (*
        //------------------------------Implementation------------------------------
        let testPlayer = Agents.FootballPlayer("test", 18<years>)
        testPlayer.Age <- 16<years> // Set Value to 16 years
        testPlayer.Print()
        printfn "TestAttributes : %A" (testPlayer.TestAttr, testPlayer.TestAttr2)
        printfn "TestAttributes2 : %A" (Attribute.value testPlayer.TestAttr + Attribute.value testPlayer.TestAttr2)
        *)
        type FootballPlayer( _name: string,
                            _age: int<years>, 
                            playerAttributesMap: playerAttributes option, 
                            physicalAttributes: physicalAttributes option,
                            mentalAttributes: MentalAttributes option) =

            inherit BasicsClasses.ACFootballplayer( _name,_age, playerAttributesMap, physicalAttributes, mentalAttributes)

            let mutable internalAge : int<years> = _age

            member this.Team : string = "Test"

            override this.Name : string = _name
            override this.Age with get () = internalAge and set (value) = internalAge <- value
            override this.Print() = printfn "%i" this.Age 


            override this.PerformanceForm = 1. 
            override this.Health = Health.fit
            override this.Injury = Injury.none
            
            override this.PlayerAttributes =

                match playerAttributesMap with
                    | None -> { Dribbling = Attribute.create( randintB(0,25) );
                            Shooting = Attribute.create( randintB(0,25) );
                            Passing = Attribute.create( randintB(0,25) );
                            Tackling = Attribute.create( randintB(0,25) );
                            Marking = Attribute.create( randintB(0,25) )}

                    | Some(playerAttributesMap) -> playerAttributesMap

            override this.PhysicalAttributes = 
                    match physicalAttributes with
                    | None -> { Speed = Attribute.create( randintB(0,25) );
                            Strength = Attribute.create( randintB(0,25) )}
                    | Some(physicalAttributes) -> physicalAttributes

            override this.mentalAttributes =  
                    match mentalAttributes with
                    | None -> { Concentration = Attribute.create( randintB(0,25))}
                    | Some(mentalAttributes) -> mentalAttributes



        type Team( _name: string) =

            inherit BasicsClasses.ACFootballTeam(_name)

            [<DefaultValue>] val mutable Players: List<FootballPlayer> 

            override this.Name = _name
            override this.Print() = ()  // Does not do anything. Just for demonstration of syntax

        
        type Leage(teams, players) = 
            member this.teams: List<Team> = teams
            member this.players: List<FootballPlayer> = players



