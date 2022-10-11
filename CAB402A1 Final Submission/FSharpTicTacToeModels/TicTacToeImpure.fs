namespace QUT

open System.Collections.Generic

    module FSharpImpureTicTacToeModel =
    
        type Player = Nought | Cross

        type GameState = 
            { mutable turn: Player; size: int; board: Dictionary<int*int,Player>} 
            member this.getPiece(row, col) = (this :> ITicTacToeGame<Player>).getPiece(row, col)
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.turn
                member this.Size with get()    = this.size
                member this.getPiece(row, col) = match this.board.ContainsKey((row,col)) with
                                                    | true -> match this.board.Item((row,col)) with
                                                                | Nought -> "O"
                                                                | Cross -> "X"
                                                    |_-> ""

        type Move = 
            { row: int; col : int }
            interface ITicTacToeMove with
                member this.Row with get() = this.row
                member this.Col with get() = this.col

        let Lines (size:int) : seq<seq<int*int>> = 
            let lineRow row = seq {for y in 0..size-1 do yield (row,y)}
            let lineCol col = seq {for x in 0..size-1 do yield (x,col)}
            // Setup diagonal lines
            let diagLeft = seq {for x in 0..size-1 do yield (x,x)}
            let diagRight = seq {for x in 0..size-1 do yield (x,size-1-x)}
            let diagonals = seq {yield diagLeft
                                 yield diagRight}
            //Construct all rows and columns
            let lineRow = seq {for x in 0..size-1 do yield lineRow x}
            let lineCol = seq {for y in 0..size-1 do yield lineCol y}
            // Combine it all lines and returns 
            let lines = Seq.append lineRow lineCol |> Seq.append diagonals
            lines

        let CheckLine (game:GameState) (line:seq<int*int>) : TicTacToeOutcome<Player> = 
            let pieces = Seq.map (fun x -> game.getPiece(fst x, snd x)) line

            if Seq.forall (fun x -> x = "O") pieces
                then Win(Nought, line)
            else if Seq.forall (fun x -> x = "X") pieces
                then Win(Cross, line)
            else if (Seq.contains "X" pieces && Seq.contains "O" pieces)
                then Draw
            else Undecided

        let GameOutcome game = 
            let outcome = Lines game.size
                          |> Seq.map(fun line -> CheckLine game line)

            let Checkwin = outcome
                           |> Seq.tryFind(fun result -> match result with
                                                                 | Win(a,b) -> true
                                                                 | _ -> false )
            if not(Checkwin = None)
            then Checkwin.Value
            else let result = outcome
                              |> Seq.tryFind(fun result -> result = Undecided)
                 if not(result = None)
                 then Undecided
                 else Draw

        let ApplyMove game move  = match game.turn with
                                   | Nought -> game.board.Add((move.row,move.col),Nought)
                                               game.turn <- Cross
                                               game
                                   | Cross -> game.board.Add((move.row,move.col), Cross)
                                              game.turn <- Nought
                                              game
        
        let UndoMove game move = game.board.Remove((move.row,move.col)) |> ignore
                                 match game.turn with
                                 | Nought -> game.turn <- Cross                                               
                                 | Cross -> game.turn <- Nought
            

        let CreateMove row col   = {row = row; col = col}


        let GameStart first size = {turn = first; size = size; board = new Dictionary<int*int,Player>()}

        let getTurn game = game.turn

        let heuristic game perspective = 
                      let player = getTurn game
                      let outcome = GameOutcome game
                      if (outcome = Draw)
                      then 0
                      // This check the "turn" after the game ended
                      else if(player = perspective)
                           then -1 
                           else 1

        let gameOver game =
            let outcome = GameOutcome game
            match outcome with 
            | Undecided -> false
            | _ -> true

        let generateMove game = // sequence that contain all moves
                                 let totalMoves = seq {for x in 0..game.size-1 do for y in 0..game.size-1 do yield (x,y)}
                                 // Filter out space
                                 let availableMoves = totalMoves 
                                                   |> Seq.filter (fun coOrd -> not(game.board.ContainsKey(coOrd))) 
                                                   |> Seq.map (fun tuple -> CreateMove (fst tuple) (snd tuple))
                                                   |> Seq.toArray
                                 availableMoves
        // Optimized MiniMax algorithm  
        let MiniMaxWithAlphaBetaPruningGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (generateMove: 'Game->'Move[]) (applyMove: 'Game -> 'Move -> 'Game) : int -> int -> 'Game -> 'Player -> Option<'Move> * int =
                      
            let rec MiniMax alpha beta oldState perspective =
                NodeCounter.Increment()

                // Store the changing alpha and beta
                let mutable newAlpha = alpha
                let mutable newBeta = beta
                
                // Check if the game is over
                if(gameOver oldState) 
                 // No move possible, return relative score
                then (None, heuristic oldState perspective)     
                else     

                       let mutable availableMoves = generateMove oldState
                       
                       let mutable flag = true
              
                       let mutable counter = 0
                       //generic move to start
                       let genericOption = Some(availableMoves.[0])

                       // Check if maximising or minimising
                       if (getTurn oldState = perspective) 
                       then
                            // Starting point with reasonably large number
                            let mutable currentBest = (genericOption, -10000)            
                            // Convenient function for finding best value
                            let findMax currentBest childNode =
                                 // Only return next child if better not equal
                                 if( (snd currentBest < snd childNode))
                                 then childNode
                                 else currentBest
                 
                            while flag do 
   
                                let newState = applyMove oldState availableMoves.[counter]
                                // Store the heuristic and the move that leads to it
                                let childValue = (Some(availableMoves.[counter]), MiniMax newAlpha beta newState perspective |> snd)
                                // Undo move
                                UndoMove oldState availableMoves.[counter]
                                currentBest <- findMax currentBest childValue
                                newAlpha <- max alpha (snd currentBest)
                                counter <- counter+1
                                if ((newBeta <= newAlpha) || counter = availableMoves.Length ) 
                                then flag <- false

                            currentBest
                        
                       else
                            // Starting point with reasonably large number
                            let mutable currentBest = (genericOption, 10000)
                            //finding best value
                            let findMin currentBest childNode =
                                // Only return next child if better not equal
                                if( (snd currentBest > snd childNode))
                                then childNode
                                else currentBest

                            while flag do 
                                
                                let newState = applyMove oldState availableMoves.[counter]
                                
                                // Store the best child and the move that points to it
                                let childValue = (Some(availableMoves.[counter]), MiniMax alpha newBeta newState perspective |> snd)
                                // Undo move
                                UndoMove oldState availableMoves.[counter]
                                currentBest <- findMin currentBest childValue
                                newBeta <- min beta (snd currentBest)
                                counter <- counter+1
                                if ((newBeta <= newAlpha) || counter = availableMoves.Length ) 
                                then flag <- false

                            currentBest

            NodeCounter.Reset()
            MiniMax

        let FindBestMove game    = 
            let minimax = MiniMaxWithAlphaBetaPruningGenerator heuristic getTurn gameOver generateMove ApplyMove 
            let move = fst <| minimax -1 1 game game.turn
            match move with
                    | Some m -> m
                    | None -> { row = -1; col = -1 }

        type WithAlphaBetaPruning() =
            override this.ToString()         = "Impure F# with Alpha Beta Pruning";
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = Cross
                member this.Nought with get()            = Nought
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game 
                member this.ApplyMove(game, move)        = ApplyMove game  move
                member this.FindBestMove(game)           = FindBestMove game