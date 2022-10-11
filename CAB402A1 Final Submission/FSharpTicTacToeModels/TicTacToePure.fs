namespace QUT

    module FSharpPureTicTacToeModel =
    
        // type to represent the two players: Noughts and Crosses
        type Player = Nought | Cross

        // type to represent a single move specified using (row, column) coordinates of the selected square
        type Move = 
            { Row: int; Col: int}
            interface ITicTacToeMove with
                member this.Row with get() = this.Row   
                member this.Col with get() = this.Col

        // type to represent the current state of the game, including the size of the game (NxN), who's turn it is and the pieces on the board
        type GameState = 
            { Turn: Player ; Size:int ; board: Map<int*int,Player> }
            member this.getPiece(row, col) = (this :> ITicTacToeGame<Player>).getPiece(row, col)
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.Turn
                member this.Size with get()    = this.Size
                member this.getPiece(row, col) = 
                    match this.board.TryFind((row,col)) with
                    | Some player -> match player with
                        | Nought -> "O" 
                        | Cross -> "X"
                    | _ -> ""
                


        let CreateMove row col = {Row = row; Col = col}

       

        // Returns a sequence containing all of the lines on the board: Horizontal, Vertical and Diagonal
        // The number of lines returned should always be (size*2+2)
        // the number of squares in each line (represented by (row,column) coordinates) should always be equal to size
        // For example, if the input size = 2, then the output would be: 
        //     seq [seq[(0,0);(0,1)];seq[(1,0);(1,1)];seq[(0,0);(1,0)];seq[(0,1);(1,1)];seq[(0,0);(1,1)];seq[(0,1);(1,0)]]
        // The order of the lines and the order of the squares within each line does not matter

        let Lines (size:int) : seq<seq<int*int>> = 
            let lineRow row = seq {for x in 0..size-1 do yield (row,x)}
            let lineCol col = seq {for y in 0..size-1 do yield (y,col)}
            // Setup diagonal lines
            let diagLeft = seq {for x in 0..size-1 do yield (x,x)}
            let diagRight = seq {for x in 0..size-1 do yield (x,size-1-x)}
            let diagonals = seq {yield diagLeft
                                 yield diagRight}
            //Construct all rows and columns
            let lineRow = seq {for x in 0..size-1 do yield lineRow x}
            let lineCol = seq {for y in 0..size-1 do yield lineCol y}
            // Combine it all in one sequence
            let lines = Seq.append lineRow lineCol |> Seq.append diagonals
            lines

        // Checks a single line to determine if one of the players
        // has won by filling all of those squares, or a Draw if the line contains at least one Nought and one Cross

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
            let outcomes = Lines game.Size
                          |> Seq.map (fun x -> CheckLine game x)

            let getWin = fun x -> match x with 
                                   | Win _ -> true
                                   | _ -> false
            
            let getDraw = fun x -> match x with
                                    | Draw -> true
                                    | _ -> false

            if Seq.exists getWin outcomes
            then Seq.filter getWin outcomes |> Seq.head
            else if Seq.forall getDraw outcomes
            then Draw
            else Undecided

        let GameStart (firstPlayer: Player) size: GameState = { Turn=firstPlayer; Size=size; board=Map.empty }

        let HeuristicScore (game: GameState) (player: Player): int =
            match GameOutcome game with
                | Win (winner, _) -> if winner = player then 1 else -1
                | _ -> 0


        let GetTurn (game: GameState) = game.Turn

        let GameOver (game: GameState): bool =
            if Map.count game.board >= game.Size * game.Size then true
            else
                match GameOutcome game with
                    | Win _ -> true
                    | Draw  -> true
                    | _ -> false

        let generateMove game = // sequence that contain all moves
                                 let totalMoves = seq {for x in 0..game.Size-1 do for y in 0..game.Size-1 do yield (x,y)}
                                 // Filter out space
                                 let availableMoves = totalMoves 
                                                   |> Seq.filter (fun coOrd -> not(game.board.ContainsKey(coOrd))) 
                                                   |> Seq.map (fun tuple -> CreateMove (fst tuple) (snd tuple))
                                 availableMoves

        let ApplyMove (oldState:GameState) (move: Move) = 
          match oldState.Turn with
                      | Nought -> { Turn = Cross; Size = oldState.Size; board = oldState.board.Add( (move.Row,move.Col),  Nought)}
                      | Cross -> { Turn = Nought; Size = oldState.Size; board = oldState.board.Add( (move.Row,move.Col), Cross)}

        let MiniMax game = 
            GameTheory.MiniMaxGenerator HeuristicScore GetTurn GameOver generateMove ApplyMove game game.Turn

            

        let MiniMaxWithPruning game = 
            GameTheory.MiniMaxWithAlphaBetaPruningGenerator HeuristicScore GetTurn GameOver generateMove ApplyMove -1 1 game game.Turn

        // plus other helper functions ...




        [<AbstractClass>]
        type Model() =
            abstract member FindBestMove : GameState -> Move
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = Cross 
                member this.Nought with get()            = Nought 
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game
                member this.ApplyMove(game, move)        = ApplyMove game move 
                member this.FindBestMove(game)           = this.FindBestMove game

        type BasicMiniMax() =
            inherit Model()
            override this.ToString()         = "Pure F# with basic MiniMax";
            override this.FindBestMove(game) = 
                let Move = fst <| MiniMax game
                match Move with
                    | Some m -> m
                    | None -> { Row = -1; Col = -1 }


        type WithAlphaBetaPruning() =
            inherit Model()
            override this.ToString()         = "Pure F# with Alpha Beta Pruning";
            override this.FindBestMove(game) = 
                let Move = fst <| MiniMaxWithPruning game
                match Move with
                    | Some m -> m
                    | None -> { Row = -1; Col = -1 }