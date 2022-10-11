using System;
using System.Collections.Generic;
namespace QUT.CSharpTicTacToe
{
    public class WithAlphaBetaPruning : ITicTacToeModel<Game, Move, Player>
    {
        public Player Nought => Player.nought;
        public Player Cross => Player.cross;
        public override string ToString()
        {
            return "Impure C# with Alpha Beta Pruning";
        }
        public Game ApplyMove(Game game, Move move)
        {
            // Apply board and current turn changes
            game.addPiece(move);
            game.newTurn();
            return game;

        }
        //creates new move with row and column
        public Move CreateMove(int row, int col)
        {
            Move move = new Move(row, col);
            return move;
        }
        //uses minimax alpha beta to find best move
        public Move FindBestMove(Game game)
        {
            Player perspective = game.Turn;
            int alpha = -1;
            int beta = 1;
            NodeCounter.Reset();
            (Move, int) bestMove = alphabeta(alpha, beta, game, perspective);
            return bestMove.Item1;

        }
        //Finds either win or draw
        

        // build game board
        public Game GameStart(Player first, int size)
        {
            // Construct board by filling 
            // every square with empty string
            string[,] board = new string[size, size];
            for (int i = 0; i < size; i++)
            {
                for (int j = 0; j < size; j++)
                {
                    board[i, j] = "";
                }

            }
            Game game = new Game(first, size, board);
            return game;
        }
        //check to see if the game is over
        public bool gameOver(Game game)
        {
            if (GameOutcome(game) == TicTacToeOutcome<Player>.Undecided)
            {
                return false;
            }
            else
            {
                return true;
            }

        }

        public int heuristic(Game game, Player perspective)
        {

            if (GameOutcome(game) == TicTacToeOutcome<Player>.Draw)
            {
                return 0;
            }
            //checks the "turn" after the game ended
            else if (game.Turn == perspective)
            {
                return -1;
            }
            else
            {
                return 1;
            }

        }

        public List<Move> generateMove(Game game)
        {
            List<Move> availableMoves = new List<Move>();

            // Check every space on the board to see if there are any empty spaces
            for (int i = 0; i < game.Size; i++)
            {
                for (int j = 0; j < game.Size; j++)
                {
                    if (game.getPiece(i, j) == "")
                    {
                        availableMoves.Add(CreateMove(i, j));
                    }

                }
            }
            // return empty spaces where moves can be placed
            return availableMoves;

        }
        public TicTacToeOutcome<Player> GameOutcome(Game game)
        {

            // Check how many lines have draws
            int numDraw = 0;
            int totalNoughts = 0; // Count Noughts
            int totalCrosses = 0; // Count Crosses
            List<Tuple<int, int>> winningLine = new List<Tuple<int, int>>();

            //Horizontal Lines
            for (int x = 0; x < game.Size; x++)
            {
                totalNoughts = 0;
                totalCrosses = 0;
                winningLine = new List<Tuple<int, int>>();
                for (int y = 0; y < game.Size; y++)
                {
                    Tuple<int, int> coOrd = Tuple.Create<int, int>(x, y);
                    winningLine.Add(coOrd);
                    if (game.getPiece(x, y) == "X")
                    {
                        totalCrosses++;
                    }
                    else if (game.getPiece(x, y) == "O")
                    {
                        totalNoughts++;
                    }

                }

                // Check for wins and draw
                if (totalNoughts == game.Size)
                {
                    return TicTacToeOutcome<Player>.NewWin(Player.nought, winningLine);
                }
                else if (totalCrosses == game.Size)
                {
                    return TicTacToeOutcome<Player>.NewWin(Player.cross, winningLine);
                }
                else if ((totalCrosses > 0) && (totalNoughts > 0))
                {
                    numDraw++;
                }

            }

            //Vertical Lines
            for (int y = 0; y < game.Size; y++)
            {
                totalNoughts = 0;
                totalCrosses = 0;
                winningLine = new List<Tuple<int, int>>();
                for (int x = 0; x < game.Size; x++)
                {
                    Tuple<int, int> position = Tuple.Create<int, int>(x, y);
                    winningLine.Add(position);
                    if (game.getPiece(x, y) == "X")
                    {
                        totalCrosses++;
                    }
                    else if (game.getPiece(x, y) == "O")
                    {
                        totalNoughts++;
                    }

                }

                // Check for wins and draw
                if (totalNoughts == game.Size)
                {
                    return TicTacToeOutcome<Player>.NewWin(Player.nought, winningLine);
                }
                else if (totalCrosses == game.Size)
                {
                    return TicTacToeOutcome<Player>.NewWin(Player.cross, winningLine);
                }
                else if ((totalCrosses > 0) && (totalNoughts > 0))
                {
                    numDraw++;
                }

            }

            // Check diagonals
            totalNoughts = 0;
            totalCrosses = 0;
            winningLine = new List<Tuple<int, int>>();

            for (int x = 0; x < game.Size; x++)
            {
                Tuple<int, int> position = Tuple.Create<int, int>(x, x);
                winningLine.Add(position);
                if (game.getPiece(x, x) == "X")
                {
                    totalCrosses++;

                }
                else if (game.getPiece(x, x) == "O")
                {
                    totalNoughts++;
                }

            }

            // Check for wins and draw
            if (totalNoughts == game.Size)
            {
                return TicTacToeOutcome<Player>.NewWin(Player.nought, winningLine);
            }
            else if (totalCrosses == game.Size)
            {
                return TicTacToeOutcome<Player>.NewWin(Player.cross, winningLine);
            }
            else if ((totalCrosses > 0) && (totalNoughts > 0))
            {
                numDraw++;
            }

            totalNoughts = 0;
            totalCrosses = 0;
            winningLine = new List<Tuple<int, int>>();
            for (int x = 0; x < game.Size; x++)
            {
                Tuple<int, int> position = Tuple.Create<int, int>(x, game.Size - 1 - x);
                winningLine.Add(position);
                if (game.getPiece(x, game.Size - 1 - x) == "X")
                {
                    totalCrosses++;
                }
                else if (game.getPiece(x, game.Size - 1 - x) == "O")
                {
                    totalNoughts++;
                }

            }

            // Check for wins and draw
            if (totalNoughts == game.Size)
            {
                return TicTacToeOutcome<Player>.NewWin(Player.nought, winningLine);
            }
            else if (totalCrosses == game.Size)
            {
                return TicTacToeOutcome<Player>.NewWin(Player.cross, winningLine);
            }
            else if ((totalCrosses > 0) && (totalNoughts > 0))
            {
                numDraw++;
            }

            // Only draws if the number of draws is equal to number of lines
            if (numDraw == (game.Size * 2 + 2))
            {
                return TicTacToeOutcome<Player>.Draw;
            }

            return TicTacToeOutcome<Player>.Undecided;

        }
        // Optimized MiniMax algorithm that uses alpha beta pruning
        public (Move, int) alphabeta(int alpha, int beta, Game oldState, Player perspective)
        {
            NodeCounter.Increment();
            // Store the changing alpha and beta
            int newAlpha = alpha;
            int newBeta = beta;

            List<Move> availableMoves = generateMove(oldState);
            // Store best value and best move sepeartely
            int optimalValue = 0;
            Move bestMove = CreateMove(9, 9);

            if (gameOver(oldState))
            {
                return (null, heuristic(oldState, perspective));
            }
            else if (oldState.Turn == perspective)
            {
                // Maximising

                // large starting value
                optimalValue = -15;

                foreach (Move move in availableMoves)
                {
                    Game newState = ApplyMove(oldState, move);
                    int childValue = alphabeta(newAlpha, newBeta, newState, perspective).Item2;
                    // Undo move so that the apply move isn't permanent
                    oldState.undoMove(move);

                    if (optimalValue < childValue)
                    {
                        optimalValue = childValue;
                        bestMove = move;
                    }
                    newAlpha = Math.Max(newAlpha, optimalValue);

                    if (beta <= newAlpha)
                        break;

                }

                return (bestMove, optimalValue);

            }
            else
            {
                // Minimising
                //large starting number
                optimalValue = 15;


                foreach (Move move in availableMoves)
                {
                    Game newState = ApplyMove(oldState, move);
                    int childValue = alphabeta(newAlpha, newBeta, newState, perspective).Item2;
                    // Undo move so that the apply move isn't permanent
                    oldState.undoMove(move);
                    if (optimalValue > childValue)
                    {
                        optimalValue = childValue;
                        bestMove = move;
                    }
                    newBeta = Math.Min(newBeta, optimalValue);
                    if (newBeta <= alpha)
                        break;
                }

                return (bestMove, optimalValue);

            }
        }
    }
}