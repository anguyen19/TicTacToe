using System.Collections.Generic;
using System;

namespace QUT.CSharpTicTacToe
{
    public class Game : ITicTacToeGame<Player>
    {
        private string[,] board;
        private int size;
        private Player currentPlayer;

        public Game(Player player, int size, string[,] board)
        {
            currentPlayer = player;
            this.size = size;
            this.board = board;
        }

        // Place piece in board
        public void addPiece(Move move)
        {
            if (currentPlayer.Equals(Player.cross))
            {
                board[move.Row, move.Col] = "X";
            }
            else
            {
                board[move.Row, move.Col] = "O";
            }

        }

        public void newTurn()
        {
            if (currentPlayer.Equals(Player.cross))
            {
                currentPlayer = Player.nought;
            }
            else
            {
                currentPlayer = Player.cross;
            }

        }

        // Undo a given move and go back to other player
        public void undoMove(Move move)
        {

            board[move.Row, move.Col] = "";

            if (currentPlayer.Equals(Player.cross))
            {
                currentPlayer = Player.nought;
            }
            else
            {
                currentPlayer = Player.cross;
            }

        }

        public int Size => size;
        public Player Turn => currentPlayer;
        public string getPiece(int row, int col)
        {
            return board[row, col];
        }
    }
}