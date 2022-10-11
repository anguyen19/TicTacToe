namespace QUT.CSharpTicTacToe
{
    public class Move : ITicTacToeMove
    {
        private int row;
        private int col;

        public Move(int row, int col)
        {
            this.row = row;
            this.col = col;
        }

        public int Row => row;
        public int Col => col;
    }
}