namespace CubeShapeCounting {
    internal class Program {
        private int _numBlocks = 0;
        //private Dictionary<>();

        public Program(int numBlocks) {
            this._numBlocks = numBlocks;
        }

        public void Start() {

        }

        static void Main(string[] args) {
            if (args.Length != 1) {
                Console.WriteLine("Expected 1 argument");
                return;
            }
            int numBlocks;
            try {
                numBlocks = Int32.Parse(args[0]);
            }
            catch {
                Console.WriteLine("Cannot parse" + args[0] + "as an Int32.");
                return;
            }
            Program P = new Program(numBlocks);
            P.Start();
        }
    }
}