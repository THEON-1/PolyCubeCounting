namespace CubeShapeCounting {
    internal class Program {
        private int _numBlocks = 0;
        private Dictionary<Shape.ImmutableOrientedShape, int>[] _knownShapes;

        public Program(int numBlocks) {
            _numBlocks = numBlocks;
            _knownShapes = new Dictionary<Shape.ImmutableOrientedShape, int>[numBlocks - 1];
            for (int i = 0; i < _numBlocks - 1; i++)
                _knownShapes[i] = new Dictionary<Shape.ImmutableOrientedShape, int>();
        }

        public void Start() {
            Stack<Shape> shapes = new Stack<Shape>();
            shapes.Push(Shape.GetCube());
            while (shapes.Count > 0) {
                Shape shape = shapes.Pop();
                List<(int, int, int)> next_spaces = shape.GetGrowableSpaces();
                
            }
        }

        static void Main(string[] args) {
            if (args.Length != 1) {
                Console.WriteLine("Expected 1 argument");
                return;
            }
            int numBlocks;
            try {
                numBlocks = Int32.Parse(args[0]);
            } catch {
                Console.WriteLine("Cannot parse" + args[0] + "as an Int32.");
                return;
            }
            Program P = new Program(numBlocks);
            P.Start();
        }
    }
}