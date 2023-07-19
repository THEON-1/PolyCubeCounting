using System.Buffers.Binary;
using System.Collections.Immutable;
using System.IO.Hashing;

public class Shape {
    private int _size;
    private (int, int, int) _minp;
    private (int, int, int) _maxp;
    private (int, int, int)[] _newest;
    private SortedDictionary<(int, int, int), int>[] _data;
    public bool this[int x, int y, int z] {
        get {
            if (x <= _maxp.Item1 && y <= _maxp.Item2 && z <= _maxp.Item3 && x >= _minp.Item1 && y >= _minp.Item2 && z >= _minp.Item3) {
                return _data[0].ContainsKey((x, y, z));
            } else
                throw new IndexOutOfRangeException();
        }
    }

    private Shape((int, int, int) minp, (int, int, int) maxp, (int, int, int)[] newest, SortedDictionary<(int, int, int), int>[] D) {
        _minp = minp;
        _maxp = maxp;
        _newest = newest;
        _data = D;
        _size = D[0].Count;
    }

    public List<(int, int, int)> GetGrowableSpaces() {
        List<(int, int, int)> L = new List<(int, int, int)>();
        foreach ((int x, int y, int z) in _newest) {
            if (!_data[0].ContainsKey((x + 1, y, z)) && !L.Contains((x + 1, y, z)))
                L.Add((x + 1, y, z));
            if (!_data[0].ContainsKey((x - 1, y, z)) && !L.Contains((x - 1, y, z)))
                L.Add((x - 1, y, z));
            if (!_data[0].ContainsKey((x, y + 1, z)) && !L.Contains((x, y + 1, z)))
                L.Add((x, y + 1, z));
            if (!_data[0].ContainsKey((x, y - 1, z)) && !L.Contains((x, y - 1, z)))
                L.Add((x, y - 1, z));
            if (!_data[0].ContainsKey((x, y, z + 1)) && !L.Contains((x, y, z + 1)))
                L.Add((x, y, z + 1));
            if (!_data[0].ContainsKey((x, y, z - 1)) && !L.Contains((x, y, z - 1)))
                L.Add((x, y, z - 1));
        }
        return L;
    }

    public Shape Grow((int, int, int)[] newPs) {
        SortedDictionary<(int, int, int), int> D = this._data[0];
        for (int i = 0; i < newPs.Length; i++) {
            (int x, int y, int z) = newPs[i];
            if (D.ContainsKey(newPs[i]) ||
                  (!D.ContainsKey((x + 1, y, z))
                && !D.ContainsKey((x - 1, y, z))
                && !D.ContainsKey((x, y + 1, z))
                && !D.ContainsKey((x, y - 1, z))
                && !D.ContainsKey((x, y, z + 1))
                && !D.ContainsKey((x, y, z - 1))))
                throw new InvalidOperationException();
        }
        SortedDictionary<(int, int, int), int>[] newD = new SortedDictionary<(int, int, int), int>[24];
        for (int i = 0; i < 24; i++) {
            int rot = i % 3;
            int corner = (i / 3) % 8;
            bool apos = corner % 2 != 0;
            bool bpos = (corner / 2) % 2 != 0;
            bool cpos = (corner / 4) % 2 != 0;
            Comparison<(int, int, int)> comparer = ((int, int, int) A, (int, int, int) B) => {
                int[] d = new int[3];
                d[0] = (apos ? 1 : -1) * (B.Item1 - A.Item1);
                d[1] = (bpos ? 1 : -1) * (B.Item2 - A.Item2);
                d[2] = (cpos ? 1 : -1) * (B.Item3 - A.Item3);
                return d[(2 + rot) % 3] != 0 ? d[(2 + rot) % 3] : (d[(1 + rot) % 3] != 0 ? d[(1 + rot) % 3] : d[rot]);
            };
            newD[i] = new SortedDictionary<(int, int, int), int>(D, Comparer<(int, int, int)>.Create(comparer));
        }
        (int, int, int) newminp = this._minp;
        (int, int, int) newmaxp = this._maxp;
        for (int i = 0; i < newPs.Length; i++) {
            for (int j = 0; j < newPs.Length; j++)
                newD[i].Add(newPs[i], 0);
            newminp = (Math.Min(newminp.Item1, newPs[i].Item1), Math.Min(newminp.Item2, newPs[i].Item2), Math.Min(newminp.Item3, newPs[i].Item3));
            newmaxp = (Math.Max(newmaxp.Item1, newPs[i].Item1), Math.Max(newmaxp.Item2, newPs[i].Item2), Math.Max(newmaxp.Item3, newPs[i].Item3));
        }
        return new Shape(newminp, newmaxp, newPs, newD);
    }

    public ImmutableOrientedShape[] GetOrientations() {
        ImmutableOrientedShape[] S = new ImmutableOrientedShape[24];
        for (int i = 0; i < 24; i++) {
            S[i] = new ImmutableOrientedShape(_data[i]);
        }
        return S;
    }

    public class ImmutableOrientedShape {
        public readonly ImmutableSortedDictionary<(int, int, int), int> Dict;
        public readonly int Hash;

        internal ImmutableOrientedShape(SortedDictionary<(int, int, int), int> D) {
            Dict = D.ToImmutableSortedDictionary();
            XxHash64 H = new XxHash64(0);
            IEnumerable<(int, int, int)> keys = Dict.Keys;
            (int a, int b, int c) = keys.First();
            Span<byte> s = new byte[12];
            foreach ((int x, int y, int z) in keys) {
                BinaryPrimitives.WriteInt32BigEndian(s.Slice(0), x - a);
                BinaryPrimitives.WriteInt32BigEndian(s.Slice(4), y - b);
                BinaryPrimitives.WriteInt32BigEndian(s.Slice(8), z - c);
                H.Append(s.ToArray());
            }
            Hash = BinaryPrimitives.ReadInt32BigEndian(H.GetCurrentHash());
        }

        public override int GetHashCode() {
            return Hash;
        }

        public override bool Equals(object? obj) {
            if (!(obj is ImmutableOrientedShape))
                return false;
            ImmutableOrientedShape o = (ImmutableOrientedShape)obj;
            IEnumerable<(int, int, int)> K1 = Dict.Keys;
            IEnumerable<(int, int, int)> K2 = o.Dict.Keys;
            if (K1.Count() != K2.Count())
                return false;
            foreach (((int a1, int a2, int a3), (int b1, int b2, int b3)) in K1.Zip(K2))
                if (a1 != b1 || a2 != b2 || a3 != b3)
                    return false;
            return true;
        }
    }

    public static Shape GetCube() {
        SortedDictionary<(int, int, int), int>[] newD = new SortedDictionary<(int, int, int), int>[24];
        for (int i = 0; i < 24; i++) {
            newD[i] = new SortedDictionary<(int, int, int), int>();
            newD[i].Add((0, 0, 0), 0);
        }
        return new Shape((0, 0, 0), (0, 0, 0), new (int, int, int)[] { (0, 0, 0) }, newD);
    }
}
