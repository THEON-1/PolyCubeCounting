using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Linq;

public class Shape {
	private int _x;
	private int _y;
	private int _z;
	private SortedDictionary<(int, int, int), int>[] _data;
	public bool this[int x, int y, int z] {;
		get {
			if (x < _x && y < _y && z < _z && x >= 0 && y >= 0 && z >= 0) {
				return _data[(x, y, z)];
			}
			else
				throw new IndexOutOfRangeException();
		}
	}

	private Shape(int x, int y, int z) {
		_x = x;
		_y = y;
		_z = z;

		_data = new SortedDictionary<(int, int, int), int>[24];
		for (int i = 0; i < 24; i++) {
            int rot = i%3;
            int corner = (i/3)%8;
			bool apos = corner%2;
			bool bpos = (corner/2)%2;
			bool cpos = (corner/4)%2;
            Func<Tuple<int, int, int>, Tuple<int, int, int>, int> comparer = (Tuple<int, int, int> A, Tuple<int, int, int> B) => {
				int[] d = new int[3];
				d[0] = (apos ? 1 : -1)*(B.Item1 - A.Item1);
				d[1] = (bpos ? 1 : -1)*(B.Item2 - A.Item2);
				d[2] = (cpos ? 1 : -1)*(B.Item3 - A.Item3);
				return d[(2+rot)%3] != 0 ? d[(2+rot)%3] : (d[(1+rot)%3] != 0 ? d[(1+rot)%3] : d[rot]);
            };
			_data[i] = new SortedDictionary<Tuple<int, int, int>, int>(comparer);
		}
	}

	public Shape Grow(int x, int y, int z) {
		Tuple<int, int, int> newP = new Tuple<int, int, int>(x, y, z);
		if (this._data[0].ContainsKey(newP) throw new InvalidOperationException();
		else if (this._data[0].ContainsKey(new Tuple<int, int, int>(x+1, y, z)))
		else throw new InvalidOperationException();
	}

	public static Shape GetCube() {
		Shape S = new Shape(1, 1, 1);
		S._data[0] = 1;
		return S;
	}
}
