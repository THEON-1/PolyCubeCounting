using System.Collections;
using System.IO.Hashing;

public class AssembledObject {
    private static XxHash32 _hash = new XxHash32();

    private BitArray _shape;

    public override int GetHashCode() {
        return 1;
    }
}
