
def reorient_tuple(t, n):
    t, _ = _invert(*_flip(*_rotate(t, n)))
    return t

def _rotate(t, n) -> tuple[tuple, int]:
    m = n%3
    return t[m:] + t[:m], n//3

def _flip(t, n) -> tuple[tuple, int]:
    match n%4:
        case 1:
            return (-t[0], -t[1], t[2]), n//4
        case 2:
            return (-t[0], t[1], -t[2]), n//4
        case 3:
            return (t[0], -t[1], -t[2]), n//4
        case _:
            return t, n//4
        

def _invert(t, n) -> tuple[tuple, int]:
    match n%2:
        case 1:
            return (-t[0], -t[1], -t[2])[::-1], n//2
        case _:
            return t, n//2
