from polycube import PolyCube
from polycube_encoder import PolyCubeEncdoder
import json

def main(maxlength:int):
    polycubes = set([PolyCube([(0, 0, 0)], [(0, 0, 0)])])
    n_size = [0]*maxlength
    todo = [PolyCube([(0, 0, 0)], [(0, 0, 0)])]
    n_size[0] = 1
    while len(todo) != 0:
        polycube = todo.pop()
        children = polycube.generate_children(maxlength)
        for child in children:
            if child in polycubes:
                continue
            todo.append(child)
            polycubes.add(child)
            n_size[len(child.cubes)-1] += 1
    with open("out.json", "w") as f:
        f.write(json.dumps(polycubes, cls=PolyCubeEncdoder))
    with open("count.json", "w") as f:
        f.write(json.dumps(n_size))

if __name__ == "__main__":
    main(4)
