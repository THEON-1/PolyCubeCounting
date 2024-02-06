from polycube import PolyCube
from polycube_encoder import PolyCubeEncdoder
import json

def main(maxlength:int):
    initial_cube = PolyCube([(0, 0, 0)], [(0, 0, 0)])
    n_size = [0]*maxlength
    todo = [initial_cube]
    polycubes = [initial_cube.oriented_offsets[0]]
    n_size[0] = 1

    while len(todo) != 0:
        polycube = todo.pop()
        children = polycube.generate_children(maxlength)
        for child in children:
            if child_exists_in(child, polycubes):
                  continue
            todo.append(child)
            polycubes.append(child.oriented_offsets[0])
            n_size[len(child.cubes)-1] += 1
    with open("out.json", "w") as f:
        f.write(json.dumps(polycubes, cls=PolyCubeEncdoder))
    with open("count.json", "w") as f:
        f.write(json.dumps(n_size))

def child_exists_in(child: PolyCube, polycubes: list):
    for orientation in child.oriented_offsets:
        if orientation in polycubes:
            return True
    return False

if __name__ == "__main__":
    main(7)
