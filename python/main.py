from polycube import PolyCube
import json

def main(maxlength:int):
    polycubes = {}
    todo = [PolyCube([(0, 0, 0)], [(0, 0, 0)])]
    while len(todo) != 0:
        polycube = todo.pop()
        children = polycube.generate_children(maxlength)
        for child in children:
            if child in polycubes:
                continue
            todo.append(child)
            polycubes[child] = 0
    with open("out.json", "w") as f:
        f.write(json.dumps(polycubes))

if __name__ == "__main__":
    main(4)
