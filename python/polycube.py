from tuple_tools import reorient_tuple, generate_neighbors, subtract
from itertools import combinations, chain

class PolyCube:

    #cubes                  set{tuple}
    #oriented_offsets     list{list{tuple{}}}
    #last_additions         set{tuple}

    def __init__(self, cubes_list, last_additions = []) -> None:
        self.cubes = set(cubes_list)
        self._compute_reorientations()
        self.last_additions = last_additions
    
    def _compute_reorientations(self):
        n_cubes = len(self.cubes)
        n_orientations = 24
        orientations = [[]]*n_orientations
        oriented_offsets = [[]]*n_orientations
        for i in range(n_orientations):
            if n_cubes == 1:
                oriented_offsets[i] = []
            else:
                orientations[i] = []
                for cube in self.cubes:
                    orientations[i].append(reorient_tuple(cube, i))
                orientations[i].sort()
                oriented_offsets[i] = [(0, 0, 0)]*(n_cubes-1)
                reference_cube = orientations[i][0]
                for j in range(n_cubes-1):
                    oriented_offsets[i][j] = subtract(*reference_cube, *orientations[i][j+1])
        self.oriented_offsets = oriented_offsets
    
    def generate_children(self, max_length:int):
        max_growth = max_length - len(self.cubes)
        growth_candidates = set([])
        for last_addition in self.last_additions:
            growth_candidates = growth_candidates.union(generate_neighbors(*last_addition))
        growth_candidates = growth_candidates.difference(self.cubes)
        realizable_growth =  chain.from_iterable(combinations(growth_candidates, i) for i in range(1, max_growth+1))
        for addition in realizable_growth:
            yield PolyCube(self.cubes.union(addition), addition)
        
