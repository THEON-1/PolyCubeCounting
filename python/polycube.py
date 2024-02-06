from tuple_tools import reorient_tuple, generate_neighbors
from itertools import combinations, chain

class PolyCube:

    #cubes              set{tuple}
    #orientations       list{set{tuple{}}}
    #last_additions     set{tuple}

    def __init__(self, cubes_list, last_additions = []) -> None:
        self.cubes = set(cubes_list)
        self._compute_reorientations()
        self.last_additions = last_additions
    
    def _compute_reorientations(self):
        n_cubes = len(self.cubes)
        n_orientations = 24
        orientations = [set()]*n_orientations
        for i in range(n_orientations):
            for cube in self.cubes:
                orientations[i].add(cube)
        self.orientations = orientations
    
    def generate_children(self, max_length:int):
        max_growth = max_length - len(self.cubes)
        growth_candidates = set([])
        for last_addition in self.last_additions:
            growth_candidates = growth_candidates.union(generate_neighbors(*last_addition))
        growth_candidates = growth_candidates.difference(self.cubes)
        realizable_growth =  chain(combinations(growth_candidates, i) for i in range(1, max_growth+1))
        for addition in realizable_growth:
            yield PolyCube(self.cubes.union(addition), addition)
        
