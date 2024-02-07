include("tuple_tools.jl")

import Combinatorics: powerset

Coord = Tuple{Number, Number, Number}

struct PolyCube
    cubes::Vector{Coord}
    oriented_difference::Vector{Vector{Coord}}
    last_added::Vector{Coord}
end

function PolyCube(cubes::Vector{Coord}, last_added::Vector{Coord})
    return PolyCube(sort!(cubes), _calculate_oriented_differences(cubes), last_added)
end

# possible improvement: since reorientation[1] == cubes, skip the insertion sterp, and benefit from having cubes sorted.
# then skip sorting in constructor

function _calculate_oriented_differences(cubes::Vector{Coord})
    n_orientations = 24
    n_cubes = length(cubes)
    reorientation = Vector{Coord}(undef, n_cubes)
    oriented_differences = Vector{Vector{Coord}}(undef, n_orientations)
    for i ∈ 1:n_orientations
        for j ∈ 1:n_cubes
            reorientation[j] = orient_tuple(cubes[j], i)
        end
        sort!(reorientation)
        reference_cube = reorientation[1]
        oriented_differences[i] = Vector{Coord}(undef, n_cubes-1)
        for j ∈ 1:n_cubes-1
            oriented_differences[j] = reference_cube - reorientation[j+1]
        end
    end
    return oriented_differences
end

function generate_children(pcube::PolyCube, n_max::Int)
    cubes = pcube.cubes
    allowed_growth = n_max - length(cubes)
    growth_candidates = Vector{Coord}(undef, 0)
    for root_cube ∈ pcube.last_added
        for neighbor ∈ neighbors(root_cube...)
            pos_growth_candidates = searchsortedfirst(growth_candidates, neighbor)
            # consider flipping following ||, it MAY imrove performance
            if growth_candidates[pos_growth_candidates] != neighbor || !isempty(searchsorted(cubes, neighbor))
                insert!(growth_candidates, pos_growth_candidates, neighbor)
            end
        end
    end
    return Iterators.map(x -> PolyCube(vcat(cubes, x), x), powerset(growth_candidates, 1, allowed_growth))
end
