include("tuple_tools.jl")

import Combinatorics: powerset
import XXhash: xxh3_64

# since differences fully represent the structure, everything may be faster if we just dump the explicit
# location data and work only on the differences
struct PolyCube
    cubes::Vector{Coord}
    oriented_differences::Vector{Vector{Coord}}
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

    sort!(cubes)
    _compute_differences!(oriented_differences, cubes, 1)

    for i ∈ 2:n_orientations
        for j ∈ 1:n_cubes
            reorientation[j] = orient_tuple(cubes[j], i)
        end
        sort!(reorientation)
        _compute_differences!(oriented_differences, reorientation, i)
    end
    return oriented_differences
end

function _compute_differences!(oriented_differences::Vector{Vector{Coord}}, reorientation::Vector{Coord}, i::Int)
    n_cubes = length(reorientation)
    reference_cube = reorientation[1]
    oriented_differences[i] = Vector{Coord}(undef, n_cubes-1)
    for j ∈ 1:n_cubes-1
        oriented_differences[i][j] = reference_cube - reorientation[j+1]
    end
end

function generate_children(pcube::PolyCube, n_max::Int)
    cubes = pcube.cubes
    allowed_growth = n_max - length(cubes)
    if allowed_growth == 0
        return []
    end
    growth_candidates = Vector{Coord}(undef, 0)
    for root_cube ∈ pcube.last_added
        for neighbor ∈ neighbors(root_cube...)
            pos_growth_candidates = searchsortedfirst(growth_candidates, neighbor)
            if (pos_growth_candidates > length(growth_candidates) || growth_candidates[pos_growth_candidates] != neighbor) && isempty(searchsorted(cubes, neighbor))
                insert!(growth_candidates, pos_growth_candidates, neighbor)
            end
        end
    end
    return Iterators.map(x -> PolyCube(vcat(cubes, x), x), powerset(growth_candidates, 1, allowed_growth))
end

function hash!(pcube::PolyCube)
    return xxh3_64(pcube.oriented_differences[1])
end
