include("Rotations.jl")

struct Polycube
    cubes::Set{Tuple{Int64, Int64, Int64}}
    recentCubes::Set{Tuple{Int64, Int64, Int64}}
    orderedLists::Vector{Vector{Tuple{Int64, Int64, Int64}}}
end

function Base.:push!(S::Polycube, t::Tuple{Int64, Int64, Int64})
    push!(S.cubes, t)
    for i ∈ 1:24
        t_rot = Rotations[i](t)
        index = searchsortedfirst(S.orderedLists[i], t_rot)
        insert!(S.orderedLists[i], index, t_rot)
    end
end

function getPossibleNeighbors(S::Polycube)
    possibleSpots = Set{Tuple{Int64, Int64, Int64}}()
    for p ∈ S.recentCubes
        push!(possibleSpots,
            p + (1, 0, 0),
            p + (0, 1, 0),
            p + (0, 0, 1),
            p - (1, 0, 0),
            p - (0, 1, 0),
            p - (0, 0, 1)
        )
    end
    spots = setdiff(possibleSpots, S.cubes)
    return spots
end

function getCube()
    return Polycube(
        Set([(0, 0, 0)]),
        Set([(0, 0, 0)]),
        [
            [(0, 0, 0)],
            [(0, 0, 0)],
            [(0, 0, 0)],
            [(0, 0, 0)],
            [(0, 0, 0)],
            [(0, 0, 0)],
            [(0, 0, 0)],
            [(0, 0, 0)],
            [(0, 0, 0)],
            [(0, 0, 0)],
            [(0, 0, 0)],
            [(0, 0, 0)],
            [(0, 0, 0)],
            [(0, 0, 0)],
            [(0, 0, 0)],
            [(0, 0, 0)],
            [(0, 0, 0)],
            [(0, 0, 0)],
            [(0, 0, 0)],
            [(0, 0, 0)],
            [(0, 0, 0)],
            [(0, 0, 0)],
            [(0, 0, 0)],
            [(0, 0, 0)],
        ])
end
