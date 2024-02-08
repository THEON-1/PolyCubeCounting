include("PolyCube.jl")

import Json: json

function main(maxsize::int)
    root_pcube = PolyCube([(0, 0, 0)], [(0, 0, 0)])

    todo = Vector{Coord}(undef, 0)
    push!(todo, root_pcube)

    polycubes = Dict{Vector{Coord}, Vector{Coord}}()
    polycubes[root_pcube.oriented_differences[0]] = root_pcube.cubes

    size_count = zeros(Int, maxsize)
    size_count[1] = 1

    while length(todo) > 0
        pcube = pop!(todo)
        for child ∈ generate_children(pcube, maxsize)
            if !child_exists_in(polycubes, child)
                polycubes[child.oriented_differences[0]] = child.cubes
                push!(todo, child)
                size_count[length(child.cubes)] += 1
            end
        end
    end
    write("out,dat", json(polycubes))
    write("out,dat", json(size_count))
end

function child_exists_in(dict::Dict, child::PolyCube)
    for orientation ∈ child.oriented_differences
        if haskey(dict, orientation)
            return true
        end
    end
    return false
end
