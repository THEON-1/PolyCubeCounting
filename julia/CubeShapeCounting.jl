include("Shape.jl")
include("ImmutableOrientedShape.jl")
include("Powerset.jl")
include("TupleMisc.jl")
include("plot.jl")
using XXhash
using Serialization
using ArgParse

function main()
    s = ArgParseSettings()
    @add_arg_table s begin
        "-g"
            help = "generator"
            arg_type = Int
        "-c"
            help = "shape count"
            action = :store_true
        "-l"
            help = "shape list"
            action = :store_true
        "-p"
            help = "plot n_cubes i_shape"
            nargs = '+'
            arg_type = Int
    end

    parsed_args = parse_args(s)

    generate = get(parsed_args, "g", nothing)
    if generate !== nothing && generate > 0
        scanForShapes(generate)
    end
    if get(parsed_args, "c", false)
        countShapes()
    end
    if get(parsed_args, "l", false)
        listShapes()
    end
    plot = get(parsed_args, "p", nothing)
    if length(plot) == 1
        plotShapes(plot)
    elseif length(plot) == 2
        plotShape(plot)
    end
end

function scanForShapes(MaxSize::Int64)
    D = Dict{UInt, ImmutableOrientedShape}()
    S = Vector{Shape}();
    cube = getCube()
    immutableCube = getImmutableOrientedShape(cube)
    D[immutableCube.hash] = immutableCube
    push!(S, cube)

    while !isempty(S)
        cube = pop!(S)
        growableSpaces = collect(getPossibleNeighbors(cube))
        acceptable_growth = MaxSize - length(cube.cubes)
        for i ∈ 1:acceptable_growth
            possibleGrowth = Powerset.getPowerSubSet(length(growableSpaces), i)
            for j ∈ axes(possibleGrowth, 1)
                cubesToAdd = growableSpaces[possibleGrowth[j, :]]
                newShape = deepcopy(cube)
                for c ∈ cubesToAdd
                    push!(newShape, c)
                end
                collision = checkForCollision(newShape, D)
                if !collision
                    push!(S, newShape)
                    immutableNewShape = getImmutableOrientedShape(newShape)
                    D[immutableNewShape.hash] = immutableNewShape
                end
            end
        end
    end
    sanitizedData = sanitize(D, MaxSize)
    serialize("results.bin", sanitizedData)
end

function countShapes()
    T = deserialize("results.bin")
    n = T[1]
    for i ∈ 1:n
        print("n = ")
        print(i)
        print(": ")
        println(length(T[2][i]))
    end
end

function listShapes()
    T = deserialize("results.bin")
    print("max size: ")
    println(T[1])
    for V ∈ T[2]
        for v ∈ V
            println(v)
        end
    end
end

function checkForCollision(S::Shape, D::Dict{UInt, ImmutableOrientedShape})
    for i ∈ 1:24
        hash = hashList(S.orderedLists[i])
        value = get(D, hash, nothing)
        if value !== nothing && hash == value.hash
            return true
        end
    end
    return false
end

function hashList(L::Vector{Tuple{Int64, Int64, Int64}})
    diffList = Vector{Tuple{Int64, Int64, Int64}}(undef, length(L)-1)
    for i ∈ eachindex(diffList)
        diffList[i] = L[i+1] - L[i]
    end
    return xxh3_64(diffList)
end

function sanitize(D::Dict{UInt, ImmutableOrientedShape}, size::Int64)
    data = Vector{Vector{Vector{Tuple{Int64, Int64, Int64}}}}(undef, size)
    for i ∈ eachindex(data)
        data[i] = Vector{Vector{Tuple{Int64, Int64, Int64}}}(undef, 0)
    end
    for (K, V) ∈ D
        push!(data[length(V.cubes)], V.cubes)
    end
    return (size, data)
end

main()
