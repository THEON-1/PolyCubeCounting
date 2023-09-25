include("Polycube.jl")
include("ImmutableOrientedPolycube.jl")
include("TupleMisc.jl")
include("plot.jl")
using XXhash
using Serialization
using ArgParse
using Combinatorics

function main()
    s = ArgParseSettings()
    @add_arg_table s begin
        "-g"
            help = "generator"
            arg_type = Int
        "-c"
            help = "Polycube count"
            action = :store_true
        "-l"
            help = "Polycube list"
            action = :store_true
        "-p"
            help = "plot n_cubes i_Polycube"
            nargs = '+'
            arg_type = Int
    end

    parsed_args = parse_args(s)

    generate = get(parsed_args, "g", nothing)
    if generate !== nothing && generate > 0
        scanForPolycubes(generate)
    end
    if get(parsed_args, "c", false)
        countPolycubes()
    end
    if get(parsed_args, "l", false)
        listPolycubes()
    end
    plot = get(parsed_args, "p", nothing)
    if length(plot) == 1
        plotPolycubes(plot)
    elseif length(plot) == 2
        plotPolycube(plot)
    end
end

function options()
    println("scanForPolycubes(Int::n): scans for Polycubes of size <=n")
    println("countPolycubes(): opens Polycube storage and displays the amount of Polycubes for the generated sizes")
    println("listPolycubes(): lists all Polycubes from the Polycube storage")
    println("plot(Vector{Int}::v): plots Polycubes of size v[1], or just v[2] from the list")
end

function scanForPolycubes(MaxSize::Int64)
    D = Dict{UInt, ImmutableOrientedPolycube}()
    S = Vector{Polycube}();
    cube = getCube()
    immutableCube = getImmutableOrientedPolycube(cube)
    D[immutableCube.hash] = immutableCube
    push!(S, cube)

    while !isempty(S)
        cube = pop!(S)
        growableSpaces = collect(getPossibleNeighbors(cube))
        acceptable_growth = MaxSize - length(cube.cubes)
        possibleGrowth = powerset(growableSpaces, 1, acceptable_growth)
        for cubesToAdd ∈ possibleGrowth
            newPolycube = deepcopy(cube)
            for c ∈ cubesToAdd
                push!(newPolycube, c)
            end
            collision = checkForCollision(newPolycube, D)
            if !collision
                push!(S, newPolycube)
                immutableNewPolycube = getImmutableOrientedPolycube(newPolycube)
                D[immutableNewPolycube.hash] = immutableNewPolycube
            end
        end
    end
    sanitizedData = sanitize(D, MaxSize)
    serialize("julia/results.bin", sanitizedData)
end

function scanForPolycubesRec(MaxSize::Int64)
    D = Dict{UInt, ImmutableOrientedPolycube}()
    singletonCube = getCube()
    immutableCube = getImmutableOrientedPolycube(singletonCube)
    D[immutableCube.hash] = immutableCube
    evaluatePolycube(singletonCube, D, MaxSize)
end

function evaluatePolycube(polycube::Polycube, D::Dict{UInt, ImmutableOrientedPolycube}, MaxSize::Int64)
    growableSpaces = collect(getPossibleNeighbors(polycube))
    acceptable_growth = MaxSize - length(polycube.cubes)
    
    possibleGrowth = powerset(growableSpaces, 1, acceptable_growth)
    for cubesToAdd ∈ possibleGrowth
        newPolycube = deepcopy(polycube)
        for c ∈ cubesToAdd
            push!(newPolycube, c)
        end
        collision = checkForCollision(newPolycube, D)
        if !collision
            immutableNewPolycube = getImmutableOrientedPolycube(newPolycube)
            D[immutableNewPolycube.hash] = immutableNewPolycube
            evaluatePolycube(newPolycube, D, MaxSize)
        end
    end
end

function countPolycubes()
    T = deserialize("julia/results.bin")
    n = T[1]
    for i ∈ 1:n
        print("n = ")
        print(i)
        print(": ")
        println(length(T[2][i]))
    end
end

function listPolycubes()
    T = deserialize("julia/results.bin")
    print("max size: ")
    println(T[1])
    for V ∈ T[2]
        for v ∈ V
            println(v)
        end
    end
end

function checkForCollision(S::Polycube, D::Dict{UInt, ImmutableOrientedPolycube})
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

function sanitize(D::Dict{UInt, ImmutableOrientedPolycube}, size::Int64)
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
