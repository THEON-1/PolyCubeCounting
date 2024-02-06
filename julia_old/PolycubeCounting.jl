include("Polycube.jl")
include("ImmutableOrientedPolycube.jl")
include("plot.jl")
using XXhash
using Serialization
using Combinatorics
using WAV

function options()
    println("scanForPolycubes(MaxSize::Int64): scans for Polycubes of size <=MaxSize")
    println("countPolycubes(): opens Polycube storage and displays the amount of Polycubes for the generated sizes")
    println("listPolycubes(): lists all Polycubes from the Polycube storage")
    println("plotPolycubes(nCubes::Int64, index::Int64=-1): plots Polycubes of size v[1], or just v[2] from the list")
end

function scanForPolycubes(MaxSize::Int64, debug::Bool=false)
    T = deserialize("julia/results.bin")
    n = T[1]
    if (~debug && MaxSize <= n) return; end
    @time begin
        D = Dict{UInt, ImmutableOrientedPolycube}()
        singletonCube = getCube()
        immutableCube = getImmutableOrientedPolycube(singletonCube)
        D[immutableCube.hash] = immutableCube
        evaluatePolycube(singletonCube, D, MaxSize)
    end
    if (~debug) serialize("julia/results.bin", sanitize(D, MaxSize)) end
    y, fs = wavread("julia/background-error.wav")
    wavplay(y, fs)
end

function evaluatePolycube(polycube::Polycube, D::Dict{UInt, ImmutableOrientedPolycube}, MaxSize::Int64)
    growableSpaces = collect(getPossibleNeighbors(polycube))
    acceptable_growth = MaxSize - length(polycube.cubes)
    
    possibleGrowth = powerset(growableSpaces, 1, acceptable_growth)
    for cubesToAdd ∈ possibleGrowth
        newPolycube = Polycube(copy(polycube.cubes), Set(cubesToAdd), deepcopy(polycube.orderedLists))
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

Base.:+(t1 = Tuple{Int64, Int64, Int64}, t2 = Tuple{Int64, Int64, Int64}) = (t1[1] + t2[1], t1[2] + t2[2], t1[3] + t2[3])
Base.:-(t1 = Tuple{Int64, Int64, Int64}, t2 = Tuple{Int64, Int64, Int64}) = t1 + t2.*-1