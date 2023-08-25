include("Shape.jl")
using XXhash

struct ImmutableOrientedShape
    cubes::Vector{Tuple{Int8, Int8, Int8}}
    hash::UInt
end

function getImmutableOrientedShape(S::Shape)
    shape = ImmutableOrientedShape(S.orderedLists[1], xxh3_64(S.orderedLists[1]))
end
