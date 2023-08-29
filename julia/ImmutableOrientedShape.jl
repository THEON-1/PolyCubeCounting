using XXhash

struct ImmutableOrientedShape
    cubes::Vector{Tuple{Int64, Int64, Int64}}
    hash::UInt
end

function getImmutableOrientedShape(S::Shape)
    shape = ImmutableOrientedShape(S.orderedLists[1], hashList(S.orderedLists[1]))
end
