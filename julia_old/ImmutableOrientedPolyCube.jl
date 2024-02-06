using XXhash

struct ImmutableOrientedPolycube
    cubes::Vector{Tuple{Int64, Int64, Int64}}
    hash::UInt
end

function getImmutableOrientedPolycube(S::Polycube)
    shape = ImmutableOrientedPolycube(S.orderedLists[1], hashList(S.orderedLists[1]))
end
