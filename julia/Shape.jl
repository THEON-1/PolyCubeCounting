include("SortingModifiers.jl")

struct Shape
    cubes::Set{Tuple{Int8, Int8, Int8}}
    recentCubes::Set{Tuple{Int8, Int8, Int8}}
    orderedLists::Array{List{Tuple{Int8, Int8, Int8}}}
end

function trypush!(S::Shape, t::Tuple{Int8, Int8, Int8})
    if t ∈ S.cubes
        return false
    else
        push!(S.cubes, t)
        for i ∈ 1:24
            index = searchsortedfirst(S.orderedLists[i], t, by=SortingModifiers[i])
            insert!(S.orderedLists, index, t)
        end
        return true
    end
end
