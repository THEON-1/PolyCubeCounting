
struct ImmutableOrientedShape
    cubes::Vector{Tuple{Int8, Int8, Int8}}
    hash::UInt
end

Base.hash(S::ImmutableOrientedShape) = S.hash
Base.hash(S::ImmutableOrientedShape, h::UInt) = S.hash ⊻ h

function Base.==(X::ImmutableOrientedShape, Y::ImmutableOrientedShape)
    if hash != hash
        return false
    elseif X.cubes == Y.cubes
        return false
    else
        return true
    end
end