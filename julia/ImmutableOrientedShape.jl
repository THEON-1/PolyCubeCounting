
struct ImmutableOrientedShape
    cubes::List{Tuple{Int8, Int8, Int8}}
    hash::UInt
end

Base.hash(S::ImmutableOrientedShape) = S.hash
Base.hash(S::ImmutableOrientedShape, h::UInt) = S.hash ⊻ h
