Coord = Tuple{Int, Int, Int}

function Base.:-(u::Coord, v::Coord)
    return (u[1] - v[1], u[2] - v[2], u[3] - v[3])
end

function neighbors(x::Int, y::Int, z::Int)
    return [
        (x+1, y, z),
        (x-1, y, z),
        (x, y+1, z),
        (x, y-1, z),
        (x, y, z+1),
        (x, y, z-1)
    ]
end

function orient_tuple(t::Coord, o::Int)
    return _shift_tuple(_flip_tuple(_mirror_tuple(t, o-1)...)...)[1]
end

function _shift_tuple(t::Coord, o::Int)
    funcs = [
        (x, y, z) -> (x, y, z),
        (x, y, z) -> (y, z, x),
        (x, y, z) -> (z, x, y),
    ]
    return (funcs[o%3+1](t...), div(o, 3))
end

function _flip_tuple(t::Coord, o::Int)
    funcs = [
        (x, y, z) -> (x, y, z),
        (x, y, z) -> (-x, -y, z),
        (x, y, z) -> (-x, y, -z),
        (x, y, z) -> (x, -y, -z)
    ]
    return (funcs[o%4+1](t...), div(o, 4))
end

function _mirror_tuple(t::Coord, o::Int)
    funcs = [
        (x, y, z) -> (x, y, z),
        (x, y, z) -> (-z, -y, -x)
    ]
    return (funcs[o%2+1](t...), div(o, 2))
end
