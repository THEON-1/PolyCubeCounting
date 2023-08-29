
const global _rot01(x)::Tuple{Int64, Int64, Int64} = ( x[1],  x[2],  x[3])
const global _rot02(x)::Tuple{Int64, Int64, Int64} = ( x[2],  x[3],  x[1])
const global _rot03(x)::Tuple{Int64, Int64, Int64} = ( x[3],  x[1],  x[2])

const global _rot04(x)::Tuple{Int64, Int64, Int64} = ( x[1], -x[2], -x[3])
const global _rot05(x)::Tuple{Int64, Int64, Int64} = ( x[2], -x[3], -x[1])
const global _rot06(x)::Tuple{Int64, Int64, Int64} = ( x[3], -x[1], -x[2])

const global _rot07(x)::Tuple{Int64, Int64, Int64} = (-x[1],  x[2], -x[3])
const global _rot08(x)::Tuple{Int64, Int64, Int64} = (-x[2],  x[3], -x[1])
const global _rot09(x)::Tuple{Int64, Int64, Int64} = (-x[3],  x[1], -x[2])

const global _rot10(x)::Tuple{Int64, Int64, Int64} = (-x[1], -x[2],  x[3])
const global _rot11(x)::Tuple{Int64, Int64, Int64} = (-x[2], -x[3],  x[1])
const global _rot12(x)::Tuple{Int64, Int64, Int64} = (-x[3], -x[1],  x[2])

const global _rot13(x)::Tuple{Int64, Int64, Int64} = (-x[3],  x[2],  x[1])
const global _rot14(x)::Tuple{Int64, Int64, Int64} = (-x[2],  x[1],  x[3])
const global _rot15(x)::Tuple{Int64, Int64, Int64} = (-x[1],  x[3],  x[2])

const global _rot16(x)::Tuple{Int64, Int64, Int64} = ( x[3], -x[2],  x[1])
const global _rot17(x)::Tuple{Int64, Int64, Int64} = ( x[2], -x[1],  x[3])
const global _rot18(x)::Tuple{Int64, Int64, Int64} = ( x[1], -x[3],  x[2])

const global _rot19(x)::Tuple{Int64, Int64, Int64} = ( x[3],  x[2], -x[1])
const global _rot20(x)::Tuple{Int64, Int64, Int64} = ( x[2],  x[1], -x[3])
const global _rot21(x)::Tuple{Int64, Int64, Int64} = ( x[1],  x[3], -x[2])

const global _rot22(x)::Tuple{Int64, Int64, Int64} = (-x[3], -x[2], -x[1])
const global _rot23(x)::Tuple{Int64, Int64, Int64} = (-x[2], -x[1], -x[3])
const global _rot24(x)::Tuple{Int64, Int64, Int64} = (-x[1], -x[3], -x[2])

const global Rotations = [
    _rot01,
    _rot02,
    _rot03,
    _rot04,
    _rot05,
    _rot06,
    _rot07,
    _rot08,
    _rot09,
    _rot10,
    _rot11,
    _rot12,
    _rot13,
    _rot14,
    _rot15,
    _rot16,
    _rot17,
    _rot18,
    _rot19,
    _rot20,
    _rot21,
    _rot22,
    _rot23,
    _rot24
]