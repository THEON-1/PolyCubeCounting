
const global _comp01(x) = (x[1], x[2], x[3])
const global _comp02(x) = (x[2], x[3], x[1])
const global _comp03(x) = (x[3], x[1], x[2])

const global _comp04(x) = (x[1], x[2], -x[3])
const global _comp05(x) = (x[2], x[3], -x[1])
const global _comp06(x) = (x[3], x[1], -x[2])

const global _comp07(x) = (x[1], -x[2], x[3])
const global _comp08(x) = (x[2], -x[3], x[1])
const global _comp09(x) = (x[3], -x[1], x[2])

const global _comp10(x) = (x[1], -x[2], -x[3])
const global _comp11(x) = (x[2], -x[3], -x[1])
const global _comp12(x) = (x[3], -x[1], -x[2])

const global _comp13(x) = (-x[1], x[2], x[3])
const global _comp14(x) = (-x[2], x[3], x[1])
const global _comp15(x) = (-x[3], x[1], x[2])

const global _comp16(x) = (-x[1], x[2], -x[3])
const global _comp17(x) = (-x[2], x[3], -x[1])
const global _comp18(x) = (-x[3], x[1], -x[2])

const global _comp19(x) = (-x[1], -x[2], x[3])
const global _comp20(x) = (-x[2], -x[3], x[1])
const global _comp21(x) = (-x[3], -x[1], x[2])

const global _comp22(x) = (-x[1], -x[2], -x[3])
const global _comp23(x) = (-x[2], -x[3], -x[1])
const global _comp24(x) = (-x[3], -x[1], -x[2])

const global SortingModifiers = [
    _comp01,
    _comp02,
    _comp03,
    _comp04,
    _comp05,
    _comp06,
    _comp07,
    _comp08,
    _comp09,
    _comp10,
    _comp11,
    _comp12,
    _comp13,
    _comp14,
    _comp15,
    _comp16,
    _comp17,
    _comp18,
    _comp19,
    _comp20,
    _comp21,
    _comp22,
    _comp23,
    _comp24
]
