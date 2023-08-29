module Powerset

global _calculatedPowerSets = Matrix{Int64}(undef, 1, 0)
global _grownUntil = 0

function getPowerSet(setSize::Integer)
    global _calculatedPowerSets
    global _grownUntil
    wantedPowerSetSize = 2^setSize
    if setSize <= _grownUntil
        return _calculatedPowerSets[1:wantedPowerSetSize, 1:setSize]
    else
        currentPowerSetSize = 2^_grownUntil
        newPowerSets = zeros(Int64, wantedPowerSetSize, setSize)
        indices = CartesianIndices(_calculatedPowerSets)
        copyto!(newPowerSets, indices, _calculatedPowerSets, indices)
        for i ∈ (_grownUntil+1):setSize
            indicesTarget = CartesianIndices((2^(i-1)+1:2^i, 1:(i-1)))
            indicesSource = CartesianIndices((1:2^(i-1), 1:(i-1)))
            copyto!(newPowerSets, indicesTarget, newPowerSets, indicesSource)
            for j ∈ 1:currentPowerSetSize
                newPowerSets[currentPowerSetSize + j, i] = 1;
            end
            currentPowerSetSize *= 2
        end
        _calculatedPowerSets = newPowerSets
        _grownUntil = setSize
        return _calculatedPowerSets
    end
end

function getPowerSubSet(setSize::Integer, subSetSize::Integer)
    PowerSet = getPowerSet(setSize)
    setSizes = dropdims(sum(PowerSet, dims = 2), dims = 2)
    powerSubSetSize = binomial(setSize, subSetSize)
    powerSubSets = Matrix{Int64}(undef, powerSubSetSize, subSetSize)
    j = 1
    for i ∈ axes(PowerSet, 1)
        if setSizes[i] == subSetSize
            l = 1
            for k ∈ axes(PowerSet, 2)
                if PowerSet[i, k] == 1
                    powerSubSets[j, l] = k
                    l += 1
                end
            end
            j += 1
        end
    end
    return powerSubSets
end

end
