
module Powerset

global _calculatedPowerSets = Matrix{Int64}(undef, 1, 0)
global _grownUntil = 0

function getPowerSet(setSize::Integer)
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
    setSizes = dropdim(sum(Powerset, dims = 2), dims = 2)

    return 
end

end
