using Plots
using Serialization

function plotPolycubes(nCubes::Int64, index::Int64=-1)
    pyplot()
    pygui(true)
    T = deserialize("julia/results.bin")
    i = if (index == -1); :; else index:index end
    v = T[2][nCubes][i]
    n = size(v)[1]

    colors = (:reds, :greens, :blues)
    
    X = [0, 0, 1, 1, 0, 0, 1, 1]
    Y = [0, 1, 1, 0, 0, 1, 1, 0]
    Z = [0, 0, 0, 0, 1, 1, 1, 1]

    I = [7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2]
    J = [3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3]
    K = [0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6]

    P = Vector{Plots.Plot{Plots.PyPlotBackend}}(undef, n)
    @show v
    for i ∈ 1:n
        polyCube = v[i]
        p = mesh3d()
        for cube ∈ polyCube
            @show cube
            mesh3d!(p, X.+cube[1], Y.+cube[2], Z.+cube[3], connections=(I, J, K), legend=:none, c=colors[rand(1:3)])
        end
        P[i] = p
    end
    plot(P..., colorbar=false)
end
