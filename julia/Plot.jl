using PyCall
using Serialization

py"""
import numpy as np
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt

def plotShapePY(v):
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    plotSingleShape(ax, v)
    
    plt.show()

def plotShapesPY(vs):
    n = len(vs)
    if n == 1:
        plotShapePY(vs[0])
    else:
        fig = plt.figure()
        n_rows = int(np.ceil(0.6*n))
        n_cols = int(np.ceil(n/n_rows))
        for i in range(len(vs)):
            v = vs[i]
            ax = fig.add_subplot(n_cols, n_rows, i+1, projection='3d')
            plotSingleShape(ax, v)
        plt.show()

def plotSingleShape(ax, v):
    l = len(v)

    for i in range(l):
        X1, Y1, Z1 = horizontalPlane(v[i][0]  , v[i][1]  , v[i][2]  )
        X2, Y2, Z2 = horizontalPlane(v[i][0]  , v[i][1]  , v[i][2]+1)
        X3, Y3, Z3 = verticalPlaneX( v[i][0]  , v[i][1]  , v[i][2]  )
        X4, Y4, Z4 = verticalPlaneX( v[i][0]  , v[i][1]+1, v[i][2]  )
        X5, Y5, Z5 = verticalPlaneY( v[i][0]  , v[i][1]  , v[i][2]  )
        X6, Y6, Z6 = verticalPlaneY( v[i][0]+1, v[i][1]  , v[i][2]  )

        ax.plot_surface(X1, Y1, Z1, alpha=0.8, color='orange')
        ax.plot_surface(X2, Y2, Z2, alpha=0.8, color='orange')
        ax.plot_surface(X3, Y3, Z3, alpha=0.8, color='green' )
        ax.plot_surface(X4, Y4, Z4, alpha=0.8, color='green' )
        ax.plot_surface(X5, Y5, Z5, alpha=0.8, color='cyan'  )
        ax.plot_surface(X6, Y6, Z6, alpha=0.8, color='cyan'  )

    x_min = np.inf
    x_max = -np.inf
    y_min = np.inf
    y_max = -np.inf
    z_min = np.inf
    z_max = -np.inf
    for i in range(l):
        x_min = np.min([x_min, v[i][0]])
        x_max = np.max([x_max, v[i][0]])
        y_min = np.min([y_min, v[i][1]])
        y_max = np.max([y_max, v[i][1]])
        z_min = np.min([z_min, v[i][2]])
        z_max = np.max([z_max, v[i][2]])
    
    biggest_length = max([x_max-x_min, y_max-y_min, z_max-z_min])
    x_diff = (biggest_length - (x_max - x_min)) / 2
    x_lim_min = x_min - x_diff
    x_lim_max = x_max + x_diff
    
    y_diff = (biggest_length - (y_max - y_min)) / 2
    y_lim_min = y_min - y_diff
    y_lim_max = y_max + y_diff
    
    z_diff = (biggest_length - (z_max - z_min)) / 2
    z_lim_min = z_min - z_diff
    z_lim_max = z_max + z_diff
    
    ax.scatter([x_lim_min, x_lim_max+1], [y_lim_min, y_lim_max+1], [z_lim_min, z_lim_max+1], alpha=0)

def getPoints(a):
    x = a[0]
    y = a[1]
    z = a[2]
    p = [
        [x,   y,   z  ],
        [x+1, y,   z  ],
        [x+1, y+1, z  ],
        [x,   y+1, z  ],
        [x,   y,   z+1],
        [x+1, y,   z+1],
        [x+1, y+1, z+1],
        [x,   y+1, z+1]
    ]
    return p

def horizontalPlane(x, y, z):
    one = np.ones(4).reshape(2, 2)
    r1 = [x, x + 1]
    r2 = [y, y + 1]
    X, Y = np.meshgrid(r1, r2)
    Z = one*z
    return X, Y, Z

def verticalPlaneX(x, y, z):
    one = np.ones(4).reshape(2, 2)
    r1 = [x, x + 1]
    r2 = [z, z + 1]
    X, Z = np.meshgrid(r1, r2)
    Y = one*y
    return X, Y, Z
    

def verticalPlaneY(x, y, z):
    one = np.ones(4).reshape(2, 2)
    r1 = [y, y + 1]
    r2 = [z, z + 1]
    Y, Z = np.meshgrid(r1, r2)
    X = one*x
    return X, Y, Z
"""

function plotShape(t::Vector{Int64})
    T = deserialize("results.bin")
    v = T[2][t[1]][t[2]]

    py"plotShapePY"(v)
end

function plotShapes(t::Vector{Int64})
    T = deserialize("results.bin")
    v = T[2][t[1]]

    py"plotShapesPY"(v)
end
