
Base.:+(X::Tuple{Integer, Integer, Integer}, Y::Tuple{Integer, Integer, Integer}) = (X[1]+Y[1], X[2]+Y[2], X[3]+Y[3])
Base.:-(X::Tuple{Integer, Integer, Integer}, Y::Tuple{Integer, Integer, Integer}) = (X[1]-Y[1], X[2]-Y[2], X[3]-Y[3])
