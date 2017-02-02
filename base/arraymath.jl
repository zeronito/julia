# This file is a part of Julia. License is MIT: http://julialang.org/license

## Unary operators ##

"""
    conj!(A)

Transform an array to its complex conjugate in-place.

See also [`conj`](@ref).

```jldoctest
julia> A = [1+im 2-im; 2+2im 3+im]
2×2 Array{Complex{Int64},2}:
 1+1im  2-1im
 2+2im  3+1im

julia> conj!(A);

julia> A
2×2 Array{Complex{Int64},2}:
 1-1im  2+1im
 2-2im  3-1im
```
"""
conj!{T<:Number}(A::AbstractArray{T}) = (@inbounds broadcast!(conj, A, A); A)

for f in (:-, :~, :conj, :real, :imag)
    @eval ($f)(A::AbstractArray) = broadcast($f, A)
end

!(A::AbstractArray{Bool}) = broadcast(!, A)

## Binary arithmetic operators ##

for f in (:+, :-)
    @eval function ($f)(A::AbstractArray, B::AbstractArray)
        promote_shape(A, B) # check size compatibility
        broadcast($f, A, B)
    end
end

for f in (:/, :\, :*, :+, :-)
    if f != :/
        @eval ($f){T}(A::Number, B::AbstractArray{T}) = broadcast($f, A, B)
    end
    if f != :\
        @eval ($f){T}(A::AbstractArray{T}, B::Number) = broadcast($f, A, B)
    end
end

## data movement ##

function flipdim{T}(A::Array{T}, d::Integer)
    nd = ndims(A)
    1 ≤ d ≤ nd || throw(ArgumentError("dimension $d is not 1 ≤ $d ≤ $nd"))
    sd = size(A, d)
    if sd == 1 || isempty(A)
        return copy(A)
    end

    B = similar(A)

    nnd = 0
    for i = 1:nd
        nnd += Int(size(A,i)==1 || i==d)
    end
    if nnd==nd
        # flip along the only non-singleton dimension
        for i = 1:sd
            B[i] = A[sd+1-i]
        end
        return B
    end

    d_in = size(A)
    leading = d_in[1:(d-1)]
    M = prod(leading)
    N = length(A)
    stride = M * sd

    if M==1
        for j = 0:stride:(N-stride)
            for i = 1:sd
                ri = sd+1-i
                B[j + ri] = A[j + i]
            end
        end
    else
        if isbits(T) && M>200
            for i = 1:sd
                ri = sd+1-i
                for j=0:stride:(N-stride)
                    offs = j + 1 + (i-1)*M
                    boffs = j + 1 + (ri-1)*M
                    copy!(B, boffs, A, offs, M)
                end
            end
        else
            for i = 1:sd
                ri = sd+1-i
                for j=0:stride:(N-stride)
                    offs = j + 1 + (i-1)*M
                    boffs = j + 1 + (ri-1)*M
                    for k=0:(M-1)
                        B[boffs + k] = A[offs + k]
                    end
                end
            end
        end
    end
    return B
end

"""
    rotl90(A)

Rotate matrix `A` left 90 degrees.

```jldoctest
julia> a = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> rotl90(a)
2×2 Array{Int64,2}:
 2  4
 1  3
```
"""
function rotl90(A::AbstractMatrix)
    ind1, ind2 = indices(A)
    B = similar(A, (ind2,ind1))
    n = first(ind2)+last(ind2)
    for i=indices(A,1), j=ind2
        B[n-j,i] = A[i,j]
    end
    return B
end

"""
    rotr90(A)

Rotate matrix `A` right 90 degrees.

```jldoctest
julia> a = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> rotr90(a)
2×2 Array{Int64,2}:
 3  1
 4  2
```
"""
function rotr90(A::AbstractMatrix)
    ind1, ind2 = indices(A)
    B = similar(A, (ind2,ind1))
    m = first(ind1)+last(ind1)
    for i=ind1, j=indices(A,2)
        B[j,m-i] = A[i,j]
    end
    return B
end
"""
    rot180(A)

Rotate matrix `A` 180 degrees.

```jldoctest
julia> a = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> rot180(a)
2×2 Array{Int64,2}:
 4  3
 2  1
```
"""
function rot180(A::AbstractMatrix)
    B = similar(A)
    ind1, ind2 = indices(A,1), indices(A,2)
    m, n = first(ind1)+last(ind1), first(ind2)+last(ind2)
    for j=ind2, i=ind1
        B[m-i,n-j] = A[i,j]
    end
    return B
end
"""
    rotl90(A, k)

Rotate matrix `A` left 90 degrees an integer `k` number of times.
If `k` is zero or a multiple of four, this is equivalent to a `copy`.

```jldoctest
julia> a = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> rotl90(a,1)
2×2 Array{Int64,2}:
 2  4
 1  3

julia> rotl90(a,2)
2×2 Array{Int64,2}:
 4  3
 2  1

julia> rotl90(a,3)
2×2 Array{Int64,2}:
 3  1
 4  2

julia> rotl90(a,4)
2×2 Array{Int64,2}:
 1  2
 3  4
```
"""
function rotl90(A::AbstractMatrix, k::Integer)
    k = mod(k, 4)
    k == 1 ? rotl90(A) :
    k == 2 ? rot180(A) :
    k == 3 ? rotr90(A) : copy(A)
end
"""
    rotr90(A, k)

Rotate matrix `A` right 90 degrees an integer `k` number of times. If `k` is zero or a
multiple of four, this is equivalent to a `copy`.

```jldoctest
julia> a = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> rotr90(a,1)
2×2 Array{Int64,2}:
 3  1
 4  2

julia> rotr90(a,2)
2×2 Array{Int64,2}:
 4  3
 2  1

julia> rotr90(a,3)
2×2 Array{Int64,2}:
 2  4
 1  3

julia> rotr90(a,4)
2×2 Array{Int64,2}:
 1  2
 3  4
```
"""
rotr90(A::AbstractMatrix, k::Integer) = rotl90(A,-k)
"""
    rot180(A, k)

Rotate matrix `A` 180 degrees an integer `k` number of times.
If `k` is even, this is equivalent to a `copy`.

```jldoctest
julia> a = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> rot180(a,1)
2×2 Array{Int64,2}:
 4  3
 2  1

julia> rot180(a,2)
2×2 Array{Int64,2}:
 1  2
 3  4
```
"""
rot180(A::AbstractMatrix, k::Integer) = mod(k, 2) == 1 ? rot180(A) : copy(A)
