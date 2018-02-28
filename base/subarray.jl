# This file is a part of Julia. License is MIT: https://julialang.org/license

abstract type AbstractCartesianIndex{N} end # This is a hacky forward declaration for CartesianIndex
const ViewIndex = Union{Real, AbstractArray}
const ScalarIndex = Real

# L is true if the view itself supports fast linear indexing
struct SubArray{T,N,P,I,L} <: AbstractArray{T,N}
    parent::P
    indices::I
    offset1::Int       # for linear indexing and pointer, only valid when L==true
    stride1::Int       # used only for linear indexing
    function SubArray{T,N,P,I,L}(parent, indices, offset1, stride1) where {T,N,P,I,L}
        @_inline_meta
        check_parent_index_match(parent, indices)
        new(parent, indices, offset1, stride1)
    end
end
# Compute the linear indexability of the indices, and combine it with the linear indexing of the parent
function SubArray(parent::AbstractArray, indices::Tuple)
    @_inline_meta
    SubArray(IndexStyle(viewindexing(indices), IndexStyle(parent)), parent, ensure_indexable(indices), index_dimsum(indices...))
end
function SubArray(::IndexCartesian, parent::P, indices::I, ::NTuple{N,Any}) where {P,I,N}
    @_inline_meta
    SubArray{eltype(P), N, P, I, false}(parent, indices, 0, 0)
end
function SubArray(::IndexLinear, parent::P, indices::I, ::NTuple{N,Any}) where {P,I,N}
    @_inline_meta
    # Compute the stride and offset
    stride1 = compute_stride1(parent, indices)
    SubArray{eltype(P), N, P, I, true}(parent, indices, compute_offset1(parent, stride1, indices), stride1)
end

check_parent_index_match(parent, indices) = check_parent_index_match(parent, index_ndims(indices...))
check_parent_index_match(parent::AbstractArray{T,N}, ::NTuple{N, Bool}) where {T,N} = nothing
check_parent_index_match(parent, ::NTuple{N, Bool}) where {N} =
    throw(ArgumentError("number of indices ($N) must match the parent dimensionality ($(ndims(parent)))"))

# This computes the linear indexing compatability for a given tuple of indices
viewindexing() = IndexLinear()
# Leading scalar indices simply increase the stride
viewindexing(I::Tuple{ScalarIndex, Vararg{Any}}) = (@_inline_meta; viewindexing(tail(I)))
# Slices may begin a section which may be followed by any number of Slices
viewindexing(I::Tuple{Slice, Slice, Vararg{Any}}) = (@_inline_meta; viewindexing(tail(I)))
# A UnitRange can follow Slices, but only if all other indices are scalar
viewindexing(I::Tuple{Slice, UnitRange, Vararg{ScalarIndex}}) = IndexLinear()
# In general, ranges are only fast if all other indices are scalar
viewindexing(I::Tuple{Union{AbstractRange, Slice}, Vararg{ScalarIndex}}) = IndexLinear()
# All other index combinations are slow
viewindexing(I::Tuple{Vararg{Any}}) = IndexCartesian()
# Of course, all other array types are slow
viewindexing(I::Tuple{AbstractArray, Vararg{Any}}) = IndexCartesian()

# Simple utilities
size(V::SubArray) = (@_inline_meta; map(n->Int(unsafe_length(n)), axes(V)))

similar(V::SubArray, T::Type, dims::Dims) = similar(V.parent, T, dims)

sizeof(V::SubArray) = length(V) * sizeof(eltype(V))

parent(V::SubArray) = V.parent
parentindices(V::SubArray) = V.indices

"""
    parentindices(A)

From an array view `A`, returns the corresponding indices in the parent.
"""
parentindices(a::AbstractArray) = ntuple(i->OneTo(size(a,i)), ndims(a))

## Aliasing detection
dataids(A::SubArray) = (dataids(A.parent)..., _splatmap(dataids, A.indices)...)
_splatmap(f, ::Tuple{}) = ()
_splatmap(f, t::Tuple) = (f(t[1])..., _splatmap(f, tail(t))...)
unaliascopy(A::SubArray) = typeof(A)(unaliascopy(A.parent), map(unaliascopy, A.indices), A.offset1, A.stride1)

# When the parent is an Array we can trim the size down a bit. In the future this
# could possibly be extended to any mutable array.
function unaliascopy(V::SubArray{T,N,A,I,LD}) where {T,N,A<:Array,I<:Tuple{Vararg{Union{Real,AbstractRange,Array}}},LD}
    dest = Array{T}(uninitialized, index_lengths(V.indices...))
    copyto!(dest, V)
    SubArray{T,N,A,I,LD}(dest, map(_trimmedindex, V.indices), 0, Int(LD))
end
# Transform indices to be "dense"
_trimmedindex(i::Real) = oftype(i, 1)
_trimmedindex(i::AbstractUnitRange) = i
_trimmedindex(i::AbstractArray) = oftype(i, reshape(linearindices(i), axes(i)))

## SubArray creation
# We always assume that the dimensionality of the parent matches the number of
# indices that end up getting passed to it, so we store the parent as a
# ReshapedArray view if necessary. The trouble is that arrays of `CartesianIndex`
# can make the number of effective indices not equal to length(I).
_maybe_reshape_parent(A::AbstractArray, ::NTuple{1, Bool}) = reshape(A, Val(1))
_maybe_reshape_parent(A::AbstractArray{<:Any,1}, ::NTuple{1, Bool}) = reshape(A, Val(1))
_maybe_reshape_parent(A::AbstractArray{<:Any,N}, ::NTuple{N, Bool}) where {N} = A
_maybe_reshape_parent(A::AbstractArray, ::NTuple{N, Bool}) where {N} = reshape(A, Val(N))
"""
    view(A, inds...)

Like [`getindex`](@ref), but returns a view into the parent array `A` with the
given indices instead of making a copy.  Calling [`getindex`](@ref) or
[`setindex!`](@ref) on the returned `SubArray` computes the
indices to the parent array on the fly without checking bounds.

```jldoctest
julia> A = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> b = view(A, :, 1)
2-element view(::Array{Int64,2}, :, 1) with eltype Int64:
 1
 3

julia> fill!(b, 0)
2-element view(::Array{Int64,2}, :, 1) with eltype Int64:
 0
 0

julia> A # Note A has changed even though we modified b
2×2 Array{Int64,2}:
 0  2
 0  4
```
"""
function view(A::AbstractArray, I::Vararg{Any,N}) where {N}
    @_inline_meta
    J = map(i->unalias(A,i), to_indices(A, I))
    @boundscheck checkbounds(A, J...)
    unsafe_view(_maybe_reshape_parent(A, index_ndims(J...)), J...)
end

function unsafe_view(A::AbstractArray, I::Vararg{ViewIndex,N}) where {N}
    @_inline_meta
    SubArray(A, I)
end
# When we take the view of a view, it's often possible to "reindex" the parent
# view's indices such that we can "pop" the parent view and keep just one layer
# of indirection. But we can't always do this because arrays of `CartesianIndex`
# might span multiple parent indices, making the reindex calculation very hard.
# So we use _maybe_reindex to figure out if there are any arrays of
# `CartesianIndex`, and if so, we punt and keep two layers of indirection.
unsafe_view(V::SubArray, I::Vararg{ViewIndex,N}) where {N} =
    (@_inline_meta; _maybe_reindex(V, I))
_maybe_reindex(V, I) = (@_inline_meta; _maybe_reindex(V, I, I))
_maybe_reindex(V, I, ::Tuple{AbstractArray{<:AbstractCartesianIndex}, Vararg{Any}}) =
    (@_inline_meta; SubArray(V, I))
# But allow arrays of CartesianIndex{1}; they behave just like arrays of Ints
_maybe_reindex(V, I, A::Tuple{AbstractArray{<:AbstractCartesianIndex{1}}, Vararg{Any}}) =
    (@_inline_meta; _maybe_reindex(V, I, tail(A)))
_maybe_reindex(V, I, A::Tuple{Any, Vararg{Any}}) = (@_inline_meta; _maybe_reindex(V, I, tail(A)))
function _maybe_reindex(V, I, ::Tuple{})
    @_inline_meta
    @inbounds idxs = to_indices(V.parent, reindex(V, V.indices, I))
    SubArray(V.parent, idxs)
end

## Re-indexing is the heart of a view, transforming A[i, j][x, y] to A[i[x], j[y]]
#
# Recursively look through the heads of the parent- and sub-indices, considering
# the following cases:
# * Parent index is array  -> re-index that with one or more sub-indices (one per dimension)
# * Parent index is Colon  -> just use the sub-index as provided
# * Parent index is scalar -> that dimension was dropped, so skip the sub-index and use the index as is

AbstractZeroDimArray{T} = AbstractArray{T, 0}

reindex(V, ::Tuple{}, ::Tuple{}) = ()

# Skip dropped scalars, so simply peel them off the parent indices and continue
reindex(V, idxs::Tuple{ScalarIndex, Vararg{Any}}, subidxs::Tuple{Vararg{Any}}) =
    (@_propagate_inbounds_meta; (idxs[1], reindex(V, tail(idxs), subidxs)...))

# Slices simply pass their subindices straight through
reindex(V, idxs::Tuple{Slice, Vararg{Any}}, subidxs::Tuple{Any, Vararg{Any}}) =
    (@_propagate_inbounds_meta; (subidxs[1], reindex(V, tail(idxs), tail(subidxs))...))

# Re-index into parent vectors with one subindex
reindex(V, idxs::Tuple{AbstractVector, Vararg{Any}}, subidxs::Tuple{Any, Vararg{Any}}) =
    (@_propagate_inbounds_meta; (idxs[1][subidxs[1]], reindex(V, tail(idxs), tail(subidxs))...))

# Parent matrices are re-indexed with two sub-indices
reindex(V, idxs::Tuple{AbstractMatrix, Vararg{Any}}, subidxs::Tuple{Any, Any, Vararg{Any}}) =
    (@_propagate_inbounds_meta; (idxs[1][subidxs[1], subidxs[2]], reindex(V, tail(idxs), tail(tail(subidxs)))...))

# In general, we index N-dimensional parent arrays with N indices
@generated function reindex(V, idxs::Tuple{AbstractArray{T,N}, Vararg{Any}}, subidxs::Tuple{Vararg{Any}}) where {T,N}
    if length(subidxs.parameters) >= N
        subs = [:(subidxs[$d]) for d in 1:N]
        tail = [:(subidxs[$d]) for d in N+1:length(subidxs.parameters)]
        :(@_propagate_inbounds_meta; (idxs[1][$(subs...)], reindex(V, tail(idxs), ($(tail...),))...))
    else
        :(throw(ArgumentError("cannot re-index $(ndims(V)) dimensional SubArray with fewer than $(ndims(V)) indices\nThis should not occur; please submit a bug report.")))
    end
end

# In general, we simply re-index the parent indices by the provided ones
SlowSubArray{T,N,P,I} = SubArray{T,N,P,I,false}
function getindex(V::SlowSubArray{T,N}, I::Vararg{Int,N}) where {T,N}
    @_inline_meta
    @boundscheck checkbounds(V, I...)
    @inbounds r = V.parent[reindex(V, V.indices, I)...]
    r
end

FastSubArray{T,N,P,I} = SubArray{T,N,P,I,true}
function getindex(V::FastSubArray, i::Int)
    @_inline_meta
    @boundscheck checkbounds(V, i)
    @inbounds r = V.parent[V.offset1 + V.stride1*i]
    r
end
# We can avoid a multiplication if the first parent index is a Colon or UnitRange
FastContiguousSubArray{T,N,P,I<:Tuple{Union{Slice, UnitRange}, Vararg{Any}}} = SubArray{T,N,P,I,true}
function getindex(V::FastContiguousSubArray, i::Int)
    @_inline_meta
    @boundscheck checkbounds(V, i)
    @inbounds r = V.parent[V.offset1 + i]
    r
end

function setindex!(V::SlowSubArray{T,N}, x, I::Vararg{Int,N}) where {T,N}
    @_inline_meta
    @boundscheck checkbounds(V, I...)
    @inbounds V.parent[reindex(V, V.indices, I)...] = x
    V
end
function setindex!(V::FastSubArray, x, i::Int)
    @_inline_meta
    @boundscheck checkbounds(V, i)
    @inbounds V.parent[V.offset1 + V.stride1*i] = x
    V
end
function setindex!(V::FastContiguousSubArray, x, i::Int)
    @_inline_meta
    @boundscheck checkbounds(V, i)
    @inbounds V.parent[V.offset1 + i] = x
    V
end

IndexStyle(::Type{<:FastSubArray}) = IndexLinear()
IndexStyle(::Type{<:SubArray}) = IndexCartesian()

# Strides are the distance in memory between adjacent elements in a given dimension
# which we determine from the strides of the parent
strides(V::SubArray) = substrides(V.parent, V.indices)

substrides(parent, I::Tuple) = substrides(parent, strides(parent), I)
substrides(parent, strds::Tuple{}, ::Tuple{}) = ()
substrides(parent, strds::NTuple{N,Int}, I::Tuple{ScalarIndex, Vararg{Any}}) where N = (substrides(parent, tail(strds), tail(I))...,)
substrides(parent, strds::NTuple{N,Int}, I::Tuple{Slice, Vararg{Any}}) where N = (first(strds), substrides(parent, tail(strds), tail(I))...)
substrides(parent, strds::NTuple{N,Int}, I::Tuple{AbstractRange, Vararg{Any}}) where N = (first(strds)*step(I[1]), substrides(parent, tail(strds), tail(I))...)
substrides(parent, strds, I::Tuple{Any, Vararg{Any}}) = throw(ArgumentError("strides is invalid for SubArrays with indices of type $(typeof(I[1]))"))

stride(V::SubArray, d::Integer) = d <= ndims(V) ? strides(V)[d] : strides(V)[end] * size(V)[end]

compute_stride1(parent::AbstractArray, I::NTuple{N,Any}) where {N} =
    (@_inline_meta; compute_stride1(1, fill_to_length(axes(parent), OneTo(1), Val(N)), I))
compute_stride1(s, inds, I::Tuple{}) = s
compute_stride1(s, inds, I::Tuple{ScalarIndex, Vararg{Any}}) =
    (@_inline_meta; compute_stride1(s*unsafe_length(inds[1]), tail(inds), tail(I)))
compute_stride1(s, inds, I::Tuple{AbstractRange, Vararg{Any}}) = s*step(I[1])
compute_stride1(s, inds, I::Tuple{Slice, Vararg{Any}}) = s
compute_stride1(s, inds, I::Tuple{Any, Vararg{Any}}) = throw(ArgumentError("invalid strided index type $(typeof(I[1]))"))

iscontiguous(A::SubArray) = iscontiguous(typeof(A))
iscontiguous(::Type{<:SubArray}) = false
iscontiguous(::Type{<:FastContiguousSubArray}) = true

first_index(V::FastSubArray) = V.offset1 + V.stride1 # cached for fast linear SubArrays
function first_index(V::SubArray)
    P, I = parent(V), V.indices
    s1 = compute_stride1(P, I)
    s1 + compute_offset1(P, s1, I)
end

# Computing the first index simply steps through the indices, accumulating the
# sum of index each multiplied by the parent's stride.
# The running sum is `f`; the cumulative stride product is `s`.
# If the parent is a vector, then we offset the parent's own indices with parameters of I
compute_offset1(parent::AbstractVector, stride1::Integer, I::Tuple{AbstractRange}) =
    (@_inline_meta; first(I[1]) - first(indices1(I[1]))*stride1)
# If the result is one-dimensional and it's a Colon, then linear
# indexing uses the indices along the given dimension. Otherwise
# linear indexing always starts with 1.
compute_offset1(parent, stride1::Integer, I::Tuple) =
    (@_inline_meta; compute_offset1(parent, stride1, find_extended_dims(1, I...), find_extended_inds(I...), I))
compute_offset1(parent, stride1::Integer, dims::Tuple{Int}, inds::Tuple{Slice}, I::Tuple) =
    (@_inline_meta; compute_linindex(parent, I) - stride1*first(axes(parent, dims[1])))  # index-preserving case
compute_offset1(parent, stride1::Integer, dims, inds, I::Tuple) =
    (@_inline_meta; compute_linindex(parent, I) - stride1)  # linear indexing starts with 1

function compute_linindex(parent, I::NTuple{N,Any}) where N
    @_inline_meta
    IP = fill_to_length(axes(parent), OneTo(1), Val(N))
    compute_linindex(1, 1, IP, I)
end
function compute_linindex(f, s, IP::Tuple, I::Tuple{ScalarIndex, Vararg{Any}})
    @_inline_meta
    Δi = I[1]-first(IP[1])
    compute_linindex(f + Δi*s, s*unsafe_length(IP[1]), tail(IP), tail(I))
end
function compute_linindex(f, s, IP::Tuple, I::Tuple{Any, Vararg{Any}})
    @_inline_meta
    Δi = first(I[1])-first(IP[1])
    compute_linindex(f + Δi*s, s*unsafe_length(IP[1]), tail(IP), tail(I))
end
compute_linindex(f, s, IP::Tuple, I::Tuple{}) = f

find_extended_dims(dim, ::ScalarIndex, I...) = (@_inline_meta; find_extended_dims(dim + 1, I...))
find_extended_dims(dim, i1, I...) = (@_inline_meta; (dim, find_extended_dims(dim + 1, I...)...))
find_extended_dims(dim) = ()
find_extended_inds(::ScalarIndex, I...) = (@_inline_meta; find_extended_inds(I...))
find_extended_inds(i1, I...) = (@_inline_meta; (i1, find_extended_inds(I...)...))
find_extended_inds() = ()

MemoryLayout(A::SubArray) = submemorylayout(MemoryLayout(parent(A)), parentindices(A))
submemorylayout(::MemoryLayout, _)= UnknownLayout()
submemorylayout(::AbstractColumnMajor, ::Tuple{I}) where {I<:Union{AbstractUnitRange{Int},Int,AbstractCartesianIndex}} =
    DenseColumnMajor()
submemorylayout(::AbstractStridedLayout, ::Tuple{I}) where {I<:Union{RangeIndex,AbstractCartesianIndex}} =
    StridedLayout()
submemorylayout(::AbstractColumnMajor, ::Tuple{I,Int}) where {I<:Union{AbstractUnitRange{Int},Int,AbstractCartesianIndex}} =
    DenseColumnMajor()
submemorylayout(::AbstractColumnMajor, ::Tuple{I,Int}) where {I<:Slice} =
    DenseColumnMajor()
submemorylayout(::AbstractRowMajor, ::Tuple{Int,I}) where {I<:Union{AbstractUnitRange{Int},Int,AbstractCartesianIndex}} =
    DenseColumnMajor()
submemorylayout(::AbstractRowMajor, ::Tuple{Int,I}) where {I<:Slice} =
    DenseColumnMajor()
submemorylayout(::DenseColumnMajor, ::Tuple{I1,I2}) where {I1<:Slice,I2<:AbstractUnitRange{Int}} =
    DenseColumnMajor()
submemorylayout(::DenseColumnMajor, ::Tuple{I1,I2}) where {I1<:AbstractUnitRange{Int},I2<:AbstractUnitRange{Int}} =
    ColumnMajor()
submemorylayout(::AbstractColumnMajor, ::Tuple{I1,I2}) where {I1<:AbstractUnitRange{Int},I2<:AbstractUnitRange{Int}} =
    ColumnMajor()
submemorylayout(::AbstractRowMajor, ::Tuple{I1,I2}) where {I1<:AbstractUnitRange{Int},I2<:Slice} =
    DenseRowMajor()
submemorylayout(::AbstractRowMajor, ::Tuple{I1,I2}) where {I1<:AbstractUnitRange{Int},I2<:AbstractUnitRange{Int}} =
    RowMajor()
submemorylayout(::AbstractStridedLayout, ::Tuple{I1,I2}) where {I1<:Union{RangeIndex,AbstractCartesianIndex},I2<:Union{RangeIndex,AbstractCartesianIndex}} =
    StridedLayout()

unsafe_convert(::Type{Ptr{T}}, V::SubArray{T,N,P,<:Tuple{Vararg{RangeIndex}}}) where {T,N,P} =
    unsafe_convert(Ptr{T}, V.parent) + (first_index(V)-1)*sizeof(T)

pointer(V::FastSubArray, i::Int) = pointer(V.parent, V.offset1 + V.stride1*i)
pointer(V::FastContiguousSubArray, i::Int) = pointer(V.parent, V.offset1 + i)
pointer(V::SubArray, i::Int) = _pointer(V, i)
_pointer(V::SubArray{<:Any,1}, i::Int) = pointer(V, (i,))
_pointer(V::SubArray, i::Int) = pointer(V, Base._ind2sub(axes(V), i))

function pointer(V::SubArray{T,N,<:Array,<:Tuple{Vararg{RangeIndex}}}, is::Tuple{Vararg{Int}}) where {T,N}
    index = first_index(V)
    strds = strides(V)
    for d = 1:length(is)
        index += (is[d]-1)*strds[d]
    end
    return pointer(V.parent, index)
end

# indices are taken from the range/vector
# Since bounds-checking is performance-critical and uses
# indices, it's worth optimizing these implementations thoroughly
axes(S::SubArray) = (@_inline_meta; _indices_sub(S, S.indices...))
_indices_sub(S::SubArray) = ()
_indices_sub(S::SubArray, ::Real, I...) = (@_inline_meta; _indices_sub(S, I...))
function _indices_sub(S::SubArray, i1::AbstractArray, I...)
    @_inline_meta
    (unsafe_indices(i1)..., _indices_sub(S, I...)...)
end

## Compatability
# deprecate?
function parentdims(s::SubArray)
    nd = ndims(s)
    dimindex = Vector{Int}(uninitialized, nd)
    sp = strides(s.parent)
    sv = strides(s)
    j = 1
    for i = 1:ndims(s.parent)
        r = s.indices[i]
        if j <= nd && (isa(r,Union{Slice,AbstractRange}) ? sp[i]*step(r) : sp[i]) == sv[j]
            dimindex[j] = i
            j += 1
        end
    end
    dimindex
end
