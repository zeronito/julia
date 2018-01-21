# This file is a part of Julia. License is MIT: https://julialang.org/license

__precompile__(true)

"""
Linear algebra module. Provides array arithmetic,
matrix factorizations and other linear algebra related
functionality.
"""
module LinearAlgebra

import Base: \, /, *, ^, +, -, ==
import Base: USE_BLAS64, abs, acos, acosh, acot, acoth, acsc, acsch, adjoint, asec, asech,
    asin, asinh, atan, atanh, axes, big, broadcast, ceil, conj, convert, copy, copyto!, cos,
    cosh, cot, coth, csc, csch, eltype, exp, findmax, findmin, fill!, floor, getindex, hcat,
    getproperty, imag, inv, isapprox, isone, IndexStyle, kron, length, log, map, ndims,
    oneunit, parent, power_by_squaring, print_matrix, promote_rule, real, round, sec, sech,
    setindex!, show, similar, sin, sincos, sinh, size, size_to_strides, sqrt, StridedReinterpretArray,
    StridedReshapedArray, ReshapedArray, ReinterpretArray, strides, stride, tan, tanh, transpose, trunc, typed_hcat, vec
using Base: hvcat_fill, iszero, IndexLinear, _length, promote_op, promote_typeof,
    @propagate_inbounds, @pure, reduce, typed_vcat, AbstractCartesianIndex, RangeIndex, Slice
# We use `_length` because of non-1 indices; releases after julia 0.5
# can go back to `length`. `_length(A)` is equivalent to `length(linearindices(A))`.

export
# Modules
    LAPACK,
    BLAS,

# Types
    Adjoint,
    Transpose,
    SymTridiagonal,
    Tridiagonal,
    Bidiagonal,
    Factorization,
    BunchKaufman,
    Cholesky,
    CholeskyPivoted,
    Eigen,
    GeneralizedEigen,
    GeneralizedSVD,
    GeneralizedSchur,
    Hessenberg,
    LU,
    LDLt,
    QR,
    QRPivoted,
    LQ,
    Schur,
    SVD,
    Hermitian,
    Symmetric,
    LowerTriangular,
    UpperTriangular,
    Diagonal,
    UniformScaling,

# Functions
    axpy!,
    axpby!,
    bkfact,
    bkfact!,
    chol,
    cholfact,
    cholfact!,
    cond,
    condskeel,
    copyto!,
    copy_transpose!,
    cross,
    adjoint,
    adjoint!,
    det,
    diag,
    diagind,
    diagm,
    diff,
    dot,
    eig,
    eigfact,
    eigfact!,
    eigmax,
    eigmin,
    eigvals,
    eigvals!,
    eigvecs,
    factorize,
    givens,
    hessfact,
    hessfact!,
    isdiag,
    ishermitian,
    isposdef,
    isposdef!,
    issuccess,
    issymmetric,
    istril,
    istriu,
    kron,
    ldltfact!,
    ldltfact,
    linreg,
    logabsdet,
    logdet,
    lu,
    lufact,
    lufact!,
    lyap,
    norm,
    normalize,
    normalize!,
    nullspace,
    ordschur!,
    ordschur,
    peakflops,
    pinv,
    qr,
    qrfact!,
    qrfact,
    lq,
    lqfact!,
    lqfact,
    rank,
    scale!,
    schur,
    schurfact!,
    schurfact,
    svd,
    svdfact!,
    svdfact,
    svdvals!,
    svdvals,
    sylvester,
    trace,
    transpose,
    transpose!,
    transpose_type,
    tril,
    triu,
    tril!,
    triu!,
    vecdot,
    vecnorm,

# Operators
    \,
    /,

# Constants
    I

const BlasFloat = Union{Float64,Float32,ComplexF64,ComplexF32}
const BlasReal = Union{Float64,Float32}
const BlasComplex = Union{ComplexF64,ComplexF32}

if USE_BLAS64
    const BlasInt = Int64
else
    const BlasInt = Int32
end

## Traits for memory layouts ##

abstract type MemoryLayout{T} end
struct UnknownLayout{T} <: MemoryLayout{T} end
abstract type AbstractStridedLayout{T} <: MemoryLayout{T} end
struct DenseLayout{T} <: AbstractStridedLayout{T} end
struct StridedLayout{T} <: AbstractStridedLayout{T} end

"""
    UnknownLayout{T}()

is returned by `MemoryLayout(A)` is unknown if or how the entries of an array `A`
are stored in memory.
"""
UnknownLayout

"""
    DenseLayout{T}()

is returned by `MemoryLayout(A)` if a matrix or vector `A` have the same storage as an
`Array`. In other words, the entries are stored consecutively in memory, ordered by column.
"""
DenseLayout

"""
    StridedLayout{T}()

is returned by `MemoryLayout(A)` if a matrix or vector `A` have the same storage as a
strided array. 
"""
StridedLayout


"""
    MemoryLayout(A)
    MemoryLayout(typeof(A))

`MemoryLayout` specifies the layout in memory for an array `A`. When
you define a new `AbstractArray` type, you can choose to implement
memory layout to indicate that an array is strided in memory. If you decide to
implement memory layout, then you must set this trait for your array
type:

    Base.MemoryLayout(::Type{M}) where M <: MyArray{T,N} where {T,N} = Base.StridedLayout{T,N}()

The default is `Base.UnknownLayout{T,N}()` to indicate that the layout
in memory is unknown.

Julia's internal linear algebra machinery will automatically (and invisibly)
dispatch to BLAS and LAPACK routines if the memory layout is BLAS and
the element type is a `Float32`, `Float64`, `ComplexF32`, or `ComplexF64`.
In this case, one must implement the strided array interface, which requires
overrides of `strides(A::MyArray)` and `unknown_convert(::Type{Ptr{T}}, A::MyArray)`
"""
MemoryLayout(A::AbstractArray{T}) where {T} = UnknownLayout{T}()
MemoryLayout(A::Vector{T}) where {T} = DenseLayout{T}()
MemoryLayout(A::Matrix{T}) where {T} = DenseLayout{T}()


MemoryLayout(A::SubArray) = submemorylayout(MemoryLayout(parent(A)), parentindices(A))
submemorylayout(::MemoryLayout{T}, _) where T = UnknownLayout{T}()
submemorylayout(::DenseLayout{T}, ::Tuple{I}) where {T,I<:Union{AbstractUnitRange{Int},Int,AbstractCartesianIndex}} =
    DenseLayout{T}()
submemorylayout(::AbstractStridedLayout{T}, ::Tuple{I}) where {T,I<:Union{RangeIndex,AbstractCartesianIndex}} =
    StridedLayout{T}()
submemorylayout(::DenseLayout{T}, ::Tuple{I1,Int}) where {T,I1<:Union{AbstractUnitRange{Int},Int,AbstractCartesianIndex}} =
    DenseLayout{T}()
submemorylayout(::DenseLayout{T}, ::Tuple{I1,Int}) where {T,I1<:Slice} =
    DenseLayout{T}()
submemorylayout(::DenseLayout{T}, ::Tuple{I1,I2}) where {T,I1<:Slice,I2<:Union{AbstractUnitRange{Int},Int,AbstractCartesianIndex}} =
    DenseLayout{T}()
submemorylayout(::AbstractStridedLayout{T}, ::Tuple{I1,I2}) where {T,I1<:Union{RangeIndex,AbstractCartesianIndex},I2<:Union{RangeIndex,AbstractCartesianIndex}} =
    StridedLayout{T}()

MemoryLayout(A::ReshapedArray) = reshapedmemorylayout(MemoryLayout(parent(A)))
reshapedmemorylayout(::MemoryLayout{T}) where T = UnknownLayout{T}()
reshapedmemorylayout(::DenseLayout{T}) where T = DenseLayout{T}()

MemoryLayout(A::ReinterpretArray{V}) where V = reinterpretedmemorylayout(MemoryLayout(parent(A)), V)
reinterpretedmemorylayout(::MemoryLayout{T}, ::Type{V}) where {T,V} = UnknownLayout{V}()
reinterpretedmemorylayout(::DenseLayout{T}, ::Type{V}) where {T,V} = DenseLayout{V}()




# MemoryLayout(A::ReinterpretArray{T}) where T =

# Check that stride of matrix/vector is 1
# Writing like this to avoid splatting penalty when called with multiple arguments,
# see PR 16416
"""
    stride1(A) -> Int

Return the distance between successive array elements
in dimension 1 in units of element size.

# Examples
```jldoctest
julia> A = [1,2,3,4]
4-element Array{Int64,1}:
 1
 2
 3
 4

julia> LinearAlgebra.stride1(A)
1

julia> B = view(A, 2:2:4)
2-element view(::Array{Int64,1}, 2:2:4) with eltype Int64:
 2
 4

julia> LinearAlgebra.stride1(B)
2
```
"""
stride1(x) = stride(x,1)
stride1(x::Array) = 1
stride1(x::DenseArray) = stride(x, 1)::Int

@inline chkstride1(A...) = _chkstride1(true, A...)
@noinline _chkstride1(ok::Bool) = ok || error("matrix does not have contiguous columns")
@inline _chkstride1(ok::Bool, A, B...) = _chkstride1(ok & (stride1(A) == 1), B...)

"""
    LinearAlgebra.checksquare(A)

Check that a matrix is square, then return its common dimension.
For multiple arguments, return a vector.

# Examples
```jldoctest
julia> A = fill(1, (4,4)); B = fill(1, (5,5));

julia> LinearAlgebra.checksquare(A, B)
2-element Array{Int64,1}:
 4
 5
```
"""
function checksquare(A)
    m,n = size(A)
    m == n || throw(DimensionMismatch("matrix is not square: dimensions are $(size(A))"))
    m
end

function checksquare(A...)
    sizes = Int[]
    for a in A
        size(a,1)==size(a,2) || throw(DimensionMismatch("matrix is not square: dimensions are $(size(a))"))
        push!(sizes, size(a,1))
    end
    return sizes
end

function char_uplo(uplo::Symbol)
    if uplo == :U
        'U'
    elseif uplo == :L
        'L'
    else
        throw(ArgumentError("uplo argument must be either :U (upper) or :L (lower)"))
    end
end

"""
    ldiv!(Y, A, B) -> Y

Compute `A \\ B` in-place and store the result in `Y`, returning the result.

The argument `A` should *not* be a matrix.  Rather, instead of matrices it should be a
factorization object (e.g. produced by [`factorize`](@ref) or [`cholfact`](@ref)).
The reason for this is that factorization itself is both expensive and typically allocates memory
(although it can also be done in-place via, e.g., [`lufact!`](@ref)),
and performance-critical situations requiring `ldiv!` usually also require fine-grained
control over the factorization of `A`.
"""
ldiv!(Y, A, B)

"""
    ldiv!(A, B)

Compute `A \\ B` in-place and overwriting `B` to store the result.

The argument `A` should *not* be a matrix.  Rather, instead of matrices it should be a
factorization object (e.g. produced by [`factorize`](@ref) or [`cholfact`](@ref)).
The reason for this is that factorization itself is both expensive and typically allocates memory
(although it can also be done in-place via, e.g., [`lufact!`](@ref)),
and performance-critical situations requiring `ldiv!` usually also require fine-grained
control over the factorization of `A`.
"""
ldiv!(A, B)


"""
    rdiv!(A, B)

Compute `A / B` in-place and overwriting `A` to store the result.

The argument `B` should *not* be a matrix.  Rather, instead of matrices it should be a
factorization object (e.g. produced by [`factorize`](@ref) or [`cholfact`](@ref)).
The reason for this is that factorization itself is both expensive and typically allocates memory
(although it can also be done in-place via, e.g., [`lufact!`](@ref)),
and performance-critical situations requiring `rdiv!` usually also require fine-grained
control over the factorization of `B`.
"""
rdiv!(A, B)

copy_oftype(A::AbstractArray{T}, ::Type{T}) where {T} = copy(A)
copy_oftype(A::AbstractArray{T,N}, ::Type{S}) where {T,N,S} = convert(AbstractArray{S,N}, A)

include("adjtrans.jl")
include("transpose.jl")
include("conjarray.jl")
include("rowvector.jl")

include("exceptions.jl")
include("generic.jl")

include("blas.jl")
include("matmul.jl")
include("lapack.jl")

include("dense.jl")
include("tridiag.jl")
include("triangular.jl")

include("factorization.jl")
include("qr.jl")
include("hessenberg.jl")
include("lq.jl")
include("eigen.jl")
include("svd.jl")
include("symmetric.jl")
include("cholesky.jl")
include("lu.jl")
include("bunchkaufman.jl")
include("diagonal.jl")
include("bidiag.jl")
include("uniformscaling.jl")
include("givens.jl")
include("special.jl")
include("bitarray.jl")
include("ldlt.jl")
include("schur.jl")
include("deprecated.jl")

const ⋅ = dot
const × = cross
export ⋅, ×


function versioninfo(io::IO=STDOUT)
    if Base.libblas_name == "libopenblas" || BLAS.vendor() == :openblas || BLAS.vendor() == :openblas64
        openblas_config = BLAS.openblas_get_config()
        println(io, "BLAS: libopenblas (", openblas_config, ")")
    else
        println(io, "BLAS: ",Base.libblas_name)
    end
    println(io, "LAPACK: ",Base.liblapack_name)
end

function __init__()
    try
        BLAS.check()
        if BLAS.vendor() == :mkl
            ccall((:MKL_Set_Interface_Layer, Base.libblas_name), Cvoid, (Cint,), USE_BLAS64 ? 1 : 0)
        end
    catch ex
        Base.showerror_nostdio(ex,
            "WARNING: Error during initialization of module LinearAlgebra")
    end
end

end # module LinearAlgebra
