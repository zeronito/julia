# This file is a part of Julia. License is MIT: https://julialang.org/license

### Methods for a Ref object that can store a single value of any type

mutable struct RefValue{T} <: Ref{T}
    x::T
    RefValue{T}() where {T} = new()
    RefValue{T}(x) where {T} = new(x)
end
RefValue(x::T) where {T} = RefValue{T}(x)
"""
    isassigned(ref::RefValue) -> Bool

Test whether the given [`Ref`](@ref) has been associated with a value. This is always true for a [`Ref`](@ref) for a bitstype. Return `false` if the reference is undefined.

# Examples
```jldoctest
julia> ref = Ref{Function}()
Base.RefValue{Function}(#undef)

julia> isassigned(ref)
false

julia> ref[] = (foobar(x) = x)
foobar (generic function with 1 method)

julia> isassigned(ref)
true

julia> isassigned(Ref{Int}())
true
```
"""
isassigned(x::RefValue) = isdefined(x, :x)

function unsafe_convert(P::Type{Ptr{T}}, b::RefValue{T}) where T
    if allocatedinline(T)
        p = pointer_from_objref(b)
    elseif isconcretetype(T) && T.mutable
        p = pointer_from_objref(b.x)
    else
        # If the slot is not leaf type, it could be either immutable or not.
        # If it is actually an immutable, then we can't take it's pointer directly
        # Instead, explicitly load the pointer from the `RefValue`,
        # which also ensures this returns same pointer as the one rooted in the `RefValue` object.
        p = pointerref(Ptr{Ptr{Cvoid}}(pointer_from_objref(b)), 1, Core.sizeof(Ptr{Cvoid}))
    end
    return convert(P, p)
end
function unsafe_convert(P::Type{Ptr{Any}}, b::RefValue{Any})
    return convert(P, pointer_from_objref(b))
end
unsafe_convert(::Type{Ptr{Cvoid}}, b::RefValue{T}) where {T} = convert(Ptr{Cvoid}, unsafe_convert(Ptr{T}, b))

getindex(b::RefValue) = b.x
setindex!(b::RefValue, x) = (b.x = x; b)
