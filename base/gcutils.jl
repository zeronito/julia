# This file is a part of Julia. License is MIT: https://julialang.org/license

==(w::WeakRef, v::WeakRef) = isequal(w.value, v.value)
==(w::WeakRef, v) = isequal(w.value, v)
==(w, v::WeakRef) = isequal(w, v.value)

"""
    finalizer(f, x)

Register a function `f(x)` to be called when there are no program-accessible references to
`x`, and return `x`. The type of `x` must be a `mutable struct`, otherwise the behavior of
this function is unpredictable.

`f` must not cause a task switch, which excludes most I/O operations such as `println`.
Using the `@async` macro (to defer context switching to outside of the finalizer) or
`ccall` to directly invoke IO functions in C may be helpful for debugging purposes.

# Examples
```julia
finalizer(my_mutable_struct) do x
    @async println("Finalizing \$x.")
end

finalizer(my_mutable_struct) do x
    ccall(:jl_safe_printf, Cvoid, (Cstring, Cstring), "Finalizing %s.", repr(x))
end
```
"""
function finalizer(@nospecialize(f), @nospecialize(o))
    if !ismutable(o)
        error("objects of type ", typeof(o), " cannot be finalized")
    end
    ccall(:jl_gc_add_finalizer_th, Cvoid, (Ptr{Cvoid}, Any, Any),
          Core.getptls(), o, f)
    return o
end

function finalizer(f::Ptr{Cvoid}, o::T) where T
    @_inline_meta
    if !ismutable(o)
        error("objects of type ", typeof(o), " cannot be finalized")
    end
    ccall(:jl_gc_add_ptr_finalizer, Cvoid, (Ptr{Cvoid}, Any, Ptr{Cvoid}),
          Core.getptls(), o, f)
    return o
end

"""
    finalize(x)

Immediately run finalizers registered for object `x`.
"""
finalize(@nospecialize(o)) = ccall(:jl_finalize_th, Cvoid, (Ptr{Cvoid}, Any,),
                                   Core.getptls(), o)

"""
    Base.GC

Module with garbage collection utilities.
"""
module GC

# mirrored from julia.h
const GC_AUTO = 0
const GC_FULL = 1
const GC_INCREMENTAL = 2

"""
    GC.gc([full=true])

Perform garbage collection. The argument `full` determines the kind of
collection: A full collection (default) sweeps all objects, which makes the
next GC scan much slower, while an incremental collection may only sweep
so-called young objects.

!!! warning
    Excessive use will likely lead to poor performance.
"""
gc(full::Bool=true) =
    ccall(:jl_gc_collect, Cvoid, (Cint,), full ? GC_FULL : GC_INCREMENTAL)

"""
    GC.enable(on::Bool)

Control whether garbage collection is enabled using a boolean argument (`true` for enabled,
`false` for disabled). Return previous GC state.

!!! warning
    Disabling garbage collection should be used only with caution, as it can cause memory
    use to grow without bound.
"""
enable(on::Bool) = ccall(:jl_gc_enable, Int32, (Int32,), on) != 0

"""
    GC.@preserve x1 x2 ... xn expr

Temporarily protect the given objects from being garbage collected, even if they would
otherwise be unreferenced.

The last argument is the expression during which the object(s) will be preserved.
The previous arguments are the objects to preserve.
"""
macro preserve(args...)
    syms = args[1:end-1]
    for x in syms
        isa(x, Symbol) || error("Preserved variable must be a symbol")
    end
    esc(Expr(:gc_preserve, args[end], syms...))
end

"""
    GC.safepoint()

Inserts a point in the program where garbage collection may run.
This can be useful in rare cases in multi-threaded programs where some threads
are allocating memory (and hence may need to run GC) but other threads are doing
only simple operations (no allocation, task switches, or I/O).
Calling this function periodically in non-allocating threads allows garbage
collection to run.

!!! compat "Julia 1.4"
    This function is available as of Julia 1.4.
"""
safepoint() = ccall(:jl_gc_safepoint, Cvoid, ())

end # module GC
