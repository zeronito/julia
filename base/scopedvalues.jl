# This file is a part of Julia. License is MIT: https://julialang.org/license

module ScopedValues

export ScopedValue, with, @with

"""
    ScopedValue(x)

Create a container that propagates values across dynamic scopes.
Use [`with`](@ref) to create and enter a new dynamic scope.

Values can only be set when entering a new dynamic scope,
and the value referred to will be constant during the
execution of a dynamic scope.

Dynamic scopes are propagated across tasks.

# Examples
```jldoctest
julia> const sval = ScopedValue(1);

julia> sval[]
1

julia> with(sval => 2) do
           sval[]
       end
2

julia> sval[]
1
```

!!! compat "Julia 1.11"
    Scoped values were introduced in Julia 1.11. In Julia 1.8+ a compatible
    implementation is available from the package ScopedValues.jl.
"""
mutable struct ScopedValue{T}
    const initial_value::T
end

Base.eltype(::Type{ScopedValue{T}}) where {T} = T

mutable struct ScopedValueCache
    parent::Union{Nothing, ScopedValueCache}
    const key::ScopedValue
    const val::Any
end

function lookup(cache::ScopedValueCache, key::ScopedValue)
    depth = 0
    while cache !== nothing
        if cache.key === key
            return true, cache.val
        end
        cache = cache.parent
        depth += 1
        if depth > 3
            break
        end
    end
    if cache !== nothing
        cache.parent = nothing
    end
    return false, nothing
end

mutable struct Scope
    const task::Base.Task
    const values::Base.PersistentDict{ScopedValue, Any}
    cache::Union{Nothing, ScopedValueCache} # Only accessed if current_task() == task
    Scope(values::Base.PersistentDict{ScopedValue, Any}) = new(Base.current_task(), values, nothing)
end
function Scope(parent::Union{Nothing, Scope}, key::ScopedValue{T}, value) where T
    val = convert(T, value)
    if parent === nothing
        return Scope(Base.PersistentDict{ScopedValue, Any}(key=>val))
    end
    return Scope(Base.PersistentDict(parent.values, key=>convert(T, val)))
end

function Scope(scope, pairs::Pair{<:ScopedValue}...)
    for pair in pairs
        scope = Scope(scope, pair...)
    end
    return scope::Scope
end
Scope(::Nothing) = nothing

"""
    current_scope()::Union{Nothing, Scope}

Return the current dynamic scope.
"""
current_scope() = current_task().scope

function Base.show(io::IO, scope::Scope)
    print(io, Scope, "(")
    first = true
    for (key, value) in scope.values
        if first
            first = false
        else
            print(io, ", ")
        end
        print(io, typeof(key), "@")
        show(io, Base.objectid(key))
        print(io, " => ")
        show(IOContext(io, :typeinfo => eltype(key)), value)
    end
    print(io, ")")
end

# Base._get is rather large and we don't want to inline that
@noinline function getindex_slow(scope::Scope, val::ScopedValue{T})::T where T
    found, v = @inline Base._get(scope.values, val)
    if found
        return v::T
    else
        return val.initial_value
    end
end

@inline function Base.getindex(val::ScopedValue{T})::T where T
    scope = current_scope()
    if scope === nothing
        return val.initial_value
    end
    scope = scope::Scope
    if scope.task !== current_task() # unlikely -- detect child.task and invalidate cache
        current_task().scope = scope = Scope(scope.values)
    end
    cache = scope.cache
    if cache !== nothing
        found, v = @inline lookup(cache, val)
        if found
            return v::T
        end
    end
    v = getindex_slow(scope, val)
    scope.cache = ScopedValueCache(cache, val, v)
    return v::T
end

function Base.show(io::IO, var::ScopedValue)
    print(io, ScopedValue)
    print(io, '{', eltype(var), '}')
    print(io, '(')
    show(IOContext(io, :typeinfo => eltype(var)), var[])
    print(io, ')')
end

"""
    with(f, (var::ScopedValue{T} => val::T)...)

Execute `f` in a new scope with `var` set to `val`.
"""
function with(f, pair::Pair{<:ScopedValue}, rest::Pair{<:ScopedValue}...)
    @nospecialize
    ct = Base.current_task()
    current_scope = ct.scope::Union{Nothing, Scope}
    scope = Scope(current_scope, pair, rest...)
    ct.scope = scope
    try
        return f()
    finally
        ct.scope = current_scope
    end
end

with(@nospecialize(f)) = f()

"""
    @with vars... expr

Macro version of `with(f, vars...)` but with `expr` instead of `f` function.
This is similar to using [`with`](@ref) with a `do` block, but avoids creating
a closure.
"""
macro with(exprs...)
    if length(exprs) > 1
        ex = last(exprs)
        exprs = exprs[1:end-1]
    elseif length(exprs) == 1
        ex = only(exprs)
        exprs = ()
    else
        error("@with expects at least one argument")
    end
    for expr in exprs
        if expr.head !== :call || first(expr.args) !== :(=>)
            error("@with expects arguments of the form `A => 2` got $expr")
        end
    end
    exprs = map(esc, exprs)
    ct = gensym(:ct)
    current_scope = gensym(:current_scope)
    body = Expr(:tryfinally, esc(ex), :($(ct).scope = $(current_scope)))
    quote
        $(ct) = $(Base.current_task)()
        $(current_scope) = $(ct).scope::$(Union{Nothing, Scope})
        $(ct).scope = $(Scope)($(current_scope), $(exprs...))
        $body
    end
end

end # module ScopedValues
