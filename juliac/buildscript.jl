# Script to run in the process that generates juliac's object file output

inputfile = ARGS[1]
output_type = ARGS[2]
add_ccallables = ARGS[3] == "true"

# Initialize some things not usually initialized when output is request
Sys.__init__()
Base.init_depot_path()
Base.init_load_path()
Base.init_active_project()
task = current_task()
task.rngState0 = 0x5156087469e170ab
task.rngState1 = 0x7431eaead385992c
task.rngState2 = 0x503e1d32781c2608
task.rngState3 = 0x3a77f7189200c20b
task.rngState4 = 0x5502376d099035ae
uuid_tuple = (UInt64(0), UInt64(0))
ccall(:jl_set_module_uuid, Cvoid, (Any, NTuple{2, UInt64}), Base.__toplevel__, uuid_tuple)
ccall(:jl_set_newly_inferred, Cvoid, (Any,), Core.Compiler.newly_inferred)

# Patch methods in Core and Base

@eval Core begin
    DomainError(@nospecialize(val), @nospecialize(msg::AbstractString)) = (@noinline; $(Expr(:new, :DomainError, :val, :msg)))
end

(f::Base.RedirectStdStream)(io::Core.CoreSTDOUT) = Base._redirect_io_global(io, f.unix_fd)

@eval Base.CoreLogging begin
    _invoked_catch_exceptions(logger) = false #catch_exceptions(logger)
    _invoked_min_enabled_level(logger) = min_enabled_level(logger)
    function make_normalization_context(logger::ConsoleLogger, nkwargs::Int)
        stream = logger.stream::IOBuffer
        dsize = displaysize(stream)::Tuple{Int,Int}
        valbuf = IOBuffer()
        rows_per_value = max(1, dsize[1] ÷ (nkwargs + 1))
        valio = IOContext(IOContext(valbuf, stream),
                          :displaysize => (rows_per_value, dsize[2] - 5),
                          :limit => logger.show_limited)
        return (valbuf, valio)
    end
    current_logger() = _global_logstate.logger::ConsoleLogger
    @noinline Base.@constprop :none function current_logger_for_env(std_level::LogLevel, group, _module)
        logstate = @inline current_logstate()
        if std_level >= logstate.min_enabled_level || env_override_minlevel(group, _module)
            return logstate.logger::ConsoleLogger
        end
        return nothing
    end
end

@eval Base begin
    _assert_tostring(msg) = ""
    reinit_stdio() = nothing
    get(::Core.CoreSTDERR, key::Symbol, default) = key === :color ? get_have_color() : default
    get(::Core.CoreSTDOUT, key::Symbol, default) = key === :color ? get_have_color() : default
    JuliaSyntax.enable_in_core!() = nothing
    init_active_project() = ACTIVE_PROJECT[] = nothing
    set_active_project(projfile::Union{AbstractString,Nothing}) = ACTIVE_PROJECT[] = projfile
    disable_library_threading() = nothing
    start_profile_listener() = nothing
    @inline function invokelatest(f::F, args...; kwargs...) where F
        return f(args...; kwargs...)
    end
    # nothrow needed here because for v in a can't prove the indexing is inbounds.

    function sprint(f::F, args::Vararg{Any,N}; context=nothing, sizehint::Integer=0) where {F<:Function,N}
        s = IOBuffer(sizehint=sizehint)
        if context isa Tuple
            f(IOContext(s, context...), args...)
        elseif context !== nothing
            f(IOContext(s, context), args...)
        else
            f(s, args...)
        end
        String(_unsafe_take!(s))
    end
    function show_typeish(io::IO, @nospecialize(T))
        if T isa Type
            show(io, T)
        elseif T isa TypeVar
            print(io, (T::TypeVar).name)
        else
            print(io, "?")
        end
    end
    function show(io::IO, T::Type)
        if T isa DataType
            print(io, T.name.name)
            if T !== T.name.wrapper && length(T.parameters) > 0
                print(io, "{")
                first = true
                for p in T.parameters
                    if !first
                        print(io, ", ")
                    end
                    first = false
                    if p isa Int
                        show(io, p)
                    elseif p isa Type
                        show(io, p)
                    elseif p isa Symbol
                        print(io, ":")
                        print(io, p)
                    elseif p isa TypeVar
                        print(io, p.name)
                    else
                        print(io, "?")
                    end
                end
                print(io, "}")
            end
        elseif T isa Union
            print(io, "Union{")
            show_typeish(io, T.a)
            print(io, ", ")
            show_typeish(io, T.b)
            print(io, "}")
        elseif T isa UnionAll
            print(io, T.body::Type)
            print(io, " where ")
            print(io, T.var.name)
        end
    end
    show_type_name(io::IO, tn::Core.TypeName) = print(io, tn.name)

    mapreduce(f::F, op::F2, A::AbstractArrayOrBroadcasted; dims=:, init=_InitialValue()) where {F, F2} =
    _mapreduce_dim(f, op, init, A, dims)
    mapreduce(f::F, op::F2, A::AbstractArrayOrBroadcasted...; kw...) where {F, F2} =
        reduce(op, map(f, A...); kw...)

    _mapreduce_dim(f::F, op::F2, nt, A::AbstractArrayOrBroadcasted, ::Colon) where {F, F2} =
        mapfoldl_impl(f, op, nt, A)

    _mapreduce_dim(f::F, op::F2, ::_InitialValue, A::AbstractArrayOrBroadcasted, ::Colon) where {F, F2} =
        _mapreduce(f, op, IndexStyle(A), A)

    _mapreduce_dim(f::F, op::F2, nt, A::AbstractArrayOrBroadcasted, dims) where {F, F2} =
        mapreducedim!(f, op, reducedim_initarray(A, dims, nt), A)

    _mapreduce_dim(f::F, op::F2, ::_InitialValue, A::AbstractArrayOrBroadcasted, dims) where {F,F2} =
        mapreducedim!(f, op, reducedim_init(f, op, A, dims), A)

    mapreduce_empty_iter(f::F, op::F2, itr, ItrEltype) where {F, F2} =
        reduce_empty_iter(MappingRF(f, op), itr, ItrEltype)
        mapreduce_first(f::F, op::F2, x) where {F,F2} = reduce_first(op, f(x))

    _mapreduce(f::F, op::F2, A::AbstractArrayOrBroadcasted) where {F,F2} = _mapreduce(f, op, IndexStyle(A), A)
    mapreduce_empty(::typeof(identity), op::F, T) where {F} = reduce_empty(op, T)
    mapreduce_empty(::typeof(abs), op::F, T) where {F}     = abs(reduce_empty(op, T))
    mapreduce_empty(::typeof(abs2), op::F, T) where {F}    = abs2(reduce_empty(op, T))
end
@eval Base.Unicode begin
    function utf8proc_map(str::Union{String,SubString{String}}, options::Integer, chartransform::F = identity) where F
        nwords = utf8proc_decompose(str, options, C_NULL, 0, chartransform)
        buffer = Base.StringVector(nwords*4)
        nwords = utf8proc_decompose(str, options, buffer, nwords, chartransform)
        nbytes = ccall(:utf8proc_reencode, Int, (Ptr{UInt8}, Int, Cint), buffer, nwords, options)
        nbytes < 0 && utf8proc_error(nbytes)
        return String(resize!(buffer, nbytes))
    end
end
@eval Base.GMP begin
    function __init__()
        try
            ccall((:__gmp_set_memory_functions, libgmp), Cvoid,
                (Ptr{Cvoid},Ptr{Cvoid},Ptr{Cvoid}),
                cglobal(:jl_gc_counted_malloc),
                cglobal(:jl_gc_counted_realloc_with_old_size),
                cglobal(:jl_gc_counted_free_with_size))
            ZERO.alloc, ZERO.size, ZERO.d = 0, 0, C_NULL
            ONE.alloc, ONE.size, ONE.d = 1, 1, pointer(_ONE)
        catch ex
            Base.showerror_nostdio(ex, "WARNING: Error during initialization of module GMP")
        end
        # This only works with a patched version of GMP, ignore otherwise
        try
            ccall((:__gmp_set_alloc_overflow_function, libgmp), Cvoid,
                (Ptr{Cvoid},),
                cglobal(:jl_throw_out_of_memory_error))
            ALLOC_OVERFLOW_FUNCTION[] = true
        catch ex
            # ErrorException("ccall: could not find function...")
            if typeof(ex) != ErrorException
                rethrow()
            end
        end
    end
end
@eval Base.Sort begin
    issorted(itr;
        lt::T=isless, by::F=identity, rev::Union{Bool,Nothing}=nothing, order::Ordering=Forward) where {T,F} =
        issorted(itr, ord(lt,by,rev,order))
end
@eval Base.TOML begin
    function try_return_datetime(p, year, month, day, h, m, s, ms)
        return DateTime(year, month, day, h, m, s, ms)
    end
    function try_return_date(p, year, month, day)
        return Date(year, month, day)
    end
    function parse_local_time(l::Parser)
        h = @try parse_int(l, false)
        h in 0:23 || return ParserError(ErrParsingDateTime)
        _, m, s, ms = @try _parse_local_time(l, true)
        # TODO: Could potentially parse greater accuracy for the
        # fractional seconds here.
        return try_return_time(l, h, m, s, ms)
    end
    function try_return_time(p, h, m, s, ms)
        return Time(h, m, s, ms)
    end
end

# Load user code

import Base.Experimental.entrypoint

let mod = Base.include(Base.__toplevel__, inputfile)
    if !isa(mod, Module)
        mod = Main
    end
    if output_type == "--output-exe" && isdefined(mod, :main) && !add_ccallables
        entrypoint(mod.main, ())
    end
    #entrypoint(join, (Base.GenericIOBuffer{Memory{UInt8}}, Array{Base.SubString{String}, 1}, String))
    #entrypoint(join, (Base.GenericIOBuffer{Memory{UInt8}}, Array{String, 1}, Char))
    entrypoint(Base.task_done_hook, (Task,))
    entrypoint(Base.wait, ())
    entrypoint(Base.trypoptask, (Base.StickyWorkqueue,))
    entrypoint(Base.checktaskempty, ())
    if add_ccallables
        ccall(:jl_add_ccallable_entrypoints, Cvoid, ())
    end
end

# Additional method patches depending on whether user code loads certain stdlibs

let loaded = Symbol.(Base.loaded_modules_array())  # TODO better way to do this
    if :LinearAlgebra in loaded
        using LinearAlgebra
        @eval LinearAlgebra.BLAS begin
            check() = nothing #TODO: this might be unsafe but needs logging macro fixes
        end
    end
    if :SparseArrays in loaded
        using SparseArrays
        @eval SparseArrays.CHOLMOD begin
            function __init__()
                ccall((:SuiteSparse_config_malloc_func_set, :libsuitesparseconfig),
                    Cvoid, (Ptr{Cvoid},), cglobal(:jl_malloc, Ptr{Cvoid}))
                ccall((:SuiteSparse_config_calloc_func_set, :libsuitesparseconfig),
                    Cvoid, (Ptr{Cvoid},), cglobal(:jl_calloc, Ptr{Cvoid}))
                ccall((:SuiteSparse_config_realloc_func_set, :libsuitesparseconfig),
                    Cvoid, (Ptr{Cvoid},), cglobal(:jl_realloc, Ptr{Cvoid}))
                ccall((:SuiteSparse_config_free_func_set, :libsuitesparseconfig),
                Cvoid, (Ptr{Cvoid},), cglobal(:jl_free, Ptr{Cvoid}))
            end
        end
    end
    if :Artifacts in loaded
        using Artifacts
        @eval Artifacts begin
            function _artifact_str(__module__, artifacts_toml, name, path_tail, artifact_dict, hash, platform, _::Val{lazyartifacts}) where lazyartifacts
                moduleroot = Base.moduleroot(__module__)
                if haskey(Base.module_keys, moduleroot)
                    # Process overrides for this UUID, if we know what it is
                    process_overrides(artifact_dict, Base.module_keys[moduleroot].uuid)
                end

                # If the artifact exists, we're in the happy path and we can immediately
                # return the path to the artifact:
                dirs = artifact_paths(hash; honor_overrides=true)
                for dir in dirs
                    if isdir(dir)
                        return jointail(dir, path_tail)
                    end
                end
            end
        end
    end
    if :Pkg in loaded
        using Pkg
        @eval Pkg begin
            __init__() = rand() #TODO, methods that do nothing don't get codegened
        end
    end
    if :StyledStrings in loaded
        using StyledStrings
        setfield!(typeof(Base.annotatedstring).name.mt, :max_args, 10, :monotonic)
        @eval StyledStrings begin
            __init__() = rand()
            function _ansi_writer(io::IO, s::Union{<:AnnotatedString, SubString{<:AnnotatedString}},
                                  string_writer::F) where {F <: Function}
                if get(io, :color, false)::Bool
                    buf = IOBuffer() # Avoid the overhead in repeatadly printing to `stdout`
                    lastface::Face = FACES.default[:default]
                    for (str, styles) in eachregion(s)
                        face = getface(styles)
                        link = let idx=findfirst(==(:link) ∘ first, styles)
                            if !isnothing(idx)
                                last(styles[idx])::String
                                # string(last(styles[idx]))::String
                            end end
                        !isnothing(link) && write(buf, "\e]8;;", link, "\e\\")
                        termstyle(buf, face, lastface)
                        string_writer(buf, str)
                        !isnothing(link) && write(buf, "\e]8;;\e\\")
                        lastface = face
                    end
                    termstyle(buf, FACES.default[:default], lastface)
                    write(io, take!(buf))
                elseif s isa AnnotatedString
                    string_writer(io, s.string)
                elseif s isa SubString
                    string_writer(
                        io, SubString(s.string.string, s.offset, s.ncodeunits, Val(:noshift)))
                end
            end
            _mergedface(face::Face) = face
            _mergedface(face::Symbol) = get(FACES.current[], face, Face())
            _mergedface(faces::Vector{Union{Face,Symbol}}) = mapfoldl(_mergedface, merge, Iterators.reverse(faces))
            function getface(faces::Vector{Face})
                isempty(faces) && return FACES.current[][:default]
                combined = faces[1]
                for face in faces[2:end]
                    combined = merge(combined, face)
                end
                if !isempty(combined.inherit)
                    combined = merge(Face(), combined)
                end
                merge(FACES.current[][:default], combined)
            end
            function getface(annotations::Vector{Pair{Symbol, Any}})
                faces = Face[]
                for (key, annot) in annotations
                    if key === :face
                        if annot isa Face
                            push!(faces, annot)
                        elseif annot isa Symbol
                            push!(faces, get(FACES.current[], annot, Face()))
                        elseif annot isa Vector{Union{Face,Symbol}}
                            push!(faces, _mergedface(annot))
                        else @assert false end
                    end
                end
                getface(faces)
            end
            function merge(a::Face, b::Face)
                if isempty(b.inherit)
                    Face(ifelse(isnothing(b.font),          a.font,          b.font),
                         if isnothing(b.height)      a.height
                         elseif isnothing(a.height)  b.height
                         elseif b.height isa Int     b.height
                         elseif a.height isa Int     round(Int, a.height::Int * b.height::Float64)
                         else a.height::Float64 * b.height::Float64 end,
                         ifelse(isnothing(b.weight),        a.weight,        b.weight),
                         ifelse(isnothing(b.slant),         a.slant,         b.slant),
                         ifelse(isnothing(b.foreground),    a.foreground,    b.foreground),
                         ifelse(isnothing(b.background),    a.background,    b.background),
                         ifelse(isnothing(b.underline),     a.underline,     b.underline),
                         ifelse(isnothing(b.strikethrough), a.strikethrough, b.strikethrough),
                         ifelse(isnothing(b.inverse),       a.inverse,       b.inverse),
                         a.inherit)
                else
                    b_noinherit = Face(
                        b.font, b.height, b.weight, b.slant, b.foreground, b.background,
                        b.underline, b.strikethrough, b.inverse, Symbol[])
                    b_inherit = getface(Face[
                        get(FACES.current[], face, Face())
                        # TODO: remove reverse after fixing getface()
                        for face in Iterators.reverse(b.inherit)
                    ])
                    b_resolved = merge(b_inherit, b_noinherit)
                    merge(a, b_resolved)
                end
            end
            function annotatedstring_optimize!(s::AnnotatedString)
                last_seen = IdDict{Pair{Symbol, Any}, Int}()
                i = 1
                while i <= length(s.annotations)
                    region, keyval = s.annotations[i]
                    prev = get(last_seen, keyval, 0)
                    if prev > 0
                        lregion, _ = s.annotations[prev]
                        if last(lregion) + 1 == first(region)
                            s.annotations[prev] =
                                Base.setindex(s.annotations[prev],
                                              first(lregion):last(region),
                                              1)
                            deleteat!(s.annotations, i)
                        else
                            delete!(last_seen, keyval)
                        end
                    else
                        last_seen[keyval] = i
                        i += 1
                    end
                end
                s
            end
        end
    end
end

empty!(Core.ARGS)
empty!(Base.ARGS)
empty!(LOAD_PATH)
empty!(DEPOT_PATH)
empty!(Base.TOML_CACHE.d)
Base.TOML.reinit!(Base.TOML_CACHE.p, "")
Base.ACTIVE_PROJECT[] = nothing
@eval Base begin
    PROGRAM_FILE = ""
end
@eval Sys begin
    BINDIR = ""
    STDLIB = ""
end
