# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Tools for collecting and manipulating stack traces. Mainly used for building errors.
"""
module StackTraces


import Base: hash, ==, show
import Core: CodeInfo, MethodInstance

export StackTrace, StackFrame, stacktrace

"""
    StackFrame

Stack information representing execution context, with the following fields:

- `func::Symbol`

  The name of the function containing the execution context.

- `linfo::Union{Core.MethodInstance, CodeInfo, Nothing}`

  The MethodInstance containing the execution context (if it could be found).

- `file::Symbol`

  The path to the file containing the execution context.

- `line::Int`

  The line number in the file containing the execution context.

- `from_c::Bool`

  True if the code is from C.

- `inlined::Bool`

  True if the code is from an inlined frame.

- `pointer::UInt64`

  Representation of the pointer to the execution context as returned by `backtrace`.

"""
struct StackFrame # this type should be kept platform-agnostic so that profiles can be dumped on one machine and read on another
    "the name of the function containing the execution context"
    func::Symbol
    "the path to the file containing the execution context"
    file::Symbol
    "the line number in the file containing the execution context"
    line::Int
    "the MethodInstance or CodeInfo containing the execution context (if it could be found)"
    linfo::Union{MethodInstance, CodeInfo, Nothing}
    "true if the code is from C"
    from_c::Bool
    "true if the code is from an inlined frame"
    inlined::Bool
    "representation of the pointer to the execution context as returned by `backtrace`"
    pointer::UInt64  # Large enough to be read losslessly on 32- and 64-bit machines.
end

StackFrame(func, file, line) = StackFrame(Symbol(func), Symbol(file), line,
                                          nothing, false, false, 0)

"""
    StackTrace

An alias for `Vector{StackFrame}` provided for convenience; returned by calls to
`stacktrace`.
"""
const StackTrace = Vector{StackFrame}

const empty_sym = Symbol("")
const UNKNOWN = StackFrame(empty_sym, empty_sym, -1, nothing, true, false, 0) # === lookup(C_NULL)


#=
If the StackFrame has function and line information, we consider two of them the same if
they share the same function/line information.
=#
function ==(a::StackFrame, b::StackFrame)
    return a.line == b.line && a.from_c == b.from_c && a.func == b.func && a.file == b.file && a.inlined == b.inlined # excluding linfo and pointer
end

function hash(frame::StackFrame, h::UInt)
    h += 0xf4fbda67fe20ce88 % UInt
    h = hash(frame.line, h)
    h = hash(frame.file, h)
    h = hash(frame.func, h)
    h = hash(frame.from_c, h)
    h = hash(frame.inlined, h)
    return h
end


"""
    lookup(pointer::Ptr{Cvoid}) -> Vector{StackFrame}

Given a pointer to an execution context (usually generated by a call to `backtrace`), looks
up stack frame context information. Returns an array of frame information for all functions
inlined at that point, innermost function first.
"""
function lookup(pointer::Ptr{Cvoid})
    infos = ccall(:jl_lookup_code_address, Any, (Ptr{Cvoid}, Cint), pointer, false)::Core.SimpleVector
    pointer = convert(UInt64, pointer)
    isempty(infos) && return [StackFrame(empty_sym, empty_sym, -1, nothing, true, false, pointer)] # this is equal to UNKNOWN
    res = Vector{StackFrame}(undef, length(infos))
    for i in 1:length(infos)
        info = infos[i]::Core.SimpleVector
        @assert(length(info) == 6)
        res[i] = StackFrame(info[1], info[2], info[3], info[4], info[5], info[6], pointer)
    end
    return res
end

const top_level_scope_sym = Symbol("top-level scope")

function lookup(ip::Base.InterpreterIP)
    code = ip.code
    if code === nothing
        # interpreted top-level expression with no CodeInfo
        return [StackFrame(top_level_scope_sym, empty_sym, 0, nothing, false, false, 0)]
    end
    codeinfo = (code isa MethodInstance ? code.uninferred : code)::CodeInfo
    # prepare approximate code info
    if code isa MethodInstance && code.def isa Method
        func = code.def.name
        file = code.def.file
        line = code.def.line
    else
        func = top_level_scope_sym
        file = empty_sym
        line = 0
    end
    i = max(ip.stmt+1, 1)  # ip.stmt is 0-indexed
    if i > length(codeinfo.codelocs) || codeinfo.codelocs[i] == 0
        return [StackFrame(func, file, line, code, false, false, 0)]
    end
    lineinfo = codeinfo.linetable[codeinfo.codelocs[i]]
    scopes = StackFrame[]
    while true
        inlined = lineinfo.inlined_at != 0
        push!(scopes, StackFrame(lineinfo.method, lineinfo.file, lineinfo.line, inlined ? nothing : code, false, inlined, 0))
        inlined || break
        lineinfo = codeinfo.linetable[lineinfo.inlined_at]
    end
    return scopes
end

"""
    stacktrace([trace::Vector{Ptr{Cvoid}},] [c_funcs::Bool=false]) -> StackTrace

Returns a stack trace in the form of a vector of `StackFrame`s. (By default stacktrace
doesn't return C functions, but this can be enabled.) When called without specifying a
trace, `stacktrace` first calls `backtrace`.
"""
function stacktrace(trace::Vector{<:Union{Base.InterpreterIP,Ptr{Cvoid}}}, c_funcs::Bool=false)
    stack = StackTrace()
    for ip in trace
        for frame in lookup(ip)
            # Skip frames that come from C calls.
            if c_funcs || !frame.from_c
                push!(stack, frame)
            end
        end
    end
    return stack
end

function stacktrace(c_funcs::Bool=false)
    stack = stacktrace(backtrace(), c_funcs)
    # Remove frame for this function (and any functions called by this function).
    remove_frames!(stack, :stacktrace)
    # also remove all of the non-Julia functions that led up to this point (if that list is non-empty)
    c_funcs && deleteat!(stack, 1:(something(findfirst(frame -> !frame.from_c, stack), 1) - 1))
    return stack
end

"""
    remove_frames!(stack::StackTrace, name::Symbol)

Takes a `StackTrace` (a vector of `StackFrames`) and a function name (a `Symbol`) and
removes the `StackFrame` specified by the function name from the `StackTrace` (also removing
all frames above the specified function). Primarily used to remove `StackTraces` functions
from the `StackTrace` prior to returning it.
"""
function remove_frames!(stack::StackTrace, name::Symbol)
    deleteat!(stack, 1:something(findlast(frame -> frame.func == name, stack), 0))
    return stack
end

function remove_frames!(stack::StackTrace, names::Vector{Symbol})
    deleteat!(stack, 1:something(findlast(frame -> frame.func in names, stack), 0))
    return stack
end

"""
    remove_frames!(stack::StackTrace, m::Module)

Returns the `StackTrace` with all `StackFrame`s from the provided `Module` removed.
"""
function remove_frames!(stack::StackTrace, m::Module)
    filter!(f -> !from(f, m), stack)
    return stack
end

is_top_level_frame(f::StackFrame) = f.linfo isa CodeInfo || (f.linfo === nothing && f.func === top_level_scope_sym)

function show_spec_linfo(io::IO, frame::StackFrame)
    linfo = frame.linfo
    if linfo === nothing
        if frame.func === empty_sym
            print(io, "ip:0x", string(frame.pointer, base=16))
        elseif frame.func === top_level_scope_sym
            print(io, "top-level scope")
        else
            Base.print_within_stacktrace(io, Base.demangle_function_name(string(frame.func)), bold=true)
        end
    elseif linfo isa MethodInstance
        def = linfo.def
        if isa(def, Method)
            sig = linfo.specTypes
            argnames = Base.method_argnames(def)
            if def.nkw > 0
                # rearrange call kw_impl(kw_args..., func, pos_args...) to func(pos_args...)
                kwarg_types = Any[ fieldtype(sig, i) for i = 2:(1+def.nkw) ]
                uw = Base.unwrap_unionall(sig)::DataType
                pos_sig = Base.rewrap_unionall(Tuple{uw.parameters[(def.nkw+2):end]...}, sig)
                kwnames = argnames[2:(def.nkw+1)]
                for i = 1:length(kwnames)
                    str = string(kwnames[i])::String
                    if endswith(str, "...")
                        kwnames[i] = Symbol(str[1:end-3])
                    end
                end
                Base.show_tuple_as_call(io, def.name, pos_sig, true, zip(kwnames, kwarg_types), argnames[def.nkw+2:end])
            else
                Base.show_tuple_as_call(io, def.name, sig, true, nothing, argnames)
            end
        else
            Base.show_mi(io, linfo, true)
        end
    elseif linfo isa CodeInfo
        print(io, "top-level scope")
    end
end

function show(io::IO, frame::StackFrame)
    show_spec_linfo(io, frame)
    if frame.file !== empty_sym
        file_info = basename(string(frame.file))
        print(io, " at ")
        print(io, file_info, ":")
        if frame.line >= 0
            print(io, frame.line)
        else
            print(io, "?")
        end
    end
    if frame.inlined
        print(io, " [inlined]")
    end
end

function Base.parentmodule(frame::StackFrame)
    if frame.linfo isa MethodInstance
        def = frame.linfo.def
        if def isa Module
            return def
        else
            return (def::Method).module
        end
    else
        # The module is not always available (common reasons include inlined
        # frames and frames arising from the interpreter)
        nothing
    end
end

"""
    from(frame::StackFrame, filter_mod::Module) -> Bool

Returns whether the `frame` is from the provided `Module`
"""
function from(frame::StackFrame, m::Module)
    return parentmodule(frame) === m
end

end
