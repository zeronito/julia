# This file is a part of Julia. License is MIT: https://julialang.org/license

# Expr head => argument count bounds
const VALID_EXPR_HEADS = Pair{Symbol,UnitRange{Int}}[
    :call => 1:typemax(Int),
    :invoke => 2:typemax(Int),
    :static_parameter => 1:1,
    :line => 1:3,
    :gotoifnot => 2:2,
    :(=) => 2:2,
    :method => 1:4,
    :const => 1:1,
    :null => 0:0, # TODO from @vtjnash: remove this + any :null handling code in Base
    :new => 1:typemax(Int),
    :return => 1:1,
    :the_exception => 0:0,
    :enter => 1:1,
    :leave => 1:1,
    :inbounds => 1:1,
    :boundscheck => 1:1,
    :copyast => 1:1,
    :meta => 0:typemax(Int),
    :global => 1:1,
    :foreigncall => 5:5,
    :isdefined => 1:1
]

function get_expr_narg_bounds(head::Symbol, notfound)
    for pair in VALID_EXPR_HEADS
        first(pair) == head && return last(pair)
    end
    return notfound
end

const ASSIGNED_FLAG = 0x02

# @enum isn't defined yet, otherwise I'd use it for this
const INVALID_EXPR_HEAD = "invalid expression head"
const INVALID_EXPR_NARGS = "invalid number of expression args"
const INVALID_LVALUE = "invalid LHS value"
const INVALID_RVALUE = "invalid RHS value"
const INVALID_CALL_ARG = "invalid :call argument"
const EMPTY_SLOTNAMES = "slotnames field is empty"
const SLOTFLAGS_MISMATCH = "length(slotnames) != length(slotflags)"
const SLOTTYPES_MISMATCH = "length(slotnames) != length(slottypes)"
const SLOTTYPES_MISMATCH_UNINFERRED = "uninferred CodeInfo slottypes field is not `nothing`"
const SSAVALUETYPES_MISMATCH = "not all SSAValues in AST have a type in ssavaluetypes"
const SSAVALUETYPES_MISMATCH_UNINFERRED = "uninferred CodeInfo ssavaluetypes field does not equal the number of present SSAValues"
const INVALID_ASSIGNMENT_SLOTFLAG = "slot has wrong assignment slotflag setting (bit flag 2 not set)"
const SIGNATURE_NARGS_MISMATCH = "number of types in method signature does not match number of arguments"
const SIGNATURE_VARARG_MISMATCH = "number of types in method signature does not match `isva` field setting"
const NON_TOP_LEVEL_METHOD = "encountered `Expr` head `:method` in non-top-level code (i.e. `nargs` > 0)"
const NARGS_MISMATCH = "CodeInfo for method contains fewer slotnames than the number of method arguments"

struct InvalidCodeError <: Exception
    kind::String
    meta::Any
end

InvalidCodeError(kind) = InvalidCodeError(kind, nothing)

"""
    validate_code!(errors::Vector{>:InvalidCodeError}, c::CodeInfo)

Validate `c`, logging any violation by pushing an `InvalidCodeError` into `errors`.
"""
function validate_code!(errors::Vector{>:InvalidCodeError}, c::CodeInfo, is_top_level::Bool = false)
    ssavals = IntSet()
    lhs_slotnums = IntSet()
    walkast(c.code) do x
        if isa(x, Expr)
            !is_top_level && x.head == :method && push!(errors, InvalidCodeError(NON_TOP_LEVEL_METHOD))
            narg_bounds = get_expr_narg_bounds(x.head, -1:-1)
            if narg_bounds == -1:-1
                push!(errors, InvalidCodeError(INVALID_EXPR_HEAD, x.head))
            elseif !in(length(x.args), narg_bounds)
                push!(errors, InvalidCodeError(INVALID_EXPR_NARGS, x.head))
            elseif x.head == :(=)
                lhs, rhs = x.args
                if !is_valid_lvalue(lhs)
                    push!(errors, InvalidCodeError(INVALID_LVALUE, lhs))
                elseif isa(lhs, SlotNumber) && !in(lhs.id, lhs_slotnums)
                    n = lhs.id
                    if isassigned(c.slotflags, n) && !is_flag_set(c.slotflags[n], ASSIGNED_FLAG)
                        push!(errors, InvalidCodeError(INVALID_ASSIGNMENT_SLOTFLAG, lhs))
                    end
                    push!(lhs_slotnums, n)
                end
                if !is_valid_rvalue(rhs)
                    push!(errors, InvalidCodeError(INVALID_RVALUE, rhs))
                end
            elseif x.head == :call || x.head == :invoke
                for arg in x.args
                    if !is_valid_rvalue(arg)
                        push!(errors, InvalidCodeError(INVALID_CALL_ARG, arg))
                    end
                end
            end
        elseif isa(x, SSAValue)
            id = x.id + 1 # ensures that id > 0 for use with IntSet
            !in(id, ssavals) && push!(ssavals, id)
        end
    end
    nslotnames = length(c.slotnames)
    nslotflags = length(c.slotflags)
    nssavals = length(ssavals)
    nslotnames == 0 && push!(errors, InvalidCodeError(EMPTY_SLOTNAMES))
    nslotnames != nslotflags && push!(errors, InvalidCodeError(SLOTFLAGS_MISMATCH, (nslotnames, nslotflags)))
    if c.inferred
        nslottypes = length(c.slottypes)
        nssavaluetypes = length(c.ssavaluetypes)
        nslottypes != nslotnames && push!(errors, InvalidCodeError(SLOTTYPES_MISMATCH, (nslotnames, nslottypes)))
        nssavaluetypes < nssavals && push!(errors, InvalidCodeError(SSAVALUETYPES_MISMATCH, (nssavals, nssavaluetypes)))
    else
        c.slottypes !== nothing && push!(errors, InvalidCodeError(SLOTTYPES_MISMATCH_UNINFERRED, c.slottypes))
        c.ssavaluetypes != nssavals && push!(errors, InvalidCodeError(SSAVALUETYPES_MISMATCH_UNINFERRED, (nssavals, c.ssavaluetypes)))
    end
    return errors
end

"""
    validate_code!(errors::Vector{>:InvalidCodeError}, mi::MethodInstance,
                   c::Union{Void,CodeInfo} = Core.Inference.retrieve_code_info(mi))

Validate `mi`, logging any violation by pushing an `InvalidCodeError` into `errors`.

If `isa(c, CodeInfo)`, also call `validate_code!(errors, c)`. It is assumed that `c` is
the `CodeInfo` instance associated with `mi`.
"""
function validate_code!(errors::Vector{>:InvalidCodeError}, mi::Core.MethodInstance,
                        c::Union{Void,CodeInfo} = Core.Inference.retrieve_code_info(mi))
    m = mi.def::Method
    sig_params = unwrap_unionall(m.sig).parameters
    if length(sig_params) != m.nargs
        push!(errors, InvalidCodeError(SIGNATURE_NARGS_MISMATCH, (length(sig_params), m.nargs)))
    end
    if m.isva && length(sig_params) < (m.nargs - 1)
        push!(errors, InvalidCodeError(SIGNATURE_VARARG_MISMATCH, (last(sig_params), m.isva)))
    end
    if isa(c, CodeInfo)
        m.nargs > length(c.slotnames) && push!(errors, InvalidCodeError(NARGS_MISMATCH))
        validate_code!(errors, c, m.nargs == 0)
    end
    return errors
end

validate_code(args...) = validate_code!(Vector{InvalidCodeError}(), args...)

function walkast(f, stmts::Array)
    for stmt in stmts
        f(stmt)
        isa(stmt, Expr) && walkast(f, stmt.args)
    end
end

is_valid_lvalue(x) = isa(x, SlotNumber) || isa(x, SSAValue) || isa(x, GlobalRef)

function is_valid_rvalue(x)
    isa(x, Expr) && return !in(x.head, (:gotoifnot, :line, :const, :meta))
    return !isa(x, GotoNode) && !isa(x, LabelNode) && !isa(x, LineNumberNode)
end

is_flag_set(byte::UInt8, flag::UInt8) = (byte & flag) == flag
