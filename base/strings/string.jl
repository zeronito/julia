# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    StringIndexError(str, i)

An error occurred when trying to access `str` at index `i` that is not valid.
"""
struct StringIndexError <: Exception
    string::AbstractString
    index::Integer
end
@noinline string_index_err(s::AbstractString, i::Integer) =
    throw(StringIndexError(s, Int(i)))
function Base.showerror(io::IO, exc::StringIndexError)
    s = exc.string
    print(io, "StringIndexError: ", "invalid index [$(exc.index)]")
    if firstindex(s) <= exc.index <= ncodeunits(s)
        iprev = thisind(s, exc.index)
        inext = nextind(s, iprev)
        escprev = escape_string(s[iprev:iprev])
        if inext <= ncodeunits(s)
            escnext = escape_string(s[inext:inext])
            print(io, ", valid nearby indices [$iprev]=>'$escprev', [$inext]=>'$escnext'")
        else
            print(io, ", valid nearby index [$iprev]=>'$escprev'")
        end
    end
end

const ByteArray = Union{CodeUnits{UInt8,String}, Vector{UInt8},Vector{Int8}, FastContiguousSubArray{UInt8,1,CodeUnits{UInt8,String}}, FastContiguousSubArray{UInt8,1,Vector{UInt8}}, FastContiguousSubArray{Int8,1,Vector{Int8}}}

@inline between(b::T, lo::T, hi::T) where {T<:Integer} = (lo ≤ b) & (b ≤ hi)

"""
    String <: AbstractString

The default string type in Julia, used by e.g. string literals.

`String`s are immutable sequences of `Char`s. A `String` is stored internally as
a contiguous byte array, and while they are interpreted as being UTF-8 encoded,
they can be composed of any byte sequence. Use [`isvalid`](@ref) to validate
that the underlying byte sequence is valid as UTF-8.
"""
String

## constructors and conversions ##

# String constructor docstring from boot.jl, workaround for #16730
# and the unavailability of @doc in boot.jl context.
"""
    String(v::AbstractVector{UInt8})

Create a new `String` object using the data buffer from byte vector `v`.
If `v` is a `Vector{UInt8}` it will be truncated to zero length and future
modification of `v` cannot affect the contents of the resulting string.
To avoid truncation of `Vector{UInt8}` data, use `String(copy(v))`; for other
`AbstractVector` types, `String(v)` already makes a copy.

When possible, the memory of `v` will be used without copying when the `String`
object is created. This is guaranteed to be the case for byte vectors returned
by [`take!`](@ref) on a writable [`IOBuffer`](@ref) and by calls to
[`read(io, nb)`](@ref). This allows zero-copy conversion of I/O data to strings.
In other cases, `Vector{UInt8}` data may be copied, but `v` is truncated anyway
to guarantee consistent behavior.
"""
String(v::AbstractVector{UInt8}) = String(copyto!(StringVector(length(v)), v))
String(v::Vector{UInt8}) = ccall(:jl_array_to_string, Ref{String}, (Any,), v)

"""
    unsafe_string(p::Ptr{UInt8}, [length::Integer])

Copy a string from the address of a C-style (NUL-terminated) string encoded as UTF-8.
(The pointer can be safely freed afterwards.) If `length` is specified
(the length of the data in bytes), the string does not have to be NUL-terminated.

This function is labeled "unsafe" because it will crash if `p` is not
a valid memory address to data of the requested length.
"""
function unsafe_string(p::Union{Ptr{UInt8},Ptr{Int8}}, len::Integer)
    p == C_NULL && throw(ArgumentError("cannot convert NULL to string"))
    ccall(:jl_pchar_to_string, Ref{String}, (Ptr{UInt8}, Int), p, len)
end
function unsafe_string(p::Union{Ptr{UInt8},Ptr{Int8}})
    p == C_NULL && throw(ArgumentError("cannot convert NULL to string"))
    ccall(:jl_cstr_to_string, Ref{String}, (Ptr{UInt8},), p)
end

# This is @assume_effects :effect_free :nothrow :terminates_globally @ccall jl_alloc_string(n::Csize_t)::Ref{String},
# but the macro is not available at this time in bootstrap, so we write it manually.
@eval _string_n(n::Integer) = $(Expr(:foreigncall, QuoteNode(:jl_alloc_string), Ref{String}, Expr(:call, Expr(:core, :svec), :Csize_t), 1, QuoteNode((:ccall,0xe)), :(convert(Csize_t, n))))

"""
    String(s::AbstractString)

Create a new `String` from an existing `AbstractString`.
"""
String(s::AbstractString) = print_to_string(s)
@assume_effects :total String(s::Symbol) = unsafe_string(unsafe_convert(Ptr{UInt8}, s))

unsafe_wrap(::Type{Vector{UInt8}}, s::String) = ccall(:jl_string_to_array, Ref{Vector{UInt8}}, (Any,), s)

Vector{UInt8}(s::CodeUnits{UInt8,String}) = copyto!(Vector{UInt8}(undef, length(s)), s)
Vector{UInt8}(s::String) = Vector{UInt8}(codeunits(s))
Array{UInt8}(s::String)  = Vector{UInt8}(codeunits(s))

String(s::CodeUnits{UInt8,String}) = s.s

## low-level functions ##

pointer(s::String) = unsafe_convert(Ptr{UInt8}, s)
pointer(s::String, i::Integer) = pointer(s) + Int(i)::Int - 1

ncodeunits(s::String) = Core.sizeof(s)
codeunit(s::String) = UInt8

codeunit(s::String, i::Integer) = codeunit(s, Int(i))
@assume_effects :foldable @inline function codeunit(s::String, i::Int)
    @boundscheck checkbounds(s, i)
    b = GC.@preserve s unsafe_load(pointer(s, i))
    return b
end

## comparison ##

@assume_effects :total _memcmp(a::String, b::String) = @invoke _memcmp(a::Union{Ptr{UInt8},AbstractString},b::Union{Ptr{UInt8},AbstractString})

_memcmp(a::Union{Ptr{UInt8},AbstractString}, b::Union{Ptr{UInt8},AbstractString}) = _memcmp(a, b, min(sizeof(a), sizeof(b)))
function _memcmp(a::Union{Ptr{UInt8},AbstractString}, b::Union{Ptr{UInt8},AbstractString}, len::Int)
    ccall(:memcmp, Cint, (Ptr{UInt8}, Ptr{UInt8}, Csize_t), a, b, len % Csize_t) % Int
end

function cmp(a::String, b::String)
    al, bl = sizeof(a), sizeof(b)
    c = _memcmp(a, b)
    return c < 0 ? -1 : c > 0 ? +1 : cmp(al,bl)
end

==(a::String, b::String) = a===b

typemin(::Type{String}) = ""
typemin(::String) = typemin(String)

##
#=
  ┌─────────────────────────────────────────────────────┐
  │                 Forward Mode State Diagram          │
  │  INCLUSIVE    ┌──────────────2──────────────┐       │
  │    UTF-8      │                             │       │
  │               ├────────3────────┐           │       │
  │   IUTF-8      │                 │           │       │
  │    ┌─0─┐      │     ┌─┐        ┌▼┐         ┌▼┐      │
  │    │   │      ├─4──►│3├───1────►2├────1────►1├────┐ │
  │   ┌▼───┴┐     │     └─┘        └─┘         └─┘    │ │
  │   │  0  ├─────┘   Needs 3    Needs 2     Needs 1  │ │
  │   └───▲─┘        ContBytes  ContBytes   ContBytes │ │
  │       │                                           │ │
  │       │           ContByte=Transition 1           │ │
  │       └─────────────────────1─────────────────────┘ │
  │  ┌─┐                                                │
  │  │4│◄───All undefined transitions result in state 4 │
  │  └─┘      State machine must be reset after state 4 │
  └─────────────────────────────────────────────────────┘

  ┌─────────────────────────────────────────────────────┐
  │                 Reverse Mode State Diagram          │
  │  INCLUSIVE    ┌──◄───────────2:4────────────┐       │
  │    UTF-8      │                             │       │
  │   IUTF-8      ├──◄─────3:4──────┐           │       │
  │               │                 │           │       │
  │  ┌─0,2:4─┐    │     ┌─┐        ┌┴┐         ┌┴┐      │
  │  │       │    ├─4───┤3│◄──1────┤2│◄───1────┤1│◄───┐ │
  │ ┌▼───────┴┐   │     └─┘        └─┘         └─┘    │ │
  │ │    0    │◄──┘   Needs 3    Needs 2     Needs 1  │ │
  │ └─────┬───┘      ContBytes  ContBytes   ContBytes │ │
  │       │                                           │ │
  │       │           ContByte=Transition 1           │ │
  │       └─────────────────────1─────────────────────┘ │
  │  ┌─┐                                                │
  │  │4│◄───All undefined transitions result in state 4 │
  │  └─┘      State machine must be reset after state 4 │
  └─────────────────────────────────────────────────────┘
=#
const _IUTF8State = UInt16
const _IUTF8_SHIFT_MASK = _IUTF8State(0b1111)
const _IUTF8_DFA_ACCEPT = _IUTF8State(0)
const _IUTF8_DFA_INVALID = _IUTF8State(4)

const _IUTF8_DFA_TABLE, _IUTF8_DFA_REVERSE_TABLE = begin
    # It should be noted that even though the invalid state is state 4 the shift is 1
    # which is the second lowest state shift.
    shifts = [0, 13, 6, 10, 4]

    # Both of these state tables are only 4 states wide even though there are 5 states
    # because the machine must be reset once it is in state 4
    forward_state_table  = [    [0,  4,  4,  4],
                                [4,  0,  1,  2],
                                [1,  4,  4,  4],
                                [2,  4,  4,  4],
                                [3,  4,  4,  4],
                                [4,  4,  4,  4] ]

    reverse_state_table  = [    [0,  4,  4,  4],
                                [1,  2,  3,  4],
                                [0,  0,  4,  4],
                                [0,  0,  0,  4],
                                [0,  0,  0,  0],
                                [4,  4,  4,  4] ]


    f(from, to) = _IUTF8State(shifts[to + 1]) << shifts[from + 1]
    r(state_row) = |([f(n - 1, state_row[n]) for n in 1:length(state_row)]...)
    forward_class_rows = [r(forward_state_table[n]) for n in 1:length(forward_state_table)]
    reverse_class_rows = [r(reverse_state_table[n]) for n in 1:length(reverse_state_table)]

    byte_class = [  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    #  0x00:0x0F      00000000:00001111
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    #  0x10:0x1F      00010000:00011111
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    #  0x20:0x2F      00100000:00101111
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    #  0x30:0x3F      00110000:00111111
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    #  0x40:0x4F      01000000:01001111
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    #  0x50:0x5F      01010000:01011111
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    #  0x60:0x6F      01100000:01101111
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    #  0x70:0x7F      01110000:01111111
                    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,    #  0x80:0x8F      10000000:10001111
                    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,    #  0x90:0x9F      10010000:10011111
                    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,    #  0xA0:0xAF      10100000:10101111
                    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,    #  0xB0:0xBF      10110000:10111111
                    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,    #  0xC0:0xCF      11000000:11001111
                    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,    #  0xD0:0xDF      11010000:11011111
                    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,    #  0xE0:0xEF      11100000:11101111
                    4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5  ]  #  0xF0:0xFF      11110000:11111111
    forward_dfa_table = zeros(_IUTF8State, 256)
    reverse_dfa_table = zeros(_IUTF8State, 256)
    for n in 1:256
        forward_dfa_table[n] = forward_class_rows[1 + byte_class[n]]
        reverse_dfa_table[n] = reverse_class_rows[1 + byte_class[n]]
    end
    (forward_dfa_table, reverse_dfa_table)
end
##
@inline function _iutf8_dfa_step(state::_IUTF8State, byte::UInt8)
    @inbounds (_IUTF8_DFA_TABLE[byte + 1] >> state) & _IUTF8_SHIFT_MASK
end

@inline function _iutf8_dfa_reverse_step(state::_IUTF8State, byte::UInt8)
    @inbounds (_IUTF8_DFA_REVERSE_TABLE[byte + 1] >> state) & _IUTF8_SHIFT_MASK
end

## thisind, nextind ##

@propagate_inbounds thisind(s::String, i::Int) = _thisind_str(s, i)

# s should be String or SubString{String}
@inline function _thisind_str(s, i::Int)
    i == 0 && return 0
    n = ncodeunits(s)
    (i == n + 1) | (i == 1) && return i
    @boundscheck Base.between(i, 1, n) || throw(BoundsError(s, i))
    bytes = codeunits(s)
    state = _IUTF8_DFA_ACCEPT
    for j in 0:3
        k = i - j
        state = @inbounds _iutf8_dfa_reverse_step(state, bytes[k])
        (state == _IUTF8_DFA_ACCEPT) && return k
        (state == _IUTF8_DFA_INVALID) | (k <= 1) && return i
    end
    return i # Should never get here
end

@propagate_inbounds nextind(s::String, i::Int) = _nextind_str(s, i)

# s should be String or SubString{String}
@inline function _nextind_str(s, i::Int)
    i == 0 && return 1
    n = ncodeunits(s)
    @boundscheck between(i, 1, n) || throw(BoundsError(s, i))
    bytes = codeunits(s)
    @inbounds l = bytes[i]
    (l < 0x80) | (0xf8 ≤ l) && return i + 1
    if l < 0xc0
        i′ = @inbounds thisind(s, i)
        (i′ >= i) && return i + 1
        i = i′
    end
    state = _IUTF8_DFA_ACCEPT
    for j in 0:3
        k = i + j
        state = @inbounds _iutf8_dfa_step(state, bytes[k])
        (state == _IUTF8_DFA_INVALID) && return k #The screening above makes sure this is never returned when k == i
        (state == _IUTF8_DFA_ACCEPT) | (k >= n) && return k + 1
    end
    return i + 4 # Should never get here
end

## checking UTF-8 & ACSII validity ##

byte_string_classify(s::Union{String,Vector{UInt8},FastContiguousSubArray{UInt8,1,Vector{UInt8}}}) =
    ccall(:u8_isvalid, Int32, (Ptr{UInt8}, Int), s, sizeof(s))
    # 0: neither valid ASCII nor UTF-8
    # 1: valid ASCII
    # 2: valid UTF-8

isvalid(::Type{String}, s::Union{Vector{UInt8},FastContiguousSubArray{UInt8,1,Vector{UInt8}},String}) = byte_string_classify(s) ≠ 0
isvalid(s::String) = isvalid(String, s)

is_valid_continuation(c) = c & 0xc0 == 0x80

## required core functionality ##

@inline function iterate(s::String, i::Int=firstindex(s))
    (i % UInt) - 1 < ncodeunits(s) || return nothing
    b = @inbounds codeunit(s, i)
    u = UInt32(b) << 24
    (b < 0x80) && return reinterpret(Char, u), i + 1
    return iterate_continued(s, i, b, u)
end

function iterate_continued(s::String, i::Int, b::UInt8, u::UInt32)
    n = ncodeunits(s)
    state = _IUTF8_DFA_ACCEPT
    state = _iutf8_dfa_step(state, b)
    k = i
    state <= _IUTF8_DFA_INVALID && @goto ret_kp1
    shift = 24
    for j in 1:3
        k = i + j
        @inbounds b = codeunit(s, k)
        state = _iutf8_dfa_step(state, b)
        state == _IUTF8_DFA_INVALID && @goto ret
        u |= UInt32(b) << (shift -= 8)
        (state == _IUTF8_DFA_ACCEPT) && @goto ret_kp1
        (i >= n) && @goto ret_kp1
    end
    @label ret_kp1
    k += 1
    @label ret
    return reinterpret(Char, u), k
end
##

@propagate_inbounds function getindex(s::String, i::Int)
    b = codeunit(s, i)
    u = UInt32(b) << 24
    #Check u rather than b here because it force compiler to calculate u now
    (u >= 0x80000000) || return reinterpret(Char, u)
    return getindex_continued(s, i, u)
end

function getindex_continued(s::String, i::Int, u::UInt32)
    @inbounds b = codeunit(s,i) #It is faster to refetch b than recalculate u
    n = ncodeunits(s)
    (i == n) && @goto ret
    shift = 24
    state = _iutf8_dfa_step(_IUTF8_DFA_ACCEPT, b)
    if (state == _IUTF8_DFA_INVALID)
        #Checks whether i not at the beginning of a character which is an error
        # or a single invalid byte which returns
        @inbounds isvalid(s, i) && @goto ret
        Base.string_index_err(s, i)
    end
    for j in 1:3
        k = i + j
        @inbounds b = codeunit(s, k)
        state = _iutf8_dfa_step(state, b)
        #If the state machine goes to invalid return value from before byte was processed
        state == _IUTF8_DFA_INVALID && break
        u |= UInt32(b) << (shift -= 8)
        ((state == _IUTF8_DFA_ACCEPT) | (k == n)) && break
    end
    @label ret
    return reinterpret(Char, u)
end

getindex(s::String, r::AbstractUnitRange{<:Integer}) = s[Int(first(r)):Int(last(r))]

@inline function getindex(s::String, r::UnitRange{Int})
    isempty(r) && return ""
    i, j = first(r), last(r)
    @boundscheck begin
        checkbounds(s, r)
        @inbounds isvalid(s, i) || string_index_err(s, i)
        @inbounds isvalid(s, j) || string_index_err(s, j)
    end
    j = nextind(s, j) - 1
    n = j - i + 1
    ss = _string_n(n)
    GC.@preserve s ss unsafe_copyto!(pointer(ss), pointer(s, i), n)
    return ss
end

# nothrow because we know the start and end indices are valid
@assume_effects :nothrow length(s::String) = length_continued(s, 1, ncodeunits(s), ncodeunits(s))

# effects needed because @inbounds
@assume_effects :consistent :effect_free @inline function length(s::String, i::Int, j::Int)
    @boundscheck begin
        0 < i ≤ ncodeunits(s)+1 || throw(BoundsError(s, i))
        0 ≤ j < ncodeunits(s)+1 || throw(BoundsError(s, j))
    end
    j < i && return 0
    @inbounds i, k = thisind(s, i), i
    c = j - i + (i == k)
    @inbounds length_continued(s, i, j, c)
end

const _STRING_LENGTH_CHUNKING_SIZE = 256
@inline function _isascii(code_units::AbstractVector{CU}, first, last) where {CU}
    r = zero(CU)
    for n in first:last
        @inbounds r |= code_units[n]
    end
    return 0 ≤ r < 0x80
end

function _length_nonascii_decrement(
    cu::AbstractVector{UInt8}, first::Int, last::Int, c::Int, state=_IUTF8_DFA_ACCEPT
)
    state = ifelse(state == _IUTF8_DFA_INVALID, _IUTF8_DFA_ACCEPT, state)
    i = ifelse(state == _IUTF8_DFA_ACCEPT, first - 1, first)
    #@inbounds b = codeunit(s, first)
    @inbounds b = cu[first]
    @inbounds while true
        #This logic enables the first state to be >_IUTF8_DFA_INVALID so that a chunk
        # can continue from a previous chunk
        (state == _IUTF8_DFA_ACCEPT) && (i += 1)
        #Logic was taken out of the n=1:3 loop below so we must correct the count here
        (state == _IUTF8_DFA_INVALID) && (c += 1)
        if state <= _IUTF8_DFA_INVALID
            #Loop through all the one byte characters
            while true
                #b = codeunit(s, i)
                b = cu[i]
                ((i += 1) <= last) || break
                0xc0 ≤ b ≤ 0xf7 && break
            end
            state = _iutf8_dfa_step(_IUTF8_DFA_ACCEPT, b)
            (i <= last) || return (c, state)
        end

        #This should get unrolled
        for n in 1:3
            #b = codeunit(s, i)
            b = cu[i]
            state = _iutf8_dfa_step(state, b)
            c -= 1
            state <= _IUTF8_DFA_INVALID && break
            ((i += 1) <= last) || return (c, state)
        end
    end
    return (c, state)
end

function _length_continued_nonascii(
    cu::AbstractVector{UInt8}, first::Int, last::Int, c::Int
)
    chunk_size = _STRING_LENGTH_CHUNKING_SIZE

    start = first
    stop = min(last, first + chunk_size - 1)
    state = _IUTF8_DFA_ACCEPT
    while start <= last
        #First we process a non ascii chunk because we assume the barrier
        # function sent it here for a reason
        (c, state) = _length_nonascii_decrement(cu, start, stop, c, state)
        start = start + chunk_size
        stop = min(last, stop + chunk_size)

        while state <= _IUTF8_DFA_INVALID
            _isascii(cu, start, stop) || break
            (start = start + chunk_size) <= last || break
            stop = min(last, stop + chunk_size)
        end
    end
    return c
end

@inline function length_continued(s::String, first::Int, last::Int, c::Int)
    cu = codeunits(s)
    chunk_size = _STRING_LENGTH_CHUNKING_SIZE
    first < last || return c
    n = last - first + 1
    prologue_bytes = rem(n, chunk_size)
    start = first
    #Prologue to get to chunks to be exact
    _isascii(cu, start, start + prologue_bytes - 1) ||
        return _length_continued_nonascii(cu, start, last, c)
    start += prologue_bytes
    start == last && return c
    for start in start:chunk_size:last
        _isascii(cu, start, start + chunk_size - 1) ||
            return _length_continued_nonascii(cu, start, last, c)
    end
    return c
end

## overload methods for efficiency ##

isvalid(s::String, i::Int) = checkbounds(Bool, s, i) && thisind(s, i) == i

isascii(s::String) = isascii(codeunits(s))

# don't assume effects for general integers since we cannot know their implementation
@assume_effects :foldable repeat(c::Char, r::BitInteger) = @invoke repeat(c::Char, r::Integer)

"""
    repeat(c::AbstractChar, r::Integer) -> String

Repeat a character `r` times. This can equivalently be accomplished by calling
[`c^r`](@ref :^(::Union{AbstractString, AbstractChar}, ::Integer)).

# Examples
```jldoctest
julia> repeat('A', 3)
"AAA"
```
"""
function repeat(c::AbstractChar, r::Integer)
    c = Char(c)::Char
    r == 0 && return ""
    r < 0 && throw(ArgumentError("can't repeat a character $r times"))
    u = bswap(reinterpret(UInt32, c))
    n = 4 - (leading_zeros(u | 0xff) >> 3)
    s = _string_n(n*r)
    p = pointer(s)
    GC.@preserve s if n == 1
        ccall(:memset, Ptr{Cvoid}, (Ptr{UInt8}, Cint, Csize_t), p, u % UInt8, r)
    elseif n == 2
        p16 = reinterpret(Ptr{UInt16}, p)
        for i = 1:r
            unsafe_store!(p16, u % UInt16, i)
        end
    elseif n == 3
        b1 = (u >> 0) % UInt8
        b2 = (u >> 8) % UInt8
        b3 = (u >> 16) % UInt8
        for i = 0:r-1
            unsafe_store!(p, b1, 3i + 1)
            unsafe_store!(p, b2, 3i + 2)
            unsafe_store!(p, b3, 3i + 3)
        end
    elseif n == 4
        p32 = reinterpret(Ptr{UInt32}, p)
        for i = 1:r
            unsafe_store!(p32, u, i)
        end
    end
    return s
end
