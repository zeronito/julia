# This file is a part of Julia. License is MIT: https://julialang.org/license

## boolean conversions ##

convert(::Type{Bool}, x::Bool) = x
convert(::Type{Bool}, x::Float16) = x==0 ? false : x==1 ? true : throw(InexactError(:convert, Bool, x))
convert(::Type{Bool}, x::Real) = x==0 ? false : x==1 ? true : throw(InexactError(:convert, Bool, x))

# promote Bool to any other numeric type
promote_rule(::Type{Bool}, ::Type{T}) where {T<:Number} = T

typemin(::Type{Bool}) = false
typemax(::Type{Bool}) = true

## boolean operations ##

"""
    !(x)

Boolean not.

# Examples
```jldoctest
julia> !true
false

julia> !false
true

julia> .![true false true]
1×3 BitArray{2}:
 false  true  false
```
"""
function !(x::Bool)
    ## We need a better heuristic to detect this automatically
    @_pure_meta
    return not_int(x)
end

bitnot(x::Bool) = !x
bitand(x::Bool, y::Bool) = and_int(x, y)
bitor(x::Bool, y::Bool) = or_int(x, y)
bitxor(x::Bool, y::Bool) = (x != y)
(&)(x::Bool, y::Bool) = and_int(x, y)
(|)(x::Bool, y::Bool) = or_int(x, y)

"""
    xor(x, y)

Logical exclusive or of `x` and `y`.

# Examples
```jldoctest
julia> xor(true, false)
true

julia> xor(false, true)
true

julia> xor(true, true)
false

julia> xor(false, false)
false
```
"""
xor(x::Bool, y::Bool) = (x != y)


>>(x::Bool, c::Unsigned) = Int(x) >> c
<<(x::Bool, c::Unsigned) = Int(x) << c
>>>(x::Bool, c::Unsigned) = Int(x) >>> c

>>(x::Bool, c::Int) = Int(x) >> c
<<(x::Bool, c::Int) = Int(x) << c
>>>(x::Bool, c::Int) = Int(x) >>> c

>>(x::Bool, c::Integer) = Int(x) >> c
<<(x::Bool, c::Integer) = Int(x) << c
>>>(x::Bool, c::Integer) = Int(x) >>> c

signbit(x::Bool) = false
sign(x::Bool) = x
abs(x::Bool) = x
abs2(x::Bool) = x
iszero(x::Bool) = !x
isone(x::Bool) = x

<(x::Bool, y::Bool) = y&!x
<=(x::Bool, y::Bool) = y|!x

## do arithmetic as Int ##

+(x::Bool) =  Int(x)
-(x::Bool) = -Int(x)

+(x::Bool, y::Bool) = Int(x) + Int(y)
-(x::Bool, y::Bool) = Int(x) - Int(y)
*(x::Bool, y::Bool) = x & y
^(x::Bool, y::Bool) = x | !y
^(x::Integer, y::Bool) = ifelse(y, x, one(x))

# preserve -0.0 in `false + -0.0`
function +(x::Bool, y::T)::promote_type(Bool,T) where T<:AbstractFloat
    return ifelse(x, oneunit(y) + y, y)
end
+(y::AbstractFloat, x::Bool) = x + y

# make `false` a "strong zero": false*NaN == 0.0
function *(x::Bool, y::T)::promote_type(Bool,T) where T<:AbstractFloat
    return ifelse(x, y, copysign(zero(y), y))
end
*(y::AbstractFloat, x::Bool) = x * y

div(x::Bool, y::Bool) = y ? x : throw(DivideError())
fld(x::Bool, y::Bool) = div(x,y)
cld(x::Bool, y::Bool) = div(x,y)
rem(x::Bool, y::Bool) = y ? false : throw(DivideError())
mod(x::Bool, y::Bool) = rem(x,y)
