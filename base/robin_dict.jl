# the load factor after which the dictionary `rehash` happens
const ROBIN_DICT_LOAD_FACTOR = 0.70

"""
    RobinDict([itr])

`RobinDict{K,V}()` constructs a hash table with keys of type `K` and values of type `V`.
Keys are compared with [`isequal`](@ref) and hashed with [`hash`](@ref).
Given a single iterable argument, constructs a [`RobinDict`](@ref) whose key-value pairs
are taken from 2-tuples `(key,value)` generated by the argument.

# Examples
```jldoctest
julia> RobinDict([("A", 1), ("B", 2)])
RobinDict{String,Int64} with 2 entries:
  "B" => 2
  "A" => 1
```

Alternatively, a sequence of pair arguments may be passed.

```jldoctest
julia> RobinDict("A"=>1, "B"=>2)
RobinDict{String,Int64} with 2 entries:
  "B" => 2
  "A" => 1
```
"""
mutable struct RobinDict{K,V} <: AbstractDict{K,V}
    hashes::Vector{UInt32}
    keys::Array{K,1}
    vals::Array{V,1}
    count::Int
    idxfloor::Int
end

function RobinDict{K, V}() where {K, V}
    n = 16
    RobinDict{K, V}(zeros(UInt32, n), Vector{K}(undef, n), Vector{V}(undef, n), 0, 0)
end

function RobinDict{K, V}(d::RobinDict{K, V}) where {K, V}
    RobinDict{K, V}(copy(d.hashes), copy(d.keys), copy(d.vals), d.count, d.idxfloor)
end

function RobinDict{K,V}(kv) where V where K
    h = RobinDict{K,V}()
    for (k,v) in kv
        h[k] = v
    end
    return h
end
RobinDict{K,V}(p::Pair) where {K,V} = setindex!(RobinDict{K,V}(), p.second, p.first)
function RobinDict{K,V}(ps::Pair...) where V where K
    h = RobinDict{K,V}()
    sizehint!(h, length(ps))
    for p in ps
        h[p.first] = p.second
    end
    return h
end

RobinDict() = RobinDict{Any,Any}()
RobinDict(kv::Tuple{}) = RobinDict()
Base.copy(d::RobinDict) = RobinDict(d)
Base.empty(d::RobinDict, ::Type{K}, ::Type{V}) where {K, V} = RobinDict{K, V}()

RobinDict(ps::Pair{K,V}...) where {K,V} = RobinDict{K,V}(ps)
RobinDict(ps::Pair...)                  = RobinDict(ps)

RobinDict(d::AbstractDict{K, V}) where {K, V} = RobinDict{K, V}(d)

function RobinDict(kv)
    try
        return dict_with_eltype((K, V) -> RobinDict{K, V}, kv, eltype(kv))
    catch e
        if !isiterable(typeof(kv)) || !all(x -> isa(x, Union{Tuple,Pair}), kv)
            !all(x->isa(x,Union{Tuple,Pair}),kv)
            throw(ArgumentError("RobinDict(kv): kv needs to be an iterator of tuples or pairs"))
        else
            rethrow(e)
        end
    end
end

hash_key(key) = (hash(key)%UInt32) | 0x80000000
desired_index(hash, sz) = (hash & (sz - 1)) + 1

function calculate_distance(h::RobinDict{K, V}, index) where {K, V}
    @assert isslotfilled(h, index)
    sz = length(h.keys)
    @inbounds index_init = desired_index(h.hashes[index], sz)
    return (index - index_init + sz) & (sz - 1)
end

# insert algorithm
function rh_insert!(h::RobinDict{K, V}, key::K, val::V) where {K, V}
    # table full
    @assert h.count != length(h.keys)

    ckey, cval, chash = key, val, hash_key(key)
    sz = length(h.keys)
    index_init = desired_index(chash, sz)

    index_curr = index_init
    probe_distance = 0
    probe_current = 0
    @inbounds while true
        if (isslotempty(h, index_curr)) || (isslotfilled(h, index_curr) && isequal(h.keys[index_curr], ckey))
            break
        end
        probe_distance = calculate_distance(h, index_curr)

        if probe_current > probe_distance
            h.vals[index_curr], cval = cval, h.vals[index_curr]
            h.keys[index_curr], ckey = ckey, h.keys[index_curr]
            h.hashes[index_curr], chash = chash, h.hashes[index_curr]
            probe_current = probe_distance
        end
        probe_current += 1
        index_curr = (index_curr & (sz - 1)) + 1
    end

    @inbounds if isslotfilled(h, index_curr) && isequal(h.keys[index_curr], ckey)
        h.vals[index_curr] = cval
        return index_curr
    end

    @inbounds if isslotempty(h, index_curr)
        h.count += 1
    end

    @inbounds h.vals[index_curr] = cval
    @inbounds h.keys[index_curr] = ckey
    @inbounds h.hashes[index_curr] = chash

    @assert probe_current >= 0

    if h.idxfloor == 0
        h.idxfloor = index_curr
    else
        h.idxfloor = min(h.idxfloor, index_curr)
    end
    return index_curr
end

function rh_insert_for_rehash!(h_new::RobinDict{K, V}, key::K, val::V, hash::UInt32) where {K, V}
    # table full
    @assert h_new.count != length(h_new.keys)

    ckey, cval, chash = key, val, hash
    sz = length(h_new.keys)
    index_init = desired_index(chash, sz)

    index_curr = index_init
    probe_distance = 0
    probe_current = 0
    @inbounds while true
        if (isslotempty(h_new, index_curr))
            break
        end
        probe_distance = calculate_distance(h_new, index_curr)

        if probe_current > probe_distance
            h_new.vals[index_curr], cval = cval, h_new.vals[index_curr]
            h_new.keys[index_curr], ckey = ckey, h_new.keys[index_curr]
            h_new.hashes[index_curr], chash = chash, h_new.hashes[index_curr]
            probe_current = probe_distance
        end
        probe_current += 1
        index_curr = (index_curr & (sz - 1)) + 1
    end

    @inbounds if isslotempty(h_new, index_curr)
        h_new.count += 1
    end

    @inbounds h_new.vals[index_curr] = cval
    @inbounds h_new.keys[index_curr] = ckey
    @inbounds h_new.hashes[index_curr] = chash

    @assert probe_current >= 0

    if h_new.idxfloor == 0
        h_new.idxfloor = index_curr
    else
        h_new.idxfloor = min(h_new.idxfloor, index_curr)
    end
    return index_curr
end

#rehash! algorithm
function rehash!(h::RobinDict{K,V}, newsz = length(h.keys)) where {K, V}
    oldk = h.keys
    oldv = h.vals
    oldh = h.hashes
    sz = length(oldk)
    newsz = _tablesz(newsz)
    if h.count == 0
        resize!(h.keys, sz)
        resize!(h.vals, sz)
        resize!(h.hashes, newsz)
        fill!(h.hashes, 0)
        h.count = 0
        h.idxfloor = 0
        return h
    end

    h.keys = Vector{K}(undef, newsz)
    h.vals = Vector{V}(undef, newsz)
    h.hashes = zeros(UInt32,newsz)
    h.count = 0
    h.idxfloor = 0

    for i = 1:sz
        @inbounds if oldh[i] != 0
            k = oldk[i]
            v = oldv[i]
            rh_insert_for_rehash!(h, k, v, oldh[i])
        end
    end
    return h
end

function Base.sizehint!(d::RobinDict, newsz)
    newsz = _tablesz(newsz*2)  # *2 for keys and values in same array
    oldsz = length(d.keys)
    # grow at least 25%
    if newsz < (oldsz*5)>>2
        return d
    end
    rehash!(d, newsz)
end

Base.@propagate_inbounds isslotfilled(h::RobinDict, index) = (h.hashes[index] != 0)
Base.@propagate_inbounds isslotempty(h::RobinDict, index) = (h.hashes[index] == 0)


function Base.setindex!(h::RobinDict{K,V}, v0, key0) where {K, V}
    key = convert(K, key0)
    isequal(key, key0) || throw(ArgumentError("$key0 is not a valid key for type $K"))
    _setindex!(h, key, v0)
end

function _setindex!(h::RobinDict{K,V}, key::K, v0) where {K, V}
    v = convert(V, v0)
    sz = length(h.keys)
    (h.count > ROBIN_DICT_LOAD_FACTOR * sz) && rehash!(h, sz<<2)
    index = rh_insert!(h, key, v)
    @assert index > 0
    return h
end

"""
    empty!(collection) -> collection

Remove all elements from a `collection`.

# Examples
```jldoctest
julia> A = RobinDict("a" => 1, "b" => 2)
RobinDict{String,Int64} with 2 entries:
  "b" => 2
  "a" => 1

julia> empty!(A);

julia> A
RobinDict{String,Int64} with 0 entries
```
"""
function Base.empty!(h::RobinDict{K,V}) where {K, V}
    sz = length(h.keys)
    empty!(h.hashes)
    empty!(h.keys)
    empty!(h.vals)
    resize!(h.keys, sz)
    resize!(h.vals, sz)
    resize!(h.hashes, sz)
    fill!(h.hashes, 0)
    h.count = 0
    h.idxfloor = 0
    return h
end

function rh_search(h::RobinDict{K, V}, key) where {K, V}
    sz = length(h.keys)
    chash = hash_key(key)
    index = desired_index(chash, sz)
    cdibs = 0
    @inbounds while true
        if isslotempty(h, index)
            return -1
        elseif cdibs > calculate_distance(h, index)
            return -1
        elseif h.hashes[index] == chash && (h.keys[index] === key || isequal(h.keys[index], key))
            return index
        end
        index = (index & (sz - 1)) + 1
    end
end

"""
    get!(collection, key, default)

Return the value stored for the given key, or if no mapping for the key is present, store
`key => default`, and return `default`.

# Examples
```jldoctest
julia> d = RobinDict("a"=>1, "b"=>2, "c"=>3);

julia> get!(d, "a", 5)
1

julia> get!(d, "d", 4)
4

julia> d
RobinDict{String,Int64} with 4 entries:
  "c" => 3
  "b" => 2
  "a" => 1
  "d" => 4
```
"""
Base.get!(collection, key, default)

Base.get!(h::RobinDict{K,V}, key0, default) where {K,V} = get!(()->default, h, key0)

"""
    get!(f::Function, collection, key)

Return the value stored for the given key, or if no mapping for the key is present, store
`key => f()`, and return `f()`.

This is intended to be called using `do` block syntax:
```julia
get!(dict, key) do
    # default value calculated here
    time()
end
```
"""
Base.get!(f::Function, collection, key)

function Base.get!(default::Callable, h::RobinDict{K,V}, key0::K) where {K, V}
    key = convert(K, key0)
    return _get!(default, h, key)
end

function _get!(default::Callable, h::RobinDict{K,V}, key::K) where V where K
    index = rh_search(h, key)

    index > 0 && return h.vals[index]

    v = convert(V, default())
    rh_insert!(h, key, v)
    return v
end

function Base.getindex(h::RobinDict{K, V}, key) where {K, V}
    index = rh_search(h, key)
    @inbounds return (index < 0) ? throw(KeyError(key)) : h.vals[index]
end

"""
    get(collection, key, default)

Return the value stored for the given key, or the given default value if no mapping for the
key is present.

# Examples
```jldoctest
julia> d = RobinDict("a"=>1, "b"=>2);

julia> get(d, "a", 3)
1

julia> get(d, "c", 3)
3
```
"""
function Base.get(h::RobinDict{K,V}, key, default) where {K, V}
    index = rh_search(h, key)
    @inbounds return (index < 0) ? default : h.vals[index]::V
end

"""
    get(f::Function, collection, key)

Return the value stored for the given key, or if no mapping for the key is present, return
`f()`.  Use [`get!`](@ref) to also store the default value in the dictionary.

This is intended to be called using `do` block syntax

```julia
get(dict, key) do
    # default value calculated here
    time()
end
```
"""
function Base.get(default::Callable, h::RobinDict{K,V}, key) where {K, V}
    index = rh_search(h, key)
    @inbounds return (index < 0) ? default() : h.vals[index]::V
end

"""
    haskey(collection, key) -> Bool

Determine whether a collection has a mapping for a given `key`.

# Examples
```jldoctest
julia> D = RobinDict('a'=>2, 'b'=>3)
RobinDict{Char,Int64} with 2 entries:
  'a' => 2
  'b' => 3

julia> haskey(D, 'a')
true

julia> haskey(D, 'c')
false
```
"""
Base.haskey(h::RobinDict, key) = (rh_search(h, key) > 0)
Base.in(key, v::KeySet{<:Any, <:RobinDict}) = (rh_search(v.dict, key) >= 0)

"""
    getkey(collection, key, default)

Return the key matching argument `key` if one exists in `collection`, otherwise return `default`.

# Examples
```jldoctest
julia> D = RobinDict('a'=>2, 'b'=>3)
RobinDict{Char,Int64} with 2 entries:
  'a' => 2
  'b' => 3

julia> getkey(D, 'a', 1)
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)

julia> getkey(D, 'd', 'a')
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)
```
"""
function Base.getkey(h::RobinDict{K,V}, key, default) where {K, V}
    index = rh_search(h, key)
    @inbounds return (index < 0) ? default : h.keys[index]::K
end

# backward shift deletion by not keeping any tombstones
function rh_delete!(h::RobinDict{K, V}, index) where {K, V}
    @assert index > 0

    # this assumes that there is a key/value present in the dictionary at index
    index0 = index
    sz = length(h.keys)
    @inbounds while true
        index0 = (index0 & (sz - 1)) + 1
        if isslotempty(h, index0) || calculate_distance(h, index0) == 0
            break
        end
    end
    #index0 represents the position before which we have to shift backwards

    # the backwards shifting algorithm
    curr = index
    next = (index & (sz - 1)) + 1

    @inbounds while next != index0
        h.vals[curr] = h.vals[next]
        h.keys[curr] = h.keys[next]
        h.hashes[curr] = h.hashes[next]
        curr = next
        next = (next & (sz-1)) + 1
    end

    #curr is at the last position, reset back to normal
    isbitstype(K) || isbitsunion(K) || ccall(:jl_arrayunset, Cvoid, (Any, UInt), h.keys, curr-1)
    isbitstype(V) || isbitsunion(V) || ccall(:jl_arrayunset, Cvoid, (Any, UInt), h.vals, curr-1)
    @inbounds h.hashes[curr] = 0x0

    h.count -= 1
    # this is necessary because key at idxfloor might get deleted
    h.idxfloor = get_next_filled(h, h.idxfloor)
    return h
end

function _pop!(h::RobinDict, index)
    @inbounds val = h.vals[index]
    rh_delete!(h, index)
    return val
end

function Base.pop!(h::RobinDict{K, V}, key0) where {K, V}
    key = convert(K, key0)
    index = rh_search(h, key)
    return index > 0 ? _pop!(h, index) : throw(KeyError(key))
end

"""
    pop!(collection, key[, default])

Delete and return the mapping for `key` if it exists in `collection`, otherwise return
`default`, or throw an error if `default` is not specified.

# Examples
```jldoctest
julia> d = RobinDict("a"=>1, "b"=>2, "c"=>3);

julia> pop!(d, "a")
1

julia> pop!(d, "d")
ERROR: KeyError: key "d" not found
Stacktrace:
[...]

julia> pop!(d, "e", 4)
4
```
"""
function Base.pop!(h::RobinDict{K, V}, key0, default) where {K, V}
    key = convert(K, key0)
    index = rh_search(h, key)
    return index > 0 ? _pop!(h, index) : default
end

function Base.pop!(h::RobinDict)
    isempty(h) && throw(ArgumentError("dict must be non-empty"))
    idx = h.idxfloor
    @inbounds key = h.keys[idx]
    @inbounds val = h.vals[idx]
    rh_delete!(h, idx)
    return key => val
end

"""
    delete!(collection, key)

Delete the mapping for the given key in a collection, and return the collection.

# Examples
```jldoctest
julia> d = RobinDict("a"=>1, "b"=>2)
RobinDict{String,Int64} with 2 entries:
  "b" => 2
  "a" => 1

julia> delete!(d, "b")
RobinDict{String,Int64} with 1 entry:
  "a" => 1
```
"""
function Base.delete!(h::RobinDict{K, V}, key0) where {K, V}
    key = convert(K, key0)
    index = rh_search(h, key)
    if index > 0
        rh_delete!(h, index)
    end
    return h
end

function get_next_filled(h::RobinDict, i)
    L = length(h.keys)
    (1 <= i <= L) || return 0
    for j = i:L
        @inbounds if isslotfilled(h, j)
            return  j
        end
    end
    return 0
end

Base.@propagate_inbounds _iterate(t::RobinDict{K,V}, i) where {K,V} = i == 0 ? nothing : (Pair{K,V}(t.keys[i],t.vals[i]), i == typemax(Int) ? 0 : get_next_filled(t, i+1))
Base.@propagate_inbounds function Base.iterate(t::RobinDict)
    _iterate(t, t.idxfloor)
end
Base.@propagate_inbounds Base.iterate(t::RobinDict, i) = _iterate(t, get_next_filled(t, i))

function _merge_kvtypes(d, others...)
    K, V = keytype(d), valtype(d)
    for other in others
        K = promote_type(K, keytype(other))
        V = promote_type(V, valtype(other))
    end
    return (K, V)
end

function Base.merge(d::RobinDict, others::AbstractDict...)
    K, V = _merge_kvtypes(d, others...)
    merge!(RobinDict{K,V}(), d, others...)
end

function Base.merge(combine::Function, d::RobinDict, others::AbstractDict...)
    K, V = _merge_kvtypes(d, others...)
    merge!(combine, RobinDict{K,V}(), d, others...)
end
