# This file is a part of Julia. License is MIT: http://julialang.org/license

# Pair
p = Pair(1,2)
@test p == (1=>2)
@test isequal(p,1=>2)
@test start(p) == 1
@test next(p, 1) == (1,2)
@test !done(p, 1)
@test !done(p,2)
@test done(p,3)
@test !done(p,0)
@test endof(p) == length(p) == 2
@test Base.indexed_next(p, 1, (1,2)) == (1,2)
@test Base.indexed_next(p, 2, (1,2)) == (2,3)
@test (1=>2) < (2=>3)
@test (2=>2) < (2=>3)
@test !((2=>3) < (2=>3))
@test (2=>3) < (4=>3)
@test (1=>100) < (4=>1)
@test p[1] == 1
@test p[2] == 2
@test_throws BoundsError p[3]
@test_throws BoundsError p[false]
@test p[true] == 1
@test p[2.0] == 2
@test p[0x01] == 1
@test_throws InexactError p[2.3]

# Dict
h = Dict()
for i=1:10000
    h[i] = i+1
end
for i=1:10000
    @test (h[i] == i+1)
end
for i=1:2:10000
    delete!(h, i)
end
for i=1:2:10000
    h[i] = i+1
end
for i=1:10000
    @test (h[i] == i+1)
end
for i=1:10000
    delete!(h, i)
end
@test isempty(h)
h[77] = 100
@test h[77]==100
for i=1:10000
    h[i] = i+1
end
for i=1:2:10000
    delete!(h, i)
end
for i=10001:20000
    h[i] = i+1
end
for i=2:2:10000
    @test h[i]==i+1
end
for i=10000:20000
    @test h[i]==i+1
end
h = Dict{Any,Any}("a" => 3)
@test h["a"] == 3
h["a","b"] = 4
@test h["a","b"] == h[("a","b")] == 4
h["a","b","c"] = 4
@test h["a","b","c"] == h[("a","b","c")] == 4

# Test eltype, keytype and valtype
@test eltype(h) == Pair{Any,Any}
@test keytype(h) == Any
@test valtype(h) == Any

let
    td = Dict{AbstractString,Float64}()
    @test eltype(td) == Pair{AbstractString,Float64}
    @test keytype(td) == AbstractString
    @test valtype(td) == Float64
    @test keytype(Dict{AbstractString,Float64}) === AbstractString
    @test valtype(Dict{AbstractString,Float64}) === Float64
end

let
    z = Dict()
    get_KeyError = false
    try
        z["a"]
    catch _e123_
        get_KeyError = isa(_e123_,KeyError)
    end
    @test get_KeyError
end

_d = Dict("a"=>0)
@test isa([k for k in filter(x->length(x)==1, collect(keys(_d)))], Vector{Any})

let
    d = Dict(((1, 2), (3, 4)))
    @test d[1] === 2
    @test d[3] === 4
    d2 = Dict(1 => 2, 3 => 4)
    d3 = Dict((1 => 2, 3 => 4))
    @test d == d2 == d3
    @test typeof(d) == typeof(d2) == typeof(d3) == Dict{Int,Int}

    d = Dict(((1, 2), (3, "b")))
    @test d[1] === 2
    @test d[3] == "b"
    d2 = Dict(1 => 2, 3 => "b")
    d3 = Dict((1 => 2, 3 => "b"))
    @test d == d2 == d3
    @test typeof(d) == typeof(d2) == typeof(d3) == Dict{Int,Any}

    d = Dict(((1, 2), ("a", 4)))
    @test d[1] === 2
    @test d["a"] === 4
    d2 = Dict(1 => 2, "a" => 4)
    d3 = Dict((1 => 2, "a" => 4))
    @test d == d2 == d3
    @test typeof(d) == typeof(d2) == typeof(d3) == Dict{Any,Int}

    d = Dict(((1, 2), ("a", "b")))
    @test d[1] === 2
    @test d["a"] == "b"
    d2 = Dict(1 => 2, "a" => "b")
    d3 = Dict((1 => 2, "a" => "b"))
    @test d == d2 == d3
    @test typeof(d) == typeof(d2) == typeof(d3) == Dict{Any,Any}
end

@test_throws ArgumentError first(Dict())
@test first(Dict(:f=>2)) == (:f=>2)

# issue #1821
let
    d = Dict{UTF8String, Vector{Int}}()
    d["a"] = [1, 2]
    @test_throws MethodError d["b"] = 1
    @test isa(repr(d), AbstractString)  # check that printable without error
end

# issue #2344
let
    local bar
    bestkey(d, key) = key
    bestkey{K<:AbstractString,V}(d::Associative{K,V}, key) = string(key)
    bar(x) = bestkey(x, :y)
    @test bar(Dict(:x => [1,2,5])) == :y
    @test bar(Dict("x" => [1,2,5])) == "y"
end

# issue #1438
type I1438T
    id
end
import Base.hash
hash(x::I1438T, h::UInt) = hash(x.id, h)

let
    local seq, xs, s
    seq = [26,28,29,30,31,32,33,34,35,36,-32,-35,-34,-28,37,38,39,40,-30,
           -31,41,42,43,44,-33,-36,45,46,47,48,-37,-38,49,50,51,52,-46,-50,53]
    xs = [ I1438T(id) for id=1:53 ]
    s = Set()
    for id in seq
        if id > 0
            x = xs[id]
            push!(s, x)
            @test in(x, s)                 # check that x can be found
        else
            delete!(s, xs[-id])
        end
    end
end

@test  isequal(Dict(), Dict())
@test  isequal(Dict(1 => 1), Dict(1 => 1))
@test !isequal(Dict(1 => 1), Dict())
@test !isequal(Dict(1 => 1), Dict(1 => 2))
@test !isequal(Dict(1 => 1), Dict(2 => 1))

# Generate some data to populate dicts to be compared
data_in = [ (rand(1:1000), randstring(2)) for _ in 1:1001 ]

# Populate the first dict
d1 = Dict{Int, AbstractString}()
for (k,v) in data_in
    d1[k] = v
end
data_in = collect(d1)
# shuffle the data
for i in 1:length(data_in)
    j = rand(1:length(data_in))
    data_in[i], data_in[j] = data_in[j], data_in[i]
end
# Inserting data in different (shuffled) order should result in
# equivalent dict.
d2 = Dict{Int, AbstractString}()
for (k,v) in data_in
    d2[k] = v
end

@test  isequal(d1, d2)
d3 = copy(d2)
d4 = copy(d2)
# Removing an item gives different dict
delete!(d1, data_in[rand(1:length(data_in))][1])
@test !isequal(d1, d2)
# Changing a value gives different dict
d3[data_in[rand(1:length(data_in))][1]] = randstring(3)
!isequal(d1, d3)
# Adding a pair gives different dict
d4[1001] = randstring(3)
@test !isequal(d1, d4)

@test isequal(Dict(), sizehint!(Dict(),96))

# Here is what currently happens when dictionaries of different types
# are compared. This is not necessarily desirable. These tests are
# descriptive rather than proscriptive.
@test !isequal(Dict(1 => 2), Dict("dog" => "bone"))
@test isequal(Dict{Int,Int}(), Dict{AbstractString,AbstractString}())

# get! (get with default values assigned to the given location)

let f(x) = x^2, d = Dict(8=>19)

    @test get!(d, 8, 5) == 19
    @test get!(d, 19, 2) == 2

    @test get!(d, 42) do  # d is updated with f(2)
        f(2)
    end == 4

    @test get!(d, 42) do  # d is not updated
        f(200)
    end == 4

    @test get(d, 13) do   # d is not updated
        f(4)
    end == 16

    @test d == Dict(8=>19, 19=>2, 42=>4)
end

# show
for d in (Dict("\n" => "\n", "1" => "\n", "\n" => "2"),
          [string(i) => i for i = 1:30],
          [reshape(1:i^2,i,i) => reshape(1:i^2,i,i) for i = 1:24],
          [utf8(Char['α':'α'+i;]) => utf8(Char['α':'α'+i;]) for i = (1:10)*10],
          Dict("key" => zeros(0, 0)))
    for cols in (12, 40, 80), rows in (2, 10, 24)
        # Ensure output is limited as requested
        s = IOBuffer()
        io = Base.IOContext(Base.IOContext(s, :limit_output => true), :displaysize => (rows, cols))
        Base.showdict(io, d)
        out = split(takebuf_string(s),'\n')
        for line in out[2:end]
            @test strwidth(line) <= cols
        end
        @test length(out) <= rows

        for f in (keys, values)
            s = IOBuffer()
            io = Base.IOContext(Base.IOContext(s, :limit_output => true), :displaysize => (rows, cols))
            Base.showkv(io, f(d))
            out = split(takebuf_string(s),'\n')
            for line in out[2:end]
                @test strwidth(line) <= cols
            end
            @test length(out) <= rows
        end
    end
    # Simply ensure these do not throw errors
    Base.showdict(IOBuffer(), d)
    @test !isempty(summary(d))
    @test !isempty(summary(keys(d)))
    @test !isempty(summary(values(d)))
end

# issue #9463
type Alpha end
Base.show(io::IO, ::Alpha) = print(io,"α")
let sbuff = IOBuffer(),
    io = Base.IOContext(Base.IOContext(sbuff, :limit_output => true), :displaysize => (10, 20))

    Base.showdict(io, Dict(Alpha()=>1))
    @test !contains(bytestring(sbuff), "…")
    @test endswith(bytestring(sbuff), "α => 1")
end

# issue #2540
let d = Dict{Any,Any}([x => 1 for x in ['a', 'b', 'c']])
    @test d == Dict('a'=>1, 'b'=>1, 'c'=> 1)
end

# issue #2629
let d = Dict{AbstractString,AbstractString}([ a => "foo" for a in ["a","b","c"]])
    @test d == Dict("a"=>"foo","b"=>"foo","c"=>"foo")
end

# issue #5886
d5886 = Dict()
for k5886 in 1:11
   d5886[k5886] = 1
end
for k5886 in keys(d5886)
   # undefined ref if not fixed
   d5886[k5886] += 1
end

# issue #8877
let
    a = Dict("foo"  => 0.0, "bar" => 42.0)
    b = Dict("フー" => 17, "バー" => 4711)
    @test is(typeof(merge(a, b)), Dict{UTF8String,Float64})
end

# issue 9295
let
    d = Dict()
    @test is(push!(d, 'a' => 1), d)
    @test d['a'] == 1
    @test is(push!(d, 'b' => 2, 'c' => 3), d)
    @test d['b'] == 2
    @test d['c'] == 3
    @test is(push!(d, 'd' => 4, 'e' => 5, 'f' => 6), d)
    @test d['d'] == 4
    @test d['e'] == 5
    @test d['f'] == 6
    @test length(d) == 6
end

# issue #10647
type T10647{T}; x::T; end
let
    a = ObjectIdDict()
    a[1] = a
    a[a] = 2
    a[3] = T10647(a)
    @test a == a
    show(IOBuffer(), a)
    Base.show(Base.IOContext(IOBuffer(), :limit_output => true), a)
    Base.showdict(IOBuffer(), a)
    Base.showdict(Base.IOContext(IOBuffer(), :limit_output => true), a)
end


# Issue #7944
let d = Dict{Int,Int}()
    get!(d, 0) do
        d[0] = 1
    end
    @test length(d) == 1
end

# iteration
d = Dict('a'=>1, 'b'=>1, 'c'=> 3)
@test [d[k] for k in keys(d)] == [d[k] for k in eachindex(d)] ==
      [v for (k, v) in d] == [d[x[1]] for (i, x) in enumerate(d)]


# Issue 12451
@test_throws ArgumentError Dict(0)
@test_throws ArgumentError Dict([1])
@test_throws ArgumentError Dict([(1,2),0])

# ImmutableDict
import Base.ImmutableDict
let d = ImmutableDict{UTF8String, UTF8String}(),
    k1 = UTF8String("key1"),
    k2 = UTF8String("key2"),
    v1 = UTF8String("value1"),
    v2 = UTF8String("value2"),
    d1 = ImmutableDict(d, k1 => v1),
    d2 = ImmutableDict(d1, k2 => v2),
    d3 = ImmutableDict(d2, k1 => v2),
    d4 = ImmutableDict(d3, k2 => v1),
    dnan = ImmutableDict{UTF8String, Float64}(k2, NaN),
    dnum = ImmutableDict(dnan, k2 => 1)

    @test isempty(collect(d))
    @test !isempty(collect(d1))
    @test isempty(d)
    @test !isempty(d1)
    @test length(d) == 0
    @test length(d1) == 1
    @test length(d2) == 2
    @test length(d3) == 3
    @test length(d4) == 4
    @test !(k1 in keys(d))
    @test k1 in keys(d1)
    @test k1 in keys(d2)
    @test k1 in keys(d3)
    @test k1 in keys(d4)

    @test !haskey(d, k1)
    @test haskey(d1, k1)
    @test haskey(d2, k1)
    @test haskey(d3, k1)
    @test haskey(d4, k1)
    @test !(k2 in keys(d1))
    @test k2 in keys(d2)
    @test !(k1 in values(d4))
    @test v1 in values(d4)
    @test collect(d1) == [Pair(k1, v1)]
    @test collect(d4) == reverse([Pair(k1, v1), Pair(k2, v2), Pair(k1, v2), Pair(k2, v1)])
    @test d1 == ImmutableDict(d, k1 => v1)
    @test !((k1 => v2) in d2)
    @test (k1 => v2) in d3
    @test (k1 => v1) in d4
    @test (k1 => v2) in d4
    @test !in(k2 => "value2", d4, is)
    @test in(k2 => v2, d4, is)
    @test in(k2 => NaN, dnan, isequal)
    @test in(k2 => NaN, dnan, is)
    @test !in(k2 => NaN, dnan, ==)
    @test !in(k2 => 1, dnum, is)
    @test in(k2 => 1.0, dnum, is)
    @test !in(k2 => 1, dnum, <)
    @test in(k2 => 0, dnum, <)
    @test get(d1, "key1", :default) === v1
    @test get(d4, "key1", :default) === v2
    @test get(d4, "foo", :default) === :default
    @test get(d, k1, :default) === :default
    @test d1["key1"] === v1
    @test d4["key1"] === v2
    @test copy(d4) === d4
    @test copy(d) === d
    @test similar(d3) === d
    @test similar(d) === d

    @test_throws KeyError d[k1]
    @test_throws KeyError d1["key2"]
end
