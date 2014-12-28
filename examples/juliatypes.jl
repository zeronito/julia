import Base: convert, show

abstract Ty

type TypeName
    name::Symbol
    super::Ty    # actually TagT
    # arity
    # representation
    # abstract, mutable
    TypeName(name, super) = new(name, super)
    TypeName(name) = new(name)
end

show(io::IO, x::TypeName) = print(io, x.name)

type TagT <: Ty
    name::TypeName
    params
    vararg::Bool
    TagT(n, p, v=false) = new(n, p, v)
end

function show(io::IO, t::TagT)
    print(io, t.name.name)
    isempty(t.params) && return
    print(io, '{')
    l = length(t.params)
    for i=1:l
        show(io, t.params[i])
        if i == l && t.vararg
            print(io, "...")
        elseif i < l
            print(io, ",")
        end
    end
    print(io, '}')
end

type BottomTy <: Ty
end

show(io::IO, ::BottomTy) = print(io, "BottomT")

type UnionT <: Ty
    a
    b
    UnionT() = BottomT
    UnionT(a) = a
    UnionT(a, b) = new(a, b)
    UnionT(a, ts...) = new(a, UnionT(ts...))
end

function show(io::IO, t::UnionT)
    print(io, "UnionT(")
    while true
        show(io, t.a)
        print(io, ",")
        if isa(t.b, UnionT)
            t = t.b
        else
            show(io, t.b)
            break
        end
    end
    print(io, ')')
end

type Var
    name::Symbol
    lb
    ub
    Var(n, lb=BottomT, ub=AnyT) = new(n, lb, ub)
end

function show_var_bounds(io::IO, v::Var)
    if v.lb !== BottomT
        if v.ub === AnyT
            print(io, v.name)
            print(io, ">:")
            show(io, v.lb)
            return
        end
        show(io, v.lb)
        print(io, "<:")
    end
    print(io, v.name)
    if v.ub !== AnyT
        print(io, "<:")
        show(io, v.ub)
    end
end

show(io::IO, v::Var) = print(io, v.name)

type UnionAllT <: Ty
    var::Var
    T
    UnionAllT(v::Var, t) = new(v, t)
    UnionAllT(v::Var, t::Union(Type,Tuple)) = new(v, convert(Ty, t))
end

function show(io::IO, x::UnionAllT)
    print(io, "(@UnionAll ")
    show_var_bounds(io, x.var)
    print(io, " ")
    show(io, x.T)
    print(io, ")")
end


# Any, Bottom, and Tuple

const AnyT = TagT(TypeName(:Any), ())
AnyT.name.super = AnyT

const BottomT = BottomTy()

const TupleName = TypeName(:Tuple, AnyT)
const TupleT = TagT(TupleName, (AnyT,), true)
tupletype(xs...) = inst(TupleName, xs...)
vatype(xs...) = (t = inst(TupleName, xs...); t.vararg = true; t)


# type application

inst(typename::TypeName, params...) = TagT(typename, params)

inst(t::TagT) = t

inst(t::UnionAllT, param) = subst(t.T, Dict{Any,Any}(t.var => param))
inst(t::UnionAllT, param, rest...) = inst(inst(t,param), rest...)

super(t::TagT) = inst(t.name.super, t.params...)

extend(d::Dict, k, v) = (x = copy(d); x[k]=v; x)

subst(t::TagT,    env) = t===AnyT ? t : TagT(t.name, map(x->subst(x,env), t.params), t.vararg)
subst(t::UnionT,  env) = UnionT(subst(t.a,env), subst(t.b,env))
subst(t::Var,     env) = get(env, t, t)
subst(t::UnionAllT, env) = (assert(!haskey(env, t.var));
                            newVar = Var(t.var.name, subst(t.var.lb, env), subst(t.var.ub, env));
                            UnionAllT(newVar, subst(t.T, extend(env, t.var, newVar))))
subst(t, env) = t


# subtype

isequal_type(x, y) = issub(x, y) && issub(y, x)

type Bounds
    # record current lower and upper bounds of a Var
    # depth: invariant position nesting depth of a Var's UnionAll
    # right: whether this Var is on the right-hand side of A <: B
    lb
    ub
    depth::Int
    right::Bool
end

type UnionSearchState
    i::Int       # traversal-order index of current union (0 <= i < nbits(idxs))
    idxs::Int64  # bit vector representing current combination being tested
    UnionSearchState() = new(0,0)
end

type UnionState
    depth::Int        # number of union decision points we're inside
    nnew::Int         # # unions found at next nesting depth
    stack::Vector{UnionSearchState}  # stack of decisions for each depth
    UnionState() = new(1,0,UnionSearchState[])
end

type Env
    vars::Dict{Var,Bounds}
    depth::Int
    Lunions::UnionState
    Runions::UnionState
    Env() = new(Dict{Var,Bounds}(), 1, UnionState(), UnionState())
end

issub(x, y) = forall_exists_issub(x, y, Env(), 0)
issub(x, y, env) = (x === y)
issub(x::Ty, y::Ty, env) = (x === y) || x === BottomT

function forall_exists_issub(x, y, env, nL)
    assert(nL < 63)
    # for all combinations of elements from Unions on the left, there must
    # exist a combination of elements from Unions on the right that makes
    # issub() true. Unions in invariant position are on both the left and
    # the right in this formula.

    for forall in 1:(1<<nL)
        if !isempty(env.Lunions.stack)
            env.Lunions.stack[end].idxs = forall
        end

        !exists_issub(x, y, env, 0) && return false

        if env.Lunions.nnew > 0
            push!(env.Lunions.stack, UnionSearchState())
            sub = forall_exists_issub(x, y, env, env.Lunions.nnew)
            pop!(env.Lunions.stack)
            if !sub
                return false
            end
        end
    end
    return true
end

function exists_issub(x, y, env, nR)
    assert(nR < 63)
    for exists in 1:(1<<nR)
        if !isempty(env.Runions.stack)
            env.Runions.stack[end].idxs = exists
        end
        for ru in env.Runions.stack; ru.i = -1; end
        for lu in env.Lunions.stack; lu.i = -1; end
        env.Lunions.depth = env.Runions.depth = 1
        env.Lunions.nnew = env.Runions.nnew = 0

        sub = issub(x, y, env)

        if env.Lunions.nnew > 0
            # return up to forall_exists_issub. the recursion must have this shape:
            # ∀₁         ∀₁
            #   ∃₁  =>     ∀₂
            #                ...
            #                ∃₁
            #                  ∃₂
            return true
        end
        if env.Runions.nnew > 0
            push!(env.Runions.stack, UnionSearchState())
            found = exists_issub(x, y, env, env.Runions.nnew)
            pop!(env.Runions.stack)
            if env.Lunions.nnew > 0
                # return up to forall_exists_issub
                return true
            end
        else
            found = sub
        end
        found && return true
    end
    return false
end

function issub_union(t::Ty, u::UnionT, env, R, state::UnionState)
    if state.depth > length(state.stack)
        # at a new nesting depth, begin by just counting unions
        state.nnew += 1
        return true
    end
    ui = state.stack[state.depth]; ui.i += 1
    state.depth += 1
    choice = u.((ui.idxs&(1<<ui.i)!=0) + 1)
    ans = R ? issub(t, choice, env) : issub(choice, t, env)
    state.depth -= 1
    return ans
end

issub(a::UnionT, b::UnionT, env) = a === b || issub_union(a, b, env, true, env.Runions)
issub(a::UnionT, b::Ty, env) = issub_union(b, a, env, false, env.Lunions)
issub(a::Ty, b::UnionT, env) = a === BottomT || issub_union(a, b, env, true, env.Runions)

function issub(a::TagT, b::TagT, env)
    a === b && return true
    b === AnyT && return true
    a === AnyT && return false
    if a.name !== b.name
        a.name === TupleName && return false
        return issub(super(a), b, env)
    end
    if a.name === TupleName
        va, vb = a.vararg, b.vararg
        la, lb = length(a.params), length(b.params)
        if va && (!vb || la < lb)
            return false
        end
        ai = bi = 1
        while true
            ai > la && return bi > lb || (bi==lb && vb)
            bi > lb && return false
            !issub(a.params[ai], b.params[bi], env) && return false
            ai==la && bi==lb && va && vb && return true
            if ai < la || !va
                ai += 1
            end
            if bi < lb || !vb
                bi += 1
            end
        end
        @assert false
    else
        env.depth += 1  # crossing invariant constructor, increment depth
        for i = 1:length(a.params)
            ai, bi = a.params[i], b.params[i]
            # use issub in both directions to test equality
            if !(ai===bi || (issub(ai, bi, env) && issub(bi, ai, env)))
                env.depth -= 1
                return false
            end
        end
        env.depth -= 1
    end
    return true
end

function issub(a::Var, b::Ty, env)
    aa = env.vars[a]
    # Vars are fully checked by the "forward" direction of A<:B in
    # invariant position. So just return true when checking the "flipped"
    # direction B<:A.
    aa.right && return true
    !issub(aa.ub, b, env) && return false
    if env.depth > aa.depth
        # invariant position; only true if a.ub <: b <: a.lb  (i.e. a.lb==a.ub)
        !issub(b, aa.lb, env) && return false
    end
    return true
end

lb(x::Var, env) = lb(env.vars[x].lb, env)
lb(x, env) = x
ub(x::Var, env) = ub(env.vars[x].ub, env)
ub(x, env) = x

join(a,b,env) = issub(a,b,env) ? b : issub(b,a,env) ? a : UnionT(a,b)

function issub(a::Union(Ty,Var), b::Var, env)
    bb = env.vars[b]
    !bb.right && return true
    b_lb, b_ub = ub(bb.lb,env), lb(bb.ub,env)
    if isa(a,Var)
        aa = env.vars[a]
        # Vars must occur at same depth
        aa.depth != bb.depth && return false
        a_lb, a_ub = ub(a.lb,env), lb(a.ub,env)
    else
        a_lb, a_ub = a, a
    end
    # make sure constraint is within the current bounds of Var
    if !isa(bb.ub,Var)
        !issub(a_ub, b_ub, env) && return false
    end
    if env.depth > bb.depth && !isa(bb.lb,Var)
        !issub(b_lb, a_lb, env) && return false
    end

    # check & update bounds for covariant position
    # for each type a<:b, grow b's lower bound to include a, or set b's
    # lower bound equal to a typevar if its lower bound is big enough.
    if isa(a,Var) && (a === bb.lb || issub(b_lb, a_lb, env))
        bb.lb = a
    else
        isa(bb.ub,Var) && !issub(a_ub, b_ub, env) && return false
        bb.lb = join(b_lb, a_ub, env)
    end

    if env.depth > bb.depth
        # check & update bounds for invariant position.
        # this would be the code for contravariant position, but since
        # we only have invariant, the covariant code above always runs too.
        if isa(a,Var) && (a === bb.ub || issub(a_ub, b_ub, env))
            bb.ub = a
        else
            #!issub(b_lb, a_lb, env) && return false
            # for true contravariance we would need to compute a meet here,
            # but because of invariance b_ub⊓a_lb = a_lb here always
            bb.ub = a_lb #issub(b_ub, a_lb, env) ? b_ub : a_lb #meet(b_ub, a_lb, env)
        end
    end
    return true
end

function rename(t::UnionAllT)
    v = Var(t.var.name, t.var.lb, t.var.ub)
    UnionAllT(v, inst(t,v))
end

function issub_unionall(t::Ty, u::UnionAllT, env, R)
    haskey(env.vars, u.var) && (u = rename(u))
    env.vars[u.var] = Bounds(u.var.lb, u.var.ub, env.depth, R)
    ans = R ? issub(t, u.T, env) : issub(u.T, t, env)
    delete!(env.vars, u.var)
    return ans
end

issub(a::UnionAllT, b::UnionAllT, env) = a === b || issub_unionall(a, b, env, true)
issub(a::UnionT, b::UnionAllT, env) = issub_unionall(a, b, env, true)
issub(a::UnionAllT, b::UnionT, env) = issub_unionall(b, a, env, false)
issub(a::Ty, b::UnionAllT, env) = a === BottomT || issub_unionall(a, b, env, true)
issub(a::UnionAllT, b::Ty, env) = issub_unionall(b, a, env, false)


# convenient syntax

macro UnionAll(var, expr)
    lb = :BottomT
    ub = :AnyT
    if isa(var,Expr) && var.head === :comparison
        if length(var.args) == 3
            v = var.args[1]
            if var.args[2] == :(<:)
                ub = esc(var.args[3])
            elseif var.args[2] == :(>:)
                lb = esc(var.args[3])
            else
                error("invalid bounds in UnionAll")
            end
        elseif length(var.args) == 5
            v = var.args[3]
            if var.args[2] == var.args[4] == :(<:)
                lb = esc(var.args[1])
                ub = esc(var.args[5])
            else
                error("invalid bounds in UnionAll")
            end
        else
            error("invalid bounds in UnionAll")
        end
    elseif !isa(var,Symbol)
        error("invalid variable in UnionAll")
    else
        v = var
    end
    quote
        let $(esc(v)) = Var($(Expr(:quote,v)), $lb, $ub)
            UnionAllT($(esc(v)), $(esc(expr)))
        end
    end
end


# translating from existing julia types

const tndict = ObjectIdDict()

xlate(t) = xlate(t, ObjectIdDict())

xlate(t, env) = t

function xlate(t::UnionType, env)
    if t === Union()
        return BottomT
    end
    UnionT(map(x->xlate(x,env), t.types)...)
end

function xlate(t::Tuple, env)
    if length(t) == 0
        return inst(TupleName)
    end
    va = Base.isvarargtype(t[end])
    ts = map(x->(Base.isvarargtype(x) ? xlate(x.parameters[1],env) : xlate(x,env)), t)
    tnew = inst(TupleName, ts...)
    tnew.vararg = va
    tnew
end

function xlate(t::TypeVar, env)
    if haskey(env, t)
        return env[t]
    end
    v = Var(t.name, xlate(t.lb,env), xlate(t.ub,env))
    env[t] = v
    v
end

function xlate(t::DataType, env)
    if t === Any
        return AnyT
    end
    if !haskey(tndict,t.name)
        para = map(x->xlate(x,env), t.name.primary.parameters)  # adds tvars to env
        sup = xlate(t.name.primary.super, env)
        for i = length(para):-1:1
            sup = UnionAllT(para[i], sup)
        end
        tn = TypeName(t.name.name, sup)
        tndict[t.name] = tn
    else
        tn = tndict[t.name]
    end
    inst(tn, map(x->xlate(x,env), t.parameters)...)
end

convert(::Type{Ty}, t::Union(Type,Tuple)) = xlate(t)
convert(::Type{Ty}, t::TypeVar) = xlate(t)

issub(a::Type, b::Type) = issub(xlate(a), xlate(b))
issub(a::Ty  , b::Type) = issub(a       , xlate(b))
issub(a::Type, b::Ty  ) = issub(xlate(a), b)


# tests

AbstractArrayT =
    let AbstractArrayName = TypeName(:AbstractArray, @UnionAll T @UnionAll N AnyT)
        @UnionAll T @UnionAll N inst(AbstractArrayName, T, N)
    end

ArrayT =
    let ArrayName = TypeName(:Array, @UnionAll T @UnionAll N inst(AbstractArrayT, T, N))
        @UnionAll T @UnionAll N inst(ArrayName, T, N)
    end

PairT = let PairName = TypeName(:Pair, @UnionAll A @UnionAll B AnyT)
    @UnionAll A @UnionAll B inst(PairName, A, B)
end

RefT = let RefName = TypeName(:Ref, @UnionAll T AnyT)
    @UnionAll T inst(RefName, T)
end

tndict[AbstractArray.name] = AbstractArrayT.T.T.name
tndict[Array.name] = ArrayT.T.T.name
tndict[Pair.name] = PairT.T.T.name

using Base.Test

issub_strict(x,y) = issub(x,y) && !issub(y,x)

# level 1: no varags, union, UnionAll
function test_1()
    @test issub_strict(Int, Integer)
    @test issub_strict(Array{Int,1}, AbstractArray{Int,1})

    @test isequal_type(Int, Int)
    @test isequal_type(Integer, Integer)
    @test isequal_type(Array{Int,1}, Array{Int,1})
    @test isequal_type(AbstractArray{Int,1}, AbstractArray{Int,1})

    @test issub_strict((Int,Int), (Integer,Integer))
    @test issub_strict((Array{Int,1},), (AbstractArray{Int,1},))

    @test isequal_type((Integer,Integer), (Integer,Integer))

    @test !issub((Int,Int), (Int,))
    @test !issub((Int,), (Integer,Integer))
end

# level 2: varargs
function test_2()
    @test issub_strict((Int,Int), (Int...,))
    @test issub_strict((Int,Int), (Int,Int...,))
    @test issub_strict((Int,Int), (Int,Integer...,))
    @test issub_strict((Int,Int), (Int,Int,Integer...,))
    @test issub_strict((Int,Int...), (Int...,))
    @test issub_strict((Int,Int,Int...), (Int...,))
    @test issub_strict((Int,Int,Int...), (Integer,Int...,))
    @test issub_strict((Int...,), (Any...,))
    @test issub_strict((), (Any...,))

    @test isequal_type((Int...,), (Int...,))
    @test isequal_type((Integer...,), (Integer...,))

    @test !issub((), (Int, Int...))
    @test !issub((Int,), (Int, Int, Int...))

    @test !issub((Int, (Real, Integer)), (Int...))
end

# level 3: UnionAll
function test_3()
    @test issub_strict(Ty(Array{Int,1}), @UnionAll T inst(ArrayT, T, 1))
    @test issub_strict((@UnionAll T inst(PairT,T,T)), (@UnionAll T @UnionAll S inst(PairT,T,S)))
    @test issub(inst(PairT,Ty(Int),Ty(Int8)), (@UnionAll T @UnionAll S inst(PairT,T,S)))
    @test issub(inst(PairT,Ty(Int),Ty(Int8)), (@UnionAll S inst(PairT,Ty(Int),S)))

    @test !issub((@UnionAll T<:Ty(Real) T), (@UnionAll T<:Ty(Integer) T))

    @test issub((@UnionAll T tupletype(T,T)), (@UnionAll T @UnionAll S tupletype(T,S)))
    @test issub((@UnionAll T @UnionAll S tupletype(T,S)), (@UnionAll T tupletype(T,T)))

    @test isequal_type((@UnionAll T tupletype(T,T)), (@UnionAll T @UnionAll S tupletype(T,S)))
    @test isequal_type((@UnionAll T @UnionAll S tupletype(T,S)), (@UnionAll T tupletype(T,T)))

    @test !issub((@UnionAll T<:Ty(Integer) @UnionAll S<:Ty(Number) (T,S)),
                 (@UnionAll T<:Ty(Integer) @UnionAll S<:Ty(Number) (S,T)))

    AUA = inst(ArrayT, (@UnionAll T inst(ArrayT,T,1)), 1)
    UAA = (@UnionAll T inst(ArrayT, inst(ArrayT,T,1), 1))

    @test !issub(AUA, UAA)
    @test !issub(UAA, AUA)
    @test !isequal_type(AUA, UAA)

    @test issub_strict((@UnionAll T Int), (@UnionAll T<:Ty(Integer) Integer))

    @test isequal_type((@UnionAll T @UnionAll S tupletype(T, tupletype(S))),
                       (@UnionAll T tupletype(T, @UnionAll S tupletype(S))))

    @test !issub((@UnionAll T inst(PairT,T,T)), inst(PairT,Ty(Int),Ty(Int8)))
    @test !issub((@UnionAll T inst(PairT,T,T)), inst(PairT,Ty(Int),Ty(Int)))

    @test isequal_type((@UnionAll T tupletype(T)), tupletype(AnyT))
    @test isequal_type((@UnionAll T<:Ty(Real) tupletype(T)), tupletype(Ty(Real)))

    @test  issub(tupletype(inst(ArrayT,Ty(Integer),1), Ty(Int)),
                 (@UnionAll T<:Ty(Integer) tupletype(inst(ArrayT,T,1),T)))

    @test !issub(tupletype(inst(ArrayT,Ty(Integer),1), Ty(Real)),
                 (@UnionAll T<:Ty(Integer) tupletype(inst(ArrayT,T,1),T)))

    @test !issub(Ty((Int,String,Vector{Integer})),
                 @UnionAll T tupletype(T, T, inst(ArrayT,T,1)))
    @test !issub(Ty((String,Int,Vector{Integer})),
                 @UnionAll T tupletype(T, T, inst(ArrayT,T,1)))

    @test  issub(Ty((Int,String,Vector{Any})),
                 @UnionAll T tupletype(T, T, inst(ArrayT,T,1)))

    @test isequal_type(Ty(Array{Int,1}), inst(ArrayT, (@UnionAll T<:Ty(Int) T), 1))
    @test isequal_type(Ty(Array{(Any,),1}), inst(ArrayT, (@UnionAll T tupletype(T)), 1))

    @test isequal_type(Ty(Array{(Integer,Integer),1}),
                       inst(ArrayT, (@UnionAll T<:Ty(Integer) tupletype(T,T)), 1))
    @test !issub(Ty(Array{(Int,Integer),1}),
                 inst(ArrayT, (@UnionAll T<:Ty(Integer) tupletype(T,T)), 1))


    @test !issub(inst(PairT,Ty(Int),Ty(Int8)), (@UnionAll T inst(PairT,T,T)))

    @test !issub(tupletype(inst(ArrayT,Ty(Int),1), Ty(Integer)),
                 (@UnionAll T<:Ty(Integer) tupletype(inst(ArrayT,T,1),T)))

    @test !issub(tupletype(Ty(Integer), inst(ArrayT,Ty(Int),1)),
                 (@UnionAll T<:Ty(Integer) tupletype(T, inst(ArrayT,T,1))))

    @test !issub(Ty(Array{Array{Int,1},Integer}),
                 (@UnionAll T inst(ArrayT,inst(ArrayT,T,1),T)))

    @test issub(Ty(Array{Array{Int,1},Int}),
                (@UnionAll T inst(ArrayT,inst(ArrayT,T,1),T)))

    @test  issub(Ty((Integer,Int)), @UnionAll T<:Ty(Integer) @UnionAll S<:T tupletype(T,S))
    @test !issub(Ty((Integer,Int)), @UnionAll T<:Ty(Int) @UnionAll S<:T tupletype(T,S))
    @test !issub(Ty((Integer,Int)), @UnionAll T<:Ty(String) @UnionAll S<:T tupletype(T,S))

    @test issub(Ty((Float32,Array{Float32,1})),
                @UnionAll T<:Ty(Real) @UnionAll S<:inst(AbstractArrayT,T,1) tupletype(T,S))

    @test !issub(Ty((Float32,Array{Float64,1})),
                 @UnionAll T<:Ty(Real) @UnionAll S<:inst(AbstractArrayT,T,1) tupletype(T,S))

    @test issub(Ty((Float32,Array{Real,1})),
                @UnionAll T<:Ty(Real) @UnionAll S<:inst(AbstractArrayT,T,1) tupletype(T,S))

    @test !issub(Ty((Number,Array{Real,1})),
                 @UnionAll T<:Ty(Real) @UnionAll S<:inst(AbstractArrayT,T,1) tupletype(T,S))

    @test issub((@UnionAll Ty(Int)<:T<:Ty(Integer) T), @UnionAll T<:Ty(Real) T)
    @test issub((@UnionAll Ty(Int)<:T<:Ty(Integer) inst(ArrayT,T,1)),
                (@UnionAll T<:Ty(Real) inst(ArrayT,T,1)))

    @test issub((@UnionAll Ty(Int)<:T<:Ty(Integer) T),
                (@UnionAll Ty(Integer)<:T<:Ty(Real) T))
    @test !issub((@UnionAll Ty(Int)<:T<:Ty(Integer) inst(ArrayT,T,1)),
                 (@UnionAll Ty(Integer)<:T<:Ty(Real) inst(ArrayT,T,1)))

    X = (@UnionAll T<:Ty(Real) @UnionAll S<:inst(AbstractArrayT,T,1) tupletype(T,S))
    Y = (@UnionAll A<:Ty(Real) @UnionAll B<:inst(AbstractArrayT,A,1) tupletype(A,B))
    @test isequal_type(X,Y)
    Z = (@UnionAll A<:Ty(Real) @UnionAll B<:inst(AbstractArrayT,A,1) tupletype(Ty(Real),B))
    @test issub_strict(X,Z)
end

# level 4: Union
function test_4()
    @test isequal_type(UnionT(BottomT,BottomT), BottomT)

    @test issub_strict(Int, Union(Int,String))
    @test issub_strict(Union(Int,Int8), Integer)

    @test isequal_type(Union(Int,Int8), Union(Int,Int8))

    @test isequal_type(UnionT(Ty(Int),Ty(Integer)), Ty(Integer))

    @test isequal_type((Union(Int,Int8),Int16), Union((Int,Int16),(Int8,Int16)))

    @test issub_strict((Int,Int8,Int), (Union(Int,Int8)...,))
    @test issub_strict((Int,Int8,Int), (Union(Int,Int8,Int16)...,))

    # nested unions
    @test !issub(UnionT(Ty(Int),inst(RefT,UnionT(Ty(Int),Ty(Int8)))),
                 UnionT(Ty(Int),inst(RefT,UnionT(Ty(Int8),Ty(Int16)))))

    A = Ty(Int);   B = Ty(Int8)
    C = Ty(Int16); D = Ty(Int32)
    @test  issub(UnionT(UnionT(A,UnionT(A,UnionT(B,C))), UnionT(D,BottomT)),
                 UnionT(UnionT(A,B),UnionT(C,UnionT(B,D))))
    @test !issub(UnionT(UnionT(A,UnionT(A,UnionT(B,C))), UnionT(D,BottomT)),
                 UnionT(UnionT(A,B),UnionT(C,UnionT(B,A))))

    @test isequal_type(UnionT(UnionT(A,B,C), UnionT(D)),  UnionT(A,B,C,D))
    @test isequal_type(UnionT(UnionT(A,B,C), UnionT(D)),  UnionT(A,UnionT(B,C),D))
    @test isequal_type(UnionT(UnionT(UnionT(UnionT(A)),B,C), UnionT(D)),
                       UnionT(A,UnionT(B,C),D))

    @test issub_strict(UnionT(UnionT(A,C), UnionT(D)),  UnionT(A,B,C,D))

    @test !issub(UnionT(UnionT(A,B,C), UnionT(D)),  UnionT(A,C,D))
end

# level 5: union and UnionAll
function test_5()
    @test issub(Ty((String,Array{Int,1})),
                (@UnionAll T UnionT(tupletype(T,inst(ArrayT,T,1)),
                                    tupletype(T,inst(ArrayT,Ty(Int),1)))))

    @test issub(Ty((Union(Vector{Int},Vector{Int8}),)),
                @UnionAll T tupletype(inst(ArrayT,T,1),))

    @test !issub(Ty((Union(Vector{Int},Vector{Int8}),Vector{Int})),
                 @UnionAll T tupletype(inst(ArrayT,T,1), inst(ArrayT,T,1)))

    @test !issub(Ty((Union(Vector{Int},Vector{Int8}),Vector{Int8})),
                 @UnionAll T tupletype(inst(ArrayT,T,1), inst(ArrayT,T,1)))

    @test !issub(Ty(Vector{Int}), @UnionAll T>:Ty(Union(Int,Int8)) inst(ArrayT,T,1))
    @test  issub(Ty(Vector{Integer}), @UnionAll T>:Ty(Union(Int,Int8)) inst(ArrayT,T,1))
    @test  issub(Ty(Vector{Union(Int,Int8)}), @UnionAll T>:Ty(Union(Int,Int8)) inst(ArrayT,T,1))

    @test issub((@UnionAll Ty(Int)<:T<:Ty(Union(Int,Int8)) inst(ArrayT,T,1)),
                (@UnionAll Ty(Int)<:T<:Ty(Union(Int,Int8)) inst(ArrayT,T,1)))

    # with varargs
    @test !issub(inst(ArrayT,tupletype(inst(ArrayT,Ty(Int)),inst(ArrayT,Ty(Vector{Int16})),inst(ArrayT,Ty(Vector{Int})),inst(ArrayT,Ty(Int)))),
                 @UnionAll T<:(@UnionAll S vatype(UnionT(inst(ArrayT,S),inst(ArrayT,inst(ArrayT,S,1))))) inst(ArrayT,T))

    @test issub(inst(ArrayT,tupletype(inst(ArrayT,Ty(Int)),inst(ArrayT,Ty(Vector{Int})),inst(ArrayT,Ty(Vector{Int})),inst(ArrayT,Ty(Int)))),
                @UnionAll T<:(@UnionAll S vatype(UnionT(inst(ArrayT,S),inst(ArrayT,inst(ArrayT,S,1))))) inst(ArrayT,T))

    @test !issub(tupletype(inst(ArrayT,Ty(Int)),inst(ArrayT,Ty(Vector{Int16})),inst(ArrayT,Ty(Vector{Int})),inst(ArrayT,Ty(Int))),
                 @UnionAll S vatype(UnionT(inst(ArrayT,S),inst(ArrayT,inst(ArrayT,S,1)))))

    @test issub(tupletype(inst(ArrayT,Ty(Int)),inst(ArrayT,Ty(Vector{Int})),inst(ArrayT,Ty(Vector{Int})),inst(ArrayT,Ty(Int))),
                @UnionAll S vatype(UnionT(inst(ArrayT,S),inst(ArrayT,inst(ArrayT,S,1)))))
end

# tricky type variable lower bounds
function test_6()
    @test  issub((@UnionAll S<:Ty(Int) (@UnionAll R<:Ty(String) tupletype(S,R,Ty(Vector{Any})))),
                 (@UnionAll T tupletype(T, T, inst(ArrayT,T,1))))

    @test !issub((@UnionAll S<:Ty(Int) (@UnionAll R<:Ty(String) tupletype(S,R,Ty(Vector{Integer})))),
                 (@UnionAll T tupletype(T, T, inst(ArrayT,T,1))))

    t = @UnionAll T tupletype(T,T,inst(RefT,T))
    @test isequal_type(t, rename(t))

    @test !issub((@UnionAll T tupletype(T,Ty(String),inst(RefT,T))),
                 (@UnionAll T tupletype(T,T,inst(RefT,T))))

    @test !issub((@UnionAll T tupletype(T,inst(RefT,T),Ty(String))),
                 (@UnionAll T tupletype(T,inst(RefT,T),T)))

    i = Ty(Int); ai = Ty(Integer)
    @test issub((@UnionAll i<:T<:i   inst(RefT,T)), inst(RefT,i))
    @test issub((@UnionAll ai<:T<:ai inst(RefT,T)), inst(RefT,ai))

    # Pair{T,S} <: Pair{T,T} can be true with certain bounds
    @test issub((@UnionAll i<:T<:i @UnionAll i<:S<:i inst(PairT,T,S)),
                @UnionAll T inst(PairT,T,T))
end

# examples that might take a long time
function test_slow()
    A = Ty(Int);   B = Ty(Int8)
    C = Ty(Int16); D = Ty(Int32)
    # obviously these unions can be simplified, but when they aren't there's trouble
    X = UnionT(UnionT(A,B,C),UnionT(A,B,C),UnionT(A,B,C),UnionT(A,B,C),
               UnionT(A,B,C),UnionT(A,B,C),UnionT(A,B,C),UnionT(A,B,C))
    Y = UnionT(UnionT(D,B,C),UnionT(D,B,C),UnionT(D,B,C),UnionT(D,B,C),
               UnionT(D,B,C),UnionT(D,B,C),UnionT(D,B,C),UnionT(A,B,C))
    @test issub_strict(X,Y)
end

# tests that don't pass yet
function test_failing()
end

function test_all()
    test_1()
    test_2()
    test_3()
    test_4()
    test_5()
    test_6()
    test_slow()
    test_failing()
end

const menagerie =
    Any[BottomT, AnyT, Ty(Int), Ty(Int8), Ty(Integer), Ty(Real),
        Ty(Array{Int,1}), Ty(AbstractArray{Int,1}),
        Ty((Int,Integer...,)), Ty((Integer,Int...,)), Ty(()),
        Ty(Union(Int,Int8)),
        (@UnionAll T inst(ArrayT, T, 1)),
        (@UnionAll T inst(PairT,T,T)),
        (@UnionAll T @UnionAll S inst(PairT,T,S)),
        inst(PairT,Ty(Int),Ty(Int8)),
        (@UnionAll S inst(PairT,Ty(Int),S)),
        (@UnionAll T tupletype(T,T)),
        (@UnionAll T<:Ty(Integer) tupletype(T,T)),
        (@UnionAll T @UnionAll S tupletype(T,S)),
        (@UnionAll T<:Ty(Integer) @UnionAll S<:Ty(Number) (T,S)),
        (@UnionAll T<:Ty(Integer) @UnionAll S<:Ty(Number) (S,T)),
        inst(ArrayT, (@UnionAll T inst(ArrayT,T,1)), 1),
        (@UnionAll T inst(ArrayT, inst(ArrayT,T,1), 1)),
        inst(ArrayT, (@UnionAll T<:Ty(Int) T), 1),
        (@UnionAll T<:Ty(Real) @UnionAll S<:inst(AbstractArrayT,T,1) tupletype(T,S)),
        UnionT(Ty(Int),inst(RefT,UnionT(Ty(Int),Ty(Int8)))),
        (@UnionAll T UnionT(tupletype(T,inst(ArrayT,T,1)),
                            tupletype(T,inst(ArrayT,Ty(Int),1)))),
        ]

let new = Any[]
    # add variants of each type
    for T in menagerie
        push!(new, inst(RefT, T))
        push!(new, tupletype(T))
        push!(new, tupletype(T,T))
        push!(new, vatype(T))
        push!(new, @UnionAll S<:T S)
        push!(new, @UnionAll S<:T inst(RefT,S))
    end
    append!(menagerie, new)
end

function test_properties()
    x→y = !x || y
    ¬T = @UnionAll X>:T inst(RefT,X)

    for T in menagerie
        # top and bottom identities
        @test issub(BottomT, T)
        @test issub(T, AnyT)
        @test issub(T, BottomT) → isequal_type(T, BottomT)
        @test issub(AnyT, T) → isequal_type(T, AnyT)

        # unionall identity
        @test isequal_type(T, @UnionAll S<:T S)

        # equality under renaming
        if isa(T, UnionAllT)
            @test isequal_type(T, rename(T))
        end

        for S in menagerie
            # transitivity
            if issub(T, S)
                for R in menagerie
                    @test issub(S, R) → issub(T, R)
                end
            end

            # union subsumption
            @test isequal_type(T, UnionT(T,S)) → issub(S, T)

            # invariance
            @test isequal_type(T, S) == isequal_type(inst(RefT,T), inst(RefT,S))

            # covariance
            @test issub(T, S) == issub(tupletype(T), tupletype(S))
            @test issub(T, S) == issub(vatype(T), vatype(S))
            @test issub(T, S) == issub(tupletype(T), vatype(S))

            # contravariance
            @test issub(T, S) == issub(¬S, ¬T)
        end
    end
end

# ideas for handling typevars in covariant position:
# - if a var only appears covariant, automatically make it range over
#   only concrete types
#
# - introduce ⟦Concrete{T}⟧ = is T concrete ? ⟦T⟧ : ∅

# function non_terminating_F()
#     # undecidable F_<: instance
#     ¬T = @ForAll α<:T α
#     θ = @ForAll α ¬(@ForAll β<:α ¬β)
#     a₀ = Var(:a₀, BottomT, θ)
#     issub(a₀, (@ForAll a₁<:a₀ ¬a₁))
# end

# attempt to implement non-terminating example from
# "On the Decidability of Subtyping with Bounded Existential Types"
function non_terminating_2()
    C = let CName = TypeName(:C, @UnionAll T AnyT)
        @UnionAll T inst(CName, T)
    end
    D = let DName = TypeName(:D, @UnionAll T AnyT)
        @UnionAll T inst(DName, T)
    end
    ¬T = @UnionAll X>:T inst(D,X)
    U = AnyT
    X = Var(:X, BottomT, ¬U)
    e = Env()
    e.vars[X] = Bounds(BottomT, ¬U, e.depth, false)
    issub(X, ¬inst(C,X), e)
end
