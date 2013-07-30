module VersionWeights

export VersionWeight

immutable HierarchicalValue{T}
    v::Vector{T}
    rest::T
end

HierarchicalValue{T}(v::Vector{T}, rest::T = zero(T)) = HierarchicalValue{T}(v, rest)
HierarchicalValue(T::Type) = HierarchicalValue(T[])

Base.zero{T}(::Type{HierarchicalValue{T}}) = HierarchicalValue(T)

Base.typemin{T}(::Type{HierarchicalValue{T}}) = HierarchicalValue(T[], typemin(T))
Base.typemax{T}(::Type{HierarchicalValue{T}}) = HierarchicalValue(T[], typemax(T))

for (bf,f) in ((:(:-), :-), (:(:+),:+))
    @eval function Base.($bf){T}(a::HierarchicalValue{T}, b::HierarchicalValue{T})
        av = a.v
        bv = b.v
        la = length(a.v)
        lb = length(b.v)
        l0 = min(la, lb)
        l1 = max(la, lb)
        ld = la - lb
        rv = Array(T, l1)
        rf = ($f)(a.rest, b.rest)
        @inbounds for i = 1:l0
            rv[i] = ($f)(av[i], bv[i])
        end
        @inbounds for i = l0+1:l0+ld
            rv[i] = ($f)(av[i], b.rest)
        end
        @inbounds for i = l0+1:l0-ld
            rv[i] = ($f)(a.rest, bv[i])
        end
        return HierarchicalValue(rv, rf)
    end
end

Base.(:-){T}(a::HierarchicalValue{T}) = HierarchicalValue(-a.v, -a.rest)

function Base.cmp{T}(a::HierarchicalValue{T}, b::HierarchicalValue{T})
    av = a.v
    bv = b.v
    la = length(a.v)
    lb = length(b.v)
    l0 = min(la, lb)
    l1 = max(la, lb)
    ld = la - lb
    rv = Array(T, l1)
    @inbounds for i = 1:l0
        c = cmp(av[i], bv[i]); c != 0 && return c
    end
    @inbounds for i = l0+1:l0+ld
        c = cmp(av[i], b.rest); c != 0 && return c
    end
    @inbounds for i = l0+1:l0-ld
        c = cmp(a.rest, bv[i]); c != 0 && return c
    end
    return cmp(a.rest, b.rest)
end
Base.isless{T}(a::HierarchicalValue{T}, b::HierarchicalValue{T}) = (cmp(a, b) == -1)
Base.isequal{T}(a::HierarchicalValue{T}, b::HierarchicalValue{T}) = (a.v == b.v) && (a.rest == b.rest)

Base.abs{T}(a::HierarchicalValue{T}) = HierarchicalValue(T[abs(x) for x in a.v], abs(a.rest))

bpencode(ident::ASCIIString) =
    Int[c == '-' ? 1 :
        ('0' <= c <= '9') ? (c - '0' + 2) :
        ('A' <= c <= 'Z') ? (c - 'A' + 12) :
        # 'a' <= c <= 'z'
        (c - 'a' + 38) for c in ident]

immutable VWPreBuildItem
    nonempty::Int
    s::HierarchicalValue{Int}
    i::Int
end
VWPreBuildItem() = VWPreBuildItem(0, HierarchicalValue(Int), 0)
VWPreBuildItem(i::Int) = VWPreBuildItem(1, HierarchicalValue(Int), i)
VWPreBuildItem(s::ASCIIString) = VWPreBuildItem(1, HierarchicalValue(bpencode(s)), 0)

Base.zero(::Type{VWPreBuildItem}) = VWPreBuildItem()

Base.typemin(::Type{VWPreBuildItem}) = (x=typemin(Int); VWPreBuildItem(x, HierarchicalValue(Int[], x), x))
Base.typemax(::Type{VWPreBuildItem}) = (x=typemax(Int); VWPreBuildItem(x, HierarchicalValue(Int[], x), x))

Base.(:-)(a::VWPreBuildItem, b::VWPreBuildItem) = VWPreBuildItem(a.nonempty-b.nonempty, a.s-b.s, a.i-b.i)
Base.(:+)(a::VWPreBuildItem, b::VWPreBuildItem) = VWPreBuildItem(a.nonempty+b.nonempty, a.s+b.s, a.i+b.i)

Base.(:-)(a::VWPreBuildItem) = VWPreBuildItem(-a.nonempty, -a.s, -a.i)

function Base.cmp(a::VWPreBuildItem, b::VWPreBuildItem)
    c = cmp(a.nonempty, b.nonempty); c != 0 && return c
    c = cmp(a.s, b.s); c != 0 && return c
    return cmp(a.i, b.i)
end
Base.isless(a::VWPreBuildItem, b::VWPreBuildItem) = (cmp(a,b) == -1)
Base.isequal(a::VWPreBuildItem, b::VWPreBuildItem) = (cmp(a,b) == 0)

Base.abs(a::VWPreBuildItem) = VWPreBuildItem(abs(a.nonempty), abs(a.s), abs(a.i))

immutable VWPreBuild
    nonempty::Int
    w::HierarchicalValue{VWPreBuildItem}
end

function VWPreBuild(ispre::Bool, desc::(Union(Int,ASCIIString)...))
    isempty(desc) && return VWPreBuild(0, HierarchicalValue(VWPreBuildItem))
    desc == ("",) && return VWPreBuild(ispre ? -1 : 1, HierarchicalValue(VWPreBuildItem[]))
    nonempty = ispre ? -1 : 0
    w = Array(VWPreBuildItem, length(desc))
    i = 1
    @inbounds for item in desc
        w[i] = VWPreBuildItem(item)
        i += 1
    end
    return VWPreBuild(nonempty, HierarchicalValue(w))
end
VWPreBuild() = VWPreBuild(0, HierarchicalValue(VWPreBuildItem))

Base.zero(::Type{VWPreBuild}) = VWPreBuild()

Base.typemin(::Type{VWPreBuild}) = VWPreBuild(typemin(Int), typemin(HierarchicalValue{VWPreBuildItem}))
Base.typemax(::Type{VWPreBuild}) = VWPreBuild(typemax(Int), typemax(HierarchicalValue{VWPreBuildItem}))

Base.(:-)(a::VWPreBuild, b::VWPreBuild) = VWPreBuild(a.nonempty-b.nonempty, a.w-b.w)
Base.(:+)(a::VWPreBuild, b::VWPreBuild) = VWPreBuild(a.nonempty+b.nonempty, a.w+b.w)

Base.(:-)(a::VWPreBuild) = VWPreBuild(-a.nonempty, -a.w)

function Base.cmp(a::VWPreBuild, b::VWPreBuild)
    c = cmp(a.nonempty, b.nonempty); c != 0 && return c
    return cmp(a.w, b.w)
end
Base.isless(a::VWPreBuild, b::VWPreBuild) = (cmp(a,b) == -1)
Base.isequal(a::VWPreBuild, b::VWPreBuild) = (cmp(a,b) == 0)

Base.abs(a::VWPreBuild) = VWPreBuild(abs(a.nonempty), abs(a.w))

# The numeric type used to determine how the different
# versions of a package should be weighed
immutable VersionWeight
    major::Int
    minor::Int
    patch::Int
    prerelease::VWPreBuild
    build::VWPreBuild
    uninstall::Int
end
VersionWeight(major::Int,minor::Int,patch::Int,prerelease::VWPreBuild,build::VWPreBuild) = VersionWeight(major, minor, patch, prerelease, build, 0)
VersionWeight(major::Int,minor::Int,patch::Int,prerelease::VWPreBuild) = VersionWeight(major, minor, patch, prerelease, zero(VWPreBuild))
VersionWeight(major::Int,minor::Int,patch::Int) = VersionWeight(major, minor, patch, zero(VWPreBuild))
VersionWeight(major::Int,minor::Int) = VersionWeight(major, minor, 0)
VersionWeight(major::Int) = VersionWeight(major, 0)
VersionWeight() = VersionWeight(0)

VersionWeight(vn::VersionNumber, uninstall=false) =
    VersionWeight(vn.major, vn.minor, vn.patch,
                  VWPreBuild(true, vn.prerelease), VWPreBuild(false, vn.build),
                  int(uninstall))

Base.zero(::Type{VersionWeight}) = VersionWeight()

Base.typemin(::Type{VersionWeight}) = (x=typemin(Int); y=typemin(VWPreBuild); VersionWeight(x,x,x,y,y,x))
Base.typemax(::Type{VersionWeight}) = (x=typemax(Int); y=typemax(VWPreBuild); VersionWeight(x,x,x,y,y,x))

Base.(:-)(a::VersionWeight, b::VersionWeight) =
    VersionWeight(a.major-b.major, a.minor-b.minor, a.patch-b.patch,
                  a.prerelease-b.prerelease, a.build-b.build,
                  a.uninstall-b.uninstall)

Base.(:+)(a::VersionWeight, b::VersionWeight) =
    VersionWeight(a.major+b.major, a.minor+b.minor, a.patch+b.patch,
                  a.prerelease+b.prerelease, a.build+b.build,
                  a.uninstall+b.uninstall)

Base.(:-)(a::VersionWeight) =
    VersionWeight(-a.major, -a.minor, -a.patch,
                  -a.prerelease, -a.build,
                  -a.uninstall)

function Base.cmp(a::VersionWeight, b::VersionWeight)
    c = cmp(a.major, b.major); c != 0 && return c
    c = cmp(a.minor, b.minor); c != 0 && return c
    c = cmp(a.patch, b.patch); c != 0 && return c
    c = cmp(a.prerelease, b.prerelease); c != 0 && return c
    c = cmp(a.build, b.build); c != 0 && return c
    return cmp(a.uninstall, b.uninstall)
end
Base.isless(a::VersionWeight, b::VersionWeight) = (cmp(a, b) == -1)
Base.isequal(a::VersionWeight, b::VersionWeight) = (cmp(a, b) == 0)

Base.abs(a::VersionWeight) =
    VersionWeight(abs(a.major), abs(a.minor), abs(a.patch),
                  abs(a.prerelease), abs(a.build),
                  abs(a.uninstall))

end
