original_load_path = copy(Base.LOAD_PATH)
pushfirst!(Base.LOAD_PATH, joinpath(@__DIR__, "plugins"))

using Test
using Tracer

# XXX: should these be in `Tracer/test/runtests.jl`?

function fib(x)
    if x <= 1
        return x
    else
        return fib(x-1) + fib(x-2)
    end
end

let tr = trace(fib, 1)
    @test tr.f == fib
    @test tr.args == (1,)
    child = only(tr.children)
    @test child.f == Base.:<=
    @test child.args == (1,1)
end

let tr = trace(fib, 2)
    @test tr.f == fib
    @test tr.args == (2,)
    @test length(tr.children) == 6
end

empty!(Base.LOAD_PATH)
append!(Base.LOAD_PATH, original_load_path)