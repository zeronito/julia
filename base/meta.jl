# This file is a part of Julia. License is MIT: https://julialang.org/license

module Meta
#
# convenience functions for metaprogramming
#

export quot,
       isexpr,
       show_sexpr,
       @dump,
       leaftypes,
       abstracts


quot(ex) = Expr(:quote, ex)

isexpr(ex::Expr, head)          = ex.head === head
isexpr(ex::Expr, heads::Union{Set,Vector,Tuple}) = in(ex.head, heads)
isexpr(ex,       head)          = false

isexpr(ex,       head, n::Int)  = isexpr(ex, head) && length(ex.args) == n


# ---- show_sexpr: print an AST as an S-expression ----

show_sexpr(ex) = show_sexpr(STDOUT, ex)
show_sexpr(io::IO, ex) = show_sexpr(io, ex, 0)
show_sexpr(io::IO, ex, indent::Int) = show(io, ex)

const sexpr_indent_width = 2

function show_sexpr(io::IO, ex::QuoteNode, indent::Int)
    inner = indent + sexpr_indent_width
    print(io, "(:quote, #QuoteNode\n", " "^inner)
    show_sexpr(io, ex.value, inner)
    print(io, '\n', " "^indent, ')')
end
function show_sexpr(io::IO, ex::Expr, indent::Int)
    inner = indent + sexpr_indent_width
    print(io, '(')
    show_sexpr(io, ex.head, inner)
    for arg in ex.args
        print(io, ex.head === :block ? ",\n"*" "^inner : ", ")
        show_sexpr(io, arg, inner)
    end
    if isempty(ex.args); print(io, ",)")
    else print(io, (ex.head === :block ? "\n"*" "^indent : ""), ')')
    end
end

"""
    @dump expr

Show every part of the representation of the given expression. Equivalent to
`dump(:(expr))`.
"""
macro dump(expr)
    dump(expr)
end

"""
    leaftypes(t::Type) -> Vector{Type}

Return an array containing all concrete subtypes of `t`, or only `t` if it is concrete.
"""
function leaftypes(t::Type)::Vector{Type}
    isleaftype(t) ? [t] : vcat(leaftypes.(subtypes(t))...)
end

"""
    abstracts(t::Type) -> Vector{Type}

Return an array containing all abstract subtypes of `t`, which may include `t`.
"""
function abstracts(t::Type)::Vector{Type}
    isleaftype(t) ? DataType[] : vcat(t, abstracts.(subtypes(t))...)
end

end # module
