using .Markdown
using Base.Markdown: MD

cd(dirname(@__FILE__))

isop(func) = ismatch(r"[^\w@!.]|^!$", func)

ident(mod, x) = "$mod.$(isop(x) ? "(:($x))" : x)"

function getdoc(mod, x)
    try
        mod = foldl((x,y) -> getfield(x,symbol(y)), Main, split(mod, "."))
        return collect(values(Docs.meta(Base)[getfield(mod, symbol(x))].meta))
    catch e
        println(e)
        warn("Mod $mod $x")
    end
    []
end

flat_content(md) = md
flat_content(xs::Vector) = reduce((xs, x) -> vcat(xs,flat_content(x)), [], xs)
flat_content(md::MD) = flat_content(md.content)

flatten(md::MD) = MD(flat_content(md))

isrst(md) =
    length(flatten(md).content) == 1 &&
    isa(flatten(md).content[1], Markdown.Code) &&
    flatten(md).content[1].language == "rst"

function tryrst(md)
    try
        return Markdown.rst(md)
    catch e
        warn("Error converting docstring:")
        display(md)
        return
    end
end

torst(md) = isrst(md) ? flatten(md).content[1].code : tryrst(md)

function translate(file)
    @assert(isfile(file))
    ls = split(readall(file), "\n")[1:end-1]
    doccing = false
    func = nothing
    mod = "Base"
    modidx = -1
    open(file, "w+") do io
        for (i,l) in enumerate(ls)
            if ismatch(r"^\.\. (current)?module::", l)
                mod = match(r"^\.\. (current)?module:: ([\w\.]+)", l).captures[2]
                modidx = i
                println(io, l)
            elseif startswith(l, ".. function::")
                func = match(r".. function:: (@?[^\(\s\{]+)(.*)", l)
                func == nothing && (warn("bad function $l"); continue)
                funcname = func.captures[1]
                rest = func.captures[2]
                full = funcname*rest
                doc = nothing
                for mdoc in getdoc(mod, funcname)
                    trst = tryrst(mdoc)
                    trst !== nothing || continue
                    if contains(trst, full)
                        if doc != nothing
                            error("duplicate $full $found $l")
                        end
                        doc = mdoc
                    end
                end
                if doc == nothing || torst(doc) == nothing
                    info("no docs for $(ident(mod, funcname))")
                    println(io, l)
                    doccing = false
                    continue
                end
                doccing = true
                println(io, l)
                println(io)
                println(io, "   .. Docstring generated from Julia source\n")
                for l in split(torst(doc), "\n")
                    ismatch(r"^\s*$", l) ? println(io) : println(io, "   ", l)
                end
                isrst(doc) && println(io)
            elseif doccing && (startswith(l, "   ") || ismatch(r"^\s*$", l))
                modidx == i-1 && println(io)
            else
                doccing = false
                println(io, l)
            end
        end
    end
end

for folder in ["stdlib", "manual", "devdocs"]
    println("\nConverting $folder/\n")
    for file in readdir("$folder")
        translate("$folder/$file")
    end
end
