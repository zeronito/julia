# Install dependencies needed to build the documentation.
ENV["JULIA_PKGDIR"] = joinpath(@__DIR__, "deps")
using Pkg
Pkg.init()
cp(joinpath(@__DIR__, "REQUIRE"), Pkg.dir("REQUIRE"); remove_destination = true)
Pkg.update()
Pkg.resolve()

using Documenter

# Include the `build_sysimg` file.

baremodule GenStdLib end
@isdefined(build_sysimg) || @eval module BuildSysImg
    include(joinpath(@__DIR__, "..", "contrib", "build_sysimg.jl"))
end

# Documenter Setup.

symlink_q(tgt, link) = isfile(link) || symlink(tgt, link)
cp_q(src, dest) = isfile(dest) || cp(src, dest)

# make links for stdlib package docs, this is needed until #522 in Documenter.jl is finished
const STDLIB_DOCS = []
const STDLIB_DIR = joinpath(@__DIR__, "..", "stdlib")
cd(joinpath(@__DIR__, "src")) do
    Base.rm("stdlib"; recursive=true, force=true)
    mkdir("stdlib")
    for dir in readdir(STDLIB_DIR)
        sourcefile = joinpath(STDLIB_DIR, dir, "docs", "src", "index.md")
        if isfile(sourcefile)
            targetfile = joinpath("stdlib", dir * ".md")
            push!(STDLIB_DOCS, (stdlib = Symbol(dir), targetfile = targetfile))
            if Sys.iswindows()
                cp_q(sourcefile, targetfile)
            else
                symlink_q(sourcefile, targetfile)
            end
        end
    end
end

# Generate a suitable markdown file from NEWS.md and put it in src
str = read(joinpath(@__DIR__, "..", "NEWS.md"), String)
splitted = split(str, "<!--- generated by NEWS-update.jl: -->")
@assert length(splitted) == 2
replaced_links = replace(splitted[1], r"\[\#([0-9]*?)\]" => s"[#\g<1>](https://github.com/JuliaLang/julia/issues/\g<1>)")
write(joinpath(@__DIR__, "src", "NEWS.md"), replaced_links)

const PAGES = [
    "Home" => "index.md",
    hide("NEWS.md"),
    "Manual" => [
        "manual/introduction.md",
        "manual/getting-started.md",
        "manual/variables.md",
        "manual/integers-and-floating-point-numbers.md",
        "manual/mathematical-operations.md",
        "manual/complex-and-rational-numbers.md",
        "manual/strings.md",
        "manual/functions.md",
        "manual/control-flow.md",
        "manual/variables-and-scoping.md",
        "manual/types.md",
        "manual/methods.md",
        "manual/constructors.md",
        "manual/conversion-and-promotion.md",
        "manual/interfaces.md",
        "manual/modules.md",
        "manual/documentation.md",
        "manual/metaprogramming.md",
        "manual/arrays.md",
        "manual/missing.md",
        "manual/networking-and-streams.md",
        "manual/parallel-computing.md",
        "manual/dates.md",
        "manual/running-external-programs.md",
        "manual/calling-c-and-fortran-code.md",
        "manual/handling-operating-system-variation.md",
        "manual/environment-variables.md",
        "manual/embedding.md",
        "manual/packages.md",
        "manual/profile.md",
        "manual/stacktraces.md",
        "manual/performance-tips.md",
        "manual/workflow-tips.md",
        "manual/style-guide.md",
        "manual/faq.md",
        "manual/noteworthy-differences.md",
        "manual/unicode-input.md",
    ],
    "Base" => [
        "base/base.md",
        "base/collections.md",
        "base/math.md",
        "base/numbers.md",
        "base/strings.md",
        "base/arrays.md",
        "base/parallel.md",
        "base/multi-threading.md",
        "base/constants.md",
        "base/file.md",
        "base/io-network.md",
        "base/punctuation.md",
        "base/sort.md",
        "base/iterators.md",
        "base/c.md",
        "base/libc.md",
        "base/stacktraces.md",
        "base/simd-types.md",
    ],
    "Standard Library" =>
        [stdlib.targetfile for stdlib in STDLIB_DOCS],
    "Developer Documentation" => [
        "devdocs/reflection.md",
        "Documentation of Julia's Internals" => [
            "devdocs/init.md",
            "devdocs/ast.md",
            "devdocs/types.md",
            "devdocs/object.md",
            "devdocs/eval.md",
            "devdocs/callconv.md",
            "devdocs/compiler.md",
            "devdocs/functions.md",
            "devdocs/cartesian.md",
            "devdocs/meta.md",
            "devdocs/subarrays.md",
            "devdocs/sysimg.md",
            "devdocs/llvm.md",
            "devdocs/stdio.md",
            "devdocs/boundscheck.md",
            "devdocs/locks.md",
            "devdocs/offset-arrays.md",
            "devdocs/require.md",
            "devdocs/inference.md",
        ],
        "Developing/debugging Julia's C code" => [
            "devdocs/backtraces.md",
            "devdocs/debuggingtips.md",
            "devdocs/valgrind.md",
            "devdocs/sanitizers.md",
        ]
    ],
]

for stdlib in STDLIB_DOCS
    @eval using $(stdlib.stdlib)
end

makedocs(
    build     = joinpath(pwd(), "_build/html/en"),
    modules   = [Base, Core, BuildSysImg, [Base.root_module(Base, stdlib.stdlib) for stdlib in STDLIB_DOCS]...],
    clean     = true,
    doctest   = "doctest" in ARGS,
    linkcheck = "linkcheck" in ARGS,
    linkcheck_ignore = ["https://bugs.kde.org/show_bug.cgi?id=136779"], # fails to load from nanosoldier?
    strict    = false,
    checkdocs = :none,
    format    = "pdf" in ARGS ? :latex : :html,
    sitename  = "The Julia Language",
    authors   = "The Julia Project",
    analytics = "UA-28835595-6",
    pages     = PAGES,
    html_prettyurls = ("deploy" in ARGS),
    html_canonical = ("deploy" in ARGS) ? "https://docs.julialang.org/en/stable/" : nothing,
)

if "deploy" in ARGS
    # Only deploy docs from 64bit Linux to avoid committing multiple versions of the same
    # docs from different workers.
    (Sys.ARCH === :x86_64 && Sys.KERNEL === :Linux) || return

    # Since the `.travis.yml` config specifies `language: cpp` and not `language: julia` we
    # need to manually set the version of Julia that we are deploying the docs from.
    ENV["TRAVIS_JULIA_VERSION"] = "nightly"

    deploydocs(
        repo = "github.com/JuliaLang/julia.git",
        target = "_build/html/en",
        dirname = "en",
        deps = nothing,
        make = nothing,
    )
end
