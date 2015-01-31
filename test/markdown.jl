using Base.Markdown
import Base.Markdown: MD, Paragraph, Italic, Bold, plain, term, html, Table, Code
import Base: writemime

# Basics
# Equality is checked by making sure the HTML output is
# the same – the structure itself may be different.

@test md"foo" == MD(Paragraph("foo"))
@test md"foo *bar* baz" == MD(Paragraph(["foo ", Italic("bar"), " baz"]))

@test md"**foo *bar* baz**" == MD(Paragraph(Bold(["foo ", Italic("bar"), " baz"])))
# @test md"**foo *bar* baz**" == MD(Paragraph(Italic(["foo ", Bold("bar"), " baz"])))

@test md"Foo [bar]" == MD(Paragraph("Foo [bar]"))
@test md"Foo [bar](baz)" != MD(Paragraph("Foo [bar](baz)"))
@test md"Foo \[bar](baz)" == MD(Paragraph("Foo [bar](baz)"))

# Basic plain (markdown) output

@test md"foo" |> plain == "foo\n"
@test md"foo *bar* baz" |> plain == "foo *bar* baz\n"

# HTML output

@test md"foo *bar* baz" |> html == "<p>foo <em>bar</em> baz</p>\n"

# Interpolation / Custom types

type Reference
    ref
end

ref(x) = Reference(x)

ref(fft)

writemime(io::IO, m::MIME"text/plain", r::Reference) =
    print(io, "$(r.ref) (see Julia docs)")

@test md"Behaves like $(ref(fft))" == md"Behaves like fft (see Julia docs)"

# GH tables
using Base.Test

@test md"""
    a  | b
    ---|---
    1  | 2""" == MD(Table(Any[["a","b"],
                              ["1","2"]], [:r, :r]))

@test md"""
    | a  |  b | c |
    | :-- | --: | --- |
    | d`gh`hg | hgh**jhj**ge | f |""" == MD(Table(Any[["a","b","c"],
                                                      Any[["d",Code("gh"),"hg"],
                                                          ["hgh",Bold("jhj"),"ge"],
                                                          "f"]],
                                                  [:l, :r, :r]))
