using Base.Markdown
import Base.Markdown: MD, Paragraph, Italic, Bold, plain, term, html
import Base: writemime

# Basics
# Equality is checked by making sure the HTML output is
# the same – the structure itself may be different.

@test md"foo" == MD(Paragraph("foo"))
@test md"foo *bar* baz" == MD(Paragraph(["foo ", Italic("bar"), " baz"]))

@test md"#title" == Markdown.MD(Markdown.Header{1}("title"))
@test md"## section" == Markdown.MD(Markdown.Header{2}("section"))
@test md"# title *foo* `bar` **baz**" == Markdown.MD(Markdown.Header{1}(["title ",
    Markdown.Italic("foo")," ",Markdown.Code("bar")," ",Markdown.Bold("baz")]))

@test md"**foo *bar* baz**" == MD(Paragraph(Bold(["foo ", Italic("bar"), " baz"])))
# @test md"**foo *bar* baz**" == MD(Paragraph(Italic(["foo ", Bold("bar"), " baz"])))

@test md"Foo [bar]" == MD(Paragraph("Foo [bar]"))
@test md"Foo [bar](baz)" != MD(Paragraph("Foo [bar](baz)"))
@test md"Foo \[bar](baz)" == MD(Paragraph("Foo [bar](baz)"))

# Basic plain (markdown) output

@test md"foo" |> plain == "foo\n"
@test md"foo *bar* baz" |> plain == "foo *bar* baz\n"
@test md"#title" |> plain == "# title\n"
@test md"## section" |> plain == "## section\n"
@test md"## section `foo`" |> plain == "## section `foo`\n"

# HTML output

@test md"foo *bar* baz" |> html == "<p>foo <em>bar</em> baz</p>\n"
@test md"# title *blah*" |> html == "<h1>title <em>blah</em></h1>\n"
@test md"## title *blah*" |> html == "<h2>title <em>blah</em></h2>\n"

# Interpolation / Custom types

type Reference
    ref
end

ref(x) = Reference(x)

ref(fft)

writemime(io::IO, m::MIME"text/plain", r::Reference) =
    print(io, "$(r.ref) (see Julia docs)")

@test md"Behaves like $(ref(fft))" == md"Behaves like fft (see Julia docs)"
