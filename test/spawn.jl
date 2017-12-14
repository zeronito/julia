# This file is a part of Julia. License is MIT: https://julialang.org/license

###################################
# Cross Platform tests for spawn. #
###################################

valgrind_off = ccall(:jl_running_on_valgrind, Cint, ()) == 0

yescmd = `yes`
echocmd = `echo`
sortcmd = `sort`
printfcmd = `printf`
truecmd = `true`
falsecmd = `false`
catcmd = `cat`
shcmd = `sh`
sleepcmd = `sleep`
lscmd = `ls`
havebb = false
if Sys.iswindows()
    busybox = joinpath(JULIA_HOME, "busybox.exe")
    havebb = try # use busybox-w32 on windows, if available
        success(`$busybox`)
        true
    catch
        false
    end
    if havebb
        yescmd = `$busybox yes`
        echocmd = `$busybox echo`
        sortcmd = `$busybox sort`
        printfcmd = `$busybox printf`
        truecmd = `$busybox true`
        falsecmd = `$busybox false`
        catcmd = `$busybox cat`
        shcmd = `$busybox sh`
        sleepcmd = `$busybox sleep`
        lscmd = `$busybox ls`
    end
end

#### Examples used in the manual ####

@test read(`$echocmd hello \| sort`, String) == "hello | sort\n"
@test read(pipeline(`$echocmd hello`, sortcmd), String) == "hello\n"
@test length(spawn(pipeline(`$echocmd hello`, sortcmd)).processes) == 2

out = read(`$echocmd hello` & `$echocmd world`, String)
@test search(out,"world") != 0:-1
@test search(out,"hello") != 0:-1
@test read(pipeline(`$echocmd hello` & `$echocmd world`, sortcmd), String) == "hello\nworld\n"

@test (run(`$printfcmd "       \033[34m[stdio passthrough ok]\033[0m\n"`); true)

# Test for SIGPIPE being treated as normal termination (throws an error if broken)
Sys.isunix() && run(pipeline(yescmd, `head`, DevNull))

let a, p
    a = Base.Condition()
    @schedule begin
        p = spawn(pipeline(yescmd,DevNull))
        Base.notify(a,p)
        @test !success(p)
    end
    p = wait(a)
    kill(p)
end

if valgrind_off
    # If --trace-children=yes is passed to valgrind, valgrind will
    # exit here with an error code, and no UVError will be raised.
    @test_throws Base.UVError run(`foo_is_not_a_valid_command`)
end

if Sys.isunix()
    prefixer(prefix, sleep) = `sh -c "while IFS= read REPLY; do echo '$prefix ' \$REPLY; sleep $sleep; done"`
    @test success(pipeline(`sh -c "for i in 1 2 3 4 5 6 7 8 9 10; do echo \$i; sleep 0.1; done"`,
                       prefixer("A", 0.2) & prefixer("B", 0.2)))
    @test success(pipeline(`sh -c "for i in 1 2 3 4 5 6 7 8 9 10; do echo \$i; sleep 0.1; done"`,
                       prefixer("X", 0.3) & prefixer("Y", 0.3) & prefixer("Z", 0.3),
                       prefixer("A", 0.2) & prefixer("B", 0.2)))
end

@test  success(truecmd)
@test !success(falsecmd)
@test success(pipeline(truecmd, truecmd))
@test_broken  success(ignorestatus(falsecmd))
@test_broken  success(pipeline(ignorestatus(falsecmd), truecmd))
@test !success(pipeline(ignorestatus(falsecmd), falsecmd))
@test !success(ignorestatus(falsecmd) & falsecmd)
@test_broken  success(ignorestatus(pipeline(falsecmd, falsecmd)))
@test_broken  success(ignorestatus(falsecmd & falsecmd))

# STDIN Redirection
let file = tempname()
    run(pipeline(`$echocmd hello world`, file))
    @test read(pipeline(file, catcmd), String) == "hello world\n"
    @test open(x->read(x,String), pipeline(file, catcmd), "r") == "hello world\n"
    rm(file)
end

# Stream Redirection
if !Sys.iswindows() # WINNT reports operation not supported on socket (ENOTSUP) for this test
    local r = Channel(1)
    local port, server, sock, client, t1, t2
    t1 = @async begin
        port, server = listenany(2326)
        put!(r, port)
        client = accept(server)
        @test read(pipeline(client, catcmd), String) == "hello world\n"
        close(server)
        return true
    end
    t2 = @async begin
        sock = connect(fetch(r))
        run(pipeline(`$echocmd hello world`, sock))
        close(sock)
        return true
    end
    @test wait(t1)
    @test wait(t2)
end

@test read(setenv(`$shcmd -c "echo \$TEST"`,["TEST=Hello World"]), String) == "Hello World\n"
@test read(setenv(`$shcmd -c "echo \$TEST"`,Dict("TEST"=>"Hello World")), String) == "Hello World\n"
@test read(setenv(`$shcmd -c "echo \$TEST"`,"TEST"=>"Hello World"), String) == "Hello World\n"
@test (withenv("TEST"=>"Hello World") do
       read(`$shcmd -c "echo \$TEST"`, String); end) == "Hello World\n"
let pathA = readchomp(setenv(`$shcmd -c "pwd -P"`;dir="..")),
    pathB = readchomp(setenv(`$shcmd -c "cd .. && pwd -P"`))
    if Sys.iswindows()
        # on windows, sh returns posix-style paths that are not valid according to ispath
        @test pathA == pathB
    else
        @test Base.samefile(pathA, pathB)
    end
end

let str = "", stdin, stdout, proc, str2, file
    for i = 1:1000
      str = "$str\n $(randstring(10))"
    end

    # Here we test that if we close a stream with pending writes, we don't lose the writes.
    stdout, stdin, proc = readandwrite(`$catcmd -`)
    write(stdin, str)
    close(stdin)
    str2 = read(stdout, String)
    @test str2 == str

    # This test hangs if the end-of-run-walk-across-uv-streams calls shutdown on a stream that is shutting down.
    file = tempname()
    open(pipeline(`$catcmd -`, file), "w") do io
        write(io, str)
    end
    rm(file)
end

# issue #3373
# fixing up Conditions after interruptions
let r, t
    r = Channel(1)
    t = @async begin
        try
            wait(r)
        end
        p = spawn(`$sleepcmd 1`); wait(p)
        @test p.exitcode == 0
        return true
    end
    yield()
    schedule(t, InterruptException(), error=true)
    yield()
    put!(r,11)
    yield()
    @test wait(t)
end

# Test marking of IO
let r, t, sock
    r = Channel(1)
    t = @async begin
        port, server = listenany(2327)
        put!(r, port)
        client = accept(server)
        write(client, "Hello, world!\n")
        write(client, "Goodbye, world...\n")
        close(server)
        return true
    end
    sock = connect(fetch(r))
    mark(sock)
    @test ismarked(sock)
    @test readline(sock) == "Hello, world!"
    @test readline(sock) == "Goodbye, world..."
    @test reset(sock) == 0
    @test !ismarked(sock)
    mark(sock)
    @test ismarked(sock)
    @test readline(sock) == "Hello, world!"
    unmark(sock)
    @test !ismarked(sock)
    @test_throws ArgumentError reset(sock)
    @test !unmark(sock)
    @test readline(sock) == "Goodbye, world..."
    #@test eof(sock) ## doesn't work
    close(sock)
    @test wait(t)
end
# issue #4535
exename = Base.julia_cmd()
if valgrind_off
    # If --trace-children=yes is passed to valgrind, we will get a
    # valgrind banner here, not "Hello World\n".
    @test read(pipeline(`$exename --startup-file=no -e 'println(STDERR,"Hello World")'`, stderr=catcmd), String) == "Hello World\n"
    out = Pipe()
    proc = spawn(pipeline(`$exename --startup-file=no -e 'println(STDERR,"Hello World")'`, stderr = out))
    close(out.in)
    @test read(out, String) == "Hello World\n"
    @test success(proc)
end

# issue #6310
@test read(pipeline(`$echocmd "2+2"`, `$exename --startup-file=no`), String) == "4\n"

# issue #5904
@test run(pipeline(ignorestatus(falsecmd), truecmd)) === nothing

@testset "redirect_*" begin
    let OLD_STDOUT = STDOUT,
        fname = tempname(),
        f = open(fname,"w")

        redirect_stdout(f)
        println("Hello World")
        redirect_stdout(OLD_STDOUT)
        close(f)
        @test "Hello World\n" == read(fname, String)
        @test OLD_STDOUT === STDOUT
        rm(fname)
    end
end

# Test that redirecting an IOStream does not crash the process
let fname = tempname(), p
    cmd = """
    # Overwrite libuv memory before freeing it, to make sure that a use after free
    # triggers an assertion.
    function thrash(handle::Ptr{Void})
        # Kill the memory, but write a nice low value in the libuv type field to
        # trigger the right code path
        ccall(:memset, Ptr{Void}, (Ptr{Void}, Cint, Csize_t), handle, 0xee, 3 * sizeof(Ptr{Void}))
        unsafe_store!(convert(Ptr{Cint}, handle + 2 * sizeof(Ptr{Void})), 15)
        nothing
    end
    OLD_STDERR = STDERR
    redirect_stderr(open($(repr(fname)), "w"))
    # Usually this would be done by GC. Do it manually, to make the failure
    # case more reliable.
    oldhandle = OLD_STDERR.handle
    OLD_STDERR.status = Base.StatusClosing
    OLD_STDERR.handle = C_NULL
    ccall(:uv_close, Void, (Ptr{Void}, Ptr{Void}), oldhandle, cfunction(thrash, Void, Tuple{Ptr{Void}}))
    sleep(1)
    import Base.zzzInvalidIdentifier
    """
    try
        io = open(pipeline(`$exename --startup-file=no`, stderr=STDERR), "w")
        write(io, cmd)
        close(io)
        wait(io)
    catch
        error("IOStream redirect failed. Child stderr was \n$(read(fname, String))\n")
    finally
        rm(fname)
    end
end

# issue #10994: libuv can't handle strings containing NUL
let bad = "bad\0name"
    @test_throws ArgumentError run(`$bad`)
    @test_throws ArgumentError run(`$echocmd $bad`)
    @test_throws ArgumentError run(setenv(`$echocmd hello`, bad=>"good"))
    @test_throws ArgumentError run(setenv(`$echocmd hello`, "good"=>bad))
end

# issue #12829
let out = Pipe(), echo = `$exename --startup-file=no -e 'print(STDOUT, " 1\t", read(STDIN, String))'`, ready = Condition(), t, infd, outfd
    @test_throws ArgumentError write(out, "not open error")
    t = @async begin # spawn writer task
        open(echo, "w", out) do in1
            open(echo, "w", out) do in2
                notify(ready)
                write(in1, 'h')
                write(in2, UInt8['w'])
                println(in1, "ello")
                write(in2, "orld\n")
            end
        end
        infd = Base._fd(out.in)
        outfd = Base._fd(out.out)
        show(out, out)
        notify(ready)
        @test isreadable(out)
        @test iswritable(out)
        close(out.in)
        @test !isopen(out.in)
        @test !iswritable(out)
        if !Sys.iswindows()
            # on UNIX, we expect the pipe buffer is big enough that the write queue was immediately emptied
            # and so we should already be notified of EPIPE on out.out by now
            # and the other task should have already managed to consume all of the output
            # it takes longer to propagate EOF through the Windows event system
            # since it appears to be unwilling to buffer as much data
            @test !isopen(out.out)
            @test !isreadable(out)
        end
        @test_throws ArgumentError write(out, "now closed error")
        if Sys.iswindows()
            # WINNT kernel appears to not provide a fast mechanism for async propagation
            # of EOF for a blocking stream, so just wait for it to catch up.
            # This shouldn't take much more than 32ms.
            Base.wait_close(out)
            # it's closed now, but the other task is expected to be behind this task
            # in emptying the read buffer
            @test isreadable(out)
        end
        @test !isopen(out)
    end
    wait(ready) # wait for writer task to be ready before using `out`
    @test nb_available(out) == 0
    @test endswith(readuntil(out, '1'), '1')
    @test Char(read(out, UInt8)) == '\t'
    c = UInt8[0]
    @test c == read!(out, c)
    Base.wait_readnb(out, 1)
    @test nb_available(out) > 0
    ln1 = readline(out)
    ln2 = readline(out)
    desc = read(out, String)
    @test !isreadable(out)
    @test !iswritable(out)
    @test !isopen(out)
    @test infd != Base._fd(out.in) == Base.INVALID_OS_HANDLE
    @test outfd != Base._fd(out.out) == Base.INVALID_OS_HANDLE
    @test nb_available(out) == 0
    @test c == UInt8['w']
    @test lstrip(ln2) == "1\thello"
    @test ln1 == "orld"
    @test isempty(read(out))
    @test eof(out)
    @test desc == "Pipe($infd open => $outfd active, 0 bytes waiting)"
    wait(t)
end

# issue #8529
let fname = tempname()
    write(fname, "test\n")
    code = """
    $(if havebb
        "cmd = pipeline(`\$$(repr(busybox)) echo asdf`, `\$$(repr(busybox)) cat`)"
    else
        "cmd = pipeline(`echo asdf`, `cat`)"
    end)
    for line in eachline(STDIN)
        run(cmd)
    end
    """
    @test success(pipeline(`$catcmd $fname`, `$exename --startup-file=no -e $code`))
    rm(fname)
end

# Ensure that quoting works
@test Base.shell_split("foo bar baz") == ["foo", "bar", "baz"]
@test Base.shell_split("foo\\ bar baz") == ["foo bar", "baz"]
@test Base.shell_split("'foo bar' baz") == ["foo bar", "baz"]
@test Base.shell_split("\"foo bar\" baz") == ["foo bar", "baz"]

# "Over quoted"
@test Base.shell_split("'foo\\ bar' baz") == ["foo\\ bar", "baz"]
@test Base.shell_split("\"foo\\ bar\" baz") == ["foo\\ bar", "baz"]

# Ensure that shell_split handles quoted spaces
let cmd = ["/Volumes/External HD/program", "-a"]
    @test Base.shell_split("/Volumes/External\\ HD/program -a") == cmd
    @test Base.shell_split("'/Volumes/External HD/program' -a") == cmd
    @test Base.shell_split("\"/Volumes/External HD/program\" -a") == cmd
end

# Test shell_escape printing quoting
# Backticks should automatically quote where necessary
let cmd = ["foo bar", "baz", "a'b", "a\"b", "a\"b\"c", "-L/usr/+", "a=b", "``", "\$", "&&", "z"]
    @test string(`$cmd`) ==
        """`'foo bar' baz "a'b" 'a"b' 'a"b"c' -L/usr/+ a=b \\`\\` '\$' '&&' z`"""
    @test Base.shell_escape(`$cmd`) ==
        """'foo bar' baz "a'b" 'a"b' 'a"b"c' -L/usr/+ a=b `` '\$' && z"""
    @test Base.shell_escape_posixly(`$cmd`) ==
        """'foo bar' baz a\\'b a\\"b 'a"b"c' -L/usr/+ a=b '``' '\$' '&&' z"""
end
let cmd = ["foo=bar", "baz"]
    @test string(`$cmd`) == "`foo=bar baz`"
    @test Base.shell_escape(`$cmd`) == "foo=bar baz"
    @test Base.shell_escape_posixly(`$cmd`) == "'foo=bar' baz"
end


@test Base.shell_split("\"\\\\\"") == ["\\"]

# issue #13616
@test_throws ErrorException collect(eachline(pipeline(`$catcmd _doesnt_exist__111_`, stderr=DevNull)))

# make sure windows_verbatim strips quotes
if Sys.iswindows()
    read(`cmd.exe /c dir /b spawn.jl`, String) == read(Cmd(`cmd.exe /c dir /b "\"spawn.jl\""`, windows_verbatim=true), String)
end

# make sure Cmd is nestable
@test string(Cmd(Cmd(`ls`, detach=true))) == "`ls`"

# equality tests for Cmd
@test Base.Cmd(``) == Base.Cmd(``)
@test Base.Cmd(`lsof -i :9090`) == Base.Cmd(`lsof -i :9090`)
@test Base.Cmd(`$echocmd test`) == Base.Cmd(`$echocmd test`)
@test Base.Cmd(``) != Base.Cmd(`$echocmd test`)
@test Base.Cmd(``, ignorestatus=true) != Base.Cmd(``, ignorestatus=false)
@test Base.Cmd(``, dir="TESTS") != Base.Cmd(``, dir="TEST")
@test Base.Set([``, ``]) == Base.Set([``])
@test Set([``, echocmd]) != Set([``, ``])
@test Set([echocmd, ``, ``, echocmd]) == Set([echocmd, ``])

# equality tests for AndCmds
@test Base.AndCmds(`$echocmd abc`, `$echocmd def`) == Base.AndCmds(`$echocmd abc`, `$echocmd def`)
@test Base.AndCmds(`$echocmd abc`, `$echocmd def`) != Base.AndCmds(`$echocmd abc`, `$echocmd xyz`)

# test for correct error when an empty command is spawned (Issue 19094)
@test_throws ArgumentError run(Base.Cmd(``))
@test_throws ArgumentError run(Base.AndCmds(``, ``))
@test_throws ArgumentError run(Base.AndCmds(``, `$truecmd`))
@test_throws ArgumentError run(Base.AndCmds(`$truecmd`, ``))

@test_throws ArgumentError spawn(Base.Cmd(``))
@test_throws ArgumentError spawn(Base.AndCmds(``, ``))
@test_throws ArgumentError spawn(Base.AndCmds(``, `$echocmd test`))
@test_throws ArgumentError spawn(Base.AndCmds(`$echocmd test`, ``))

# tests for reducing over collection of Cmd
@test_throws ArgumentError reduce(&, Base.AbstractCmd[])
@test_throws ArgumentError reduce(&, Base.Cmd[])
@test reduce(&, [`$echocmd abc`, `$echocmd def`, `$echocmd hij`]) == `$echocmd abc` & `$echocmd def` & `$echocmd hij`

# test for proper handling of FD exhaustion
if Sys.isunix()
    let ps = Pipe[]
        ulimit_n = tryparse(Int, readchomp(`sh -c 'ulimit -n'`))
        try
            for i = 1 : 100 * coalesce(ulimit_n, 1000)
                p = Pipe()
                Base.link_pipe(p)
                push!(ps, p)
            end
            if ulimit_n === nothing
                @warn "`ulimit -n` is set to unlimited, fd exhaustion cannot be tested"
                @test_broken false
            else
                @test false
            end
        catch ex
            isa(ex, Base.UVError) || rethrow(ex)
            @test ex.code in (Base.UV_EMFILE, Base.UV_ENFILE)
        finally
            foreach(close, ps)
        end
    end
end

# readlines(::Cmd), accidentally broken in #20203
@test sort(readlines(`$lscmd -A`)) == sort(readdir())

# issue #19864 (PR #20497)
@test readchomp(pipeline(ignorestatus(
        `$exename --startup-file=no -e '
            struct Error19864 <: Exception; end
            Base.showerror(io::IO, e::Error19864) = print(io, "correct19864")
            throw(Error19864())'`),
    stderr=catcmd)) == "ERROR: correct19864"

# accessing the command elements as an array or iterator:
let c = `ls -l "foo bar"`
    @test collect(c) == ["ls", "-l", "foo bar"]
    @test first(c) == "ls" == c[1]
    @test last(c) == "foo bar" == c[3] == c[end]
    @test c[1:2] == ["ls", "-l"]
    @test eltype(c) == String
    @test length(c) == 3
    @test eachindex(c) == 1:3
end

## Deadlock in spawning a cmd (#22832)
# FIXME?
#let stdout = Pipe(), stdin = Pipe()
#    Base.link_pipe(stdout, julia_only_read=true)
#    Base.link_pipe(stdin, julia_only_write=true)
#    p = spawn(pipeline(catcmd, stdin=stdin, stdout=stdout, stderr=DevNull))
#    @async begin # feed cat with 2 MB of data (zeros)
#        write(stdin, zeros(UInt8, 1048576 * 2))
#        close(stdin)
#    end
#    sleep(0.5) # give cat a chance to fill the write buffer for stdout
#    close(stdout.in) # make sure we can still close the write end
#    @test sizeof(readstring(stdout)) == 1048576 * 2 # make sure we get all the data
#    @test success(p)
#end

# `kill` error conditions
let p = spawn(`$sleepcmd 100`)
    # Should throw on invalid signals
    @test_throws Base.UVError kill(p, typemax(Cint))
    kill(p)
    wait(p)
    # Should not throw if already dead
    kill(p)
end

