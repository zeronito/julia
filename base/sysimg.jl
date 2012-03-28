## Load essential files and libraries

include("base.jl")

# core operations & types
include("range.jl")
include("tuple.jl")
include("cell.jl")
include("expr.jl")
include("error.jl")

# core numeric operations & types
include("bool.jl")
include("number.jl")
include("int.jl")
include("promotion.jl")
include("operators.jl")
include("pointer.jl")

_jl_libfdm = dlopen("libfdm")

include("float.jl")
include("char.jl")
include("reduce.jl")
include("complex.jl")
include("rational.jl")

# core data structures (used by type inference)
include("abstractarray.jl")
include("subarray.jl")
include("array.jl")
include("intset.jl")
include("table.jl")
include("set.jl")

# compiler
include("inference.jl")

# I/O, strings & printing
include("io.jl")
#set_current_output_stream(make_stdout_stream()) # for error reporting
include("string.jl")
include("ascii.jl")
include("utf8.jl")
include("regex.jl")
include("show.jl")
include("grisu.jl")
include("printf.jl")

# system & environment
include("libc.jl")
include("env.jl")
include("errno_h.jl")

# concurrency and parallelism
include("iterator.jl")
include("task.jl")
include("process.jl")
include("serialize.jl")
include("multi.jl")

# front end
include("client.jl")

# core math functions
include("intfuncs.jl")
include("floatfuncs.jl")
include("math.jl")
include("math_libm.jl")
include("sort.jl")
include("combinatorics.jl")
include("statistics.jl")

# random number generation
include("random.jl")

# sparse matrices
include("sparse.jl")

# distributed arrays
include("darray.jl")

# utilities - version, timing, help, edit
include("version.jl")
include("util.jl")
include("datafmt.jl")

## Load optional external libraries

# linear algebra
include("linalg.jl")
include("linalg_blas.jl")
include("linalg_lapack.jl")
include("linalg_suitesparse.jl")

# signal processing
include("signal.jl")
include("signal_fftw.jl")


# prime method cache with some things we know we'll need right after startup
length(1:2:3)
(HashTable(0)[1])=()->()
numel(intset())
has(intset(),2)
del_all(FDSet())
start(HashTable(0))
done(HashTable(0),0)
get(HashTable(0), 0, ())
add(FDSet(),int32(0))
2==2.0
2.0==2.0
has(FDSet(),0)
isequal(int32(2),int32(2))
isequal(int64(2),int64(2))

compile_hint(getcwd, ())
compile_hint(fdio, (Int32,))
compile_hint(ProcessGroup, (Int, Array{Any,1}, Array{Any,1}))
compile_hint(select_read, (FDSet, Float64))
compile_hint(next, (HashTable{Any,Any}, Int))
compile_hint(start, (HashTable{Any,Any},))
compile_hint(perform_work, ())
compile_hint(isempty, (Array{Any,1},))
compile_hint(isempty, (Array{WorkItem,1},))
compile_hint(ref, (HashTable{Any,Any}, Int32))
compile_hint(event_loop, (Bool,))
compile_hint(_start, ())
compile_hint(_jl_color_available, ())
compile_hint(process_options, (Array{Any,1},))
compile_hint(run_repl, ())
compile_hint(anyp, (Function, Array{Any,1}))
compile_hint(HashTable, (Int,))
compile_hint(HashTable{Any,Any}, (Int,))
compile_hint(Set, ())
compile_hint(assign, (HashTable{Any,Any}, Bool, Cmd))
compile_hint(rehash, (HashTable{Any,Any}, Int))
compile_hint(run, (Cmd,))
compile_hint(spawn, (Cmd,))
compile_hint(assign, (HashTable{Any,Any}, Bool, FileDes))
compile_hint(wait, (Int32,))
compile_hint(system_error, (ASCIIString, Bool))
compile_hint(SystemError, (ASCIIString,))
compile_hint(has, (EnvHash, ASCIIString))
compile_hint(parse_input_line, (ASCIIString,))
compile_hint(cmp, (Int32, Int32))
compile_hint(min, (Int32, Int32))
compile_hint(==, (ASCIIString, ASCIIString))
compile_hint(arg_gen, (ASCIIString,))
compile_hint(_jl_librandom_init, ())
compile_hint(srand, (ASCIIString, Int))
compile_hint(open, (ASCIIString, Bool, Bool, Bool, Bool))
compile_hint(srand, (Uint64,))
compile_hint(done, (IntSet, Int64))
compile_hint(next, (IntSet, Int64))
compile_hint(ht_keyindex, (HashTable{Any,Any}, Int32))
compile_hint(perform_work, (WorkItem,))
compile_hint(notify_done, (WorkItem,))
compile_hint(work_result, (WorkItem,))
compile_hint(del_fd_handler, (Int32,))
compile_hint(enqueue, (Array{WorkItem,1}, WorkItem))
compile_hint(enq_work, (WorkItem,))
compile_hint(pop, (Array{WorkItem,1},))
compile_hint(string, (Int,))
compile_hint(parse_int, (Type{Int}, ASCIIString, Int))
compile_hint(repeat, (ASCIIString, Int))
compile_hint(KeyError, (Int,))
compile_hint(show, (Float64,))
compile_hint(match, (Regex, ASCIIString))
compile_hint(strlen, (ASCIIString,))
compile_hint(dims2string, (Tuple,))
compile_hint(alignment, (Float64,))
compile_hint(repl_callback, (Expr, Int))
compile_hint(istaskdone, (Task,))
compile_hint(make_stdout_stream, ())
compile_hint(set_current_output_stream, (IOStream,))
compile_hint(int, (Uint64,))
compile_hint(copy, (Bool,))
compile_hint(bool, (Bool,))
compile_hint(bool, (RemoteRef,))
compile_hint(wait, (RemoteRef,))
compile_hint(hash, (RemoteRef,))
compile_hint(take, (RemoteRef,))
compile_hint(bitmix, (Int, Int))
compile_hint(bitmix, (Uint, Int))
compile_hint(bitmix, (Uint64, Int64))
compile_hint(hash, (Int,))
compile_hint(isequal, (Symbol, Symbol))
compile_hint(isequal, (Bool, Bool))
compile_hint(WaitFor, (Symbol, RemoteRef))
compile_hint(_jl_answer_color, ())
compile_hint(get, (EnvHash, ASCIIString, ASCIIString))
compile_hint(notify_empty, (WorkItem,))
compile_hint(rr2id, (RemoteRef,))
compile_hint(isequal, (RemoteRef, WeakRef))
compile_hint(isequal, (RemoteRef, RemoteRef))
compile_hint(_ieval, (Symbol,))
compile_hint(static_convert, (Any, Any))
compile_hint(assign, (Array{Any,1}, WeakRef, Int))
compile_hint(hash, (Tuple,))
compile_hint(assign, (HashTable{Any,Any}, WorkItem, (Int,Int)))
compile_hint(isequal, ((Int,Int),(Int,Int)))
compile_hint(RemoteRef, (Int, Int, Int))
compile_hint(inlining_pass, (LambdaStaticData, Array{Any,1}))
compile_hint(_jl_eval_user_input, (Expr, Bool))
compile_hint(print, (Float64,))
compile_hint(a2t, (Array{Any,1},))
compile_hint(flush, (IOStream,))
compile_hint(ref, (Type{String}, ASCIIString, ASCIIString, ASCIIString))
