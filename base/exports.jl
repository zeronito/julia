
export
# Modules
    PCRE,
    FFTW,
    Collections,
    DSP,
    LinAlg,
    LibRandom,
    Random,
    Math,
    MPFR,
    GMP,
    QuadGK,
    Sort,
    Sys,
    Test,
    Pkg,
    Operators,
    Errno,
    Meta,
    Graphics,

# Types
    AbstractMatrix,
    AbstractSparseMatrix,
    AbstractVector,
    Array,
    Associative,
    AsyncStream,
    Bidiagonal,
    BitArray,
    BigFloat,
    BigInt,
    BitMatrix,
    BitVector,
    CharString,
    Cmd,
    Colon,
    Complex,
    Complex128,
    Complex64,
    DArray,
    Dict,
    Dims,
    EachLine,
    Enumerate,
    EnvHash,
    FileMonitor,
    FileOffset,
    Filter,
    IO,
    IOStream,
    IOBuffer,
    ImaginaryUnit,
    IntSet,
    LocalProcess,
    MathConst,
    Matrix,
    ObjectIdDict,
    PollingFileWatcher,
    ProcessGroup,
    Range,
    Range1,
    RangeIndex,
    Ranges,
    Rational,
    Regex,
    RegexMatch,
    RegexMatchIterator,
    RemoteRef,
    RepString,
    RevString,
    Reverse,
    RopeString,
    Set,
    SparseMatrixCSC,
    SpawnNullStream,
    StridedArray,
    StridedMatrix,
    StridedVecOrMat,
    StridedVector,
    SubArray,
    SubDArray,
    SubOrDArray,
    SubString,
    SymTridiagonal,
    TcpSocket,
    TmStruct,
    Tridiagonal,
    UVError,
    VecOrMat,
    Vector,
    VersionNumber,
    WeakKeyDict,
    WeakRef,
    Woodbury,
    Zip,
    Stat,
    Factorization,
    BunchKaufman,
    Cholesky,
    CholeskyPivoted,
    Eigen,
    GeneralizedSchur,
    GeneralizedSVD,
    Hessenberg,
    LU,
    LUTridiagonal,
    LDLTTridiagonal,
    QR,
    QRPivoted,
    Schur,
    SVD,
    GeneralizedSVD,
    Hermitian,
    Triangular,
    Diagonal,
    InsertionSort,
    QuickSort,
    MergeSort,
    TimeoutAsyncWork,
    TimSort,

# Ccall types
    Cchar,
    Cuchar,
    Cshort,
    Cushort,
    Cint,
    Cuint,
    Clong,
    Culong,
    Cptrdiff_t,
    Csize_t,
    Cssize_t,
    Clonglong,
    Culonglong,
    Coff_t,
    Cfloat,
    Cdouble,
    Cwchar_t,
    #Ccomplex_float,
    #Ccomplex_double,

# Exceptions
    ArgumentError,
    DisconnectException,
    EOFError,
    ErrorException,
    KeyError,
    LoadError,
    MethodError,
    ParseError,
    SystemError,
    TypeError,

# Global constants and variables
    ARGS,
    C_NULL,
    CPU_CORES,
    OS_NAME,
    ENDIAN_BOM,
    ENV,
    Inf,
    Inf32,
    LOAD_PATH,
    MS_ASYNC,
    MS_INVALIDATE,
    MS_SYNC,
    NaN,
    NaN32,
    OUTPUT_STREAM,
    RTLD_LOCAL,
    RTLD_GLOBAL,
    RTLD_LAZY,
    RTLD_NOW,
    RTLD_NOLOAD,
    RTLD_NODELETE,
    RTLD_DEEPBIND,
    RTLD_FIRST,
    STDERR,
    STDIN,
    STDOUT,
    VERSION,
    WORD_SIZE,

# Mathematical constants
    im,
    π, pi,
    e, eu,
    γ, eulergamma,
    catalan,
    φ, golden,

# Operators
    !,
    !=,
    !==,
    $,
    %,
    &,
    *,
    +,
    -,
    .!=,
    .+,
    .-,
    .*,
    ./,
    .<,
    .<=,
    .==,
    .>,
    .>=,
    .\,
    .^,
    /,
    //,
    <,
    <:,
    <<,
    <=,
    ==,
    >,
    >=,
    >>,
    #.>>,
    #.<<,
    >>>,
    \,
    ^,
    |,
    ~,
    :,
    A_ldiv_Bc,
    A_ldiv_Bt,
    A_mul_B,
    A_mul_Bc,
    A_mul_Bt,
    A_rdiv_Bc,
    A_rdiv_Bt,
    Ac_ldiv_B,
    Ac_ldiv_Bc,
    #Ac_mul_b_RFP,
    Ac_mul_B,
    Ac_mul_Bc,
    Ac_rdiv_B,
    Ac_rdiv_Bc,
    At_ldiv_B,
    At_ldiv_Bt,
    At_mul_B,
    At_mul_Bt,
    At_rdiv_B,
    At_rdiv_Bt,

# scalar math
    abs,
    abs2,
    acos,
    acosd,
    acosh,
    acot,
    acotd,
    acoth,
    acsc,
    acscd,
    acsch,
    angle,
    asec,
    asecd,
    asech,
    asin,
    asind,
    asinh,
    atan,
    atan2,
    atand,
    atanh,
    big,
    bitmix,
    bool,
    binomial,
    bswap,
    cbrt,
    ceil,
    cis,
    clamp,
    cmp,
    combinations,
    complex,
    complex128,
    complex64,
    conj,
    copysign,
    cos,
    cosc,
    cosd,
    cosh,
    cot,
    cotd,
    coth,
    count_ones,
    count_zeros,
    csc,
    cscd,
    csch,
    dawson,
    degrees2radians,
    den,
    digamma,
    div,
    eps,
    erf,
    erfc,
    erfcx,
    erfi,
    erfinv,
    erfcinv,
    exp,
    exp2,
    exp10,
    expm1,
    exponent,
    factor,
    factorial,
    fld,
    flipsign,
    float,
    float32,
    float64,
    floor,
    frexp,
    gamma,
    gcd,
    gcdx,
    hex2num,
    hypot,
    iceil,
    ifloor,
    imag,
    inf,
    int,
    int128,
    int16,
    int32,
    int64,
    int8,
    integer,
    integer_partitions,
    inv,
    invdigamma,
    invmod,
    iround,
    iseltype,
    iseven,
    isfinite,
    isfloat64,
    isinf,
    isinteger,
    isnan,
    isodd,
    ispow2,
    isprime,
    isqrt,
    isreal,
    issubnormal,
    itrunc,
    lcm,
    ldexp,
    leading_ones,
    leading_zeros,
    lfact,
    lgamma,
    log,
    log10,
    log1p,
    log2,
    maxintfloat,
    mod,
    mod1,
    modf,
    nan,
    nextfloat,
    nextpow,
    nextpow2,
    num,
    num2hex,
    one,
    powermod,
    prevfloat,
    prevpow,
    prevpow2,
    primes,
    radians2degrees,
    rationalize,
    real,
    realmax,
    realmin,
    reim,
    reinterpret,
    rem,
    round,
    sec,
    secd,
    sech,
    sign,
    signbit,
    signed,
    signif,
    significand,
    sin,
    sinc,
    sind,
    sinh,
    sqrt,
    square,
    tan,
    tand,
    tanh,
    trailing_ones,
    trailing_zeros,
    trigamma,
    trunc,
    uint,
    uint128,
    uint16,
    uint32,
    uint64,
    uint8,
    unsigned,
    zero,
    nextprod,
    prevprod,
    typemax,
    typemin,

# specfun
    airy,
    airyai,
    airyprime,
    airyaiprime,
    airybi,
    airybiprime,
    besselj0,
    besselj1,
    besselj,
    bessely0,
    bessely1,
    bessely,
    hankelh1,
    hankelh2,
    besseli,
    besselk,
    besselh,
    beta,
    lbeta,
    eta,
    polygamma,
    zeta,

# arrays
    mapslices,
    reducedim,
    bsxfun,
    broadcast,
    broadcast!,
    broadcast_function,
    broadcast!_function,
    broadcast_getindex,
    broadcast_setindex!,
    cartesianmap,
    cat,
    cell,
    circshift,
    colon,
    conj!,
    copy!,
    cumprod,
    cumsum,
    cumsum_kbn,
    cummin,
    cummax,
    fill,
    fill!,
    find,
    findin,
    findmax,
    findmin,
    findn,
    findnz,
    findfirst,
    findnext,
    first,
    flipdim,
    fliplr,
    flipud,
    full,
    gradient,
    hcat,
    hvcat,
    ind2sub,
    indmax,
    indmin,
    invperm,
    ipermute!,
    ipermutedims,
    isperm,
    issorted,
    last,
    linspace,
    logspace,
    max,
    min,
    ndims,
    nnz,
    nonzeros,
    nthperm,
    nthperm!,
    ones,
    partitions,
    pascal,
    permute!,
    permutedims,
    prod,
    promote_shape,
    randcycle,
    randperm,
    repmat,
    reshape,
    reverse,
    reverse!,
    rot180,
    rotl90,
    rotr90,
    searchsorted,
    searchsortedfirst,
    searchsortedlast,
    select,
    select!,
    shuffle!,
    size,
    slice,
    slicedim,
    sort,
    sort!,
    sortby,
    sortby!,
    sortperm,
    sortrows,
    sortcols,
    squeeze,
    step,
    stride,
    strides,
    sub,
    sub2ind,
    sum,
    sum_kbn,
    vcat,
    vec,
    zeros,
    index_shape,
    setindex_shape_check,
    checkbounds,

# linear algebra
    chol,
    cholfact,
    cholfact!,
    cholpfact,
    cholpfact!,
    cond,
    cross,
    ctranspose,
    det,
    diag,
    diagind,
    diagm,
    diff,
    dot,
    eig,
    eigfact,
    eigfact!,
    eigmax,
    eigmin,
    eigs,
    eigvals,
    eigvecs,
    expm,
    sqrtm,
    eye,
    hessfact,
    hessfact!,
    ishermitian,
    isposdef,
    isposdef!,
    issym,
    istril,
    istriu,
    kron,
    ldltd!,
    ldltd,
    linreg,
    logdet,
    lu,
    lufact,
    lufact!,
    norm,
    normfro,
    null,
    pinv,
    qr,
    qrfact!,
    qrfact,
    qrp,
    qrpfact!,
    qrpfact,
    randsym,
    rank,
    rref,
    scale,
    scale!,
    schur,
    schurfact,
    schurfact!,
    solve,
    svd,
    svdfact!,
    svdfact,
    svds,
    svdvals!,
    svdvals,
    symmetrize!,
    trace,
    transpose,
    tril,
    triu,
    tril!,
    triu!,

# sparse
    dense,
    full,
    etree,
    issparse,
    sparse,
    sparsevec,
    spdiagm,
    speye,
    spones,
    sprand,
    sprandbool,
    sprandn,
    spzeros,

# bitarrays
    bitpack,
    bitunpack,
    falses,
    flipbits!,
    rol,
    ror,
    trues,

# dequeues
    append!,
    prepend!,
    resize!,
    insert!,
    shift!,
    unshift!,
    pop!,
    push!,

# collections
    add!,
    all,
    any,
    collect,
    complement,
    complement!,
    contains,
    count,
    delete!,
    empty!,
    endof,
    eltype,
    get,
    getindex,
    haskey,
    intersect,
    intersect!,
    isempty,
    issubset,
    getkey,
    keys,
    length,
    map,
    map!,
    mapreduce,
    merge,
    merge!,
    reduce,
    setindex!,
    sizehint,
    similar,
    setdiff,
    setdiff!,
    splice!,
    symdiff,
    symdiff!,
    union,
    union!,
    unique,
    values,
    filter,
    filter!,

# strings and text output
    ascii,
    beginswith,
    char,
    charwidth,
    chomp,
    chop,
    chr2ind,
    bytestring,
    eachmatch,
    endswith,
    escape_string,
    first_utf8_byte,
    ind2chr,
    is_utf8_start,
    is_valid_ascii,
    is_valid_utf8,
    is_valid_char,
    isvalid,
    isalnum,
    isalpha,
    isascii,
    isblank,
    iscntrl,
    isdigit,
    isgraph,
    islower,
    isprint,
    ispunct,
    isspace,
    isupper,
    isxdigit,
    join,
    lcfirst,
    lowercase,
    lpad,
    lstrip,
    match,
    ismatch,
    nextind,
    prevind,
    replace,
    rpad,
    rsearch,
    rstrip,
    search,
    split,
    string,
    strip,
    strwidth,
    thisind,
    ucfirst,
    uppercase,
    utf8,
    randstring,
    bin,
    bits,
    dec,
    dump,
    float32_isvalid,
    float64_isvalid,
    hex,
    xdump,
    ndigits,
    ndigits0z,
    digits,
    oct,
    parsefloat,
    parseint,
    hex2bytes,
    bytes2hex,
    print,
    print_escaped,
    print_joined,
    print_matrix,
    print_quoted,
    print_quoted_literal,
    print_shortest,
    print_unescaped,
    print_unescaped_chars,
    println,
    repeat,
    repl_show,
    show,
    showall,
    showcompact,
    sprint,
    repr,
    summary,
    unescape_chars,
    unescape_string,
    base,
    print_with_color,
    info,
    warn,

# random numbers
    AbstractRNG,
    MersenneTwister,
    rand!,
    rand,
    randbool!,
    randbool,
    randn!,
    randn,
    srand,

# bigfloat & precision
    get_precision,
    get_bigfloat_precision,
    set_bigfloat_precision,
    with_bigfloat_precision,
    get_bigfloat_rounding,
    set_bigfloat_rounding,
    with_bigfloat_rounding,

# statistics
    cor,
    cov,
    hist,
    hist2d,
    histrange,
    mean,
    median!,
    median,
    midpoints,
    quantile!,
    quantile,
    std,
    stdm,
    var,
    varm,

# signal processing
    bfft,
    bfft!,
    plan_bfft,
    plan_bfft!,
    brfft,
    plan_brfft,
    conv,
    conv2,
    deconv,
    fft,
    fft!,
    plan_fft,
    plan_fft!,
    fftshift,
    filt,
    ifft,
    ifft!,
    plan_ifft,
    plan_ifft!,
    ifftshift,
    irfft,
    plan_irfft,
    rfft,
    plan_rfft,
    xcorr,
    dct,
    idct,
    dct!,
    idct!,
    plan_dct,
    plan_idct,
    plan_dct!,
    plan_idct!,

#   numerical integration
    quadgk,

# iteration
    start,
    done,
    next,
    enumerate,
    zip,

# object identity and equality
    copy,
    deepcopy,
    deepcopy_internal,
    isequal,
    isless,
    hash,
    identity,
    object_id,
    sizeof,
    isimmutable,
    isbits,

# tasks
    consume,
    current_task,
    istaskdone,
    produce,
    task_local_storage,

# time
    sleep,
    strftime,
    strptime,
    tic,
    time,
    time_ns,
    toc,
    toq,

# errors
    assert,
    error,
    rethrow,
    backtrace,
    catch_backtrace,
    systemerror,

# types
    convert,
    isleaftype,
    oftype,
    promote,
    promote_rule,
    promote_type,
    super,
    subtypes,
    typeintersect,
    typejoin,

# syntax
    expand,
    macroexpand,
    esc,
    gensym,
    parse,
    symbol,

# help and reflection
    ans,
    apropos,
    functionloc,
    functionlocs,
    edit,
    methods,
    methodswith,
    help,
    less,
    names,
    module_name,
    module_parent,
    current_module,
    versioninfo,
    which,
    whicht,
    whos,
    isinteractive,
    disassemble,
    finfer,
    isconst,
    isgeneric,

# loading source files
    evalfile,
    include,
    include_string,
    reload,
    require,

# RTS internals
    precompile,
    finalizer,
    gc,
    gc_disable,
    gc_enable,

# misc
    exit,
    quit,
    atexit,
    ntuple,
    peakflops,
    tty_cols,
    tty_rows,

# IP address stuff
    IPv4, 
    IPv6, 
    parse_ipv4,
    parse_ipv6,
    @ip_str,

# I/O and events
    accept,
    listen,
    bind,
    connect,
    close,
    isopen,
    countlines,
    readcsv,
    writecsv,
    deserialize,
    readdlm,
    writedlm,
    eachline,
    eatwspace,
    eatwspace_comment,
    eof,
    fd,
    fdio,
    FDWatcher,
    UV_READABLE,
    UV_WRITEABLE,
    flush,
    gethostname,
    getipaddr,
    htol,
    hton,
    ltoh,
    ntoh,
    memio,
    mmap,
    mmap_array,
    mmap_bitarray,
    mmap_grow,
    mmap_stream_settings,
    msync,
    munmap,
    nb_available,
    open,
    open_any_tcp_port,
    OS_FD,
    OS_SOCKET,
    PipeBuffer,
    position,
    read,
    readall,
    readchomp,
    readdir,
    readline,
    readlines,
    readuntil,
    readavailable,
    seek,
    seekend,
    seekstart,
    serialize,
    skip,
    start_reading,
    start_watching,
    stop_reading,
    start_timer,
    stop_timer,
    poll_fd,
    poll_file,
    takebuf_array,
    takebuf_string,
    truncate,
    uv_error,
    write,

# multiprocessing
    addprocs,
    addprocs_sge,
    fetch,
    isready,
    yield,
    myid,
    nprocs,
    nworkers,
    procs,
    workers,
    rmprocs,
    pmap,
    put,
    remotecall,
    remotecall_fetch,
    remotecall_wait,
    take,
    wait,

# distributed arrays
    distribute,
    dfill,
    dones,
    drand,
    drandn,
    dzeros,
    localpart,
    myindexes,
    procs,

# paths and file names
    splitdir,
    splitdrive,
    splitext,
    dirname,
    basename,
    isabspath,
    isdirpath,
    joinpath,
    normpath,
    abspath,
    realpath,
    expanduser,

# filesystem operations
    cd,
    pwd,
    ls,
    cp,
    rm,
    touch,
    mv,
    mkdir,
    mkpath,
    rmdir,
    tempdir,
    tempname,
    mktemp,
    mktempdir,
    download,
    filemode,
    filesize,
    mtime,
    ctime,
    stat,
    lstat,
    isfifo,
    ispath,
    ischardev,
    isdir,
    isblockdev,
    isfile,
    islink,
    issocket,
    issetuid,
    issetgid,
    issticky,
    isreadable,
    iswriteable,
    isexecutable,
    uperm,
    gperm,
    operm,

# external processes ## TODO: whittle down these exports.
    getpid,
    ignorestatus,
    kill,
    pipeline_error,
    process_exit_status,
    process_exited,
    process_options,
    process_running,
    process_signaled,
    process_status,
    #process_stop_signal,
    #process_stopped,
    process_term_signal,
    readsfrom,
    readandwrite,
    run,
    spawn,
    spawn_nostdin,
    success,
    writesto,

# C interface
    c_free,
    cglobal,
    dlopen,
    dlopen_e,
    dlclose,
    dlsym,
    dlsym_e,
    errno,
    pointer,
    pointer_to_array,
    cfunction,
    strerror,
    unsafe_copy!,
    unsafe_load,
    unsafe_store!,
    unsafe_pointer_to_objref,
    pointer_from_objref,
    disable_sigint,
    reenable_sigint,

# Macros
    @b_str,
    @r_str,
    @v_str,
    @mstr,
    @unexpected,
    @assert,
    @cmd,
    @time,
    @timed,
    @elapsed,
    @which,
    @windows_only,
    @unix_only,
    @osx_only,
    @linux_only,
    @sync,
    @async,
    @spawn,
    @spawnlocal,  # deprecated
    @spawnat,
    @everywhere,
    @parallel,
    @gensym,
    @eval,
    @task,
    @thunk,
    @vectorize_1arg,
    @vectorize_2arg,
    @show,
    @printf,
    @sprintf,
    @deprecate
