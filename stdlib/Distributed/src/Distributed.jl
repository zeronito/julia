# This file is a part of Julia. License is MIT: https://julialang.org/license

__precompile__(true)

"""
Tools for distributed parallel processing.
"""
module Distributed

# imports for extension
import Base: getindex, wait, put!, take!, fetch, isready, push!, length,
             hash, ==, connect, kill, serialize, deserialize, close, showerror

# imports for use
using Base: Process, Semaphore, JLOptions, AnyDict, buffer_writes, wait_connected,
            VERSION_STRING, sync_begin, sync_add, sync_end, async_run_thunk,
            binding_module, notify_error, atexit, julia_exename, julia_cmd,
            AsyncGenerator, acquire, release, invokelatest,
            shell_escape_posixly, uv_error, coalesce, notnothing
using Base.Unicode: isascii, isdigit, isnumeric

# NOTE: clusterserialize.jl imports additional symbols from Base.Serializer for use

export
    @spawn,
    @spawnat,
    @fetch,
    @fetchfrom,
    @everywhere,
    @parallel,

    AbstractWorkerPool,
    addprocs,
    CachingPool,
    clear!,
    ClusterManager,
    default_worker_pool,
    init_worker,
    interrupt,
    launch,
    manage,
#    myid,   # accessed via Base
#    nprocs, # accessed via Base
    nworkers,
    pmap,
    procs,
    remote,
    remotecall,
    remotecall_fetch,
    remotecall_wait,
    remote_do,
    rmprocs,
    workers,
    WorkerPool,
    RemoteChannel,
    Future,
    WorkerConfig,
    RemoteException,
    ProcessExitedException,

    process_messages,
    remoteref_id,
    channel_from_id,
    worker_id_from_socket,
    cluster_cookie,
    start_worker,

# Used only by shared arrays.
    check_same_host

include("clusterserialize.jl")
include("cluster.jl")   # cluster setup and management, addprocs
include("messages.jl")
include("process_messages.jl")  # process incoming messages
include("remotecall.jl")  # the remotecall* api
include("macros.jl")      # @spawn and friends
include("workerpool.jl")
include("pmap.jl")
include("managers.jl")    # LocalManager and SSHManager
include("precompile.jl")

__init__() = init_parallel()

end
