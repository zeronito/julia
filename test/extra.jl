runtests("suitesparse")
runtests("arpack")
runtests("bigfloat")
runtests("poly")
runtests("file")
#runtests("Rmath")
runtests("zlib")
# runtests("options")
runtests("image")
# runtests("iterators")
@unix_only runtests("gzip")

runtests("perf")
