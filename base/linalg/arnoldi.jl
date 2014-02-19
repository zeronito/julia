using .ARPACK

## eigs

function eigs(A;nev::Integer=6, ncv::Integer=20, which::ASCIIString="LM",
          tol=0.0, maxiter::Integer=1000, sigma=nothing, v0::Vector=zeros((0,)),
          ritzvec::Bool=true, complexOP::Bool=false)
    n = chksquare(A)
    (n <= 6) && (nev = min(n-1, nev))
    ncv = blas_int(min(max(2*nev+2, ncv), n))

    sym   = issym(A)
    T     = eltype(A)
    cmplx = T<:Complex
    bmat  = "I"
    const arpack_which_lm = "LM"   # always looking for largest EV
    arpack_sigma = 0
    
    if !isempty(v0)
        length(v0)==n || throw(DimensionMismatch(""))
        eltype(v0)==T || error("Starting vector must have eltype $T")
    else
        v0=zeros(T,(0,))
    end

    if sigma == nothing && which == "LM"
        # normal iteration
        mode = 1
        linop(x) = A * x
    else
        if (sigma == nothing && which == "SM") || sigma == 0
            # invert only
            C = lufact(A)
        else
            # shift and invert
            arpack_sigma = sigma
            C = lufact(A - sigma*eye(A))
        end
        if cmplx
            mode = 3
            linop(x) = C\x
        else
            if !complexOP
                mode = 3
                linop(x) = real(C\x)
            else
                mode = 4
                linop(x) = imag(C\x)
            end
        end     
    end
        
    # Compute the Ritz values and Ritz vectors
    (resid, v, ldv, iparam, ipntr, workd, workl, lworkl, rwork, TOL) = 
       ARPACK.aupd_wrapper(T, linop, n, sym, cmplx, bmat, nev, ncv, arpack_which_lm, tol, maxiter, mode, v0)
    
    # Postprocessing to get eigenvalues and eigenvectors
    ARPACK.eupd_wrapper(T, n, sym, cmplx, bmat, nev, arpack_which_lm, ritzvec, TOL,
        resid, ncv, v, ldv, arpack_sigma, iparam, ipntr, workd, workl, lworkl, rwork)

end
