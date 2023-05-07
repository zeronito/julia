WITH_TRACY, WITH_ITTAPI = let
    include("build_h.jl")
    WITH_TRACY, WITH_ITTAPI
end

if WITH_TRACY
    const Tracy = include("compiler/profilers/tracy.jl")
end

if WITH_ITTAPI
    const ITTAPI = include("compiler/profilers/ittapi.jl")
end

if WITH_TRACY || WITH_ITTAPI
    macro zone(name::String, ex::Expr)
        srcloc = WITH_TRACY && Tracy._zone_create(name, ex, __source__)
        tracy_begin_expr = WITH_TRACY ? :(ctx_tracy = Tracy._zone_begin($srcloc, true)) : :()
        tracy_end_expr = WITH_TRACY ? :(Tracy._zone_end(ctx_tracy)) : :()

        event = WITH_ITTAPI && ITTAPI._zone_create(name, ex, __source__)
        ittapi_begin_expr = WITH_ITTAPI ? :(ctx_ittapi = ITTAPI._zone_begin($event, true)) : :()
        ittapi_end_expr = WITH_ITTAPI ? :(ITTAPI._zone_end(ctx_ittapi)) : :()

        return quote
            $tracy_begin_expr
            $ittapi_begin_expr
            $(Expr(:tryfinally,
            :($(esc(ex))),
                quote
                    $tracy_end_expr
                    $ittapi_end_expr
                end
        ))
        end
    end
else
    macro zone(name::String, ex::Expr)
        esc(ex)
    end
end
