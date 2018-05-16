"""
    This struct keeps track of all uses of some mutable struct allocated
    in the current function. `uses` are all instances of `getfield` on the
    struct. `defs` are all instances of `setfield!` on the struct. The terminology
    refers to the uses/defs of the ``slot bundle'' that the mutable struct represents.

    In addition we keep track of all instances of a foreigncall preserve of this mutable
    struct. Somewhat counterintuitively, we don't actually need to make sure that the
    struct itself is live (or even allocated) at a ccall site. If there are no other places
    where the struct escapes (and thus e.g. where its address is taken), it need not be
    allocated. We do however, need to make sure to preserve any elments of this struct.
"""
struct SSADefUse
    uses::Vector{Int}
    defs::Vector{Int}
    ccall_preserve_uses::Vector{Int}
end
SSADefUse() = SSADefUse(Int[], Int[], Int[])

function try_compute_fieldidx_expr(@nospecialize(typ), @nospecialize(use_expr))
    field = use_expr.args[3]
    isa(field, QuoteNode) && (field = field.value)
    isa(field, Union{Int, Symbol}) || return nothing
    return try_compute_fieldidx(typ, field)
end

function lift_defuse(cfg::CFG, ssa::SSADefUse)
    # We remove from `uses` any block where all uses are dominated
    # by a def. This prevents insertion of dead phi nodes at the top
    # of such a block if that block happens to be in a loop
    ordered = Tuple{Int, Int, Bool}[(x, block_for_inst(cfg, x), true) for x in ssa.uses]
    for x in ssa.defs
        push!(ordered, (x, block_for_inst(cfg, x), false))
    end
    ordered = sort(ordered, by=x->x[1])
    bb_defs = Int[]
    bb_uses = Int[]
    last_bb = last_def_bb = 0
    for (_, bb, is_use) in ordered
        if bb != last_bb && is_use
            push!(bb_uses, bb)
        end
        last_bb = bb
        if last_def_bb != bb && !is_use
            push!(bb_defs, bb)
            last_def_bb = bb
        end
    end
    SSADefUse(bb_uses, bb_defs, Int[])
end

function find_curblock(domtree::DomTree, allblocks, curblock::Int)
    # TODO: This can be much faster by looking at current level and only
    # searching for those blocks in a sorted order
    while !(curblock in allblocks)
        curblock = domtree.idoms[curblock]
    end
    return curblock
end

function val_for_def_expr(ir::IRCode, def::Int, fidx::Int)
    if isexpr(ir[SSAValue(def)], :new)
        return ir[SSAValue(def)].args[1+fidx]
    else
        # The use is whatever the setfield was
        return ir[SSAValue(def)].args[4]
    end
end

function compute_value_for_block(ir::IRCode, domtree::DomTree, allblocks, du, phinodes, fidx, curblock)
    curblock = find_curblock(domtree, allblocks, curblock)
    def = reduce(max, 0, stmt for stmt in du.defs if block_for_inst(ir.cfg, stmt) == curblock)
    def == 0 ? phinodes[curblock] : val_for_def_expr(ir, def, fidx)
end

function compute_value_for_use(ir::IRCode, domtree::DomTree, allblocks, du, phinodes, fidx, use_idx)
    # Find the first dominating def
    curblock = stmtblock = block_for_inst(ir.cfg, use_idx)
    curblock = find_curblock(domtree, allblocks, curblock)
    defblockdefs = [stmt for stmt in du.defs if block_for_inst(ir.cfg, stmt) == curblock]
    def = 0
    if !isempty(defblockdefs)
        if curblock != stmtblock
            # Find the last def in this block
            def = maximum(defblockdefs)
        else
            # Find the last def before our use
            def = mapreduce(x->x >= use_idx ? 0 : x, max, defblockdefs)
        end
    end
    if def == 0
        if !haskey(phinodes, curblock)
            # If this happens, we need to search the predecessors for defs. Which
            # one doesn't matter - if it did, we'd have had a phinode
            return compute_value_for_block(ir, domtree, allblocks, du, phinodes, fidx, first(ir.cfg.blocks[stmtblock].preds))
        end
        # The use is the phinode
        return phinodes[curblock]
    else
        return val_for_def_expr(ir, def, fidx)
    end
end

function simple_walk(compact::IncrementalCompact, defssa::Union{SSAValue, NewSSAValue, OldSSAValue}, pi_callback=(pi,idx)->nothing)
    while true
        if isa(defssa, OldSSAValue) && already_inserted(compact, defssa)
            rename = compact.ssa_rename[defssa.id]
            @assert rename != defssa
            if isa(rename, Union{SSAValue, OldSSAValue, NewSSAValue})
                defssa = rename
                continue
            end
            return rename
        end
        def = compact[defssa]
        if isa(def, PiNode)
            pi_callback(def, defssa)
            if isa(def.val, SSAValue)
                defssa = def.val
            else
                return def.val
            end
        elseif isa(def, Union{SSAValue, OldSSAValue, NewSSAValue})
            defssa = def
        elseif isa(def, Union{PhiNode, Expr})
            return defssa
        else
            return def
        end
    end
end

function simple_walk_constraint(compact, defidx, typeconstraint = types(compact)[defidx])
    def = simple_walk(compact, defidx, (pi,_)->(typeconstraint = typeintersect(typeconstraint, pi.typ)))
    def, typeconstraint
end

"""
    walk_to_defs(compact, val, intermediaries)

Starting at `val` walk use-def chains to get all the leaves feeding into
this val (pruning those leaves rules out by path conditions).
"""
function walk_to_defs(compact, defssa, typeconstraint, visited_phinodes=Any[])
    # Step 2: Figure out what the struct is defined as
    def = compact[defssa]
    ## Track definitions through PiNode/PhiNode
    found_def = false
    ## Track which PhiNodes, SSAValue intermediaries
    ## we forwarded through.
    visited = IdSet{Any}()
    worklist = Tuple{Any, Any}[]
    leaves = Any[]
    push!(worklist, (defssa, typeconstraint))
    while !isempty(worklist)
        defssa, typeconstraint = pop!(worklist)
        push!(visited, defssa)
        def = compact[defssa]
        if isa(def, PhiNode)
            push!(visited_phinodes, defssa)
            possible_predecessors = let def=def, typeconstraint=typeconstraint
                collect(Iterators.filter(1:length(def.edges)) do n
                    isassigned(def.values, n) || return false
                    value = def.values[n]
                    edge_typ = widenconst(compact_exprtype(compact, value))
                    return typeintersect(edge_typ, typeconstraint) !== Union{}
                end)
            end
            for n in possible_predecessors
                pred = def.edges[n]
                val = def.values[n]
                if isa(val, Union{SSAValue, OldSSAValue, NewSSAValue})
                    new_def, new_constraint = simple_walk_constraint(compact, val, typeconstraint)
                    if isa(new_def, Union{SSAValue, OldSSAValue, NewSSAValue})
                        if !(new_def in visited)
                            push!(worklist, (new_def, new_constraint))
                        end
                        continue
                    end
                end
                if def == val
                    # This shouldn't really ever happen, but
                    # patterns like this can occur in dead code,
                    # so bail out.
                    break
                else
                    push!(leaves, val)
                end
                continue
            end
        else
            push!(leaves, defssa)
        end
    end
    leaves
end

function process_immutable_preserve(new_preserves::Vector{Any}, compact::IncrementalCompact, def::Expr)
    for arg in (isexpr(def, :new) ? def.args : def.args[2:end])
        if !isbitstype(widenconst(compact_exprtype(compact, arg)))
            push!(new_preserves, arg)
        end
    end
end

struct FastForward
    to::SSAValue
    phi_locs::Vector{Tuple{Int, Int}}
end

function already_inserted(compact, old::OldSSAValue)
    id = old.id
    if id < length(compact.ir.stmts)
        return id <= compact.idx
    end
    id -= length(compact.ir.stmts)
    if id < length(compact.ir.new_nodes)
        error()
    end
    id -= length(compact.ir.new_nodes)
    @assert id <= length(compact.pending_nodes)
    return !(id in compact.pending_perm)
end

function lift_leaves(compact, stmt, result_t, field, leaves)
    # For every leaf, the lifted value
    lifted_leaves = IdDict{Any, Any}()
    maybe_undef = false
    for leaf in leaves
        leaf_key = leaf
        if isa(leaf, Union{SSAValue, OldSSAValue, NewSSAValue})
            if isa(leaf, OldSSAValue) && already_inserted(compact, leaf)
                leaf = compact.ssa_rename[leaf.id]
                if isa(leaf, Union{SSAValue, OldSSAValue, NewSSAValue})
                    leaf = simple_walk(compact, leaf)
                end
                if isa(leaf, Union{SSAValue, OldSSAValue, NewSSAValue})
                    def = compact[leaf]
                else
                    def = leaf
                end
            else
                def = compact[leaf]
            end
            if is_tuple_call(compact.ir, def) && isa(field, Int) && 1 <= field < length(def.args)
                lifted_leaves[leaf_key] = Ref{Any}(def.args[1+field])
                continue
            elseif isexpr(def, :new)
                typ = def.typ
                if isa(typ, UnionAll)
                    typ = unwrap_unionall(typ)
                end
                (isa(typ, DataType) && (!typ.abstract)) || return nothing
                @assert !typ.mutable
                field = try_compute_fieldidx(typ, stmt)
                field === nothing && return nothing
                if length(def.args) < 1 + field
                    ftyp = fieldtype(typ, field)
                    if !isbits(ftyp)
                        # On this branch, this will be a guaranteed UndefRefError.
                        # We use the regular undef mechanic to lift this to a boolean slot
                        maybe_undef = true
                        lifted_leaves[leaf_key] = nothing
                        continue
                    end
                    # Expand the Expr(:new) to include it's element Expr(:new) nodes up until the one we want
                    compact[leaf] = nothing
                    for i = (length(def.args) + 1):(1+field)
                        ftyp = fieldtype(typ, i - 1)
                        isbits(ftyp) || return nothing
                        push!(def.args, insert_node!(compact, leaf, result_t, Expr(:new, ftyp)))
                    end
                    compact[leaf] = def
                end
                lifted_leaves[leaf_key] = Ref{Any}(def.args[1+field])
                continue
            else
                typ = compact_exprtype(compact, leaf)
                if !isa(typ, Const)
                    # If the leaf is an old ssa value, insert a getfield here
                    # We will revisit this getfield later when compaction gets
                    # to the appropriate point.
                    # N.B.: This can be a bit dangerous because it can lead to
                    # infinite loops if we accidentally insert a node just ahead
                    # of where we are
                    if isa(leaf, OldSSAValue) && (isa(field, Int) || isa(field, Symbol))
                        (isa(typ, DataType) && (!typ.abstract)) || return nothing
                        @assert !typ.mutable
                        # If there's the potential for an undefref error on access, we cannot insert a getfield
                        if field > typ.ninitialized && !isbits(fieldtype(typ, field))
                            lifted_leaves[leaf] = Ref{Any}(insert_node!(compact, leaf, make_MaybeUndef(result_t), Expr(:call, :unchecked_getfield, SSAValue(leaf.id), field), true))
                            maybe_undef = true
                        else
                            lifted_leaves[leaf] = Ref{Any}(insert_node!(compact, leaf, result_t, Expr(:call, getfield, SSAValue(leaf.id), field), true))
                        end
                        continue
                    end
                    return nothing
                end
                leaf = typ.val
                # Fall through to below
            end
        elseif isa(leaf, Union{Argument, Expr})
            return nothing
        end
        isimmutable(leaf) || return nothing
        isdefined(leaf, field) || return nothing
        val = getfield(leaf, field)
        is_inlineable_constant(val) || return nothing
        lifted_leaves[leaf_key] = Ref{Any}(quoted(val))
    end
    lifted_leaves, maybe_undef
end

make_MaybeUndef(typ) = isa(typ, MaybeUndef) ? typ : MaybeUndef(typ)

const AnySSAValue = Union{SSAValue, OldSSAValue, NewSSAValue}

function getfield_elim_pass!(ir::IRCode, domtree)
    compact = IncrementalCompact(ir)
    insertions = Vector{Any}()
    defuses = IdDict{Int, Tuple{IdSet{Int}, SSADefUse}}()
    lifting_cache = IdDict{Tuple{AnySSAValue, Int}, AnySSAValue}()
    revisit_worklist = Int[]
    #ndone, nmax = 0, 200
    for (idx, stmt) in compact
        isa(stmt, Expr) || continue
        #ndone >= nmax && continue
        #ndone += 1
        result_t = compact_exprtype(compact, SSAValue(idx))
        is_getfield = false
        is_ccall = false
        is_unchecked = false
        # Step 1: Check whether the statement we're looking at is a getfield/setfield!
        if is_known_call(stmt, setfield!, compact)
            is_setfield = true
        elseif is_known_call(stmt, getfield, compact)
            is_getfield = true
        elseif isexpr(stmt, :call) && stmt.args[1] == :unchecked_getfield
            is_getfield = true
            is_unchecked = true
        elseif isexpr(stmt, :foreigncall)
            nccallargs = stmt.args[5]
            new_preserves = Any[]
            old_preserves = stmt.args[(6+nccallargs):end]
            for (pidx, preserved_arg) in enumerate(old_preserves)
                intermediaries = IdSet()
                isa(preserved_arg, SSAValue) || continue
                def = simple_walk(compact, preserved_arg, (pi, idx)->push!(intermediaries, idx))
                isa(def, SSAValue) || continue
                defidx = def.id
                def = compact[defidx]
                if is_tuple_call(compact, def)
                    process_immutable_preserve(new_preserves, compact, def)
                    old_preserves[pidx] = nothing
                    continue
                elseif isexpr(def, :new)
                    typ = def.typ
                    if isa(typ, UnionAll)
                        typ = unwrap_unionall(typ)
                    end
                    if !typ.mutable
                        process_immutable_preserve(new_preserves, compact, def)
                        old_preserves[pidx] = nothing
                        continue
                    end
                else
                    continue
                end
                mid, defuse = get!(defuses, defidx, (IdSet{Int}(), SSADefUse()))
                push!(defuse.ccall_preserve_uses, idx)
                union!(mid, intermediaries)
                continue
            end
            if !isempty(new_preserves)
                old_preserves = filter(ssa->ssa !== nothing, old_preserves)
                new_expr = Expr(:foreigncall, stmt.args[1:(6+nccallargs-1)]...,
                    old_preserves..., new_preserves...)
                new_expr.typ = stmt.typ
                compact[idx] = new_expr
            end
            continue
        else
            continue
        end
        ## Normalize the field argument to getfield/setfield
        field = stmt.args[3]
        isa(field, QuoteNode) && (field = field.value)
        isa(field, Union{Int, Symbol}) || continue

        struct_typ = unwrap_unionall(widenconst(compact_exprtype(compact, stmt.args[2])))
        (isa(struct_typ, DataType) && !struct_typ.mutable) || continue

        def, typeconstraint = stmt.args[2], struct_typ

        if struct_typ.mutable
            isa(def, SSAValue) || continue
            intermediaries = IdSet()
            def = simple_walk(compact, def, (pi, idx)->push!(intermediaries, idx))
            # Mutable stuff here
            isa(def, SSAValue) || continue
            mid, defuse = get!(defuses, def.id, (IdSet{Int}(), SSADefUse()))
            push!(defuse.defs, idx)
            union!(mid, intermediaries)
            continue
        end

        if isa(def, Union{OldSSAValue, SSAValue})
            def, typeconstraint = simple_walk_constraint(compact, def, typeconstraint)
        end

        visited_phinodes = Any[]
        if isa(def, Union{OldSSAValue, SSAValue, NewSSAValue}) && isa(compact[def], PhiNode)
            leaves = walk_to_defs(compact, def, typeconstraint, visited_phinodes)
        else
            leaves = [def]
        end

        isempty(leaves) && continue

        field = try_compute_fieldidx(struct_typ, stmt)
        field === nothing && continue

        r = lift_leaves(compact, stmt, result_t, field, leaves)
        r === nothing && continue
        lifted_leaves, any_undef = r

        reverse_mapping = IdDict{Any, Any}(ssa => id for (id, ssa) in enumerate(visited_phinodes))

        if any_undef
            result_t = make_MaybeUndef(result_t)
        end

        # Insert PhiNodes
        lifted_phis = map(visited_phinodes) do item
            if (item, field) in keys(lifting_cache)
                ssa = lifting_cache[(item, field)]
                return (ssa, compact[ssa], false)
            end
            n = PhiNode()
            ssa = insert_node!(compact, item, result_t, n)
            lifting_cache[(item, field)] = ssa
            (ssa, n, true)
        end

        # Fix up arguments
        for (old_node, (_, new_node, need_argupdate)) in zip(map(x->compact[x], visited_phinodes), lifted_phis)
            need_argupdate || continue
            for i = 1:length(old_node.edges)
                edge = old_node.edges[i]
                isassigned(old_node.values, i) || continue
                val = old_node.values[i]
                if isa(val, Union{NewSSAValue, SSAValue, OldSSAValue})
                    val = simple_walk(compact, val)
                end
                if val in keys(lifted_leaves)
                    push!(new_node.edges, edge)
                    lifted_val = lifted_leaves[val]
                    if lifted_val === nothing
                        resize!(new_node.values, length(new_node.values)+1)
                        continue
                    end
                    lifted_val = lifted_val.x
                    if isa(lifted_val, Union{NewSSAValue, SSAValue, OldSSAValue})
                        lifted_val = simple_walk(compact, lifted_val)
                    end
                    push!(new_node.values, lifted_val)
                elseif isa(val, Union{NewSSAValue, SSAValue, OldSSAValue}) && val in keys(reverse_mapping)
                    push!(new_node.edges, edge)
                    push!(new_node.values, lifted_phis[reverse_mapping[val]][1])
                else
                    # Probably ignored by path condition, skip this
                end
            end
        end

        for (_, node) in lifted_phis
            count_added_node!(compact, node)
        end

        # Fixup the stmt itself
        val = stmt.args[2]
        if isa(val, Union{SSAValue, OldSSAValue})
            val = simple_walk(compact, val)
        end
        if val in keys(lifted_leaves)
            val = lifted_leaves[val]
            @assert val !== nothing
            val = val.x
        else
            isa(val, Union{SSAValue, OldSSAValue}) && val in keys(reverse_mapping)
            val = lifted_phis[reverse_mapping[val]][1]
        end

        # Insert the undef check if necessary
        if any_undef && !is_unchecked
            insert_node!(compact, SSAValue(idx), Nothing, Expr(:undefcheck, :getfield, val))
        end

        compact[idx] = val
    end

    ir = finish(compact)
    # Now go through any mutable structs and see which ones we can eliminate
    for (idx, (intermediaries, defuse)) in defuses
        intermediaries = collect(intermediaries)
        # Check if there are any uses we did not account for. If so, the variable
        # escapes and we cannot eliminate the allocation. This works, because we're guaranteed
        # not to include any intermediaries that have dead uses. As a result, missing uses will only ever
        # show up in the nuses_total count.
        nleaves = length(defuse.uses) + length(defuse.defs) + length(defuse.ccall_preserve_uses)
        nuses_total = compact.used_ssas[idx] + mapreduce(idx->compact.used_ssas[idx], +, 0, intermediaries) - length(intermediaries)
        nleaves == nuses_total || continue
        # Find the type for this allocation
        defexpr = ir[SSAValue(idx)]
        isexpr(defexpr, :new) || continue
        typ = defexpr.typ
        if isa(typ, UnionAll)
            typ = unwrap_unionall(typ)
        end
        # Could still end up here if we tried to setfield! and immutable, which would
        # error at runtime, but is not illegal to have in the IR.
        typ.mutable || continue
        # Partition defuses by field
        fielddefuse = SSADefUse[SSADefUse() for _ = 1:fieldcount(typ)]
        ok = true
        for use in defuse.uses
            field = try_compute_fieldidx_expr(typ, ir[SSAValue(use)])
            field === nothing && (ok = false; break)
            push!(fielddefuse[field].uses, use)
        end
        ok || continue
        for use in defuse.defs
            field = try_compute_fieldidx_expr(typ, ir[SSAValue(use)])
            field === nothing && (ok = false; break)
            push!(fielddefuse[field].defs, use)
        end
        ok || continue
        preserve_uses = IdDict{Int, Vector{Any}}((idx=>Any[] for idx in IdSet{Int}(defuse.ccall_preserve_uses)))
        # Everything accounted for. Go field by field and perform idf
        for (fidx, du) in pairs(fielddefuse)
            ftyp = fieldtype(typ, fidx)
            if !isempty(du.uses)
                push!(du.defs, idx)
                ldu = compute_live_ins(ir.cfg, du)
                phiblocks = []
                if !isempty(ldu.live_in_bbs)
                    phiblocks = idf(ir.cfg, ldu, domtree)
                end
                phinodes = IdDict{Int, SSAValue}()
                for b in phiblocks
                    n = PhiNode()
                    phinodes[b] = insert_node!(ir, first(ir.cfg.blocks[b].stmts), ftyp, n)
                end
                # Now go through all uses and rewrite them
                allblocks = sort(vcat(phiblocks, ldu.def_bbs))
                for stmt in du.uses
                    ir[SSAValue(stmt)] = compute_value_for_use(ir, domtree, allblocks, du, phinodes, fidx, stmt)
                end
                if !isbitstype(fieldtype(typ, fidx))
                    for (use, list) in preserve_uses
                        push!(list, compute_value_for_use(ir, domtree, allblocks, du, phinodes, fidx, use))
                    end
                end
                for b in phiblocks
                    for p in ir.cfg.blocks[b].preds
                        n = ir[phinodes[b]]
                        push!(n.edges, p)
                        push!(n.values, compute_value_for_block(ir, domtree,
                            allblocks, du, phinodes, fidx, p))
                    end
                end
            end
            for stmt in du.defs
                stmt == idx && continue
                ir[SSAValue(stmt)] = nothing
            end
            continue
        end
        isempty(defuse.ccall_preserve_uses) && continue
        push!(intermediaries, idx)
        # Insert the new preserves
        for (use, new_preserves) in preserve_uses
            useexpr = ir[SSAValue(use)]
            nccallargs = useexpr.args[5]
            old_preserves = filter(ssa->!isa(ssa, SSAValue) || !(ssa.id in intermediaries), useexpr.args[(6+nccallargs):end])
            new_expr = Expr(:foreigncall, useexpr.args[1:(6+nccallargs-1)]...,
                old_preserves..., new_preserves...)
            new_expr.typ = useexpr.typ
            ir[SSAValue(use)] = new_expr
        end
    end
    ir
end

function adce_erase!(phi_uses, extra_worklist, compact, idx)
    if isa(compact.result[idx], PhiNode)
        maybe_erase_unused!(extra_worklist, compact, idx, val->phi_uses[val.id]-=1)
    else
        maybe_erase_unused!(extra_worklist, compact, idx)
    end
end

function count_uses(stmt, uses)
    for ur in userefs(stmt)
        if isa(ur[], SSAValue)
            uses[ur[].id] += 1
        end
    end
end

function mark_phi_cycles(compact, safe_phis, phi)
    worklist = Int[]
    push!(worklist, phi)
    while !isempty(worklist)
        phi = pop!(worklist)
        push!(safe_phis, phi)
        for ur in userefs(compact.result[phi])
            val = ur[]
            isa(val, SSAValue) || continue
            isa(compact[val], PhiNode) || continue
            (val.id in safe_phis) && continue
            push!(worklist, val.id)
        end
    end
end

function adce_pass!(ir)
    phi_uses = fill(0, length(ir.stmts) + length(ir.new_nodes))
    all_phis = Int[]
    compact = IncrementalCompact(ir)
    for (idx, stmt) in compact
        if isa(stmt, PhiNode)
            push!(all_phis, idx)
        end
    end
    non_dce_finish!(compact)
    for phi in all_phis
        count_uses(compact.result[phi], phi_uses)
    end
    # Perform simple DCE for unused values
    extra_worklist = Int[]
    for (idx, nused) in Iterators.enumerate(compact.used_ssas)
        idx >= compact.result_idx && break
        nused == 0 || continue
        adce_erase!(phi_uses, extra_worklist, compact, idx)
    end
    while !isempty(extra_worklist)
        adce_erase!(phi_uses, extra_worklist, compact, pop!(extra_worklist))
    end
    # Go back and erase any phi cycles
    changed = true
    while changed
        changed = false
        safe_phis = IdSet{Int}()
        for phi in all_phis
            # Save any phi cycles that have non-phi uses
            if compact.used_ssas[phi] - phi_uses[phi] != 0
                mark_phi_cycles(compact, safe_phis, phi)
            end
        end
        for phi in all_phis
            if !(phi in safe_phis)
                push!(extra_worklist, phi)
            end
        end
        while !isempty(extra_worklist)
            if adce_erase!(phi_uses, extra_worklist, compact, pop!(extra_worklist))
                changed = true
            end
        end
    end
    complete(compact)
end

function type_lift_pass!(ir::IRCode)
    type_ctx_uses = Vector{Vector{Int}}[]
    has_non_type_ctx_uses = IdSet{Int}()
    lifted_undef = IdDict{Int, Any}()
    for (idx, stmt) in pairs(ir.stmts)
        if stmt isa Expr && (stmt.head === :isdefined || stmt.head === :undefcheck)
            val = (stmt.head === :isdefined) ? stmt.args[1] : stmt.args[2]
            # undef can only show up by being introduced in a phi
            # node (or an UpsilonNode() argument to a PhiC node),
            # so lift all these nodes that have maybe undef values
            processed = IdDict{Int, Union{SSAValue, Bool}}()
            if !isa(val, SSAValue)
                if stmt.head === :undefcheck
                    ir.stmts[idx] = nothing
                end
                continue
            end
            worklist = Tuple{Int, Int, SSAValue, Int}[(val.id, 0, SSAValue(0), 0)]
            stmt_id = val.id
            while isa(ir.stmts[stmt_id], PiNode)
                stmt_id = ir.stmts[stmt_id].val.id
            end
            def = ir.stmts[stmt_id]
            if !isa(def, PhiNode) && !isa(def, PhiCNode)
                if stmt.head === :isdefined
                    ir.stmts[idx] = true
                else
                    ir.stmts[idx] = nothing
                end
                continue
            end
            if !haskey(lifted_undef, stmt_id)
                first = true
                while !isempty(worklist)
                    item, w_up_id, which, use = pop!(worklist)
                    def = ir.stmts[item]
                    if isa(def, PhiNode)
                        edges = copy(def.edges)
                        values = Vector{Any}(undef, length(edges))
                        new_phi = length(values) == 0 ? false : insert_node!(ir, item, Bool, PhiNode(edges, values))
                    else
                        values = Vector{Any}(undef, length(def.values))
                        new_phi = length(values) == 0 ? false : insert_node!(ir, item, Bool, PhiCNode(values))
                    end
                    processed[item] = new_phi
                    if first
                        lifted_undef[stmt_id] = new_phi
                        first = false
                    end
                    local id::Int = 0
                    for i = 1:length(values)
                        if !isassigned(def.values, i)
                            val = false
                        elseif !isa(def.values[i], SSAValue)
                            val = true
                        else
                            up_id = id = def.values[i].id
                            @label restart
                            if !isa(ir.types[id], MaybeUndef)
                                val = true
                            else
                                if isa(ir.stmts[id], UpsilonNode)
                                    up = ir.stmts[id]
                                    if !isdefined(up, :val)
                                        val = false
                                    elseif !isa(up.val, SSAValue)
                                        val = true
                                    else
                                        id = up.val.id
                                        @goto restart
                                    end
                                else
                                    while isa(ir.stmts[id], PiNode)
                                        id = ir.stmts[id].val.id
                                    end
                                    if isa(ir.stmts[id], Union{PhiNode, PhiCNode})
                                        if haskey(processed, id)
                                            val = processed[id]
                                        else
                                            push!(worklist, (id, up_id, new_phi, i))
                                            continue
                                        end
                                    else
                                        val = true
                                    end
                                end
                            end
                        end
                        if isa(def, PhiNode)
                            values[i] = val
                        else
                            values[i] = insert_node!(ir, up_id, Bool, UpsilonNode(val))
                        end
                    end
                    if which !== SSAValue(0)
                        phi = ir[which]
                        if isa(phi, PhiNode)
                            phi.values[use] = new_phi
                        else
                            phi = phi::PhiCNode
                            ir[which].values[use] = insert_node!(ir, w_up_id, Bool, UpsilonNode(new_phi))
                        end
                    end
                end
            end
            if stmt.head === :isdefined
                ir.stmts[idx] = lifted_undef[stmt_id]
            else
                ir.stmts[idx] = Expr(:throw_undef_if_not, stmt.args[1], lifted_undef[stmt_id])
            end
        end
    end
    ir
end
