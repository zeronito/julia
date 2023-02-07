# This file is a part of Julia. License is MIT: https://julialang.org/license

const UnoptSlot = Union{SlotNumber, TypedSlot}

mutable struct SlotInfo
    defs::Vector{Int}
    uses::Vector{Int}
    any_newvar::Bool
end
SlotInfo() = SlotInfo(Int[], Int[], false)

function scan_entry!(result::Vector{SlotInfo}, idx::Int, @nospecialize(stmt))
    # NewVarNodes count as defs for the purpose
    # of liveness analysis (i.e. they kill use chains)
    if isa(stmt, NewvarNode)
        result[slot_id(stmt.slot)].any_newvar = true
        push!(result[slot_id(stmt.slot)].defs, idx)
        return
    elseif isexpr(stmt, :(=))
        arg1 = stmt.args[1]
        if isa(arg1, SlotNumber)
            push!(result[slot_id(arg1)].defs, idx)
        end
        stmt = stmt.args[2]
    end
    if isa(stmt, UnoptSlot)
        push!(result[slot_id(stmt)].uses, idx)
        return
    end
    for op in userefs(stmt)
        val = op[]
        if isa(val, UnoptSlot)
            push!(result[slot_id(val)].uses, idx)
        end
    end
end

function scan_slot_def_use(nargs::Int, ci::CodeInfo, code::Vector{Any})
    nslots = length(ci.slotflags)
    result = SlotInfo[SlotInfo() for i = 1:nslots]
    # Set defs for arguments
    for var in result[1:nargs]
        push!(var.defs, 0)
    end
    for idx in 1:length(code)
        stmt = code[idx]
        scan_entry!(result, idx, stmt)
    end
    result
end

function renumber_ssa(stmt::SSAValue, ssanums::Vector{SSAValue}, new_ssa::Bool=false)
    id = stmt.id
    if id > length(ssanums)
        return stmt
    end
    val = ssanums[id]
    @assert val.id > 0
    return val
end

function renumber_ssa!(@nospecialize(stmt), ssanums::Vector{SSAValue}, new_ssa::Bool=false)
    isa(stmt, SSAValue) && return renumber_ssa(stmt, ssanums, new_ssa)
    return ssamap(val->renumber_ssa(val, ssanums, new_ssa), stmt)
end

function make_ssa!(ci::CodeInfo, code::Vector{Any}, idx::Int, @nospecialize(typ))
    stmt = code[idx]
    @assert isexpr(stmt, :(=))
    code[idx] = stmt.args[2]
    (ci.ssavaluetypes::Vector{Any})[idx] = typ
    return SSAValue(idx)
end

function new_to_regular(@nospecialize(stmt), new_offset::Int)
    urs = userefs(stmt)
    for op in urs
        val = op[]
        if isa(val, NewSSAValue)
            op[] = SSAValue(val.id + new_offset)
        end
    end
    return urs[]
end

function fixup_slot!(ir::IRCode, ci::CodeInfo, idx::Int, slot::Int, stmt::UnoptSlot, @nospecialize(ssa))
    # We don't really have the information here to get rid of these.
    # We'll do so later
    if ssa === UNDEF_TOKEN
        insert_node!(ir, idx, NewInstruction(
            Expr(:throw_undef_if_not, ci.slotnames[slot], false), Any))
        return UNDEF_TOKEN
    end
    if !isa(ssa, Argument) && !(ssa === nothing) && ((ci.slotflags[slot] & SLOT_USEDUNDEF) != 0)
        # insert a temporary node. type_lift_pass! will remove it
        insert_node!(ir, idx, NewInstruction(
            Expr(:undefcheck, ci.slotnames[slot], ssa), Any))
    end
    if isa(stmt, SlotNumber)
        return ssa
    elseif isa(stmt, TypedSlot)
        return NewSSAValue(insert_node!(ir, idx, NewInstruction(PiNode(ssa, stmt.typ), stmt.typ)).id - length(ir.stmts))
    end
    @assert false # unreachable
end

function fixemup!(@specialize(slot_filter), @specialize(rename_slot), ir::IRCode, ci::CodeInfo, idx::Int, @nospecialize(stmt))
    if isa(stmt, UnoptSlot) && slot_filter(stmt)
        return fixup_slot!(ir, ci, idx, slot_id(stmt), stmt, rename_slot(stmt))
    end
    if isexpr(stmt, :(=))
        stmt.args[2] = fixemup!(slot_filter, rename_slot, ir, ci, idx, stmt.args[2])
        return stmt
    end
    if isa(stmt, PhiNode)
        for i = 1:length(stmt.edges)
            isassigned(stmt.values, i) || continue
            val = stmt.values[i]
            isa(val, UnoptSlot) || continue
            slot_filter(val) || continue
            bb_idx = block_for_inst(ir.cfg, Int(stmt.edges[i]))
            from_bb_terminator = last(ir.cfg.blocks[bb_idx].stmts)
            stmt.values[i] = fixup_slot!(ir, ci, from_bb_terminator, slot_id(val), val, rename_slot(val))
        end
        return stmt
    end
    if isexpr(stmt, :isdefined)
        val = stmt.args[1]
        if isa(val, UnoptSlot)
            slot = slot_id(val)
            if (ci.slotflags[slot] & SLOT_USEDUNDEF) == 0
                return true
            else
                ssa = rename_slot(val)
                if ssa === UNDEF_TOKEN
                    return false
                elseif !isa(ssa, SSAValue) && !isa(ssa, NewSSAValue)
                    return true
                end
            end
            # temporarily corrupt the isdefined node. type_lift_pass! will fix it
            stmt.args[1] = ssa
        end
        return stmt
    end
    urs = userefs(stmt)
    for op in urs
        val = op[]
        if isa(val, UnoptSlot) && slot_filter(val)
            x = fixup_slot!(ir, ci, idx, slot_id(val), val, rename_slot(val))
            # We inserted an undef error node. Delete subsequent statement
            # to avoid confusing the optimizer
            if x === UNDEF_TOKEN
                return nothing
            end
            op[] = x
        elseif isa(val, GlobalRef) && !(isdefined(val.mod, val.name) && isconst(val.mod, val.name))
            op[] = NewSSAValue(insert_node!(ir, idx,
                NewInstruction(val, typ_for_val(val, ci, ir.sptypes, idx, Any[]))).id - length(ir.stmts))
        elseif isexpr(val, :static_parameter)
            ty = typ_for_val(val, ci, ir.sptypes, idx, Any[])
            if isa(ty, Const)
                inst = NewInstruction(quoted(ty.val), ty)
            else
                inst = NewInstruction(val, ty)
            end
            op[] = NewSSAValue(insert_node!(ir, idx, inst).id - length(ir.stmts))
        end
    end
    return urs[]
end

function fixup_uses!(ir::IRCode, ci::CodeInfo, code::Vector{Any}, uses::Vector{Int}, slot::Int, @nospecialize(ssa))
    for use in uses
        code[use] = fixemup!(x::UnoptSlot->slot_id(x)==slot, stmt::UnoptSlot->ssa, ir, ci, use, code[use])
    end
end

function rename_uses!(ir::IRCode, ci::CodeInfo, idx::Int, @nospecialize(stmt), renames::Vector{Any})
    return fixemup!(stmt::UnoptSlot->true, stmt::UnoptSlot->renames[slot_id(stmt)], ir, ci, idx, stmt)
end

function strip_trailing_junk!(ci::CodeInfo, code::Vector{Any}, info::Vector{CallInfo})
    # Remove `nothing`s at the end, we don't handle them well
    # (we expect the last instruction to be a terminator)
    ssavaluetypes = ci.ssavaluetypes::Vector{Any}
    (; codelocs, ssaflags) = ci
    for i = length(code):-1:1
        if code[i] !== nothing
            resize!(code, i)
            resize!(ssavaluetypes, i)
            resize!(codelocs, i)
            resize!(info, i)
            resize!(ssaflags, i)
            break
        end
    end
    # If the last instruction is not a terminator, add one. This can
    # happen for implicit return on dead branches.
    term = code[end]
    if !isa(term, GotoIfNot) && !isa(term, GotoNode) && !isa(term, ReturnNode)
        push!(code, ReturnNode())
        push!(ssavaluetypes, Union{})
        push!(codelocs, 0)
        push!(info, NoCallInfo())
        push!(ssaflags, IR_FLAG_NOTHROW)
    end
    nothing
end

struct DelayedTyp
    phi::NewSSAValue
end

# maybe use expr_type?
function typ_for_val(@nospecialize(x), ci::CodeInfo, sptypes::Vector{Any}, idx::Int, slottypes::Vector{Any})
    if isa(x, Expr)
        if x.head === :static_parameter
            return sptypes[x.args[1]::Int]
        elseif x.head === :boundscheck
            return Bool
        elseif x.head === :copyast
            return typ_for_val(x.args[1], ci, sptypes, idx, slottypes)
        end
        return (ci.ssavaluetypes::Vector{Any})[idx]
    end
    isa(x, GlobalRef) && return abstract_eval_globalref(x)
    isa(x, SSAValue) && return (ci.ssavaluetypes::Vector{Any})[x.id]
    isa(x, Argument) && return slottypes[x.n]
    isa(x, NewSSAValue) && return DelayedTyp(x)
    isa(x, QuoteNode) && return Const(x.value)
    isa(x, Union{Symbol, PiNode, PhiNode, SlotNumber, TypedSlot}) && error("unexpected val type")
    return Const(x)
end

struct BlockLiveness
    def_bbs::Vector{Int}
    live_in_bbs::Vector{Int}
end

"""
    iterated_dominance_frontier(cfg::CFG, liveness::BlockLiveness, domtree::DomTree)
        -> phinodes::Vector{Int}

Run iterated dominance frontier.
The algorithm we have here essentially follows LLVM, which itself is a
a cleaned up version of the linear-time algorithm described in [^SG95].

The algorithm here, is quite straightforward. Suppose we have a CFG:

    A -> B -> D -> F
     \\-> C ------>/

and a corresponding dominator tree:

    A
    |- B - D
    |- C
    |- F

Now, for every definition of our slot, we simply walk down the dominator
tree and look for any edges that leave the sub-domtree rooted by our definition.

In our example above, if we have a definition in `B`, we look at its successors,
which is only `D`, which is dominated by `B` and hence doesn't need a ϕ-node.
Then we descend down the subtree rooted at `B` and end up in `D`. `D` has a successor
`F`, which is not part of the current subtree, (i.e. not dominated by `B`),
so it needs a ϕ-node.

Now, the key insight of that algorithm is that we have two defs, in blocks `A` and `B`,
and `A` dominates `B`, then we do not need to recurse into `B`, because the set of
potential backedges from a subtree rooted at `B` (to outside the subtree) is a strict
subset of those backedges from a subtree rooted at `A` (out outside the subtree rooted
at `A`). Note however that this does not work the other way. Thus, the algorithm
needs to make sure that we always visit `B` before `A`.

[^SG95]: Vugranam C. Sreedhar and Guang R. Gao. 1995.
         A linear time algorithm for placing φ-nodes.
         In Proceedings of the 22nd ACM SIGPLAN-SIGACT symposium on Principles of programming languages (POPL '95).
         Association for Computing Machinery, New York, NY, USA, 62–73.
         DOI: <https://doi.org/10.1145/199448.199464>.
"""
function iterated_dominance_frontier(cfg::CFG, liveness::BlockLiveness, domtree::DomTree)
    defs = liveness.def_bbs
    heap = Tuple{Int, Int}[(defs[i], domtree.nodes[defs[i]].level) for i in 1:length(defs)]
    heap_order = By(x -> -x[2])
    heapify!(heap, heap_order)
    phiblocks = Int[]
    # This bitset makes sure we only add a phi node to a given block once.
    processed = BitSet()
    # This bitset implements the `key insight` mentioned above. In particular, it prevents
    # us from visiting a subtree that we have already visited before.
    visited = BitSet()
    while !isempty(heap)
        # We pop from the end of the array - i.e. the element with the highest level.
        node, level = heappop!(heap, heap_order)
        worklist = Int[]
        push!(worklist, node)
        while !isempty(worklist)
            active = pop!(worklist)
            for succ in cfg.blocks[active].succs
                # Check whether the current root (`node`) dominates succ.
                # We are guaranteed that `node` dominates `active`, since
                # we've arrived at `active` by following dominator tree edges.
                # If `succ`'s level is less than or equal to that of `node`,
                # it cannot possibly be dominated by `node`. On the other hand,
                # since at this point we know that there is an edge from `node`'s
                # subtree to `succ`, we know that if succ's level is greater than
                # that of `node`, it must be dominated by `node`.
                succ_level = domtree.nodes[succ].level
                succ_level > level && continue
                # We don't dominate succ. We need to place a phinode,
                # unless liveness said otherwise.
                succ in processed && continue
                push!(processed, succ)
                if !(succ in liveness.live_in_bbs)
                    continue
                end
                push!(phiblocks, succ)
                # Basically: Consider the phi node we just added another
                # def of this value. N.B.: This needs to retain the invariant that it
                # is processed before any of its parents in the dom tree. This is guaranteed,
                # because succ_level <= level, which is the greatest level we have currently
                # processed. Thus, we have not yet processed any subtrees of level < succ_level.
                if !(succ in defs)
                    heappush!(heap, (succ, succ_level), heap_order)
                end
            end
            # Recurse down the current subtree
            for child in domtree.nodes[active].children
                child in visited && continue
                push!(visited, child)
                push!(worklist, child)
            end
        end
    end
    phiblocks
end

function rename_incoming_edge(old_edge::Int, old_to::Int, result_order::Vector{Int}, bb_rename::Vector{Int})
    old_edge == 0 && return 0
    new_edge_from = bb_rename[old_edge]
    new_edge_from < 0 && return new_edge_from
    if old_edge == old_to - 1
        # Could have been a crit edge break
        if new_edge_from < length(result_order) && result_order[new_edge_from + 1] == 0
            new_edge_from += 1
        end
    end
    new_edge_from
end

function rename_outgoing_edge(old_to::Int, old_from::Int, result_order::Vector{Int}, bb_rename::Vector{Int})
    new_edge_to = bb_rename[old_to]
    if old_from == old_to - 1
        # Could have been a crit edge break
        if bb_rename[old_from] < length(result_order) && result_order[bb_rename[old_from]+1] == 0
            new_edge_to = bb_rename[old_from] + 1
        end
    end
    new_edge_to
end

function rename_phinode_edges(node::PhiNode, bb::Int, result_order::Vector{Int}, bb_rename::Vector{Int})
    new_values = Any[]
    new_edges = Int32[]
    for (idx, edge) in pairs(node.edges)
        edge = Int(edge)
        (edge == 0 || bb_rename[edge] != -1) || continue
        new_edge_from = edge == 0 ? 0 : rename_incoming_edge(edge, bb, result_order, bb_rename)
        push!(new_edges, new_edge_from)
        if isassigned(node.values, idx)
            push!(new_values, node.values[idx])
        else
            resize!(new_values, length(new_values)+1)
        end
    end
    return PhiNode(new_edges, new_values)
end

"""
Sort the basic blocks in `ir` into domtree order (i.e. if `bb1` is higher in
the domtree than `bb2`, it will come first in the linear order). The resulting
`ir` has the property that a linear traversal of basic blocks will also be a
RPO traversal and in particular, any use of an SSA value must come after
(by linear order) its definition.
"""
function domsort_ssa!(ir::IRCode, domtree::DomTree)
    # First compute the new order of basic blocks
    result_order = Int[]
    stack = Int[]
    bb_rename = fill(-1, length(ir.cfg.blocks))
    node = 1
    ncritbreaks = 0
    nnewfallthroughs = 0
    while node !== -1
        push!(result_order, node)
        bb_rename[node] = length(result_order)
        cs = domtree.nodes[node].children
        terminator = ir.stmts[last(ir.cfg.blocks[node].stmts)][:inst]
        next_node = node + 1
        node = -1
        # Adding the nodes in reverse sorted order attempts to retain
        # the original source order of the nodes as much as possible.
        # This is not required for correctness, but is easier on the humans
        for child in Iterators.Reverse(cs)
            if child == next_node
                # Schedule the fall through node first,
                # so we can retain the fall through
                node = next_node
            else
                push!(stack, child)
            end
        end
        if node == -1 && !isempty(stack)
            node = pop!(stack)
        end
        if node != next_node && !isa(terminator, Union{GotoNode, ReturnNode})
            if isa(terminator, GotoIfNot)
                # Need to break the critical edge
                ncritbreaks += 1
                push!(result_order, 0)
            else
                nnewfallthroughs += 1
            end
        end
    end
    new_bbs = Vector{BasicBlock}(undef, length(result_order))
    nstmts = 0
    for i in result_order
        if i !== 0
            nstmts += length(ir.cfg.blocks[i].stmts)
        end
    end
    result = InstructionStream(nstmts + ncritbreaks + nnewfallthroughs)
    inst_rename = Vector{SSAValue}(undef, length(ir.stmts) + length(ir.new_nodes))
    @inbounds for i = 1:length(ir.stmts)
        inst_rename[i] = SSAValue(-1)
    end
    @inbounds for i = 1:length(ir.new_nodes)
        inst_rename[i + length(ir.stmts)] = SSAValue(i + length(result))
    end
    bb_start_off = 0
    crit_edge_breaks_fixup = Tuple{Int, Int}[]
    for (new_bb, bb) in pairs(result_order)
        if bb == 0
            nidx = bb_start_off + 1
            inst = result[nidx][:inst]
            @assert isa(inst, GotoNode)
            # N.B.: The .label has already been renamed when it was created.
            new_bbs[new_bb] = BasicBlock(nidx:nidx, [new_bb - 1], [inst.label])
            bb_start_off += 1
            continue
        end
        old_inst_range = ir.cfg.blocks[bb].stmts
        inst_range = (bb_start_off+1):(bb_start_off+length(old_inst_range))
        for (nidx, idx) in zip(inst_range, old_inst_range)
            inst_rename[idx] = SSAValue(nidx)
            @assert !isassigned(result.inst, nidx)
            node = result[nidx]
            node[] = ir.stmts[idx]
            inst = node[:inst]
            if isa(inst, PhiNode)
                node[:inst] = rename_phinode_edges(inst, bb, result_order, bb_rename)
            end
        end
        # Now fix up the terminator
        terminator = result[inst_range[end]][:inst]
        if isa(terminator, GotoNode)
            # Convert to implicit fall through
            if bb_rename[terminator.label] == new_bb + 1
                result[inst_range[end]][:inst] = nothing
            else
                result[inst_range[end]][:inst] = GotoNode(bb_rename[terminator.label])
            end
        elseif isa(terminator, GotoIfNot)
            # Check if we need to break the critical edge
            if bb_rename[bb + 1] != new_bb + 1
                @assert result_order[new_bb + 1] == 0
                # Add an explicit goto node in the next basic block (we accounted for this above)
                nidx = inst_range[end] + 1
                node = result[nidx]
                node[:inst], node[:type], node[:line] = GotoNode(bb_rename[bb + 1]), Any, 0
            end
            result[inst_range[end]][:inst] = GotoIfNot(terminator.cond, bb_rename[terminator.dest])
        elseif !isa(terminator, ReturnNode)
            if isa(terminator, Expr)
                if terminator.head === :enter
                    terminator.args[1] = bb_rename[terminator.args[1]]
                end
            end
            if bb_rename[bb + 1] != new_bb + 1
                # Add an explicit goto node
                nidx = inst_range[end] + 1
                node = result[nidx]
                node[:inst], node[:type], node[:line] = GotoNode(bb_rename[bb + 1]), Any, 0
                inst_range = first(inst_range):(last(inst_range) + 1)
            end
        end
        bb_start_off += length(inst_range)
        local new_preds, new_succs
        let bb = bb, bb_rename = bb_rename, result_order = result_order
            new_preds = Int[bb for bb in (rename_incoming_edge(i, bb, result_order, bb_rename) for i in ir.cfg.blocks[bb].preds) if bb != -1]
            new_succs = Int[             rename_outgoing_edge(i, bb, result_order, bb_rename) for i in ir.cfg.blocks[bb].succs]
        end
        new_bbs[new_bb] = BasicBlock(inst_range, new_preds, new_succs)
    end
    for i in 1:length(result)
        result[i][:inst] = renumber_ssa!(result[i][:inst], inst_rename, true)
    end
    cfg = CFG(new_bbs, Int[first(bb.stmts) for bb in new_bbs[2:end]])
    new_new_nodes = NewNodeStream(length(ir.new_nodes))
    for i = 1:length(ir.new_nodes)
        new_info = ir.new_nodes.info[i]
        new_new_info = NewNodeInfo(inst_rename[new_info.pos].id, new_info.attach_after)
        new_new_nodes.info[i] = new_new_info
        new_node = new_new_nodes.stmts[i]
        new_node[] = ir.new_nodes.stmts[i]
        new_node_inst = new_node[:inst]
        if isa(new_node_inst, PhiNode)
            new_node_inst = rename_phinode_edges(new_node_inst, block_for_inst(ir.cfg, new_info.pos), result_order, bb_rename)
        end
        new_node[:inst] = renumber_ssa!(new_node_inst, inst_rename, true)
    end
    new_ir = IRCode(ir, result, cfg, new_new_nodes)
    return new_ir
end

compute_live_ins(cfg::CFG, slot::SlotInfo) = compute_live_ins(cfg, slot.defs, slot.uses)

function compute_live_ins(cfg::CFG, defs::Vector{Int}, uses::Vector{Int})
    # We remove from `uses` any block where all uses are dominated
    # by a def. This prevents insertion of dead phi nodes at the top
    # of such a block if that block happens to be in a loop
    bb_defs = Int[] # blocks with a def
    bb_uses = Int[] # blocks with a use that is not dominated by a def

    # We do a sorted joint iteration over the instructions listed
    # in defs and uses following a pattern similar to mergesort
    last_block, block_has_def = 0, false
    defs_i = uses_i = 1
    while defs_i <= lastindex(defs) || uses_i <= lastindex(uses)
        is_def = uses_i > lastindex(uses) || defs_i <= lastindex(defs) && defs[defs_i] < uses[uses_i]
        block = block_for_inst(cfg, is_def ? defs[defs_i] : uses[uses_i])
        defs_i += is_def
        uses_i += !is_def
        if last_block != block || is_def && !block_has_def
            push!(is_def ? bb_defs : bb_uses, block)
            block_has_def = is_def
        end
        last_block = block
    end
    # To obtain live ins from bb_uses, recursively add predecessors
    extra_liveins = BitSet()
    worklist = Int[]
    for bb in bb_uses
        append!(worklist, Iterators.filter(p->p != 0 && !(p in bb_defs), cfg.blocks[bb].preds))
    end
    while !isempty(worklist)
        elem = pop!(worklist)
        (elem in bb_uses || elem in extra_liveins) && continue
        push!(extra_liveins, elem)
        append!(worklist, Iterators.filter(p->p != 0 && !(p in bb_defs), cfg.blocks[elem].preds))
    end
    append!(bb_uses, extra_liveins)
    BlockLiveness(bb_defs, bb_uses)
end

function recompute_type(node::Union{PhiNode, PhiCNode}, ci::CodeInfo, ir::IRCode,
    sptypes::Vector{Any}, slottypes::Vector{Any}, nstmts::Int, 𝕃ₒ::AbstractLattice)
    new_typ = Union{}
    for i = 1:length(node.values)
        if isa(node, PhiNode) && !isassigned(node.values, i)
            if !isa(new_typ, MaybeUndef)
                new_typ = MaybeUndef(new_typ)
            end
            continue
        end
        typ = typ_for_val(node.values[i], ci, sptypes, -1, slottypes)
        was_maybe_undef = false
        if isa(typ, MaybeUndef)
            typ = typ.typ
            was_maybe_undef = true
        end
        @assert !isa(typ, MaybeUndef)
        while isa(typ, DelayedTyp)
            typ = types(ir)[new_to_regular(typ.phi::NewSSAValue, nstmts)]
        end
        new_typ = tmerge(𝕃ₒ, new_typ, was_maybe_undef ? MaybeUndef(typ) : typ)
    end
    return new_typ
end

struct TryCatchRegion
    enter_block::Int
    leave_block::Int
end
struct NewPhiNode
    ssaval::NewSSAValue
    node::PhiNode
end
struct NewPhiCNode
    slot::SlotNumber
    ssaval::NewSSAValue
    node::PhiCNode
end

function construct_ssa!(ci::CodeInfo, ir::IRCode, domtree::DomTree,
                        defuses::Vector{SlotInfo}, slottypes::Vector{Any},
                        𝕃ₒ::AbstractLattice)
    code = ir.stmts.inst
    cfg = ir.cfg
    catch_entry_blocks = TryCatchRegion[]
    for idx in 1:length(code)
        stmt = code[idx]
        if isexpr(stmt, :enter)
            push!(catch_entry_blocks, TryCatchRegion(
                block_for_inst(cfg, idx),
                block_for_inst(cfg, stmt.args[1]::Int)))
        end
    end

    exc_handlers = IdDict{Int, TryCatchRegion}()
    # Record the correct exception handler for all cricitcal sections
    for catch_entry_block in catch_entry_blocks
        (; enter_block, leave_block) = catch_entry_block
        exc_handlers[enter_block+1] = catch_entry_block
        # TODO: Cut off here if the terminator is a leave corresponding to this enter
        for block in dominated(domtree, enter_block+1)
            exc_handlers[block] = catch_entry_block
        end
    end

    phi_slots = Vector{Int}[Int[] for _ = 1:length(ir.cfg.blocks)]
    new_phi_nodes = Vector{NewPhiNode}[NewPhiNode[] for _ = 1:length(cfg.blocks)]
    phi_ssas = SSAValue[]
    new_phic_nodes = IdDict{Int, Vector{NewPhiCNode}}()
    for (; leave_block) in catch_entry_blocks
        new_phic_nodes[leave_block] = NewPhiCNode[]
    end
    @timeit "idf" for (idx, slot) in Iterators.enumerate(defuses)
        # No uses => no need for phi nodes
        isempty(slot.uses) && continue
        # TODO: Restore this optimization
        if false # length(slot.defs) == 1 && slot.any_newvar
            if slot.defs[] == 0
                typ = slottypes[idx]
                ssaval = Argument(idx)
                fixup_uses!(ir, ci, code, slot.uses, idx, ssaval)
            elseif isa(code[slot.defs[]], NewvarNode)
                typ = MaybeUndef(Union{})
                ssaval = nothing
                for use in slot.uses[]
                    insert_node!(ir, use,
                        NewInstruction(Expr(:throw_undef_if_not, ci.slotnames[idx], false), Union{}))
                end
                fixup_uses!(ir, ci, code, slot.uses, idx, nothing)
            else
                val = code[slot.defs[]].args[2]
                typ = typ_for_val(val, ci, ir.sptypes, slot.defs[], slottypes)
                ssaval = make_ssa!(ci, code, slot.defs[], typ)
                fixup_uses!(ir, ci, code, slot.uses, idx, ssaval)
            end
            continue
        end
        @timeit "liveness" (live = compute_live_ins(cfg, slot))
        for li in live.live_in_bbs
            cidx = findfirst(x::TryCatchRegion->x.leave_block==li, catch_entry_blocks)
            if cidx !== nothing
                # The slot is live-in into this block. We need to
                # Create a PhiC node in the catch entry block and
                # an upsilon node in the corresponding enter block
                node = PhiCNode(Any[])
                phic_ssa = NewSSAValue(
                    insert_node!(ir, first_insert_for_bb(code, cfg, li),
                        NewInstruction(node, Union{})).id - length(ir.stmts))
                push!(new_phic_nodes[li], NewPhiCNode(SlotNumber(idx), phic_ssa, node))
                # Inform IDF that we now have a def in the catch block
                if !(li in live.def_bbs)
                    push!(live.def_bbs, li)
                end
            end
        end
        phiblocks = iterated_dominance_frontier(cfg, live, domtree)
        for block in phiblocks
            push!(phi_slots[block], idx)
            node = PhiNode()
            ssaval = NewSSAValue(insert_node!(ir,
                first_insert_for_bb(code, cfg, block), NewInstruction(node, Union{})).id - length(ir.stmts))
            push!(new_phi_nodes[block], NewPhiNode(ssaval, node))
        end
    end
    # Perform SSA renaming
    initial_incoming_vals = Any[
        if 0 in defuses[x].defs
            Argument(x)
        elseif !defuses[x].any_newvar
            UNDEF_TOKEN
        else
            SSAValue(-2)
        end for x in 1:length(ci.slotflags)
    ]
    worklist = Tuple{Int, Int, Vector{Any}}[(1, 0, initial_incoming_vals)]
    visited = BitSet()
    type_refine_phi = BitSet()
    new_nodes = ir.new_nodes
    @timeit "SSA Rename" while !isempty(worklist)
        (item::Int, pred, incoming_vals) = pop!(worklist)
        # Rename existing phi nodes first, because their uses occur on the edge
        # TODO: This isn't necessary if inlining stops replacing arguments by slots.
        for idx in cfg.blocks[item].stmts
            stmt = code[idx]
            if isexpr(stmt, :(=))
                stmt = stmt.args[2]
            end
            isa(stmt, PhiNode) || continue
            for (edgeidx, edge) in pairs(stmt.edges)
                from_bb = edge == 0 ? 0 : block_for_inst(cfg, Int(edge))
                from_bb == pred || continue
                isassigned(stmt.values, edgeidx) || break
                stmt.values[edgeidx] = rename_uses!(ir, ci, Int(edge), stmt.values[edgeidx], incoming_vals)
                break
            end
        end
        # Insert phi nodes if necessary
        for (idx, slot) in Iterators.enumerate(phi_slots[item])
            (; ssaval, node) = new_phi_nodes[item][idx]
            incoming_val = incoming_vals[slot]
            if incoming_val === SSAValue(-1)
                # Optimistically omit this path.
                # Liveness analysis would probably have prevented us from inserting this phi node
                continue
            end
            push!(node.edges, pred)
            if incoming_val === UNDEF_TOKEN
                resize!(node.values, length(node.values)+1)
            else
                push!(node.values, incoming_val)
            end
            # TODO: Remove the next line, it shouldn't be necessary
            push!(type_refine_phi, ssaval.id)
            if isa(incoming_val, NewSSAValue)
                push!(type_refine_phi, ssaval.id)
            end
            typ = incoming_val === UNDEF_TOKEN ? MaybeUndef(Union{}) : typ_for_val(incoming_val, ci, ir.sptypes, -1, slottypes)
            old_entry = new_nodes.stmts[ssaval.id]
            if isa(typ, DelayedTyp)
                push!(type_refine_phi, ssaval.id)
            end
            new_typ = isa(typ, DelayedTyp) ? Union{} : tmerge(𝕃ₒ, old_entry[:type], typ)
            old_entry[:type] = new_typ
            old_entry[:inst] = node
            incoming_vals[slot] = ssaval
        end
        (item in visited) && continue
        # Record phi_C nodes if necessary
        if haskey(new_phic_nodes, item)
            for (; slot, ssaval) in new_phic_nodes[item]
                incoming_vals[slot_id(slot)] = ssaval
            end
        end
        # Record initial upsilon nodes if necessary
        eidx = findfirst((; enter_block)::TryCatchRegion->enter_block==item, catch_entry_blocks)
        if eidx !== nothing
            for (; slot, node) in new_phic_nodes[catch_entry_blocks[eidx].leave_block]
                ival = incoming_vals[slot_id(slot)]
                ivalundef = ival === UNDEF_TOKEN
                Υ = NewInstruction(ivalundef ? UpsilonNode() : UpsilonNode(ival),
                                   ivalundef ? MaybeUndef(Union{}) : typ_for_val(ival, ci, ir.sptypes, -1, slottypes))
                # insert `UpsilonNode` immediately before the `:enter` expression
                Υssa = insert_node!(ir, first_insert_for_bb(code, cfg, item), Υ)
                push!(node.values, NewSSAValue(Υssa.id - length(ir.stmts)))
            end
        end
        push!(visited, item)
        for idx in cfg.blocks[item].stmts
            stmt = code[idx]
            (isa(stmt, PhiNode) || (isexpr(stmt, :(=)) && isa(stmt.args[2], PhiNode))) && continue
            if isa(stmt, NewvarNode)
                incoming_vals[slot_id(stmt.slot)] = UNDEF_TOKEN
                code[idx] = nothing
            else
                stmt = rename_uses!(ir, ci, idx, stmt, incoming_vals)
                if stmt === nothing && isa(code[idx], Union{ReturnNode, GotoIfNot}) && idx == last(cfg.blocks[item].stmts)
                    # preserve the CFG
                    stmt = ReturnNode()
                end
                code[idx] = stmt
                # Record a store
                if isexpr(stmt, :(=))
                    arg1 = stmt.args[1]
                    if isa(arg1, SlotNumber)
                        id = slot_id(arg1)
                        val = stmt.args[2]
                        typ = typ_for_val(val, ci, ir.sptypes, idx, slottypes)
                        # Having UNDEF_TOKEN appear on the RHS is possible if we're on a dead branch.
                        # Do something reasonable here, by marking the LHS as undef as well.
                        if val !== UNDEF_TOKEN
                            incoming_vals[id] = make_ssa!(ci, code, idx, typ)
                        else
                            code[idx] = nothing
                            incoming_vals[id] = UNDEF_TOKEN
                        end
                        enter_block = item
                        while haskey(exc_handlers, enter_block)
                            (; enter_block, leave_block) = exc_handlers[enter_block]
                            cidx = findfirst((; slot)::NewPhiCNode->slot_id(slot)==id, new_phic_nodes[leave_block])
                            if cidx !== nothing
                                node = UpsilonNode(incoming_vals[id])
                                if incoming_vals[id] === UNDEF_TOKEN
                                    node = UpsilonNode()
                                    typ = MaybeUndef(Union{})
                                end
                                push!(new_phic_nodes[leave_block][cidx].node.values,
                                      NewSSAValue(insert_node!(ir, idx, NewInstruction(node, typ), true).id - length(ir.stmts)))
                            end
                        end
                    end
                end
            end
        end
        for succ in cfg.blocks[item].succs
            push!(worklist, (succ, item, copy(incoming_vals)))
        end
    end
    # Delete any instruction in unreachable blocks (except for terminators)
    for bb in setdiff(BitSet(1:length(cfg.blocks)), visited)
        for idx in cfg.blocks[bb].stmts
            if isa(code[idx], Union{GotoNode, GotoIfNot, ReturnNode})
                code[idx] = ReturnNode()
            else
                code[idx] = nothing
            end
        end
    end
    # Convert into IRCode form
    ssavaluetypes = ci.ssavaluetypes::Vector{Any}
    nstmts = length(ir.stmts)
    new_code = Vector{Any}(undef, nstmts)
    ssavalmap = fill(SSAValue(-1), length(ssavaluetypes) + 1)
    result_types = Any[Any for _ in 1:nstmts]
    # Detect statement positions for assignments and construct array
    for (bb, idx) in bbidxiter(ir)
        stmt = code[idx]
        # Convert GotoNode/GotoIfNot/PhiNode to BB addressing
        if isa(stmt, GotoNode)
            new_code[idx] = GotoNode(block_for_inst(cfg, stmt.label))
        elseif isa(stmt, GotoIfNot)
            new_dest = block_for_inst(cfg, stmt.dest)
            if new_dest == bb+1
                # Drop this node - it's a noop
                new_code[idx] = Expr(:call, GlobalRef(Core, :typeassert), stmt.cond, GlobalRef(Core, :Bool))
            else
                new_code[idx] = GotoIfNot(stmt.cond, new_dest)
            end
        elseif isexpr(stmt, :enter)
            new_code[idx] = Expr(:enter, block_for_inst(cfg, stmt.args[1]::Int))
            ssavalmap[idx] = SSAValue(idx) # Slot to store token for pop_exception
        elseif isexpr(stmt, :leave) || isexpr(stmt, :(=)) || isa(stmt, ReturnNode) ||
            isexpr(stmt, :meta) || isa(stmt, NewvarNode)
            new_code[idx] = stmt
        else
            ssavalmap[idx] = SSAValue(idx)
            result_types[idx] = ssavaluetypes[idx]
            if isa(stmt, PhiNode)
                edges = Int32[edge == 0 ? 0 : block_for_inst(cfg, Int(edge)) for edge in stmt.edges]
                new_code[idx] = PhiNode(edges, stmt.values)
            else
                new_code[idx] = stmt
            end
        end
    end
    for (_, nodes) in new_phic_nodes
        for (; ssaval, node) in nodes
            new_typ = Union{}
            # TODO: This could just be the ones that depend on other phis
            push!(type_refine_phi, ssaval.id)
            new_idx = ssaval.id
            node = new_nodes.stmts[new_idx]
            phic_values = (node[:inst]::PhiCNode).values
            for i = 1:length(phic_values)
                orig_typ = typ = typ_for_val(phic_values[i], ci, ir.sptypes, -1, slottypes)
                @assert !isa(typ, MaybeUndef)
                while isa(typ, DelayedTyp)
                    typ = types(ir)[new_to_regular(typ.phi::NewSSAValue, nstmts)]
                end
                new_typ = tmerge(𝕃ₒ, new_typ, typ)
            end
            node[:type] = new_typ
        end
    end
    # This is a bit awkward, because it basically duplicates what type
    # inference does. Ideally, we'd just use this representation earlier
    # to make sure phi nodes have accurate types
    changed = true
    while changed
        changed = false
        for new_idx in type_refine_phi
            node = new_nodes.stmts[new_idx]
            new_typ = recompute_type(node[:inst]::Union{PhiNode,PhiCNode}, ci, ir, ir.sptypes, slottypes, nstmts, 𝕃ₒ)
            if !⊑(𝕃ₒ, node[:type], new_typ) || !⊑(𝕃ₒ, new_typ, node[:type])
                node[:type] = new_typ
                changed = true
            end
        end
    end
    for i in 1:length(result_types)
        rt_i = result_types[i]
        if rt_i isa DelayedTyp
            result_types[i] = types(ir)[new_to_regular(rt_i.phi::NewSSAValue, nstmts)]
        end
    end
    for i = 1:length(new_nodes)
        local node = new_nodes.stmts[i]
        local typ = node[:type]
        if isa(typ, DelayedTyp)
            node[:type] = types(ir)[new_to_regular(typ.phi::NewSSAValue, nstmts)]
        end
    end
    # Renumber SSA values
    @assert isempty(ir.stmts.type)
    resize!(ir.stmts.type, nstmts)
    for i in 1:nstmts
        local node = ir.stmts[i]
        node[:inst] = new_to_regular(renumber_ssa!(new_code[i], ssavalmap), nstmts)
        node[:type] = result_types[i]
    end
    for i = 1:length(new_nodes)
        local node = new_nodes.stmts[i]
        node[:inst] = new_to_regular(renumber_ssa!(node[:inst], ssavalmap), nstmts)
    end
    @timeit "domsort" ir = domsort_ssa!(ir, domtree)
    return ir
end
