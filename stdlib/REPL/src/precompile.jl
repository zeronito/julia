function _precompile_()
    ccall(:jl_generating_output, Cint, ()) == 1 || return nothing
    precompile(Tuple{typeof(Base.__atreplinit), REPL.LineEditREPL})
    precompile(Tuple{typeof(Base.banner), REPL.Terminals.TTYTerminal})
    precompile(Tuple{typeof(Base._setindex!), Base.Dict{Symbol, Any}, REPL.LineEdit.Prompt, Symbol, Int64})
    precompile(Tuple{typeof(Base.setindex!), Base.Dict{Symbol, Any}, REPL.LineEdit.Prompt, Symbol})
    precompile(Tuple{typeof(REPL.LineEdit.getEntry), Base.Dict{Char, Any}, Char})
    precompile(Tuple{typeof(REPL.LineEdit.getEntry), Base.Dict{Char, Any}, String})
    precompile(Tuple{typeof(Base.promote_type), Type{REPL.LineEdit.HistoryPrompt{REPLHistoryProvider}}, Type{REPL.LineEdit.PrefixHistoryPrompt{REPLHistoryProvider}}})
    precompile(Tuple{typeof(Base.promote_rule), Type{REPL.LineEdit.HistoryPrompt{REPLHistoryProvider}}, Type{REPL.LineEdit.PrefixHistoryPrompt{REPLHistoryProvider}}})
    precompile(Tuple{typeof(Base.promote_rule), Type{REPL.LineEdit.PrefixHistoryPrompt{REPLHistoryProvider}}, Type{REPL.LineEdit.HistoryPrompt{REPLHistoryProvider}}})
    precompile(Tuple{typeof(Base.promote_result), Type{REPL.LineEdit.HistoryPrompt{REPLHistoryProvider}}, Type{REPL.LineEdit.PrefixHistoryPrompt{REPLHistoryProvider}}, Type{Union{}}, Type{Union{}}})
    precompile(Tuple{typeof(Base.promote_type), Type{REPL.LineEdit.Prompt}, Type{REPL.LineEdit.TextInterface}})
    precompile(Tuple{typeof(Base.promote_rule), Type{REPL.LineEdit.Prompt}, Type{REPL.LineEdit.TextInterface}})
    precompile(Tuple{typeof(Base.promote_rule), Type{REPL.LineEdit.TextInterface}, Type{REPL.LineEdit.Prompt}})
    precompile(Tuple{typeof(Base.promote_result), Type{REPL.LineEdit.Prompt}, Type{REPL.LineEdit.TextInterface}, Type{Union{}}, Type{Union{}}})
    precompile(Tuple{typeof(Base.setindex!), Array{REPL.LineEdit.TextInterface, 1}, REPL.LineEdit.Prompt, Int64})
    precompile(Tuple{typeof(Base.setindex!), Array{REPL.LineEdit.TextInterface, 1}, REPL.LineEdit.HistoryPrompt{REPLHistoryProvider}, Int64})
    precompile(Tuple{typeof(Base.setindex!), Array{REPL.LineEdit.TextInterface, 1}, REPL.LineEdit.PrefixHistoryPrompt{REPLHistoryProvider}, Int64})
    precompile(Tuple{typeof(Base.copyto!), Array{REPL.LineEdit.TextInterface, 1}, Tuple{REPL.LineEdit.Prompt, REPL.LineEdit.Prompt, REPL.LineEdit.Prompt, REPL.LineEdit.HistoryPrompt{REPLHistoryProvider}, REPL.LineEdit.PrefixHistoryPrompt{REPLHistoryProvider}}})
    precompile(Tuple{typeof(REPL.Terminals.cmove_up), REPL.Terminals.TerminalBuffer, Int64})
    precompile(Tuple{typeof(REPL.Terminals.cmove_down), REPL.Terminals.TerminalBuffer, Int64})
    precompile(Tuple{typeof(REPL.Terminals.cmove_right), REPL.Terminals.TerminalBuffer, Int64})
    precompile(Tuple{typeof(REPL.LineEdit._clear_input_area), REPL.Terminals.TerminalBuffer, REPL.LineEdit.InputAreaState})
    precompile(Tuple{getfield(Base, Symbol("#kw##readline")), Array{Any, 1}, typeof(Base.readline), REPL.Terminals.TerminalBuffer})
    precompile(Tuple{typeof(REPL.LineEdit.write_prompt), REPL.Terminals.TerminalBuffer, REPL.LineEdit.Prompt})
    precompile(Tuple{getfield(REPL.LineEdit, Symbol("#kw##refresh_multi_line")), Array{Any, 1}, typeof(REPL.LineEdit.refresh_multi_line), REPL.Terminals.TerminalBuffer, REPL.Terminals.TerminalBuffer, REPL.Terminals.TerminalBuffer, REPL.LineEdit.InputAreaState, String})
    precompile(Tuple{typeof(REPL.LineEdit.update_key_repeats), REPL.LineEdit.MIState, Array{Char, 1}})
    precompile(Tuple{typeof(REPL.LineEdit.reset_state), REPL.LineEdit.MIState})
    precompile(Tuple{typeof(Base.write), REPL.Terminals.TerminalBuffer, Array{UInt8, 1}})
    precompile(Tuple{typeof(REPL.LineEdit.keymap), Array{Base.Dict{Any, Any}, 1}})
    precompile(Tuple{typeof(REPL.LineEdit.add_specialisations), Base.Dict{Char, Any}, Base.Dict{Char, Any}, Int64})
    precompile(Tuple{typeof(REPL.LineEdit.keymap_merge), Base.Dict{Char, Any}, Base.Dict{Any, Any}})
    precompile(Tuple{typeof(REPL.LineEdit.postprocess!), Base.Dict{Char, Any}})
    precompile(Tuple{typeof(REPL.LineEdit.keymap_unify), Array{Base.Dict{Any, Any}, 1}})
    precompile(Tuple{typeof(REPL.LineEdit.validate_keymap), Base.Dict{Char, Any}})
    precompile(Tuple{getfield(Core, Symbol("#kw#Type")), Array{Any, 1}, Type{REPL.LineEdit.Prompt}, String})
    precompile(Tuple{Type{Base.Dict{Symbol, Any}}, Base.Pair{Symbol, REPL.LineEdit.Prompt}, Base.Pair{Symbol, REPL.LineEdit.Prompt}, Base.Pair{Symbol, REPL.LineEdit.Prompt}})
    precompile(Tuple{Type{REPLHistoryProvider}, Base.Dict{Symbol, Any}})
    precompile(Tuple{typeof(find_hist_file)})
    precompile(Tuple{typeof(history_reset_state), REPLHistoryProvider})
    precompile(Tuple{typeof(REPL.LineEdit.setup_search_keymap), REPLHistoryProvider})
    precompile(Tuple{typeof(REPL.LineEdit.setup_prefix_keymap), REPLHistoryProvider, REPL.LineEdit.Prompt})
    precompile(Tuple{typeof(mode_keymap), REPL.LineEdit.Prompt})
    precompile(Tuple{typeof(Base.Multimedia.popdisplay), REPLDisplay{REPL.LineEditREPL}})
    precompile(Tuple{typeof(ends_with_semicolon), String})
    precompile(Tuple{typeof(Base.Multimedia.popdisplay), REPLDisplay{BasicREPL}})
    precompile(Tuple{typeof(Base.Multimedia.popdisplay), REPLDisplay{StreamREPL}})
    precompile(Tuple{typeof(start_repl_backend), Base.Channel{Any}, Base.Channel{Any}})
    precompile(Tuple{typeof(REPL.Terminals.hascolor), REPL.Terminals.TTYTerminal})
    precompile(Tuple{Type{REPL.LineEditREPL}, REPL.Terminals.TTYTerminal, Bool})
    precompile(Tuple{Type{REPL.Terminals.TTYTerminal}, String, Base.TTY, Base.TTY, Base.IOStream})
    precompile(Tuple{typeof(Base.:(==)), Base.Multimedia.TextDisplay, REPLDisplay{REPL.LineEditREPL}})
    precompile(Tuple{typeof(Base.:(==)), REPLDisplay{REPL.LineEditREPL}, REPLDisplay{REPL.LineEditREPL}})
    precompile(Tuple{typeof(hist_from_file), REPLHistoryProvider, Base.IOStream, String})
    precompile(Tuple{typeof(hist_getline), Base.IOStream})
    precompile(Tuple{typeof(Base.setindex!), Base.Dict{Any, Any}, REPL.LineEdit.KeyAlias, Int64})
    precompile(Tuple{typeof(Base._setindex!), Base.Dict{Any, Any}, REPL.LineEdit.KeyAlias, Int64, Int64})
    precompile(Tuple{typeof(Base.setindex!), Base.Dict{Any, Any}, REPL.LineEdit.KeyAlias, String})
    precompile(Tuple{typeof(Base._setindex!), Base.Dict{Any, Any}, REPL.LineEdit.KeyAlias, String, Int64})
    precompile(Tuple{getfield(REPL.LineEdit, Symbol("#kw##add_nested_key!")), Array{Any, 1}, typeof(REPL.LineEdit.add_nested_key!), Base.Dict{Char, Any}, String, Nothing})
    precompile(Tuple{getfield(REPL.LineEdit, Symbol("#kw##add_nested_key!")), Array{Any, 1}, typeof(REPL.LineEdit.add_nested_key!), Base.Dict{Char, Any}, String, REPL.LineEdit.KeyAlias})
    precompile(Tuple{typeof(Base._setindex!), Base.Dict{Char, Any}, REPL.LineEdit.KeyAlias, Char, Int64})
    precompile(Tuple{typeof(Base.setindex!), Base.Dict{Char, Any}, REPL.LineEdit.KeyAlias, Char})
    precompile(Tuple{typeof(REPL.LineEdit.fixup_keymaps!), Base.Dict{Char, Any}, Int64, Char, Nothing})
    precompile(Tuple{typeof(REPL.LineEdit.run_interface), REPL.Terminals.TTYTerminal, REPL.LineEdit.ModalInterface})
    precompile(Tuple{getfield(REPL.LineEdit, Symbol("#kw##refresh_multi_line")), Array{Any, 1}, typeof(REPL.LineEdit.refresh_multi_line), REPL.Terminals.TerminalBuffer, REPL.Terminals.TTYTerminal, Base.GenericIOBuffer{Array{UInt8, 1}}, REPL.LineEdit.InputAreaState, REPL.LineEdit.PromptState})
    precompile(Tuple{getfield(REPL.LineEdit, Symbol("#kw##refresh_multi_line")), Array{Any, 1}, typeof(REPL.LineEdit.refresh_multi_line), REPL.Terminals.TerminalBuffer, REPL.Terminals.TTYTerminal, Base.GenericIOBuffer{Array{UInt8, 1}}, REPL.LineEdit.InputAreaState, REPL.LineEdit.PrefixSearchState})
    precompile(Tuple{typeof(Base.write), REPL.Terminals.TTYTerminal, Array{UInt8, 1}})
    precompile(Tuple{typeof(REPL.LineEdit.init_state), REPL.Terminals.TTYTerminal, REPL.LineEdit.ModalInterface})
    precompile(Tuple{typeof(REPL.LineEdit.prompt!), REPL.Terminals.TTYTerminal, REPL.LineEdit.ModalInterface, REPL.LineEdit.MIState})
    precompile(Tuple{typeof(Base.getindex), Array{REPL.LineEdit.TextInterface, 1}, Int64})
    precompile(Tuple{typeof(Base.start), Array{REPL.LineEdit.TextInterface, 1}})
    precompile(Tuple{typeof(Base.done), Array{REPL.LineEdit.TextInterface, 1}, Int64})
    precompile(Tuple{typeof(Base.next), Array{REPL.LineEdit.TextInterface, 1}, Int64})
    precompile(Tuple{typeof(REPL.LineEdit.init_state), REPL.Terminals.TTYTerminal, REPL.LineEdit.Prompt})
    precompile(Tuple{typeof(Base.setindex!), Base.Dict{Any, Any}, REPL.LineEdit.PromptState, REPL.LineEdit.Prompt})
    precompile(Tuple{typeof(Base.ht_keyindex2!), Base.Dict{Any, Any}, REPL.LineEdit.Prompt})
    precompile(Tuple{typeof(Base._setindex!), Base.Dict{Any, Any}, REPL.LineEdit.PromptState, REPL.LineEdit.Prompt, Int64})
    precompile(Tuple{typeof(REPL.LineEdit.init_state), REPL.Terminals.TTYTerminal, REPL.LineEdit.HistoryPrompt{REPLHistoryProvider}})
    precompile(Tuple{typeof(Base.setindex!), Base.Dict{Any, Any}, REPL.LineEdit.SearchState, REPL.LineEdit.HistoryPrompt{REPLHistoryProvider}})
    precompile(Tuple{typeof(Base.ht_keyindex2!), Base.Dict{Any, Any}, REPL.LineEdit.HistoryPrompt{REPLHistoryProvider}})
    precompile(Tuple{typeof(Base._setindex!), Base.Dict{Any, Any}, REPL.LineEdit.SearchState, REPL.LineEdit.HistoryPrompt{REPLHistoryProvider}, Int64})
    precompile(Tuple{typeof(REPL.LineEdit.init_state), REPL.Terminals.TTYTerminal, REPL.LineEdit.PrefixHistoryPrompt{REPLHistoryProvider}})
    precompile(Tuple{typeof(Base.setindex!), Base.Dict{Any, Any}, REPL.LineEdit.PrefixSearchState, REPL.LineEdit.PrefixHistoryPrompt{REPLHistoryProvider}})
    precompile(Tuple{typeof(Base.ht_keyindex2!), Base.Dict{Any, Any}, REPL.LineEdit.PrefixHistoryPrompt{REPLHistoryProvider}})
    precompile(Tuple{typeof(Base._setindex!), Base.Dict{Any, Any}, REPL.LineEdit.PrefixSearchState, REPL.LineEdit.PrefixHistoryPrompt{REPLHistoryProvider}, Int64})
    precompile(Tuple{typeof(REPL.LineEdit.activate), REPL.LineEdit.Prompt, REPL.LineEdit.MIState, REPL.Terminals.TTYTerminal, REPL.Terminals.TTYTerminal})
    precompile(Tuple{typeof(Base.:(==)), REPL.LineEdit.Prompt, REPL.LineEdit.Prompt})
    precompile(Tuple{typeof(Base.getindex), Base.Dict{Any, Any}, REPL.LineEdit.Prompt})
    precompile(Tuple{typeof(Base.ht_keyindex), Base.Dict{Any, Any}, REPL.LineEdit.Prompt})
    precompile(Tuple{typeof(REPL.LineEdit.activate), REPL.LineEdit.Prompt, REPL.LineEdit.PromptState, REPL.Terminals.TTYTerminal, REPL.Terminals.TTYTerminal})
    precompile(Tuple{typeof(REPL.LineEdit.refresh_multi_line), REPL.Terminals.TerminalBuffer, REPL.Terminals.TerminalBuffer, REPL.Terminals.TTYTerminal, REPL.LineEdit.PromptState})
    precompile(Tuple{getfield(REPL.LineEdit, Symbol("#kw##refresh_multi_line")), Array{Any, 1}, typeof(REPL.LineEdit.refresh_multi_line), REPL.Terminals.TerminalBuffer, REPL.Terminals.TerminalBuffer, REPL.Terminals.TTYTerminal, REPL.LineEdit.PromptState})
    precompile(Tuple{getfield(REPL.LineEdit, Symbol("#kw##refresh_multi_line")), Array{Any, 1}, typeof(REPL.LineEdit.refresh_multi_line), REPL.Terminals.TerminalBuffer, REPL.Terminals.TTYTerminal, REPL.LineEdit.PromptState})
    precompile(Tuple{typeof(REPL.LineEdit.refresh_multi_line), REPL.Terminals.TerminalBuffer, REPL.Terminals.TTYTerminal, REPL.LineEdit.PromptState})
    precompile(Tuple{typeof(REPL.LineEdit.match_input), Base.Dict{Char, Any}, REPL.LineEdit.MIState})
    precompile(Tuple{typeof(REPL.LineEdit.match_input), Base.Dict{Char, Any}, REPL.LineEdit.MIState, Base.GenericIOBuffer{Array{UInt8, 1}}, Array{Char, 1}, Base.Dict{Char, Any}})
    precompile(Tuple{typeof(REPL.LineEdit.terminal), REPL.LineEdit.PromptState})
    precompile(Tuple{typeof(REPL.LineEdit.match_input), Base.Dict{Char, Any}, REPL.LineEdit.MIState, REPL.Terminals.TTYTerminal, Array{Char, 1}, Base.Dict{Char, Any}})
    precompile(Tuple{typeof(Base.read), REPL.Terminals.TTYTerminal, Type{Char}})
    precompile(Tuple{typeof(REPL.LineEdit.edit_splice!), Base.GenericIOBuffer{Array{UInt8, 1}}, Base.Pair{Int,Int}, String})
    precompile(Tuple{typeof(REPL.LineEdit.refresh_multi_line), REPL.Terminals.TerminalBuffer, REPL.LineEdit.PromptState})
    precompile(Tuple{typeof(REPL.LineEdit.edit_insert), Base.GenericIOBuffer{Array{UInt8, 1}}, String})
    precompile(Tuple{typeof(REPL.LineEdit.edit_insert), REPL.LineEdit.PromptState, String})
    precompile(Tuple{typeof(REPL.Terminals.width), REPL.Terminals.TTYTerminal})
    precompile(Tuple{typeof(mode_idx), REPLHistoryProvider, REPL.LineEdit.Prompt})
    precompile(Tuple{typeof(REPL.LineEdit.commit_line), REPL.LineEdit.MIState})
    precompile(Tuple{typeof(REPL.LineEdit.on_enter), REPL.LineEdit.PromptState})
    precompile(Tuple{typeof(return_callback), REPL.LineEdit.PromptState})
    precompile(Tuple{typeof(REPL.LineEdit.refresh_multi_line), REPL.LineEdit.PromptState})
    precompile(Tuple{typeof(REPL.LineEdit.refresh_multi_line), REPL.Terminals.TTYTerminal, REPL.LineEdit.PromptState})
    precompile(Tuple{typeof(Base.println), REPL.Terminals.TTYTerminal})
    precompile(Tuple{typeof(Base.write), REPL.Terminals.TTYTerminal, Char})
    precompile(Tuple{typeof(REPL.LineEdit.add_history), REPL.LineEdit.PromptState})
    precompile(Tuple{typeof(REPL.LineEdit.add_history), REPLHistoryProvider, REPL.LineEdit.PromptState})
    precompile(Tuple{typeof(REPL.LineEdit.mode), REPL.LineEdit.PromptState})
    precompile(Tuple{typeof(reset), REPL.LineEditREPL})
    precompile(Tuple{typeof(eval_user_input), Expr, REPLBackend})
    precompile(Tuple{typeof(print_response), REPL.LineEditREPL, Int64, Nothing, Bool, Bool})
    precompile(Tuple{typeof(print_response), REPL.Terminals.TTYTerminal, Int64, Nothing, Bool, Bool, Nothing})
    precompile(Tuple{typeof(Base.print), REPL.Terminals.TTYTerminal, DataType})
    precompile(Tuple{typeof(Base.print), REPL.Terminals.TTYTerminal, Char})
    precompile(Tuple{typeof(Base.print), REPL.Terminals.TTYTerminal, String, DataType, String, Char})
    precompile(Tuple{typeof(Base.print), REPL.Terminals.TTYTerminal, String, Char})
    precompile(Tuple{Type{Base.IOContext{REPL.Terminals.TTYTerminal}}, REPL.Terminals.TTYTerminal, Base.ImmutableDict{Symbol, Any}})
    precompile(Tuple{getfield(Base, Symbol("#kw##with_output_color")), Array{Any, 1}, typeof(Base.with_output_color), typeof(Base.print), Symbol, Base.IOContext{REPL.Terminals.TTYTerminal}, String})
    precompile(Tuple{getfield(Base, Symbol("#kw##with_output_color")), Array{Any, 1}, typeof(Base.with_output_color), typeof(Base.print), Int64, Base.IOContext{REPL.Terminals.TTYTerminal}, String})
    precompile(Tuple{typeof(Base.write), Base.IOContext{REPL.Terminals.TTYTerminal}, Symbol})
    precompile(Tuple{typeof(Base.show), Base.IOContext{REPL.Terminals.TTYTerminal}, Module})
    precompile(Tuple{typeof(Base.print), Base.IOContext{REPL.Terminals.TTYTerminal}, String})
    precompile(Tuple{typeof(Base.print), Base.IOContext{REPL.Terminals.TTYTerminal}, Module})
    precompile(Tuple{typeof(Base.show), Base.IOContext{REPL.Terminals.TTYTerminal}, Int32})
    precompile(Tuple{typeof(Base.print), Base.IOContext{REPL.Terminals.TTYTerminal}, Symbol})
    precompile(Tuple{typeof(Base.print), Base.IOContext{REPL.Terminals.TTYTerminal}, Int32})
    precompile(Tuple{typeof(Base.show), Base.IOContext{REPL.Terminals.TTYTerminal}, Int64})
    precompile(Tuple{typeof(Base.print), Base.IOContext{REPL.Terminals.TTYTerminal}, Int64})
    precompile(Tuple{typeof(Base.print), Base.IOContext{REPL.Terminals.TTYTerminal}, String, Int64, String})
    precompile(Tuple{typeof(Base.write), Base.IOContext{REPL.Terminals.TTYTerminal}, Char})
    precompile(Tuple{typeof(Base.show_circular), Base.IOContext{REPL.Terminals.TTYTerminal}, Array{Any, 1}})
    precompile(Tuple{typeof(Base.show_delim_array), Base.IOContext{REPL.Terminals.TTYTerminal}, Array{Any, 1}, Char, Char, Char, Bool, Int64, Int64})
    precompile(Tuple{typeof(Base.join), Base.IOContext{REPL.Terminals.TTYTerminal}, Array{Any, 1}, String, String})
    precompile(Tuple{typeof(Base.join), Base.IOContext{REPL.Terminals.TTYTerminal}, Tuple{}, String, String})
    precompile(Tuple{typeof(Base.show_method_params), Base.IOContext{REPL.Terminals.TTYTerminal}, Array{Any, 1}})
    precompile(Tuple{typeof(Base.print), Base.IOContext{REPL.Terminals.TTYTerminal}, String, Module})
    precompile(Tuple{typeof(Base.print), Base.IOContext{REPL.Terminals.TTYTerminal}, String, Symbol, String, Int32})
    precompile(Tuple{typeof(Base.show_tuple_as_call), Base.IOContext{REPL.Terminals.TTYTerminal}, Symbol, Type})
    precompile(Tuple{typeof(getfield(Base, Symbol("#kw##printstyled"))), Array{Any,1}, typeof(Base.printstyled), Base.IOContext{REPL.Terminals.TTYTerminal}, String})
    precompile(Tuple{typeof(Base.show), Base.IOContext{REPL.Terminals.TTYTerminal}, Core.MethodInstance})
    precompile(Tuple{typeof(Base.StackTraces.show_spec_linfo), Base.IOContext{REPL.Terminals.TTYTerminal}, Base.StackTraces.StackFrame})
    precompile(Tuple{getfield(Base, Symbol("#kw##show")), Array{Any, 1}, typeof(Base.show), Base.IOContext{REPL.Terminals.TTYTerminal}, Base.StackTraces.StackFrame})
    precompile(Tuple{getfield(Base, Symbol("#kw##show_trace_entry")), Array{Any, 1}, typeof(Base.show_trace_entry), Base.IOContext{REPL.Terminals.TTYTerminal}, Base.StackTraces.StackFrame, Int64})
    precompile(Tuple{typeof(Base.show_backtrace), REPL.Terminals.TTYTerminal, Array{Ptr{Cvoid}, 1}})
    precompile(Tuple{typeof(Base.Multimedia.display), REPLDisplay{REPL.LineEditREPL}, Int64})
    precompile(Tuple{typeof(Base.Multimedia.display), REPLDisplay{REPL.LineEditREPL}, Base.MIME{Symbol("text/plain")}, Int64})
    precompile(Tuple{typeof(REPL.LineEdit.reset_state), REPL.LineEdit.PromptState})
    precompile(Tuple{typeof(REPL.LineEdit.reset_state), REPL.LineEdit.SearchState})
    precompile(Tuple{typeof(REPL.LineEdit.reset_state), REPLHistoryProvider})
    precompile(Tuple{typeof(REPL.LineEdit.reset_state), REPL.LineEdit.PrefixSearchState})
    precompile(Tuple{typeof(Base.haskey), Base.Dict{Any, Any}, REPL.LineEdit.Prompt})
    precompile(Tuple{typeof(REPL.LineEdit.deactivate), REPL.LineEdit.Prompt, REPL.LineEdit.PromptState, REPL.Terminals.TerminalBuffer, REPL.Terminals.TTYTerminal})
    precompile(Tuple{typeof(REPL.LineEdit.activate), REPL.LineEdit.Prompt, REPL.LineEdit.PromptState, REPL.Terminals.TerminalBuffer, REPL.Terminals.TTYTerminal})
    precompile(Tuple{typeof(REPL.LineEdit.commit_changes), REPL.Terminals.TTYTerminal, REPL.Terminals.TerminalBuffer})
    precompile(Tuple{typeof(REPL.LineEdit.complete_line), REPL.LineEdit.PromptState, Int64})
    precompile(Tuple{typeof(REPL.LineEdit.input_string_newlines_aftercursor), REPL.LineEdit.PromptState})
    precompile(Tuple{typeof(REPL.LineEdit.complete_line), REPL.REPLCompletionProvider, REPL.LineEdit.PromptState})
    precompile(Tuple{getfield(REPLCompletions, Symbol("#kw##find_start_brace")), Array{Any, 1}, typeof(REPLCompletions.find_start_brace), String})
    precompile(Tuple{typeof(REPLCompletions.dict_identifier_key), String, Symbol})
    precompile(Tuple{typeof(REPLCompletions.bslash_completions), String, Int64})
    precompile(Tuple{typeof(REPLCompletions.should_method_complete), String})
    precompile(Tuple{typeof(REPLCompletions.afterusing), String, Int64})
    precompile(Tuple{typeof(REPLCompletions.complete_keyword), String})
    precompile(Tuple{typeof(beforecursor), Base.GenericIOBuffer{Array{UInt8, 1}}})
    precompile(Tuple{typeof(REPLCompletions.completions), String, Int64})
    precompile(Tuple{typeof(REPL.LineEdit.common_prefix), Array{String, 1}})
    precompile(Tuple{typeof(REPL.LineEdit.show_completions), REPL.LineEdit.PromptState, Array{String, 1}})
    precompile(Tuple{typeof(REPL.Terminals.cmove_down), REPL.Terminals.TTYTerminal, Int64})
    precompile(Tuple{typeof(REPL.Terminals.cmove_col), REPL.Terminals.TTYTerminal, Int64})
    precompile(Tuple{typeof(REPL.Terminals.cmove_right), REPL.Terminals.TTYTerminal, Int64})
    precompile(Tuple{typeof(REPL.LineEdit.match_input), REPL.LineEdit.KeyAlias, REPL.LineEdit.MIState, REPL.Terminals.TTYTerminal, Array{Char, 1}, Base.Dict{Char, Any}})
    precompile(Tuple{typeof(REPL.LineEdit.char_move_left), Base.GenericIOBuffer{Array{UInt8, 1}}})
    precompile(Tuple{typeof(REPL.LineEdit.edit_backspace), Base.GenericIOBuffer{Array{UInt8, 1}}})
    precompile(Tuple{typeof(REPL.LineEdit.edit_backspace), REPL.LineEdit.PromptState})
    precompile(Tuple{typeof(REPL.Terminals.beep), REPL.Terminals.TTYTerminal})
    precompile(Tuple{typeof(REPL.LineEdit.edit_move_down), Base.GenericIOBuffer{Array{UInt8, 1}}})
    precompile(Tuple{typeof(REPL.LineEdit.replace_line), REPL.LineEdit.PrefixSearchState, String})
    precompile(Tuple{typeof(REPL.LineEdit.refresh_multi_line), REPL.Terminals.TerminalBuffer, REPL.LineEdit.PrefixSearchState})
    precompile(Tuple{typeof(history_move), REPL.LineEdit.PrefixSearchState, REPLHistoryProvider, Int64, Int64})
    precompile(Tuple{typeof(REPL.LineEdit.edit_move_down), REPL.LineEdit.MIState})
    precompile(Tuple{typeof(REPL.LineEdit.enter_prefix_search), REPL.LineEdit.MIState, REPL.LineEdit.PrefixHistoryPrompt{REPLHistoryProvider}, Bool})
    precompile(Tuple{typeof(Base.haskey), Base.Dict{Any, Any}, REPL.LineEdit.PrefixHistoryPrompt{REPLHistoryProvider}})
    precompile(Tuple{typeof(Base.ht_keyindex), Base.Dict{Any, Any}, REPL.LineEdit.PrefixHistoryPrompt{REPLHistoryProvider}})
    precompile(Tuple{typeof(Base.getindex), Base.Dict{Any, Any}, REPL.LineEdit.PrefixHistoryPrompt{REPLHistoryProvider}})
    precompile(Tuple{typeof(REPL.LineEdit.copybuf!), Base.GenericIOBuffer{Array{UInt8, 1}}, Base.GenericIOBuffer{Array{UInt8, 1}}})
    precompile(Tuple{typeof(REPL.LineEdit.activate), REPL.LineEdit.PrefixHistoryPrompt{REPLHistoryProvider}, REPL.LineEdit.PrefixSearchState, REPL.Terminals.TerminalBuffer, REPL.Terminals.TTYTerminal})
    precompile(Tuple{typeof(REPL.LineEdit.refresh_multi_line), REPL.Terminals.TerminalBuffer, REPL.Terminals.TTYTerminal, REPL.LineEdit.PrefixSearchState})
    precompile(Tuple{typeof(REPL.LineEdit.history_next_prefix), REPL.LineEdit.PrefixSearchState, REPLHistoryProvider, String})
    precompile(Tuple{typeof(history_move_prefix), REPL.LineEdit.PrefixSearchState, REPLHistoryProvider, String, Bool, Int64})
    precompile(Tuple{typeof(REPL.LineEdit.keymap), REPL.LineEdit.PrefixSearchState, REPL.LineEdit.PrefixHistoryPrompt{REPLHistoryProvider}})
    precompile(Tuple{typeof(REPL.LineEdit.terminal), REPL.LineEdit.PrefixSearchState})
    precompile(Tuple{typeof(REPL.LineEdit.keymap_data), REPL.LineEdit.PrefixSearchState, REPL.LineEdit.PrefixHistoryPrompt{REPLHistoryProvider}})
    precompile(Tuple{typeof(REPL.LineEdit.history_prev_prefix), REPL.LineEdit.PrefixSearchState, REPLHistoryProvider, String})
    precompile(Tuple{typeof(REPL.LineEdit.transition), REPL.LineEdit.MIState, REPL.LineEdit.Prompt})
    precompile(Tuple{typeof(REPL.LineEdit.deactivate), REPL.LineEdit.PrefixHistoryPrompt{REPLHistoryProvider}, REPL.LineEdit.PrefixSearchState, REPL.Terminals.TerminalBuffer, REPL.Terminals.TTYTerminal})
    precompile(Tuple{typeof(REPL.LineEdit.refresh_multi_line), REPL.Terminals.TTYTerminal, REPL.LineEdit.PrefixSearchState})
    precompile(Tuple{typeof(REPL.LineEdit.refresh_multi_line), REPL.Terminals.TerminalBuffer, REPL.Terminals.TerminalBuffer, REPL.Terminals.TTYTerminal, REPL.LineEdit.PrefixSearchState})
    precompile(Tuple{getfield(REPL.LineEdit, Symbol("#kw##refresh_multi_line")), Array{Any, 1}, typeof(REPL.LineEdit.refresh_multi_line), REPL.Terminals.TerminalBuffer, REPL.Terminals.TerminalBuffer, REPL.Terminals.TTYTerminal, REPL.LineEdit.PrefixSearchState})
    precompile(Tuple{getfield(REPL.LineEdit, Symbol("#kw##refresh_multi_line")), Array{Any, 1}, typeof(REPL.LineEdit.refresh_multi_line), REPL.Terminals.TerminalBuffer, REPL.Terminals.TTYTerminal, REPL.LineEdit.PrefixSearchState})
    precompile(Tuple{typeof(REPL.LineEdit.replace_line), REPL.LineEdit.PrefixSearchState, Base.GenericIOBuffer{Array{UInt8, 1}}})
    precompile(Tuple{typeof(REPL.LineEdit.accept_result), REPL.LineEdit.MIState, REPL.LineEdit.PrefixHistoryPrompt{REPLHistoryProvider}})
    precompile(Tuple{typeof(REPL.LineEdit.replace_line), REPL.LineEdit.PromptState, Base.GenericIOBuffer{Array{UInt8, 1}}})
    precompile(Tuple{typeof(REPL.LineEdit.match_input), Base.Dict{Char, Any}, REPL.LineEdit.MIState, Base.GenericIOBuffer{Array{UInt8, 1}}})
    precompile(Tuple{typeof(REPL.LineEdit.edit_move_left), Base.GenericIOBuffer{Array{UInt8, 1}}})
    precompile(Tuple{typeof(REPL.LineEdit.edit_move_left), REPL.LineEdit.PromptState})
    precompile(Tuple{typeof(REPL.LineEdit.edit_move_right), Base.GenericIOBuffer{Array{UInt8, 1}}})
    precompile(Tuple{typeof(REPL.LineEdit.edit_move_right), REPL.LineEdit.PromptState})
    precompile(Tuple{typeof(REPL.LineEdit.move_line_start), REPL.LineEdit.MIState})
    precompile(Tuple{typeof(REPL.LineEdit.move_line_end), Base.GenericIOBuffer{Array{UInt8, 1}}})
    precompile(Tuple{typeof(REPL.LineEdit.move_line_end), REPL.LineEdit.MIState})
    precompile(Tuple{typeof(REPL.LineEdit.edit_move_up), Base.GenericIOBuffer{Array{UInt8, 1}}})
    precompile(Tuple{typeof(REPL.LineEdit.edit_move_up), REPL.LineEdit.MIState})
    precompile(Tuple{typeof(Base.:(==)), Symbol, REPL.LineEdit.Prompt})
    precompile(Tuple{typeof(Base.isempty), REPL.LineEdit.PromptState})
    precompile(Tuple{typeof(REPL.LineEdit.default_enter_cb), REPL.LineEdit.PromptState})
    precompile(Tuple{typeof(Base.Multimedia.display), REPLDisplay{REPL.LineEditREPL}, Base.Markdown.MD})
    precompile(Tuple{typeof(Base.displaysize), REPL.Terminals.TTYTerminal})
    precompile(Tuple{typeof(Base.Markdown.term), REPL.Terminals.TTYTerminal, Array{Any, 1}, Int64})
    precompile(Tuple{typeof(Base.Markdown.term), REPL.Terminals.TTYTerminal, Base.Markdown.MD, Int64})
    precompile(Tuple{typeof(Base.Markdown.term), REPL.Terminals.TTYTerminal, Base.Markdown.Code, Int64})
    precompile(Tuple{typeof(Base.write), REPL.Terminals.TTYTerminal, Base.SubString{String}})
    precompile(Tuple{typeof(Base.print), REPL.Terminals.TTYTerminal, Base.SubString{String}})
    precompile(Tuple{typeof(Base.print), REPL.Terminals.TTYTerminal, Base.SubString{String}, Char})
    precompile(Tuple{typeof(Base.Markdown.term), REPL.Terminals.TTYTerminal, Base.Markdown.Paragraph, Int64})
    precompile(Tuple{typeof(Base.Multimedia.display), REPLDisplay{REPL.LineEditREPL}, Array{Int64, 1}})
    precompile(Tuple{typeof(Base.Multimedia.display), REPLDisplay{REPL.LineEditREPL}, Base.MIME{Symbol("text/plain")}, Array{Int64, 1}})
    precompile(Tuple{typeof(Base.show_delim_array), Base.IOContext{REPL.Terminals.TTYTerminal}, Array{Int64, 1}, String, String, String, Bool, Int64, Int64})
    precompile(Tuple{typeof(Base.print), Base.IOContext{REPL.Terminals.TTYTerminal}, Char})
    precompile(Tuple{typeof(Base.print), Base.IOContext{REPL.Terminals.TTYTerminal}, String, Char})
    precompile(Tuple{typeof(Base.print), Base.IOContext{REPL.Terminals.TTYTerminal}, String, String})
    precompile(Tuple{typeof(Base.print), Base.IOContext{REPL.Terminals.TTYTerminal}, String, String, Char})
    precompile(Tuple{typeof(Base.show_vector), Base.IOContext{REPL.Terminals.TTYTerminal}, Array{Int64, 1}, String, String})
    precompile(Tuple{typeof(Base._show_nonempty), Base.IOContext{REPL.Terminals.TTYTerminal}, Array{Int64, 1}, String})
    precompile(Tuple{typeof(Base._show_empty), Base.IOContext{REPL.Terminals.TTYTerminal}, Array{Int64, 1}})
    precompile(Tuple{typeof(Base.print_matrix), Base.IOContext{REPL.Terminals.TTYTerminal}, Array{Int64, 1}, String, String, String})
    precompile(Tuple{typeof(Base.print_matrix_vdots), Base.IOContext{REPL.Terminals.TTYTerminal}, String, Array{Tuple{Int64, Int64}, 1}, String, Int64, Int64})
    precompile(Tuple{typeof(Base.print_matrix), Base.IOContext{REPL.Terminals.TTYTerminal}, Array{Int64, 1}, String, String, String, String, String, String, Int64, Int64})
    precompile(Tuple{typeof(Base.alignment), Base.IOContext{REPL.Terminals.TTYTerminal}, Array{Int64, 1}, Array{Int64, 1}, Array{Int64, 1}, Int64, Int64, Int64})
    precompile(Tuple{typeof(Base.print_matrix_row), Base.IOContext{REPL.Terminals.TTYTerminal}, Array{Int64, 1}, Array{Tuple{Int64, Int64}, 1}, Int64, Array{Int64, 1}, String})
    precompile(Tuple{typeof(REPL.LineEdit.edit_delete), Base.GenericIOBuffer{Array{UInt8, 1}}})
end
