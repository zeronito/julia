"""global menu configuration parameters"""
CONFIG = Dict()


"""

    config( <see arguments> )

Keyword-only function to configure global menu parameters

# Arguments
 - `charset::Symbol=:na`: ui characters to use (`:ascii` or `:unicode`); overridden by other arguments
 - `cursor::Char='>'|'→'`: character to use for cursor
 - `up_arrow::Char='^'|'↑'`: character to use for up arrow
 - `down_arrow::Char='v'|'↓'`: character to use for down arrow
 - `checked::String="[X]"|"✓"`: string to use for checked
 - `unchecked::String="[ ]"|"⬚")`: string to use for unchecked
 - `scroll::Symbol=:nowrap`: If `:wrap` then wrap the cursor around top and bottom, if :`nowrap` do not wrap cursor
 - `supress_output::Bool=false`: For testing. If true, menu will not be printed to console.
 - `ctrl_c_interrupt::Bool=true`: If `false`, return empty on ^C, if `true` throw InterruptException() on ^C
"""
function config(;charset::Symbol = :na,
                scroll::Symbol = :na,
                cursor::Char = '\0',
                up_arrow::Char = '\0',
                down_arrow::Char = '\0',
                checked::String = "",
                unchecked::String = "",
                supress_output::Union{Nothing, Bool}=nothing,
                ctrl_c_interrupt::Union{Nothing, Bool}=nothing)

    if !(charset in [:na, :ascii, :unicode])
        error("charset should be :ascii or :unicode, recieved $charset")
    end

    if !(scroll in [:na, :wrap, :nowrap])
        error("scroll must be :wrap or :nowrap, recieved $scroll")
    end

    if scroll == :wrap
        CONFIG[:scroll_wrap] = true
    elseif scroll == :nowrap
        CONFIG[:scroll_wrap] = false
    end

    if charset == :ascii
        cursor = '>'
        up_arrow = '^'
        down_arrow = 'v'
        checked = "[X]"
        unchecked = "[ ]"
    elseif charset == :unicode
        cursor = '→'
        up_arrow = '↑'
        down_arrow = '↓'
        checked = "✓"
        unchecked = "⬚"
    end

    if isa(supress_output, Bool)
        CONFIG[:supress_output] = supress_output
    end

    if isa(ctrl_c_interrupt, Bool)
        CONFIG[:ctrl_c_interrupt] = ctrl_c_interrupt
    end

    cursor != '\0'      && (CONFIG[:cursor] = cursor)
    up_arrow != '\0'    && (CONFIG[:up_arrow] = up_arrow)
    down_arrow != '\0'  && (CONFIG[:down_arrow] = down_arrow)
    checked != ""       && (CONFIG[:checked] = checked)
    unchecked != ""     && (CONFIG[:unchecked] = unchecked)

    # Don't return anything
    nothing
end

# Set up defaults
config(charset=:ascii, scroll=:nowrap, supress_output=false, ctrl_c_interrupt=true)
