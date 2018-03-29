# Enum for escaped (multi-byte) keys such as the arrows or the home/end keys
@enum(Key,
    ARROW_LEFT = 1000,
    ARROW_RIGHT,
    ARROW_UP,
    ARROW_DOWN,
    DEL_KEY,
    HOME_KEY,
    END_KEY,
    PAGE_UP,
    PAGE_DOWN)

# Enable raw mode. Allows us to process keyboard inputs directly.
function enableRawMode(term)
    try
        REPL.Terminals.raw!(term, true)
        return true
    catch err
        @warn("TerminalMenus: Unable to enter raw mode: $err")
    end
    return false
end

# Disable raw mode. Give control back to Julia REPL if interactive session.
function disableRawMode(term)
    try
        REPL.Terminals.raw!(term, false)
        return true
    catch err
        @warn("TerminalMenus: Unable to disable raw mode: $err")
    end
    return false
end


# Reads a single byte from STDIN
readNextChar(stream::IO=STDIN) = Char(read(stream,1)[1])

# Read the next key from STDIN. It is also able to read several bytes for
#   escaped keys such as the arrow keys, home/end keys, etc.
# Escaped keys are returned using the `Key` enum.
readKey(stream::IO=STDIN) = UInt32(_readKey(stream))
function _readKey(stream::IO=STDIN)
    c = readNextChar(stream)

    # Escape characters
    if c == '\x1b'
        stream.buffer.size < 2 && return '\x1b'
        esc_a = readNextChar(stream)
        esc_a == 'v' && return PAGE_UP  # M-v
        esc_a == '<' && return HOME_KEY # M-<
        esc_a == '>' && return END_KEY  # M->

        stream.buffer.size < 3 && return '\x1b'
        esc_b = readNextChar(stream)

        if esc_a == '[' || esc_a == 'O'
            if esc_b >= '0' && esc_b <= '9'
                stream.buffer.size < 4 && return '\x1b'
                esc_c = readNextChar(stream)
                if esc_c == '~'
                    esc_b == '1' && return HOME_KEY
                    esc_b == '4' && return END_KEY
                    esc_b == '3' && return DEL_KEY
                    esc_b == '5' && return PAGE_UP
                    esc_b == '6' && return PAGE_DOWN
                    esc_b == '7' && return HOME_KEY
                    esc_b == '8' && return END_KEY
                    return '\x1b'
                end
            else
                # Arrow keys
                esc_b == 'A' && return ARROW_UP
                esc_b == 'B' && return ARROW_DOWN
                esc_b == 'C' && return ARROW_RIGHT
                esc_b == 'D' && return ARROW_LEFT
                esc_b == 'H' && return HOME_KEY
                esc_b == 'F' && return END_KEY
                return '\x1b'
            end
        end
        esc_a == 'H' && return HOME_KEY
        esc_a == 'F' && return END_KEY
        return '\x1b'
    end
    c == '\x16' && return PAGE_DOWN  # C-v
    c == '\x10' && return ARROW_UP   # C-p
    c == '\x0e' && return ARROW_DOWN # C-n
    return c
end
