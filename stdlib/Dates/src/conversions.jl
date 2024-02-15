# This file is a part of Julia. License is MIT: https://julialang.org/license

# Conversion/Promotion

"""
    Date(dt::DateTime) -> Date

Convert a `DateTime` to a `Date`. The hour, minute, second, and millisecond parts of
the `DateTime` are truncated, so only the year, month and day parts are used in
construction.
"""
Date(dt::TimeType) = convert(Date, dt)

"""
    DateTime(dt::Date) -> DateTime

Convert a `Date` to a `DateTime`. The hour, minute, second, and millisecond parts of
the new `DateTime` are assumed to be zero.
"""
DateTime(dt::TimeType) = convert(DateTime, dt)

"""
    Time(dt::DateTime) -> Time

Convert a `DateTime` to a `Time`. The hour, minute, second, and millisecond parts of
the `DateTime` are used to create the new `Time`. Microsecond and nanoseconds are zero by default.
"""
Time(dt::DateTime) = convert(Time, dt)

Base.convert(::Type{DateTime}, dt::Date) = DateTime(UTM(value(dt) * 86400000))
Base.convert(::Type{Date}, dt::DateTime) = Date(UTD(days(dt)))
Base.convert(::Type{Time}, dt::DateTime) = Time(Nanosecond((value(dt) % 86400000) * 1000000))

Base.convert(::Type{DateTime},x::Millisecond)  = DateTime(Dates.UTInstant(x))  # Converts Rata Die milliseconds to a DateTime
Base.convert(::Type{Millisecond},dt::DateTime) = Millisecond(value(dt))        # Converts DateTime to Rata Die milliseconds
Base.convert(::Type{Date},x::Day)  = Date(Dates.UTInstant(x))  # Converts Rata Die days to a Date
Base.convert(::Type{Day},dt::Date) = Day(value(dt))            # Converts Date to Rata Die days

### External Conversions
const UNIXEPOCH = value(DateTime(1970)) #Rata Die milliseconds for 1970-01-01T00:00:00
localepoch() = value(DateTime(Libc.TmStruct(0))) #Rata Die for local time at UTC 1970-01-01T00:00:00

"""
    unix2datetime(x::Real; localtime::Bool=false) -> DateTime

Take the number of seconds since unix epoch `1970-01-01T00:00:00` (UTC) and convert to the
corresponding `DateTime`. If `localtime` is `true`, then the output is in the local
time zone, otherwise it is in UTC/GMT.
"""
function unix2datetime(x::Real; localtime::Bool=false)
    # Rounding should match `now` below
    rata = (localtime ? localepoch() : UNIXEPOCH) + trunc(Int64, Int64(1000) * x)
    return DateTime(UTM(rata))
end

"""
    datetime2unix(dt::DateTime; localtime::Bool=false) -> Float64

Take the given `DateTime` and return the number of seconds
since the unix epoch `1970-01-01T00:00:00` (UTC) as a [`Float64`](@ref).

If `localtime` is `true`, then `dt` is interpreted as being in the local time zone,
and is otherwise interpreted as being UTC/GMT.
"""
function datetime2unix(dt::DateTime; localtime::Bool=false)
    return (value(dt) - (localtime ? localepoch() : UNIXEPOCH)) / 1000.0
end

"""
    now() -> DateTime

Return a `DateTime` corresponding to the user's system time including the system timezone
locale.
"""
function now()
    tv = Libc.TimeVal()
    tm = Libc.TmStruct(tv.sec)
    return DateTime(tm.year + 1900, tm.month + 1, tm.mday, tm.hour, tm.min, tm.sec, div(tv.usec, 1000))
end

"""
    today() -> Date

Return the date portion of `now()`.
"""
today() = Date(now())

"""
    now(::Type{UTC}) -> DateTime

Return a `DateTime` corresponding to the user's system time as UTC/GMT.
For other time zones, see the TimeZones.jl package.

# Examples
```julia
julia> now(UTC)
2023-01-04T10:52:24.864
```
"""
now(::Type{UTC}) = unix2datetime(time())

"""
    rata2datetime(days) -> DateTime

Take the number of Rata Die days since epoch `0000-12-31T00:00:00` and return the
corresponding `DateTime`.
"""
rata2datetime(days) = DateTime(yearmonthday(days)...)

"""
    datetime2rata(dt::TimeType) -> Int64

Return the number of Rata Die days since epoch from the given `Date` or `DateTime`.
"""
datetime2rata(dt::TimeType) = days(dt)

# Julian conversions
const JULIANEPOCH = value(DateTime(-4713, 11, 24, 12))
localjulianepoch() = JULIANEPOCH + (localepoch() - UNIXEPOCH)

"""
    julian2datetime(julian_days::Real; localtime::Bool=false) -> DateTime

Take the number of Julian calendar days since epoch `-4713-11-24T12:00:00` (UTC) and return the
corresponding `DateTime`. If `localtime` is `true`, then the output is in the local
time zone, otherwise it is in UTC/GMT.
"""
function julian2datetime(f::Real; localtime::Bool=false)
    rata = (localtime ? localjulianepoch() : JULIANEPOCH) + round(Int64, Int64(86400000) * f)
    return DateTime(UTM(rata))
end

"""
    datetime2julian(dt::DateTime; localtime::Bool=false) -> Float64

Take the given `DateTime` and return the number of Julian calendar days since the julian
epoch `-4713-11-24T12:00:00` (UTC) as a [`Float64`](@ref).

If `localtime` is `true`, then `dt` is interpreted as being in the local time zone,
and is otherwise interpreted as being UTC/GMT.
"""
function datetime2julian(dt::DateTime; localtime::Bool=false)
    return (value(dt) - (localtime ? localjulianepoch() : JULIANEPOCH)) / 86400000.0
end
