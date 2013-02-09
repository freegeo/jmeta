-define(EXCEPTION(ExceptionClass, ExceptionPattern, Expression),
        try
            Expression,
            error(expected_exception_missed, {ExceptionClass, ExceptionPattern})
        catch
            ExceptionClass:ExceptionPattern -> ok
        end).
