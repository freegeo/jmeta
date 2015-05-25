-define(EXCEPTION(ExceptionClass, ExceptionPattern, Expression),
  try
    Expression,
    error(expected_exception_missed, {ExceptionClass, ExceptionPattern})
  catch
    ExceptionClass:ExceptionPattern -> ok
  end).

-define(N, 'jmeta.namespace.test'). % test namespace
-define(TN(X), {?N, X}).
