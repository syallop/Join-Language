module Join.Interpreter.Basic.Debug where
import qualified Debug.Trace as Debug

debug = False

traceIO = if debug then Debug.traceIO else \_ -> return ()
trace = if debug then Debug.trace else \_ a -> a

