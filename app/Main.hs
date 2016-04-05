{-# LANGUAGE RecordWildCards, DeriveDataTypeable,
             QuasiQuotes, TemplateHaskell #-}
module Main where

import qualified CPython as Py
import qualified CPython.Types.Module as Py
import qualified CPython.Protocols.Object as Py
import qualified CPython.Types.Capsule as PyCapsule
import qualified CPython.Types.Dictionary as PyDict
import qualified CPython.Types.Tuple as PyTuple
import qualified CPython.Types.Unicode as PyUnicode
import qualified CPython.Types.Exception as PyExc
import Data.Text hiding(take)
import Emitter
import Embed
import System.Console.CmdArgs
import System.Exit(exitSuccess)
import Foreign.StablePtr
import Foreign.Ptr
import Foreign.C.String

data Arguments = Arguments {plugin :: FilePath} deriving (Show, Data, Typeable)
sample = Arguments{plugin = def &= help "The Python Plugin to load"}
         &= summary "Example Main Program"

main :: IO ()
main = do
    Arguments {..} <- cmdArgs sample
    registerTheEmbededModule
    print "embeded module registered, i hope"
    handle pyExceptionHandlerWithoutPythonTraceback Py.initialize
    handle pyExceptionHandler $ runPlugin plugin
    handle pyExceptionHandlerWithoutPythonTraceback Py.finalize
    print $ sum $ take 10002 [(1::Int)..]
  where
    pyExceptionHandler :: PyExc.Exception -> IO ()
    pyExceptionHandler exception = handle pyExceptionHandlerWithoutPythonTraceback $ do
        tracebackModule <- Py.importModule "traceback"
        print_exc <- PyUnicode.toUnicode "print_exception" >>= Py.getAttribute tracebackModule
        kwargs <- PyDict.new
        args <- case PyExc.exceptionTraceback exception of
          Just tb -> PyTuple.toTuple [PyExc.exceptionType exception, PyExc.exceptionValue exception, tb]
          _ -> PyTuple.toTuple [PyExc.exceptionType exception, PyExc.exceptionValue exception]
        _ <- Py.call print_exc args kwargs
        return ()
    pyExceptionHandlerWithoutPythonTraceback :: PyExc.Exception -> IO ()
    pyExceptionHandlerWithoutPythonTraceback exception =
        putStrLn "Unexpected Python exception (Please report a bug)"

runPlugin :: FilePath -> IO ()
runPlugin plugin = do
    pyModule <- Py.importModule $ pack plugin
    emitterCapsule <- capsulate Emitter
    attrName <- PyUnicode.toUnicode "emit"
    args <- PyTuple.toTuple []
    runArgs <- PyTuple.toTuple [Py.toObject emitterCapsule]
    kwargs <- PyDict.new
    pyPrinter <- PyUnicode.toUnicode "Printer" >>=  Py.getAttribute pyModule >>= \classObject -> Py.call classObject args kwargs
    _ <- PyUnicode.toUnicode "load" >>=  Py.getAttribute pyPrinter >>= \classObject -> Py.call classObject args kwargs
    _ <- PyUnicode.toUnicode "run" >>=  Py.getAttribute pyPrinter >>= \classObject -> Py.call classObject runArgs kwargs
    putStrLn "DEBUG: RunPlugin returns"
    return ()

capsulate :: a -> IO PyCapsule.Capsule
capsulate x = do
    -- The garbage collecting is pretty funny in this case:
    -- When Haskell GC will find the PyCapsule, then it calls
    -- derefence only! (since Python might use it somewhere)
    -- Then actually Python GC will free the memory of this
    -- stablePtr
    --
    -- You need my fork of haskell-cpython for this to work,
    -- otherwise you will see a Segmentation fault or a GHC
    -- error message about wrong Pointer usage
    stablePtr <- newStablePtr x
    let ptr = castStablePtrToPtr stablePtr
        destructor ptr = void $
            deRefStablePtr stablePtr
    PyCapsule.new ptr (Just "Emitter") destructor



incapsulate :: PyCapsule.Capsule -> IO a
incapsulate capsule = do
    ptr <- PyCapsule.getPointer capsule (Just "Emitter")
    deRefStablePtr $ castPtrToStablePtr ptr

foreign import ccall "wrapper"
    createEmitFunction :: (Ptr PyCapsule.Capsule -> CString -> IO ()) -> IO (FunPtr(Ptr PyCapsule.Capsule -> CString -> IO ()))
