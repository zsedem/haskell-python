{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}
module Main where

import qualified CPython as Py
import qualified CPython.Types.Module as Py
import qualified CPython.Protocols.Object as Py
import qualified CPython.Types.Dictionary as PyDict
import qualified CPython.Types.Tuple as PyTuple
import qualified CPython.Types.Unicode as PyUnicode
import qualified CPython.Types.Exception as PyExc
import Data.Text
import Control.Exception(handle)
import System.Console.CmdArgs
import System.Exit(exitSuccess)

data Arguments = Arguments {plugin :: FilePath} deriving (Show, Data, Typeable)
sample = Arguments{plugin = def &= help "The Python Plugin to load"}
         &= summary "Example Main Program"
main :: IO ()
main = do
    Arguments {..} <- cmdArgs sample
    handle pyExceptionHandler $ runPlugin plugin
    exitSuccess
  where
    pyExceptionHandler :: PyExc.Exception -> IO ()
    pyExceptionHandler exception = do
        tracebackModule <- Py.importModule "traceback"
        print_exc <- PyUnicode.toUnicode "print_exception" >>= Py.getAttribute tracebackModule
        kwargs <- PyDict.new
        args <- case PyExc.exceptionTraceback exception of
          Just tb -> PyTuple.toTuple [PyExc.exceptionType exception, PyExc.exceptionValue exception, tb]
          _ -> PyTuple.toTuple [PyExc.exceptionType exception, PyExc.exceptionValue exception]
        _ <- Py.call print_exc args kwargs
        return ()

runPlugin :: FilePath -> IO ()
runPlugin plugin = do
    Py.initialize
    pyModule <- Py.importModule $ pack plugin
    args <- PyTuple.toTuple []
    kwargs <- PyDict.new
    pyPrinter <- PyUnicode.toUnicode "Printer" >>=  Py.getAttribute pyModule >>= \classObject -> Py.call classObject args kwargs
    _ <- PyUnicode.toUnicode "load" >>=  Py.getAttribute pyPrinter >>= \classObject -> Py.call classObject args kwargs
    _ <- PyUnicode.toUnicode "run" >>=  Py.getAttribute pyPrinter >>= \classObject -> Py.call classObject args kwargs
    return ()

