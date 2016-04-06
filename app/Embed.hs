{-# LANGUAGE ForeignFunctionInterface #-}
module Embed where
import qualified CPython as Py
import qualified CPython.Internal as Py
import qualified CPython.Types.Module as Py
import qualified CPython.Protocols.Object as Py
import qualified CPython.Types.Capsule as PyCapsule
import qualified CPython.Types.Dictionary as PyDict
import qualified CPython.Types.Tuple as PyTuple
import qualified CPython.Types.Unicode as PyUnicode
import qualified CPython.Types.Exception as PyExc
import Data.Text hiding(take)
import Emitter
import System.Console.CmdArgs
import System.Exit(exitSuccess)
import Foreign.StablePtr
import Foreign.Ptr
import Foreign.Concurrent
import Foreign.C.String

type EmitCFunction = Ptr () -> Ptr () -> IO ()

foreign import ccall "embeded_module_init"
    embeded_module_init :: FunPtr EmitCFunction
                        -> IO ()
foreign import ccall "wrapper"
    wrapEmit :: EmitCFunction -> IO (FunPtr EmitCFunction)

registerTheEmbededModule :: IO ()
registerTheEmbededModule = do
    funPtr <- wrapEmit emitCFunction
    embeded_module_init funPtr

emitCFunction :: EmitCFunction
emitCFunction _self_ptr argtuple_ptr = do
    [maycapsule, object] <- PyTuple.fromTuple =<< Py.fromForeignPtr <$> newForeignPtr (castPtr argtuple_ptr) (return ())
    Just capsule <- Py.cast maycapsule
    print capsule
    emitter <- incapsulate capsule :: IO Emitter
    print emitter
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
    capsule <- PyCapsule.new ptr (Just "Emitter") destructor
    print ptr
    print capsule
    return capsule



incapsulate :: PyCapsule.Capsule -> IO a
incapsulate capsule = do
    ptr <- PyCapsule.getPointer capsule (Just "Emitter")
    deRefStablePtr $ castPtrToStablePtr ptr

foreign import ccall "wrapper"
    createEmitFunction :: (Ptr PyCapsule.Capsule -> CString -> IO ()) -> IO (FunPtr(Ptr PyCapsule.Capsule -> CString -> IO ()))
