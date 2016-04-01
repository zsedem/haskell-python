{-# LANGUAGE ForeignFunctionInterface #-}
module Emitter where
import Foreign.Ptr
import Foreign.C.String
data Emitter = Emitter deriving(Show, Eq)

emitter :: Emitter
emitter = Emitter


emit :: Emitter -> CString -> IO ()
emit emitter message =
    print $ "Haskell called emitter="++show emitter ++" message='" ++ show message ++ "'"

