{-# LANGUAGE PackageImports, CPP #-}
module Prelude
  ( module P
  ) where
import "basic-prelude" CorePrelude as P
import "base" Prelude as P((++), sum, reverse, show, take)
import Control.Monad.Identity as P(Identity(..))
import Control.Monad as P(void)

