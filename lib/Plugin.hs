module Plugin
    ( loadPlugin
    ) where

loadPlugin :: FilePath -> Plugin ()
loadPlugin = undefined

newtype Plugin a = Plugin {unPlugin::Identity a} deriving(Functor, Applicative, Monad)
