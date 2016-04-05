{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Embed where
import qualified Language.C.Inline as C
C.include "<Python.h>"
C.include "EmbedStatics.h"

registerTheEmbededModule :: IO ()
registerTheEmbededModule = [C.block|
void {
    PyImport_AppendInittab("emb", &PyInit_emb);
} |]


