{-# LANGUAGE ViewPatterns #-}
import Data.JSString (pack, unpack)
import GHCJS.Marshal
import GHCJS.Foreign.Callback

import Glam.Interpreter

foreign import javascript unsafe "init($1)"
    initJS :: Callback a -> IO ()

main = do
    eval <- syncCallback1' $ \v -> do
        Just (unpack -> input) <- fromJSVal v
        toJSVal $ pack
                $ either id unlines
                $ runGlam
                $ runFile "" input
    initJS eval
