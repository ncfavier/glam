{-# LANGUAGE ViewPatterns #-}
import GHCJS.Marshal
import GHCJS.Foreign.Callback

import Glam.Run

foreign import javascript unsafe "glam = $1"
    setGlam :: Callback a -> IO ()

main = do
    setGlam =<< syncCallback1' \v -> do
        Just input <- fromJSVal v
        toJSVal $ either id unlines
                $ runGlam
                $ runFile "" input
