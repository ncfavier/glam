import Data.JSString (pack, unpack)
import GHCJS.Marshal
import GHCJS.Foreign.Callback

import Glam.Interpreter

foreign import javascript unsafe "init($1)" initJS :: Callback a -> IO ()

main = do
    eval <- syncCallback1' $ \v -> do
        Just input <- fromJSVal v
        output <- runGlam $ either id unlines <$> runFile "" (unpack input)
        toJSVal (pack output)
    initJS eval
