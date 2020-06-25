import Data.JSString
import GHCJS.Marshal
import GHCJS.Foreign.Callback

import Glam.Term
import Glam.Parse

foreign import javascript unsafe "init($1)"
    init_ :: Callback a -> IO ()

main = do
    eval <- syncCallback1' $ \v -> do
        Just input <- fromJSVal v
        toJSVal $ pack $ case parseOne term (unpack input) of
            Right t -> show (normalise t)
            Left e -> errorBundlePretty e
    init_ eval
