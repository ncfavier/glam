import Control.Monad
import GHC.JS.Prim
import GHC.JS.Foreign.Callback

import Glam.Run

foreign import javascript unsafe "(f => glam = f)"
    setGlam :: Callback a -> IO ()

main = do
    setGlam <=< syncCallback1' $
        pure . toJSString
             . either id unlines
             . runGlam
             . runFile ""
             . fromJSString
