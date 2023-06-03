module Common.Debug where

import qualified Text.PrettyPrint
import qualified Text.Show.Pretty

pps :: (Show a) => a -> String
pps x = do
    Text.PrettyPrint.renderStyle
        (Text.PrettyPrint.style {Text.PrettyPrint.lineLength = 80})
        (Text.Show.Pretty.ppDoc x)

pp :: (Show a) => a -> IO ()
pp = putStrLnRNF . pps

putStrLnRNF :: String -> IO ()
putStrLnRNF str = do
    foldr seq (putStrLn str) str
