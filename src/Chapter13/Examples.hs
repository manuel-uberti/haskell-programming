module Chapter13.Examples where

main :: IO ()
main = do
    c <- getChar
    c' <- getChar
    if c == c' then putStrLn "True" else return ()
