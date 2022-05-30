-- Monads
monadd :: (Monad m, Num b) => m b -> m b -> m b
monadd mx my =
    mx >>= (\x -> my >>= (\y -> return $ x + y))

monadd_do :: (Monad m, Num b) => m b -> m b -> m b
monadd_do mx my = do
    x <- mx
    y <- my
    return $ x + y
