class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

instance MonadPlus [] where
    mzero = []
    mplus = (++)

instance MonadPlus Maybe where
    mzero                   = Nothing
    Nothing `mplus` Nothing = Nothing
    Just x  `mplus` Nothing = Just x
    Nothing `mplus` Just x  = Just x
    Just x  `mplus` Just y  = Just x

msum :: MonadPlus m => [m a] -> m a
msum = foldr mplus mzero
