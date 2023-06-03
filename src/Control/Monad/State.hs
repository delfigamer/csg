{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}

module Control.Monad.State
    ( State (State)
    , runState
    , get
    , put
    , modify
    , modify'
    )
where

pattern State :: (s -> (a, s)) -> State s a
pattern State fn <- (runState -> fn)
  where
    State fn = State# $ \s1 -> case fn s1 of (r, s2) -> (# r, s2 #)

runState :: State s a -> s -> (a, s)
runState (State# f) s1 = case f s1 of (# r, s2 #) -> (r, s2)

newtype State s a = State# (s -> (# a, s #))
    deriving (Functor)

instance Applicative (State s) where
    pure x =
        State# $
            \s -> (# x, s #)
    ff <*> fx =
        ff >>= \f -> fmap f fx

instance Monad (State s) where
    State# fx >>= sel =
        State# $ \s1 ->
            case fx s1 of
                (# x, s2 #) ->
                    case sel x of
                        State# fy ->
                            fy s2

get :: State s s
get = State# $ \s -> (# s, s #)

put :: s -> State s ()
put s = State# $ \_ -> (# (), s #)

modify :: (s -> s) -> State s ()
modify f = State# $ \s -> (# (), f s #)

modify' :: (s -> s) -> State s ()
modify' f = State# $ \s1 -> let !s2 = f s1 in (# (), s2 #)
