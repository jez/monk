{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module FirstTry where

-- Temp -----------------------------------------------------------------------

class (Eq t, Ord t, Show t) => Temp t where
  new :: String -> t
  getUserString :: t -> String

----- Implementation -----

data IntTemp = IntTemp { name :: String, uuid :: Int }
             deriving Show

instance Eq IntTemp where
  (IntTemp _ uuid1) == (IntTemp _ uuid2) = uuid1 == uuid2

instance Ord IntTemp where
  compare (IntTemp _ uuid1) (IntTemp _ uuid2) = compare uuid1 uuid2

instance Temp IntTemp where
  new = undefined
  getUserString = undefined

-- Oper -----------------------------------------------------------------------

class (Eq f, Show f) => Oper f where
  arity :: f -> [Int]

----- Implementation -----

data TermOps = Z | S | Ifz | Lam | Ap | Fix
             deriving (Eq, Show)

instance Oper TermOps where
  arity Z = []
  arity S = [0]
  arity Ifz = [0, 0, 1]
  arity Lam = [0, 1]  -- TODO(jez) where do i check the sort of the arg?
  arity Ap = [0, 0]
  arity Fix = [1]

-- Abt ------------------------------------------------------------------------
--
-- ???
--
--data (Temp x, Oper f) => View x f t
--  = Var x
--  | x :\ t
--  | f :$ [t]
--
--class (Temp x, Oper f) => Abt x f t | x f -> t where
--  into :: View x f t -> t
--  out :: t -> View x f t
--  -- TODO(jez) ABT_UTIL functions here
--
--instance (Abt x f t) => Eq t where
--  (==) = undefined
--
--instance Functor (View x f) where
--  fmap = undefined

