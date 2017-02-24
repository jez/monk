{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module SecondTry where

--import Control.Monad.Ref (MonadRef)
import Control.Monad.Gen (Gen, gen)

-- Temp -----------------------------------------------------------------------

------- Interface --------

-- TODO(jez) Don't force the choice of the Gen monad on people
data (Eq x, Ord x, Show x) => Temp x = Temp {
  new :: String -> Gen x x,
  getUserString :: x -> String,
  nextTemp :: x -> x
}

----- Implementation -----

data IntStrTemp = IntStrTemp { uuid :: Int, name :: String }
                   deriving Show

instance Eq IntStrTemp where
  (IntStrTemp _ uuid1) == (IntStrTemp _ uuid2) = uuid1 == uuid2

instance Ord IntStrTemp where
  compare (IntStrTemp _ uuid1) (IntStrTemp _ uuid2) = compare uuid1 uuid2

mkIntTemp :: Temp IntStrTemp
mkIntTemp = Temp tempNew tempGetUserString tempNextTemp
  where tempNew :: String -> Gen IntStrTemp IntStrTemp
        tempNew name = do
          IntStrTemp uuid _ <- gen
          return $ IntStrTemp uuid name

        tempGetUserString :: IntStrTemp -> String
        tempGetUserString = name

        -- Zero out the string; 'new' will properly fill in the string later
        tempNextTemp :: IntStrTemp -> IntStrTemp
        tempNextTemp IntStrTemp{..} = IntStrTemp (uuid + 1) ""

-- Oper -----------------------------------------------------------------------

------- Interface --------

data (Eq f, Show f) => Oper f = Oper {
  arity :: f -> [Int]
}

----- Implementation -----

data PcfOps = Z | S | Ifz | Lam | Ap | Fix
            deriving (Eq, Show)

mkPcfOps :: Oper PcfOps
mkPcfOps = Oper pcfOpsArity
  where pcfOpsArity :: PcfOps -> [Int]
        pcfOpsArity Z = []
        pcfOpsArity S = [0]
        pcfOpsArity Ifz = [0, 0, 1]
        pcfOpsArity Lam = [0, 1]  -- TODO(jez) where do i check the sort of the arg?
        pcfOpsArity Ap = [0, 0]
        pcfOpsArity Fix = [1]

-- Abt ------------------------------------------------------------------------

------- Interface --------

data (Eq x, Ord x, Show x, Eq f, Show f) => View x f t
  = Var x
  | x :\ t
  | f :$ [t]

data (Eq x, Ord x, Show x, Eq f, Show f) => Abt x f t = Abt {
  into :: View x f t -> Gen x (Either String t),
  out :: t -> Gen x (Either String (View x f t))
}

----- Implementation -----

data (Eq x, Ord x, Show x, Eq f, Show f) => LocallyNameless f x
  = FV x
  | BV Int
  | ABS (LocallyNameless f x)
  | f :$: [LocallyNameless f x]


mkAbt
  :: ( Eq x
     , Ord x
     , Show x
     , Eq f
     , Show f
     )
  => Temp x
  -> Oper f
  -> Abt x f (LocallyNameless f x)
mkAbt t@Temp{..} o@Oper{..} = Abt abtInto abtOut
  where bind x i (FV y)     = if x == y then BV i else FV y
        bind _ _ (BV i)     = BV i
        bind x i (ABS e)    = ABS $ bind x i e
        bind x i (f :$: es) = f :$: map (bind x i) es

        check_valence (n, ABS e) = (n >= 0) && check_valence (n - 1, e)
        check_valence (0, _)     = True
        check_valence _          = False

        abtInto (Var x)   = return . Right $ FV x
        abtInto (x :\ e)  = return . Right $ bind x 0 e
        abtInto (f :$ es) = do
          let valences = arity f
          if length es /= length valences
          then return $ Left "Operator applied to wrong number of arguments"
          else
            if all check_valence $ zip valences es
            then return $ Right (f :$: es)
            else return $ Left "Valence mismatch"

        unbind x i (BV j)     = if i == j then FV x else BV j
        unbind x i (FV y)     = FV y
        unbind x i (ABS e)    = ABS $ unbind x i e
        unbind x i (f :$: es) = f :$: map (unbind x i) es

        abtOut (BV _) = return $ Left "Called out on BV"
        abtOut (FV x) = return . Right $ Var x
        abtOut (ABS e) = do
          x <- gen
          return . Right $ x :\ unbind x 0 e
        abtOut (f :$: es) = return . Right $ f :$ es


