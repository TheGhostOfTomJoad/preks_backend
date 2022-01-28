{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE FlexibleContexts #-}
module Converter where
import           Control.Monad.Except
import           Data.Type.Equality

import           Vector



data Prek = ConstZero Int | Succ | Proj Int Int | PrekC Prek Prek | Comp Prek [Prek]


data CheckedPrek :: Nat -> * where
  CSucc      ::CheckedPrek (Suc Zero)
  CConstZero ::CheckedPrek n
  CProj      ::Fin n -> CheckedPrek n
  CComp      ::CheckedPrek m -> Vec m (CheckedPrek n) -> CheckedPrek n
  CPrekC     ::CheckedPrek n -> CheckedPrek (Suc (Suc n)) -> CheckedPrek (Suc n)
deriving instance Show (CheckedPrek n)

--- source: https://softwareengineering.stackexchange.com/questions/276867/is-it-possible-to-bake-dimension-into-a-type-in-haskell
congSuc :: (n :~: m) -> (Suc n :~: Suc m)
congSuc Refl = Refl

instance TestEquality Natty where
    testEquality Zy     Zy     = Just Refl
    testEquality (Sy n) (Sy m) = fmap congSuc (testEquality n m)  -- check whether the predecessors are equal, then make use of congruence
    testEquality Zy     _      = Nothing
    testEquality _      Zy     = Nothing



data ACPrek :: *  where
    ACPrek ::Natty n -> CheckedPrek n -> ACPrek
deriving instance Show ACPrek


testEquality' :: (MonadError e m) => Natty a -> Natty b -> e -> m (a :~: b)
testEquality' n m e = case testEquality n m of
    Nothing -> throwError e
    Just a  -> return a




-- apply the CPrekC COnstructor if the aritys have a difference of two
prekChelper :: MonadError String m => ACPrek -> ACPrek -> m ACPrek
prekChelper (ACPrek n g) (ACPrek (Sy (Sy m)) h) = fmap
    (\proof -> gcastWith proof (ACPrek (Sy m) (CPrekC g h)))
    (testEquality'
        n
        m
        ("The Aritys of the Base Case Function and the Function in the Induction Case should have a difference of two. Thats not true for  "
        ++ show (nattyToInt n)
        ++ " and "
        ++ show (2 + nattyToInt m)
        ++ " "
        )
    )
prekChelper _ _ = throwError
    "The Function for the Inductive Case needs an arity of at least two"


data AVPrek = forall n m . AVPrek (Natty n) (Natty m) (Vec n (CheckedPrek m))




--convert a vec of Preks to an datastructure that has a natty for the length an and a natty for the arity of of the preks
convertAVec :: MonadError String m => Vec (Suc n) ACPrek -> m AVPrek
convertAVec (VCons (ACPrek n f) VNil) =
    return $ AVPrek (Sy Zy) n (VCons f VNil)
convertAVec (VCons (ACPrek m1 f) gs@(VCons (ACPrek m2 g) xs)) = do
    AVPrek n m3 ys <- convertAVec gs
    fmap
        (\proof -> gcastWith proof (AVPrek (Sy n) m3 (VCons f ys)))
        (testEquality'
            m1
            m3
            ("In a list of functions the aritys of all functions must be equal. Fist Mismatch: "
            ++ show (nattyToInt m1)
            ++ " and "
            ++ show (nattyToInt m3)
            ++ " are not equal!"
            )
        )



--convert from preks to AGPREK if the aritys are alright
convert :: MonadError [Char] m => Prek -> m ACPrek
convert Succ          = return $ ACPrek (Sy Zy) CSucc
convert (ConstZero n) = case intToNatty n of
    HidedNatty na -> return $ ACPrek na CConstZero
convert (Proj _     0    ) = throwError "Indices start from One not from Zero"
convert (Proj arity index) = case intToHideFinNatty2 arity index of
    Nothing ->
        throwError
            $  "In the Projection P "
            ++ show arity
            ++ " "
            ++ show index
            ++ " the index is bigger than the arity."
    Just (HideFinNatty arity' index') -> return $ ACPrek arity' (CProj index')
convert (Comp af axs) = do
    xs <- mapM convert axs
    case fromList xs of
        AVec Zy _ -> throwError
            "Concatenation with an empty list of functions is not allowed."
        AVec (Sy n2) zs1 -> do
            (ACPrek arrf f)           <- convert af
            AVPrek lenzs2 arrgzs2 zs2 <- convertAVec zs1
            fmap
                (\proof -> gcastWith proof (ACPrek arrgzs2 (CComp f zs2)))
                (testEquality'
                    arrf
                    lenzs2
                    "The arity of the function and the length of the list of functions do not match."
                )

convert (PrekC mf mg) = do
    f <- convert mf
    g <- convert mg
    prekChelper f g




-- helpers


data HideFinNatty where
    HideFinNatty ::Natty (Suc n) -> Fin (Suc n) -> HideFinNatty


intToHideFinNatty2 :: Int -> Int -> Maybe HideFinNatty
intToHideFinNatty2 n m = intToHideFinNatty (n - 1) (m - 1)

intToHideFinNatty :: Int -> Int -> Maybe HideFinNatty
intToHideFinNatty 0 0 = return $ HideFinNatty (Sy Zy) FZero
intToHideFinNatty 0 n = Nothing --throwError ""
intToHideFinNatty n 0 =
    let hnf = intToHideFinNatty (n - 1) 0
        f (HideFinNatty na fin) = HideFinNatty (Sy na) (liftFin fin)
    in  f <$> hnf
intToHideFinNatty nna nf =
    let hnf = intToHideFinNatty (nna - 1) (nf - 1)
        f (HideFinNatty na fin) = HideFinNatty (Sy na) (FSuc fin)
    in  f <$> hnf

liftFin :: Fin n -> Fin (Suc n)
liftFin FZero      = FZero
liftFin (FSuc fin) = FSuc $ liftFin fin


data  HidedNatty where
    HidedNatty ::Natty n -> HidedNatty

intToNatty :: (Eq t, Num t) => t -> HidedNatty
intToNatty 0 = HidedNatty Zy
intToNatty n = case intToNatty $ n - 1 of
    HidedNatty na -> HidedNatty (Sy na)

--- just for errors 
nattyToInt :: Natty n -> Int
nattyToInt Zy     = 0
nattyToInt (Sy n) = 1 + nattyToInt n
