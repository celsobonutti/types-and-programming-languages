{-# LANGUAGE LambdaCase #-}
module Arithmetic where

type Info = String

data Term
  = TmTrue Info
  | TmFalse Info
  | TmIf (Info, Term, Term, Term)
  | TmZero Info
  | TmSucc (Info, Term)
  | TmPred (Info, Term)
  | TmIsZero (Info, Term)

isNumericalVal :: Term -> Bool
isNumericalVal = \case
    TmZero _       -> True
    TmSucc (_, t1) -> isNumericalVal t1
    _              -> False

isVal :: Term -> Bool
isVal = \case
    TmTrue  _            -> True
    TmFalse _            -> True
    t | isNumericalVal t -> True
    _                    -> False

data Error = NoRuplesApplied

eval1 :: Term -> Either Error Term
eval1 = \case
    TmIf (_, TmTrue _, t2, t3) -> Right t2
    TmIf (_, TmFalse _, t2, t3) -> Right t3
    TmIf (fi, t1, t2, t3) -> (\t1' -> TmIf (fi, t1', t2, t3)) <$> eval1 t1
    TmSucc (fi, t1) -> (\t1' -> TmSucc (fi, t1')) <$> eval1 t1
    TmPred (_, TmZero _) -> Right $ TmZero "dummy"
    TmPred (_, TmSucc (_, nv1)) | isNumericalVal nv1 -> Right nv1
    TmPred (fi, t1) -> (\t1' -> TmPred (fi, t1')) <$> eval1 t1
    TmIsZero (_, TmZero _) -> Right $ TmTrue "dummy"
    TmIsZero (_, TmSucc (_, nv1)) | isNumericalVal nv1 ->
        Right $ TmFalse "dummy"
    TmIsZero (fi, t1) -> (\t1' -> TmIsZero (fi, t1')) <$> eval1 t1
    _                 -> error "No rules apply"

eval :: Term -> Term
eval t = case eval1 t of
    Right t' -> eval t'
    Left  _  -> t

bEval :: Term -> Either Error Term
bEval = \case
    term | isVal term             -> Right term
    TmIf   (_, TmTrue _ , t2, _ ) -> bEval t2
    TmIf   (_, TmFalse _, _ , t3) -> bEval t3
    TmSucc (fi, t1)               -> bEval t1 >>= \t1' -> if isNumericalVal t1'
        then Right $ TmSucc (fi, t1')
        else Left NoRuplesApplied
    TmPred (_, t) -> bEval t >>= \case
        TmZero _        -> Right $ TmZero "dummy"
        TmSucc (_, nv1) -> Right nv1
        _               -> Left NoRuplesApplied
    TmIsZero (_, t) -> bEval t >>= \case
        TmZero _      -> Right $ TmTrue "dummy"
        TmSucc (_, _) -> Right $ TmFalse "dummy"
        _             -> Left NoRuplesApplied
    _ -> Left NoRuplesApplied
