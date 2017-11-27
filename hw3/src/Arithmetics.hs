{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Arithmetics (
        Expr (..)
        , evaluateExpr
        , Exception (..)
        ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map.Lazy        as M

data Expr = Lit Int
          | Var String
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Let String Expr Expr
    deriving (Eq, Show)


data Exception = DivizionByZero
               | NoVar String
    deriving (Eq, Show)

newtype Evaluator a = Evaluator { runEvaluator :: (ReaderT (M.Map String Int) (Either Exception)) a }
    deriving (Functor, Applicative, Monad, MonadError Exception, MonadReader (M.Map String Int))

type MonadEvaluator m = (MonadReader (M.Map String Int) m, MonadError Exception m)

exprEvaluation :: MonadEvaluator m => Expr -> m Int
exprEvaluation (Lit i)     = return i
exprEvaluation (Var v)     = do
    val <- asks (M.lookup v)
    case val of
      Just i  -> return i
      Nothing -> throwError $ NoVar v
exprEvaluation (Add l r)   = liftM2 (+) (exprEvaluation l) (exprEvaluation r)
exprEvaluation (Sub l r)   = liftM2 (-) (exprEvaluation l) (exprEvaluation r)
exprEvaluation (Mul l r)   = liftM2 (*) (exprEvaluation l) (exprEvaluation r)
exprEvaluation (Div l r)   = do
    r' <- exprEvaluation r
    if r' == 0 then
      throwError DivizionByZero
    else do
      l' <- exprEvaluation l
      return $ div l' r'
exprEvaluation (Let v d e) = do
    d' <- exprEvaluation d
    local (M.insert v d') (exprEvaluation e)

evaluate :: Evaluator a -> M.Map String Int -> Either Exception a
evaluate = runReaderT . runEvaluator

evaluateExpr :: Expr -> M.Map String Int -> Either Exception Int
evaluateExpr = evaluate . exprEvaluation
