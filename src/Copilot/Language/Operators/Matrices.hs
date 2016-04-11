-- |
-- Module: Matrixes
-- Description: Matrixes in Copilot implemented as arrays of arrays
-- Copyright: (c) 2015 National Institute of Aerospace / Galois, Inc.
--

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}

module Copilot.Language.Operators.Matrices
    where

-- import Copilot.Language
import qualified Copilot.Core as C
--import Copilot.Language.Prelude
import Prelude


--validMatrix :: [[a]] -> Bool
--validMatrix [] = True
--validMatrix [_] = True
--validMatrix (r:r':m) = (length r == length r') && (validMatrix (r':m))

--matrixRowsNum :: [[a]] -> Int
--matrixRowsNum m = length m

--matrixColsNum :: [[a]] -> Int
--matrixColsNum [] = 0
--matrixColsNum m@(r:_)
--    | validMatrix m = length r
--    | otherwise = C.badUsage "The input is not a valid matrix"

--matrixSum :: (Num a) => [[a]] -> [[a]] -> [[a]]
--matrixSum m1 m2
--    | (not (validMatrix m1)) || (not (validMatrix m2)) = C.badUsage "The input is not a valid matrix"
--    | validDim m1 m2 = zipWith (zipWith (+)) m1 m2
--    | otherwise = C.badUsage "matrixSum: Matrix dimension mismatch"
--    where
--        validDim m1 m2 = (matrixColsNum m1) == (matrixColsNum m2) && (matrixRowsNum m1) == (matrixRowsNum m2)    

--matrixExprSum :: (Num a) => C.Expr [[a]] -> C.Expr [[a]] -> C.Expr [[a]]
----matrixExprSum m1@(C.ExternMatrix _ _ _ _ _ _ _ _ _) m2@(C.ExternMatrix _ _ _ _ _ _ _ _ _) = matrixSum (evalExpr k e locs strms) (evalExpr k e locs strms)
----matrixExprSum m1@(C.ExternMatrix _ _ _ _ _ _ _ _ _) m2@(C.Matrix _ _) = matrixSum (evalExpr k e locs strms) (evalExpr k e locs strms)
----matrixExprSum m1@(C.Matrix _ _) m2@(C.ExternMatrix _ _ _ _ _ _ _ _ _) = matrixSum (evalExpr k e locs strms) (evalExpr k e locs strms)
--matrixExprSum (C.Matrix t1 m1) (C.Matrix t2 m2) | t1==t2 = C.Matrix t1 (matrixSum m1 m2)
--											    | otherwise = C.badUsage "matrixSum: type mismatch"
--matrixExprSum _ _ = C.badUsage "matrixSum expects two Matrices as input"


--matrixSubt :: (Num a) => [[a]] -> [[a]] -> [[a]]
--matrixSubt m1 m2
--    | (P.not (validMatrix m1)) P.|| (P.not (validMatrix m2)) = Core.badUsage "The input is not a valid matrix"
--    | validDim m1 m2 = P.zipWith (P.zipWith (-)) m1 m2
--    | otherwise = Core.badUsage "matrixSum: Matrix dimension mismatch"
--    where
--        validDim m1 m2 = (matrixColsNum m1) P.== (matrixColsNum m2) P.&& (matrixRowsNum m1) P.== (matrixRowsNum m2)

--matrixTranspose :: (Num a) => [[a]] -> [[a]]
--matrixTranspose [] = []
--matrixTranspose (r:m) = matrixAppend (rowTraspose r) (matrixTranspose m)

--rowTraspose :: (Num a) => [a] -> [[a]]
--rowTraspose [] = []
--rowTraspose (x:xs) = [x]:(rowTraspose xs)

--matrixAppend :: (Num a) => [[a]] -> [[a]] -> [[a]]
--matrixAppend [] x = x
--matrixAppend x [] = x
--matrixAppend (x:xs) (y:ys) = (x P.++ y):(matrixAppend xs ys)

--matrixMult :: (Num a) => [[a]] -> [[a]] -> [[a]]
--matrixMult m1 m2
--    | (P.not (validMatrix m1)) P.|| (P.not (validMatrix m2)) = Core.badUsage "matrixMult: The input is not a valid matrix"
--    | (matrixColsNum m1) P./= (matrixRowsNum m2) = Core.badUsage "matrixMult: Matrix dimension mismatch"
--    | otherwise = matrixMult' m1 (matrixTranspose m2)
--    where
--        matrixMult' [] _ = []
--        matrixMult' (r:m) m2 = (newMatrixRow r m2) : (matrixMult' m m2)
--        newMatrixRow _ [] = []
--        newMatrixRow r (c:m) = (newMatrixCell r c) : (newMatrixRow r m)
--        newMatrixCell [] [] = 0
--        newMatrixCell (r:rx) (c:cx) = (r*c) + (newMatrixCell rx cx)
--        newMatrixCell _ _ = Core.badUsage "matrixMult: Matrix dimension mismatch"




