-- Different variants of a Calculator 
module Calc where

-- Import of two monads
import DurationMonad
import Probability 

------ Exceptions --------
-- The calculator may raise exceptions

-- Raises an Exception
e :: () -> Maybe a
e () = Nothing

-- A program that can possibly raise an exception.
myDiv :: (Double,Double) -> Maybe Double
myDiv (x,0) = e ()
myDiv (x,y) = return (x / y)


-- Calculates x / (y / z). Note that there are two possible ways
-- of raising an exception.
calc1 :: (Double,Double,Double) -> Maybe Double
calc1 (x,y,z) = do r1 <- myDiv(y,z) ; myDiv(x, r1)

-- Calculates (x / y) / z.
calc2 :: (Double,Double,Double) -> Maybe Double
calc2 (x,y,z) = undefined

-- A program that can possibly raise an exception.
mysqrt :: Double -> Maybe Double
mysqrt = undefined

-- Calculates sqrt ( sqrt(x) / y ).
calc3 :: (Double,Double,Double) -> Maybe Double
calc3 = undefined

------ Durations ---------
-- The calculator takes time to calculate

myDiv' :: (Double,Double) -> Duration Double
myDiv' (x,y) = wait2 ( return (x / y) )

-- Calculates x / (y / z) 
calc1' :: (Double,Double,Double) -> Duration Double
calc1' (x,y,z) = do r1 <- myDiv'(y,z) 
                    myDiv'(x, r1)

-- Calculates (x / y) / z
calc2' :: (Double,Double,Double) -> Duration Double
calc2' (x,y,z) = undefined

mysqrt' :: Double -> Duration Double
mysqrt' = undefined

-- Calculates sqrt ( sqrt(x) / y )
calc3' :: (Double,Double,Double) -> Duration Double
calc3' = undefined

------ Non-determinism ---------
-- The calculator nondeterministically outputs wrong values.

nor :: ([a],[a]) -> [a]
nor (l1,l2) = l1 ++ l2

myDiv'' :: (Double,Double) -> [Double]
myDiv'' (x,y) = nor ( return (x / y), return 7 )

-- Calculates x / (y / z) 
calc1'' :: (Double,Double,Double) -> [Double]
calc1'' (x,y,z) = do r1 <- myDiv''(y,z) 
                     myDiv''(x, r1)

-- Calculates (x / y) / z
calc2'' :: (Double,Double,Double) -> [Double]
calc2'' (x,y,z) = undefined

mysqrt'' :: Double -> [Double]
mysqrt'' = undefined

-- Calculates sqrt ( sqrt(x) / y )
calc3'' :: (Double,Double,Double) -> [Double]
calc3'' = undefined

------ Probabilities ---------
-- The calculator outputs wrong values with a certain probability

por :: (Dist a, Dist a) -> Dist a
por (x,y) = do a <- x
               b <- y
               choose 0.5 a b

myDiv''' :: (Double,Double) -> Dist Double
myDiv''' (x,y) = por ( return (x / y), return 7 )

-- Calculates x / (y / z) 
calc1''' :: (Double,Double,Double) -> Dist Double
calc1''' (x,y,z) = do r1 <- myDiv'''(y,z) 
                      myDiv'''(x, r1)

-- Calculates (x / y) / z
calc2''' :: (Double,Double,Double) -> [Double]
calc2''' (x,y,z) = undefined

mysqrt''' :: Double -> [Double]
mysqrt''' = undefined

-- Calculates sqrt ( sqrt(x) / y )
calc3''' :: (Double,Double,Double) -> [Double]
calc3''' = undefined


