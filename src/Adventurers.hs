{-# LANGUAGE FlexibleInstances #-}
module Adventurers where

import DurationMonad
import Probability

-- List of adventurers
data Adventurer = P1 | P2 | P5 | P10 deriving (Show,Eq)

-- Adventurers + the lantern
type Objects = Either Adventurer ()

{-- 
 - State of the game, i.e. the current position of each adventurer
 - + the lantern. The function (const False) represents the initial state of the
 - game, i.e. all adventurers + the lantern on the left side of the bridge.  The
 - function (const True) represents the end state of the game, i.e. all
 - adventurers + the lantern on the right side of the bridge.  
--}
type State = Objects -> Bool

instance Show State where
  show s = (show . (fmap show)) [s (Left P1),
                                 s (Left P2),
                                 s (Left P5),
                                 s (Left P10),
                                 s (Right ())]

instance Eq State where
  (==) s1 s2 = and [s1 (Left P1) == s2 (Left P1),
                    s1 (Left P2) == s2 (Left P2),
                    s1 (Left P5) == s2 (Left P5),
                    s1 (Left P10) == s2 (Left P10),
                    s1 (Right ()) == s2 (Right ())]

-- Initial state of the game
gInit :: State
gInit = const False

-- Changes the state s of the game for a given object o
changeState :: Objects -> State -> State
changeState o s = \x -> if x == o then not (s o) else s x

-- Changes the state of the game for a list of objects 
mChangeState :: [Objects] -> State -> State
mChangeState os s = foldr changeState s os

--- TASK 1 -------------------------------------------------------

-- Time that each adventurer takes to cross the bridge
getTimeAdv :: Adventurer -> Int
getTimeAdv P1  = 1
getTimeAdv P2  = 2
getTimeAdv P5  = 5
getTimeAdv P10 = 10

{-- 
 - For a given state of the game, the function presents 
 - all possible moves that the adventurers can make.  
--}
allValidPlays :: State -> ListDur State
allValidPlays s = manyChoice (singles ++ pairs)

      where singles   = [ LD [Duration 
                          (getTimeAdv a,
                          mChangeState [Left a, Right ()] s)]
                        | a <- (advsWithLantern s) ]

            pairs     = [ LD [Duration 
                          (max (getTimeAdv a1) (getTimeAdv a2),
                          mChangeState [Left a1, Left a2, Right ()] s)]
                        | (a1, a2) <- makePairs (advsWithLantern s) ]

-- auxiliary function
advsWithLantern :: State -> [Adventurer]
advsWithLantern s = filter (\a -> s (Left a) == s(Right())) [P1, P2, P5, P10]

{-- 
 - For a given number n and initial state, the function calculates
 - all possible n-sequences of moves that the adventures can make 
--}
exec :: Int -> State -> ListDur State
exec 0 s = return s
exec n s = do s' <- allValidPlays s
              exec (n-1) s'

{-- 
 - Is it possible for all adventurers to be on the other side
 - in <=17 min and not exceeding 5 moves ? 
--}
leq17 :: Bool
leq17 = any (\(Duration (t,s)) -> s == gFinal && t <= 17)
  (remLD (manyChoice [exec n gInit | n <- [0..]]))

-- auxiliary function
gFinal :: State
gFinal = const True

{-- Is it possible for all adventurers to be on the other side
 - in < 17 min ? 
--}
l17 :: Bool
l17 = any (\(Duration (t,s)) -> s == gFinal)
  (remLD (reachableDurStates 17 gInit))

-- auxiliary function
reachableDurStates :: Int -> State -> ListDur State
reachableDurStates limit s0 = LD (go [Duration (0, s0)] [(s0, 0)])
  where
    go [] _ = []

    go (Duration (t, s) : ws) seen =
      Duration (t, s) : go (ws ++ newNexts) seen'
      where
        nexts = remLD (allValidPlays s)

        newNexts =
          [ Duration (t + dt, s')
          | Duration (dt, s') <- nexts
          , t + dt < limit
          , notElem (s', t + dt) seen
          ]

        seen' =
          map (\(Duration (t', s')) -> (s', t')) newNexts ++ seen

--- END OF TASK 1 -------------------------------------------------

--- TASK 2 --------------------------------------------------------

-- Represents which adventurer(s) to move next
type Move = Either Adventurer (Adventurer, Adventurer)

-- Calculates the resulting state based on which adventurers to move
-- next and their probabilistic crossing times
play :: Move -> State -> DistDur State
play (Left a) s =
  DD (do t <- probTime (getTimeAdv a)
         return (Duration (t, mChangeState [Left a, Right ()] s)))
play (Right (a1, a2)) s =
  DD (do t1 <- probTime (getTimeAdv a1)
         t2 <- probTime (getTimeAdv a2)
         return (Duration (max t1 t2, mChangeState [Left a1, Left a2, Right ()] s)))

-- auxiliary function
probTime :: Int -> Dist Int
probTime x = uniform [x - x `div` 2, x, x + x `div` 2]

instance Ord State where
  compare s1 s2 = compare (show s1) (show s2)

-- Extends the previous function to lists of movements
plays :: [Move] -> State -> DistDur State 
plays []     s = return s
plays (m:ms) s = do s' <- play m s
                    plays ms s'

-- auxiliary function
distExample :: DistDur State
distExample = plays [Right (P1,P2), Left P1, Right (P5,P10), Left P2, Right (P1,P2)] gInit

 
--- END OF TASK 2 -------------------------------------------------

--- TASK 3 --------------------------------------------------------

--- Interactive Game Using the IO Monad
game :: IO ()
game = do
  gameLoop (Duration (0,gInit))

gameLoop :: Duration State -> IO ()
gameLoop ds@(Duration (t,s)) = do
  putStr "\ESC[2J\ESC[1H"
  putStrLn ""
  putStrLn "  \ESC[35mBridge and Torch Game\ESC[0m — Save everyone in 17 minutes or less"
  drawDurationState ds
  if s == gFinal
    then do
      if t <= 17
        then do
          putStrLn "  Mission: \ESC[32mCOMPLETED\ESC[0m -- you won!"
        else do
          putStrLn "  Mission: \ESC[31mFAILED\ESC[0m -- try again!"
      pickChoice ds []
    else do
      let available = advsWithLantern s
          singles   = map Left available
          pairs     = map (\(a1,a2) -> Right (a1,a2)) (makePairs available)
          moves     = singles ++ pairs
      putStrLn "  Mission: IN PROGRESS"
      pickChoice ds moves

pickChoice :: Duration State -> [Move] -> IO ()
pickChoice ds moves = do
  putStrLn ""
  putStrLn "  Available choices:"

  mapM_ (\(i,m) ->
    putStrLn $ "  " ++ show i ++ ") " ++ showMove m
    ) (zip [1..] moves)

  let restartChoiceNum = length moves + 1
      quitChoiceNum    = length moves + 2

  putStrLn $ "  " ++ show restartChoiceNum ++ ") [RESTART GAME]"
  putStrLn $ "  " ++ show quitChoiceNum ++ ") [QUIT GAME]"

  putStrLn ""
  putStr "  Choice: "
  line <- getLine

  case reads line of
    [(n,"")] | n >= 1 && n <= length moves ->
      gameLoop (applyMove (moves !! (n-1)) ds)

    [(n,"")] | n == restartChoiceNum ->
      gameLoop (Duration (0,gInit))

    [(n,"")] | n == quitChoiceNum ->
      putStr "\ESC[2J\ESC[1H"

    _ -> gameLoop ds

showMove :: Move -> String
showMove (Left a) =
  show a ++ " [" ++ show (getTimeAdv a) ++ " min]"
showMove (Right (a1,a2)) =
  show a1 ++ " + " ++ show a2 ++ " [" ++
  show (max (getTimeAdv a1) (getTimeAdv a2)) ++ " min]"

applyMove :: Move -> Duration State -> Duration State
applyMove (Left a) (Duration (t,s)) =
  Duration ((getTimeAdv a) + t,
  mChangeState [Left a, Right ()] s)
applyMove (Right (a1,a2)) (Duration (t,s)) =
  Duration ((max (getTimeAdv a1) (getTimeAdv a2)) + t,
  mChangeState [Left a1, Left a2, Right ()] s)

drawDurationState :: Duration State -> IO ()
drawDurationState (Duration (t,s)) = do
  putStrLn ""
  putStrLn   "  -----not saved---------------bridge---------------saved-----"
  putStrLn ""
  putStrLn $ "  " ++ drawLeftSide s ++ "  ▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀  " ++ drawRightSide s
  putStrLn ""
  putStrLn $ "  " ++ drawTimerBar t
  putStrLn ""

drawLeftSide :: State -> String
drawLeftSide s =
  let torch = if not (s (Right())) then "💡 " else "   "
      p1  = if not (s (Left P1))  then "P1 "  else "   "
      p2  = if not (s (Left P2))  then "P2 "  else "   "
      p5  = if not (s (Left P5))  then "P5 "  else "   "
      p10 = if not (s (Left P10)) then "P10 " else "    "
  in  torch ++ p1 ++ p2 ++ p5 ++ p10

drawRightSide :: State -> String
drawRightSide s =
  let torch = if s (Right ()) then "💡 " else "   "
      p1  = if s (Left P1)  then "P1 "  else "   "
      p2  = if s (Left P2)  then "P2 "  else "   "
      p5  = if s (Left P5)  then "P5 "  else "   "
      p10 = if s (Left P10) then "P10 " else "    "
  in  torch ++ p1 ++ p2 ++ p5 ++ p10

drawTimerBar :: Int -> String
drawTimerBar t = 
  let barLength = min t 17
      dotsLength = 17 - barLength
      status = if t > 17 
                 then " OVER 17!"
                 else " 17 min"
  in show t ++ " min " ++ replicate barLength '='
  ++ replicate dotsLength '.' ++ status

--- State Sequence of Adventurers’ Movements
data ListDurLog a = LDL [Duration ([State], a)] deriving Show

remLDL :: ListDurLog a -> [Duration ([State], a)]
remLDL (LDL x) = x

instance Functor ListDurLog where
  fmap f = LDL . map (fmap (\(log, x) -> (log, f x))) . remLDL

instance Applicative ListDurLog where
  pure x = LDL [Duration (0, ([], x))]
  l1 <*> l2 = LDL $ do
    x <- remLDL l1
    y <- remLDL l2
    return $ do (log1, f) <- x; (log2, a) <- y; return (log1 ++ log2, f a)

instance Monad ListDurLog where
  return = pure
  l >>= k = LDL $ do
    Duration (t, (log1, x)) <- remLDL l
    Duration (t', (log2, y)) <- remLDL (k x)
    return $ Duration (t + t', (log1 ++ log2, y))

manyLDLChoice :: [ListDurLog a] -> ListDurLog a
manyLDLChoice = LDL . concatMap remLDL

playLog :: Move -> State -> ListDurLog State
playLog (Left a) s =
  LDL [Duration (getTimeAdv a,
  ([mChangeState [Left a, Right ()] s],
  mChangeState [Left a, Right ()] s))]
playLog (Right (a1, a2)) s =
  LDL [Duration (max (getTimeAdv a1) (getTimeAdv a2),
  ([mChangeState [Left a1, Left a2, Right ()] s],
  mChangeState [Left a1, Left a2, Right ()] s))]

playsLog :: [Move] -> State -> ListDurLog State
playsLog []     s = return s
playsLog (m:ms) s = do s' <- playLog m s
                       playsLog ms s'

diffStates :: State -> State -> [Adventurer]
diffStates s1 s2 = filter(\a -> s1 (Left a) /= s2 (Left a)) [P1, P2, P5, P10]

crossTime :: State -> State -> Int
crossTime s1 s2 =
  maximum [ getTimeAdv a | a <- diffStates s1 s2 ]

showMove' :: State -> State -> String
showMove' s1 s2 = show (diffStates s1 s2) ++ " " ++ dir
  where dir = if s2 (Right ()) then "cross" else "cross back"

showPlaysLog :: [Move] -> State -> IO ()
showPlaysLog ms s0 =
  case remLDL (playsLog ms s0) of
    [] -> putStrLn "No result."
    (Duration (t, (steps, _)):_) ->
      putStr $ unlines $
        [""] ++ showSteps 0 (s0 : steps) ++ [""]
  where
    showSteps :: Int -> [State] -> [String]
    showSteps _ []  = []
    showSteps t [s] = [show t ++ " min  " ++ show s]
    showSteps t (s1:s2:rest) =
      [ show t ++ " min  " ++ show s1
      , showMove' s1 s2]
      ++ showSteps (t + crossTime s1 s2) (s2:rest)

showPlaysLog2 :: [Move] -> State -> IO ()
showPlaysLog2 ms s0 =
  case remLDL (playsLog ms s0) of
    [] -> putStrLn "No result."
    (Duration (t, (steps, _)):_) -> drawSteps 0 (s0 : steps)
  where
    drawSteps :: Int -> [State] -> IO ()
    drawSteps _ []  = return ()
    drawSteps t [s] = drawDurationState (Duration (t, s))
    drawSteps t (s1:s2:rest) = do
      drawDurationState (Duration (t, s1))
      putStrLn $ "      \ESC[35m" ++ showMove' s1 s2 ++ "\ESC[0m"
      drawSteps (t + crossTime s1 s2) (s2:rest)

allValidPlaysLog :: State -> ListDurLog State
allValidPlaysLog s = manyLDLChoice (singles ++ pairs)

  where singles = [ LDL [Duration
                    (getTimeAdv a,
                    ([mChangeState [Left a, Right ()] s ],
                    mChangeState [Left a, Right ()] s ))]
                    | a <- (advsWithLantern s)]

        pairs   = [ LDL [Duration
                    (max (getTimeAdv a1) (getTimeAdv a2),
                    ([mChangeState [Left a1, Left a2, Right ()] s ],
                    mChangeState [Left a1, Left a2, Right ()] s ))]
                  | (a1, a2) <- makePairs (advsWithLantern s)]

execLog :: Int -> State -> ListDurLog State
execLog 0 s = return s
execLog n s = do s' <- allValidPlaysLog s
                 execLog (n-1) s'

bestPaths :: Int -> [[State]]
bestPaths maxMoves =
  [ log
  | n <- [0..maxMoves]
  , Duration (t, (log, s)) <- remLDL (execLog n gInit)
  , s == gFinal
  , t <= 17
  ]

statesToMoves :: [State] -> [Move]
statesToMoves []  = []
statesToMoves [_] = []
statesToMoves (s1:s2:rest) =
  case diffStates s1 s2 of
    [a]     -> Left a : statesToMoves (s2:rest)
    [a1,a2] -> Right (a1,a2) : statesToMoves (s2:rest)
    _       -> statesToMoves (s2:rest)

printBestPath :: IO ()
printBestPath = showPlaysLog (statesToMoves (gInit: (head (bestPaths 5)))) gInit

printBestPath2 :: IO ()
printBestPath2 = showPlaysLog2 (statesToMoves (gInit: (head (bestPaths 5)))) gInit

--- END OF TASK 3 -------------------------------------------------

--- MONAD IMPLEMENTATIONS -----------------------------------------

-- Non-determinism combined with durations
data ListDur a = LD [Duration a] deriving Show

remLD :: ListDur a -> [Duration a]
remLD (LD x) = x

instance Functor ListDur where
   fmap f = let f' = (fmap f) in
     LD . (map f') . remLD

instance Applicative ListDur where
   pure x = LD [Duration (0,x)]
   l1 <*> l2 = LD $ do x <- remLD l1
                       y <- remLD l2
                       return $ do f <- x; a <- y; return (f a)

instance Monad ListDur where
   return = pure
   l >>= k = LD $ do x <- remLD l
                     g x where
                       g(Duration (i,x)) = let u = (remLD (k x))
                          in map (\(Duration (i',x)) -> Duration (i + i', x)) u

manyChoice :: [ListDur a] -> ListDur a
manyChoice = LD . concat . (map remLD)

-- Probabilistic behaviour combined with durations (note the similarity
-- with the previous code)
data DistDur a = DD (Dist (Duration a)) deriving Show

remDD :: DistDur a -> Dist (Duration a)
remDD (DD x) = x

instance Functor DistDur where
        fmap f = DD . (fmap (fmap f)) . remDD

instance Applicative DistDur where
        pure x = DD (return $ return x)
        d1 <*> d2 = DD $ do x <- remDD d1
                            y <- remDD d2
                            return $ do f <- x; a <- y; return (f a)

instance Monad DistDur where
        return = pure
        d >>= k = DD $ do x <- remDD d
                          g x where
                          g(Duration (i,x)) = let u = (remDD (k x))
                                in fmap (\(Duration (i',x)) -> Duration (i + i', x)) u


--- END OF MONAD IMPLEMENTATIONS ----------------------------------

--------- LIST UTILS ----------------------------------------------

makePairs :: (Eq a) => [a] -> [(a,a)]
makePairs as = normalize $ do a1 <- as; a2 <- as; [(a1,a2)]
                                
normalize :: (Eq a) => [(a,a)] -> [(a,a)]
normalize l = removeSw $ filter p1 l where
  p1 (x,y) = if x /= y then True else False

removeSw :: (Eq a) => [(a,a)] -> [(a,a)]
removeSw [] = []
removeSw ((a,b):xs) = if elem (b,a) xs then removeSw xs else (a,b):(removeSw xs)
