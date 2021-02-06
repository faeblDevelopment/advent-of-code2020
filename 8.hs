{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Lib
import Data.Char
import Control.DeepSeq
import Data.Maybe
import GHC.Generics
import Data.Function
import Control.Monad.Fix 
import Debug.Trace

dummyInput = [ "nop +0"
             , "acc +1"
             , "jmp +4"
             , "acc +3"
             , "jmp -3"
             , "acc -99"
             , "acc +1"
             , "jmp -4"
             , "acc +6" ]

dummyBand = fromJust $ toBand dummyInput
    where toBand = sequence . map ((fmap (flip (,) False)) . parseInstruction)

main = do
    let toBand = sequence . map ((fmap (flip (,) False)) . parseInstruction) 
        dummyBand = fromJust $ toBand dummyInput
    --print $ toBand dummyInput
    --print $ runState (fixState eval dummyBand) $ EvalState 0 0

    contents <- readFile "8.input"
    let band = fromJust $  toBand $ lines contents 
    --print $ runState (fixState eval band) $ EvalState 0 0

    putStrLn "fixing bug:"
    let res = untilLastExecuted band (-1)
    print res
    print $ (!!627) . fst $ res

untilLastExecuted :: Band (ArithFunc Int) -> Int -> (Band (ArithFunc Int), EvalState)
untilLastExecuted band i =
    let result = runState (fixState eval (changeIth i band)) $ EvalState 0 0
    in if (snd . last . fst $ result) 
         then result
         else untilLastExecuted band (i+1)

changeIth :: Int -> Band (ArithFunc Int) -> Band (ArithFunc Int)
changeIth _ [] = []
changeIth 0 ((Jmp x,b):t) = (Nop x,b) : t
changeIth 0 ((Nop x,b):t) = (Jmp x,b) : t
changeIth i (instr@(Jmp _,_):t) = instr : changeIth (i-1) t
changeIth i (instr@(Nop _,_):t) = instr : changeIth (i-1) t
changeIth i (h:t)               = h : changeIth i t

parseArithFunc :: forall a. Read a => String -> Maybe (ArithFunc a)
parseArithFunc (a:s) 
    | a == '+'  = Just $ Plus rest
    | a == '-'  = Just $ Minus rest 
    | a == '*'  = Just $ Times rest
    | otherwise = Nothing
    where
        rest = read @a s

parseInstruction :: String -> Maybe (Instruction (ArithFunc Int))
parseInstruction s = case inst of
                        "jmp" -> Jmp <$> f
                        "acc" -> Acc <$> f
                        "nop" -> Nop <$> f
                        _     -> Nothing
    where inst = take 3 s
          f = parseArithFunc $ drop 4 s

data Instruction a = Nop a | Acc a | Jmp a
    deriving (Show, Generic, Read)

instance NFData a => NFData (Instruction a)

type Band a = [(Instruction a, Bool)] 

data EvalState = EvalState { instrPointer :: Int
                           , accumulator  :: Int }
                           deriving (Show, Generic, Eq)

incr :: EvalState -> EvalState
incr (EvalState i a) = EvalState (i+1) a

modAcc :: (Int -> Int) -> EvalState -> EvalState
modAcc f (EvalState i a) = EvalState i $ f a

modInstr :: (Int -> Int) -> EvalState -> EvalState
modInstr f (EvalState i a) = EvalState (f i) a 

newtype State s a = State { runState :: s -> (a, s) }

contramap :: (a -> b) -> (a, c) -> (b, c)
contramap f (a, c) = (f a, c)

instance Functor (State s) where
    f `fmap` (State s) = State $ contramap f . s

instance Applicative (State s) where
    pure a = State $ \s -> (a, s)
    (State act) <*> s2 = State $ \s -> 
        let (f, s') = act s
         in runState (f <$> s2) s'

instance Monad (State s) where
    (State act) >>= f = State $ \s -> 
                            let (a, s') = act s
                             in runState (f a) s'

    {-
instance MonadFix (State s) where
    mfix f = mfix $ \st -> f st 
      -}      

get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put x = State $ \sth -> ((), x)

modify :: (s -> s) -> State s ()
modify f = do
            x <- get
            put (f x)

(!!!) :: NFData a => [a] -> Int -> Maybe a
(!!!) l a = deepTry (l!!) a

data ArithFunc a = Plus a | Minus a | Times a 
    deriving (Show, Generic) 

instance NFData a => NFData (ArithFunc a)

toFunc :: Num a => ArithFunc a -> (a -> a)
toFunc (Plus a) = ((+) a)
toFunc (Minus a) = (flip (-) a)
toFunc (Times a) = (*a)

firstBy :: Eq a => (a -> a -> Bool) -> [a] -> a
firstBy f (a:as) = fa as a
    where 
          fa (a:as) b 
            | f a b = b
            | otherwise = fa as a

modList :: Int -> (a -> a) -> [a] -> [a]
modList i f l 
  | length l > i  = take (i) l ++ [f $ l!!i] ++ (drop (i+1) l )
  | otherwise     = l

fixState :: (Eq a, Show a) => (b -> State a b) -> b -> State a b
fixState action lastOutput = do
    last <- get
    newOutput <- action lastOutput
    new <- get
    if last == new --compare states
       then return lastOutput
       else fixState action newOutput
    {-
    let now = f last
        valNow = fst $ runState (now >> get) $ last
     in if valNow == last
           then now
           else fixState f valNow 
-}
eval :: Band (ArithFunc Int) -> State EvalState (Band (ArithFunc Int))
eval instructions = do
    s@(EvalState p a) <- get
    let newEval = 
            (case instructions !!! p of
              Nothing -> s
              (Just (_, True)) -> s
              (Just (Nop f, _)) -> EvalState (p+1) a
              (Just (Acc f, _)) -> EvalState (p+1) (toFunc f $ a)
              (Just (Jmp f, _)) -> EvalState (toFunc f $ p) a )
    
    put $ newEval
    return $ modList p (\(a, _) -> (a, True)) instructions
