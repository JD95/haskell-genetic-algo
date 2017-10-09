{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Prelude (String, foldr1)
import           Protolude

import           Control.Arrow
import           Control.Monad.Free
import           Control.Parallel
import           Control.Parallel.Strategies
import           Data.Functor.Foldable
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Read as T
import           Data.Vector (Vector(..))
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as V
import           Lens.Micro.Platform
import           Options.Applicative
import           System.Random
import           Text.Read (readEither)

data Gene
  = Gene
  { a :: Double
  , b :: Double
  , c :: Double
  , d :: Double
  , e :: Bool
  } deriving (Show, Eq)

data TData = TData Double Double Double deriving (Show, Eq)

initPop :: RandomGen g => Int -> State g (Vector Gene) 
initPop = fmap V.fromList . sequence . flip replicate randomGene 

withinRange :: Gene -> TData -> Bool
withinRange g (TData d1 d2 _)
  = and [ a g <= d1
        , d1 <= b g
        , c g <= d2
        , d2 <= d g
        ] 

calcFit :: Vector TData -> Gene -> (Gene, Double)
calcFit tData gene = (gene, sum . inParallel $ fmap (f gene) tData) 
 where f :: Gene -> TData -> Double
       f g t@(TData d1 d2 profit)
         | withinRange g t = if e g then -1 * profit else profit
         | otherwise = 0.0

lower :: Applicative f => f (a -> b) -> (a -> f b)
lower f a = f <*> pure a 

lower2 :: Applicative f => f (a -> b -> c) -> (a -> b -> f c)
lower2 = fmap lower . lower

reproduce :: RandomGen g => Gene -> Gene -> State g Gene
reproduce = lower2 $ randomlyApply identity flip f 
  where f p1 p2 = Gene (a p1) (b p1) (c p2) (d p2) (e p2)

randomlyApply :: RandomGen g => (a -> b) -> (a -> b) -> a -> State g b
randomlyApply f g a = do
  chance <- state (random @Float) 
  if chance > 0.5
    then pure (f a) 
    else pure (g a) 

randomGene :: RandomGen g => State g Gene
randomGene = do
  a <- state (random @Double)
  b <- state (random @Double)
  c <- state (random @Double)
  d <- state (random @Double)
  e <- state (random @Bool)
  pure (Gene a b c d e)

mutate :: RandomGen g => Gene -> State g Gene
mutate = join . randomlyApply pure (const randomGene)

pairOff :: [a] -> [(a,a)]
pairOff [] = []
pairOff [x] = []
pairOff (x:y:rest) = (x,y) : pairOff rest

elitist :: Vector (Gene, Double) -> Vector Gene
elitist = fmap fst . V.modify (V.sortBy (comparing snd))   

tornament :: Vector (Gene, Double) -> Vector Gene
tornament = undefined

inParallel :: Traversable t => t a -> t a
inParallel = withStrategy (parTraversable rpar)

train :: RandomGen g
      => (Vector (Gene, Double) -> Vector Gene)
      -> Vector TData
      -> Vector Gene
      -> State g (Vector Gene)
train select tData
  = uncurry (liftA2 (<>)) -- ^ Combine survivors and babies
  . (pure &&& repo)       -- ^ Reproduce
  . select                -- ^ Use Selection
  . inParallel 
  . fmap (calcFit tData)  -- ^ Calculate fitness

  where repo = sequence . V.fromList
             . fmap (mutate <=< uncurry reproduce)
             . pairOff . V.toList 

parseData :: Text -> Either String [TData]
parseData = sequence . inParallel . fmap parseLine . T.lines
  where parseLine = parse <=< sequence
                  . fmap (readEither @Double)
                  . fmap toS . T.splitOn "\t"
        parse [a,b,c] = Right (TData a b c)
        parse _ = Left "Invalid Data"

runAlgo :: Vector TData -> IO ()
runAlgo tdata = do
  g <- getStdGen
  popSize <- pure 100 
  gens <- pure 10 
  let result = flip evalState g $ do
        genes <- initPop popSize 
        foldr1 (<=<) (replicate gens (train elitist tdata)) genes
  print "Results!"
  mapM_ print result

main :: IO ()
main = do
  contents <- parseData <$> readFile "genAlgData1.txt"
  case contents of
    Left e -> putStrLn $ "ERROR: " <> e
    Right tdata -> runAlgo $ V.fromList tdata
