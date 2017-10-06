{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude ()
import Protolude

import Data.Semigroup ((<>))
import Options.Applicative
import Lens.Micro.Platform
import System.Random
import Data.Functor.Foldable
import Control.Monad.Free
import qualified Data.Vector as V
import Data.Vector (Vector(..))

data Options = Options { message :: Text }

opts :: Parser Options
opts = Options <$> option auto
     ( long "message"
    <> short 'm'
    <> metavar "MESSAGE"
    <> help "The message to print"
    <> showDefault
    <> value "Hello, World!"
      )

optsInfo :: ParserInfo Options
optsInfo = info (opts <**> helper)
  ( fullDesc
  <> progDesc "Print a simple message"
  <> header "genetic-algo - a minimal application"
  )

data Gene
  = Gene
  { a :: Double
  , b :: Double
  , c :: Double
  , d :: Double
  , e :: Bool
  } deriving (Show, Eq)

data TData = TData Double Double Double deriving (Show, Eq)

initGene :: RandomGen g => State g Gene
initGene = undefined

calcFit :: Vector TData -> Gene -> (Gene, Double)
calcFit tData gene = (gene, sum . fmap (f gene) $ tData) 
 where f :: Gene -> TData -> Double
       f t g = undefined

reproduce :: RandomGen g => Gene -> Gene -> State g Gene
reproduce = undefined

mutate :: RandomGen g => Gene -> State g Gene
mutate = undefined

pairOff :: [a] -> [(a,a)]
pairOff [] = []
pairOff [x] = []
pairOff (x:y:rest) = (x,y) : pairOff rest

train :: RandomGen g
      => (Vector (Gene, Double)
      -> Vector Gene)
      -> Vector TData
      -> Vector Gene
      -> State g (Vector Gene)
train select tData genes = do
  let survivors = select . fmap (calcFit tData) $ genes
  babies <- sequence . V.fromList . fmap (uncurry reproduce) . pairOff . V.toList $ survivors 
  pure (V.concat [survivors, babies])

main :: IO ()
main = do
  options <- execParser optsInfo
  putStrLn (message options)

