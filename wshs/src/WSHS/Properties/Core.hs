{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module WSHS.Properties.Core
  ( Property(..),
    PropertyCheckResults(..),
    satisfyProperties,
    checkProperties,
    satisfyProperty,
    isSatisfied
  )
where

import RIO

import Data.Text.IO (putStrLn)

import Turtle hiding ((<&>))

data Property =
  Property
  { name :: Text
  , checker :: IO PropertyCheckResults
  , satisfier :: IO ()
  }

data PropertyCheckResults
  = Satisfied
  | Unsatisfied
  deriving (Eq, Show)

satisfyProperties :: [Property] -> IO ()
satisfyProperties = void . traverse satisfyProperty

satisfyProperty :: Property -> IO ()
satisfyProperty (Property _ checker satisfier) = do
  result <- checker
  case result of
    Satisfied -> return ()
    Unsatisfied -> satisfier

isSatisfied :: Bool -> PropertyCheckResults
isSatisfied = \case
  True -> Satisfied
  False -> Unsatisfied

isSatisfied' :: PropertyCheckResults -> Bool
isSatisfied' = (== Satisfied)

checkProperties :: [Property] -> IO ()
checkProperties props = do
  results <- mapM checkProperty props
  when (not $ all (isSatisfied' . snd)  results) $
    forM_ (filter ((not . isSatisfied') . snd) results) $ \(propName,_) -> do
      putStrLn $ "property unfulfilled: " <> propName
      exit $ ExitFailure 33

checkProperty :: Property -> IO (Text, PropertyCheckResults)
checkProperty (Property name checker _) =
  checker <&> (name,)
