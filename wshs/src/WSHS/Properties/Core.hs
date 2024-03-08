{-# LANGUAGE LambdaCase #-}
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

import Prelude (putStrLn)

import Turtle

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

checkProperties :: [Property] -> IO ()
checkProperties props = do
  results <- mapM checkProperty props
  if (and $ (== Satisfied ) <$> results) then
    putStrLn "Properties fulfilled"
  else
    error "properties unfulfilleed"

checkProperty :: Property -> IO PropertyCheckResults
checkProperty (Property name checker _) = checker

  -- do
  -- result <- checker
  -- undefined
  -- error "checkProperties is yet undefined"
