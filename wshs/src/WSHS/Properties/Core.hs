{-# LANGUAGE LambdaCase #-}

module WSHS.Properties.Core
  ( Property(..),
    PropertyCheckResults(..),
    satisfyProperties,
    satisfyProperty,
    isSatisfied
  )
where

import RIO

import Turtle

data Property =
  Property
  { checker :: IO PropertyCheckResults
  , satisfier :: IO ()
  }

data PropertyCheckResults
  = Satisfied
  | Unsatisfied
  deriving (Eq, Show)

satisfyProperties :: [Property] -> IO ()
satisfyProperties = void . traverse satisfyProperty

satisfyProperty :: Property -> IO ()
satisfyProperty (Property checker satisfier) = do
  result <- checker
  case result of
    Satisfied -> return ()
    Unsatisfied -> satisfier

isSatisfied :: Bool -> PropertyCheckResults
isSatisfied = \case
  True -> Satisfied
  False -> Unsatisfied
