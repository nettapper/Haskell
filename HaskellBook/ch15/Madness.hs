module Madness where

import Data.Monoid

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbin' e adv noun adj =
  e <> "! He said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."

madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinBetter' e adv noun adj = foldr mappend mempty madLibList
  where madLibList = [e, "! He said ",
                     adv, " as he jumped into his car ",
                     noun, " and drove off with his ",
                     adj, " wife."
                     ]


madlibbinBetter'' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinBetter'' e adv noun adj =
  mappend e $ mappend "! He said " $
  mappend adv $ mappend " as he jumped into his car " $
  mappend noun $ mappend " and drove off with his " $
  mappend adj " wife."
