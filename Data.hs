module Data ( ProgramPart(..)
            , Statement(..)
            , Expression(..)
            ) where

import System.IO

data ProgramPart = Method String [String] [Statement]
                 | Block String [Statement]
                 deriving  (Show)

data Statement = Delegate String [Expression]
               | RunBlock String
               | Assign String Expression
               | Compare String [Expression]
               | Conditional Bool Statement
               | Return Expression
               | Write Handle Expression
               | ExprStatement Expression
               deriving (Show)

data Expression = Identifier String
                | Add [Expression]
                | Sub [Expression]
                | Mul [Expression]
                | Div [Expression]
                | Call String [Expression]
                deriving (Show)
