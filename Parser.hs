module Parser ( parseProgram, parseProgramFile ) where

import Data

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)
import System.IO

lexer  = P.makeTokenParser emptyDef

symbol = P.symbol lexer
identifier = P.identifier lexer
reserved = P.reserved lexer


-- a list of p's in the form "p, p, p and p"
list2 p = do l <- p `sepBy` (symbol ",")
             reserved "and"
             end <- p
             return $ l ++ [end]

-- a list of p's, starting with the reserved word q
-- or possibly 'q only p' for one item.
qualifiedList q p = do reserved q
                       l <- do reserved "only"
                               li <- p
                               return [li]
                        <|> list2 p
                       return l
                <|> return []


-- several reserved words.
manyReserved = sequence . map reserved


-- expressions

addParser = do reserved "synergise"
               args <- list2 expressionParser
               return $ Add args

subParser = do reserved "contextualise"
               args <- list2 expressionParser
               return $ Sub args

mulParser = do reserved "project"
               args <- list2 expressionParser
               return $ Mul args

divParser = do reserved "migrate"
               args <- list2 expressionParser
               return $ Div args

consParser = do reserved "leverage"
                arg1 <- expressionParser
                reserved "and"
                arg2 <- expressionParser
                return $ Cons arg1 arg2

carParser = do reserved "decentralise"
               arg <- expressionParser
               return $ Car arg

cdrParser = do reserved "centralise"
               arg <- expressionParser
               return $ Cdr arg

callParser = do reserved "dispatch"
                name <- identifier
                arglist <- qualifiedList "integrating" expressionParser
                return $ Call name arglist

identifierParser = do name <- identifier
                      return $ Identifier name

expressionParser = choice $ map try [ addParser
                                    , subParser
                                    , mulParser
                                    , divParser
                                    , callParser
                                    , consParser
                                    , carParser
                                    , cdrParser
                                    , identifierParser
                                    ]


-- statements
runBlockParser = do reserved "implement"
                    name <- identifier
                    return $ RunBlock name

assignParser = do reserved "integrate"
                  expr <- expressionParser
                  reserved "into"
                  name <- identifier
                  return $ Assign name expr

compareParser = do manyReserved $ words "consult market analyst"
                   cmp <- identifier
                   args <- qualifiedList "regarding" expressionParser
                   return $ Compare cmp args

conditionalParser = do manyReserved $ words "if the lines go"
                       direction <- symbol "up" <|> symbol "down"
                       reserved "then"
                       stmt <- statementParser
                       return $ Conditional (if direction == "up" then True else False) stmt

returnParser = do reserved "report"
                  value <- expressionParser
                  return $ Return value

displayParser = do reserved "display"
                   expr <- expressionParser
                   manyReserved $ words "on the business dashboard"
                   return $ Write stdout expr

exprStatementParser = do reserved "progressively"
                         expr <- expressionParser
                         return $ ExprStatement expr


statementParser = choice $ map try [ runBlockParser
                                   , assignParser
                                   , compareParser
                                   , conditionalParser
                                   , returnParser
                                   , displayParser
                                   , exprStatementParser
                                   ]


-- 'blocks'
methodParser = do name <- identifier
                  manyReserved $ words "is a best practice"
                  arglist <- qualifiedList "orchestrating" identifier
                  statements <- many statementParser
                  return $ Method name arglist statements

blockParser = do name <- identifier
                 manyReserved $ words "is a business stratergy"
                 statements <- many statementParser
                 return $ Block name statements

-- a program is several blocks
programParser = many $ choice $ map try [ methodParser
                                        , blockParser
                                        ]


-- parse from a string
parseProgram = parse programParser ""
-- parse a whole file
parseProgramFile = parseFromFile programParser
