{- 
 
   Parser.hs: Parser for the Flounder interface definition language
                      
   Part of Flounder: a strawman device definition DSL for Barrelfish
   
  Copyright (c) 2009, ETH Zurich.

  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
-}
  
module Parser where

import FuguBackend

import Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Pos
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( javaStyle )
import Data.Char
import Numeric
import Data.List
import Text.Printf

parse filename = parseFromFile errorFile filename
                      
lexer = P.makeTokenParser (javaStyle
                           { P.reservedNames = [ "errors", 
                                                 "success",
                                                 "failure"
                                               ]
                           , P.reservedOpNames = ["*","/","+","-"]
                           , P.commentStart = "/*"
                           , P.commentEnd = "*/"
                           , P.commentLine = "//"
                           })

whiteSpace = P.whiteSpace lexer 
reserved   = P.reserved lexer
identifier = P.identifier lexer
stringLit  = P.stringLiteral lexer
comma      = P.comma lexer
commaSep   = P.commaSep lexer
commaSep1  = P.commaSep1 lexer
parens     = P.parens lexer
braces     = P.braces lexer
squares    = P.squares lexer
semiSep    = P.semiSep lexer
symbol     = P.symbol lexer


errorFile = 
    do 
      whiteSpace
      errors <- many1 errorClass
      return errors


errorClass = 
    do 
      reserved "errors"
      name <- identifier
      classE <- identifier
      errors <- braces $ many1 (errorCase classE)
      symbol ";" <?> " ';' missing from end of " ++ name ++ " error definition"
      return $ ErrorClass name errors

errorCase classE =
    do
      successCase classE
      <|> (failureCase classE)
      <|> (defaultSuccessCase classE)
  
defaultSuccessCase classE =
    do
      reserved "default"
      (ErrorField _ name descr) <- successCase classE
      return $ ErrorField DefaultSuccess name descr

successCase classE =
    do
      reserved "success"
      acronym <- identifier
      description <- stringLit
      symbol "," <?> " ',' missing from end of " ++ acronym ++ " definition"
      return $ ErrorField Success (classE ++ acronym) description

failureCase classE =
    do
      reserved "failure"
      acronym <- identifier
      description <- stringLit
      symbol "," <?> " ',' missing from end of " ++ acronym ++ " definition"
      return $ ErrorField Failure (classE ++ acronym) description
