{-

   Parser.hs: Parser for the pleco interface definition language

   Part of Pleco: a trace definition DSL for Barrelfish

  Copyright (c) 2013, ETH Zurich.

  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
-}

module Parser where

import Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Pos
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( javaStyle )
import Data.Char
import Numeric
import Data.List
import Text.Printf

parse filename = parseFromFile traceFile filename

lexer = P.makeTokenParser (javaStyle
                           { P.reservedNames = [ "subsystem",
                                                 "event"
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

data EventField = EventField String String
data SubsystemClass = SubsystemClass String [ EventField ]

traceFile =
    do
      whiteSpace
      subsystems <- many1 subsystemClass
      return subsystems


subsystemClass =
    do
      reserved "subsystem"
      name <- identifier
      events <- braces $ many1 eventCase
      symbol ";" <?> " ';' missing from end of " ++ name ++ " subsystem definition"
      return $ SubsystemClass name events


eventCase =
    do
      reserved "event"
      acronym <- identifier
      description <- stringLit
      symbol "," <?> " ',' missing from end of " ++ acronym ++ " definition"
      return $ EventField acronym description
