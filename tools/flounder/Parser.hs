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

import Syntax

import Prelude 
import Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Pos
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( javaStyle )
import Data.Char
import Numeric
import Data.List
import Text.Printf

parse_intf predefDecls filename = parseFromFile (intffile predefDecls) filename
parse_include predefDecls filename = parseFromFile (includefile predefDecls) filename

lexer = P.makeTokenParser (javaStyle
                           { P.reservedNames = [ "interface", 
                                                 "message",
                                                 "rpc",
                                                 "in",
                                                 "out"
                                               ]
                           , P.reservedOpNames = ["*","/","+","-"]
                           , P.commentStart = "/*"
                           , P.commentEnd = "*/"
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
natural    = P.natural lexer

builtinTypes = map show [UInt8 ..] ++ ["int"] -- int is legacy -AB

-- identifyBuiltin :: [(String, Declaration)] -> String -> TypeRef
identifyBuiltin typeDcls typeName = 
    do {
      if typeName `elem` builtinTypes then
          return $ Builtin $ (read typeName::TypeBuiltin)
      else 
          case typeName `lookup` typeDcls of
            Just (Typedef (TAliasT new orig)) -> return $ TypeAlias new orig
            Just _ -> return $ TypeVar typeName 
            Nothing -> 
                do {
                ; pos <- getPosition
                -- This is ugly, I agree:
                ; return $ error ("Use of undeclared type '" ++ typeName ++ "' in "
                                  ++ show (sourceName pos) ++ " at l. "
                                  ++ show (sourceLine pos) ++ " col. "
                                  ++ show (sourceColumn pos))
                }
    }

intffile predefDecls = do { whiteSpace
             ; i <- iface predefDecls
             ; return i
              }

includefile predefDecls = do { whiteSpace
             ; typeDecls <- typeDeclaration predefDecls
             ; return typeDecls
              }

iface predefDecls = do { reserved "interface"
           ; name <- identifier 
           ; descr <- option name stringLit
           ; decls <- braces $ do {
                               ; typeDecls <- typeDeclaration predefDecls
                               ; msgDecls <- many1 $ mesg typeDecls
                               ; return ((map snd typeDecls) ++ msgDecls)
                               }
           ; symbol ";" <?> " ';' missing from end of " ++ name ++ " interface specification"
           ;  return (Interface name (Just descr) decls)
           }


typeDeclaration typeDcls = do {
                           ; decl <- try (do {
                                           ; x <- transparentAlias 
                                           ; return $ Just x
                                           })
                                     <|> try (do {
                                               ; x <- typedefinition typeDcls
                                               ; return $ Just x
                                               })
                                    <|> return Nothing
                           ; case decl of 
                               Nothing -> return typeDcls
                               Just x -> typeDeclaration (x : typeDcls)
                           }       

mesg typeDcls = do { bckArgs <- many backendParams
                   ; def <- msg typeDcls bckArgs <|> rpc typeDcls bckArgs
                   ; return $ Messagedef def
                   }

msg typeDcls bckArgs = do { t <- msgtype
                          ; i <- identifier
                          ; a <- parens $ commaSep (marg typeDcls)
                          ; symbol ";"
                          ; return $ Message t i a bckArgs
                          }

rpc typeDcls bckArgs= do { _ <- rpctype
                         ; i <- identifier
                         ; a <- parens $ commaSep (rpcArg typeDcls)
                         ; symbol ";"
                         ; return $ RPC i a bckArgs
                         }

rpctype = do { reserved "rpc"
             ; return () }

rpcArg typeDcls = do { reserved "in"
                       ; Arg b n <- marg typeDcls
                       ; return $ RPCArgIn b n
                       }
                       <|> do { reserved "out"
                       ; Arg b n <- marg typeDcls
                       ; return $ RPCArgOut b n
                       }

backendParams = do { char '@'
                   ; i <- identifier
                   ; p <- parens $ commaSep backendParam
                   ; return (i, p)
                   }

backendParam = do { name <- identifier
                  ; symbol "="
                  ;     do { num <- natural ; return $ (name, BackendInt num) }
                    <|> do { arg <- identifier ; return $ (name, BackendMsgArg arg) }
                  }

msgtype = do { reserved "message"; return MMessage }
          <|> do  { reserved "call"; return MCall }
          <|> do  { reserved "response"; return MResponse }

marg typeDcls = try (marg_array typeDcls)
               <|> (marg_simple typeDcls)

marg_simple typeDcls = do { t <- identifier
                          ; n <- identifier
                          ; b <- identifyBuiltin typeDcls t
                          ; return (Arg b (Name n))
                          }

marg_array typeDcls  = do { t <- identifier
                          ; n <- identifier
                          ; symbol "["
                          ; l <- identifier
                          ; symbol "]"
                          ; bType <- identifyBuiltin typeDcls t
                          ; return (Arg bType (DynamicArray n l))
                          }

transparentAlias = do { whiteSpace 
                      ; reserved "alias"
                      ; newType <- identifier
                      ; originType <- identifier
                      ; symbol ";"
                      ; return (newType, Typedef $ TAliasT newType 
                                                           (read originType::TypeBuiltin))
                      }

typedefinition typeDcls = do { whiteSpace
                             ; reserved "typedef"
                             ; (name, typeDef) <- typedef_body typeDcls
                             ; symbol ";"
                             ; return (name, Typedef typeDef)
                             }

typedef_body typeDcls = try (struct_typedef typeDcls)
                        <|> try (array_typedef typeDcls)
                        <|> try enum_typedef
                        <|> (alias_typedef typeDcls)
 
struct_typedef typeDcls = do { reserved "struct"
                             ; f <- braces $ many1 (struct_field typeDcls)
                             ; i <- identifier
                             ; return (i, (TStruct i f))
                             }

struct_field typeDcls = do { t <- identifier
                           ; i <- identifier 
                           ; symbol ";"
                           ; b <- identifyBuiltin typeDcls t
                           ; return (TStructField b i)
                           }

array_typedef typeDcls = do { t <- identifier
                            ; i <- identifier
                            ; symbol "["
                            ; sz <- integer
                            ; symbol "]"
                            ; b <- identifyBuiltin typeDcls t
                            ; return (i, (TArray b i sz))
                            }

enum_typedef = do { reserved "enum"
                  ; v <- braces $ commaSep1 identifier
                  ; i <- identifier
                  ; return (i, (TEnum i v))
                  }

alias_typedef typeDcls = do { t <- identifier
                            ; i <- identifier
                            ; b <- identifyBuiltin typeDcls t
                            ; return (i, (TAlias i b))
                            }

integer = P.integer lexer
