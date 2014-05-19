{- 
 
   MackerelParser.hs: Mackerel parser for parsing dev file and building the 
                      syntax tree. 
                      
   Part of Mackerel: a strawman device definition DSL for Barrelfish
   
  Copyright (c) 2007, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
-}
  
-- TODO
module MackerelParser where

{- Extra testing code must be commented after testing
-}
--module Main where 
--import System 
{- End of testing code -}

import Prelude 
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( javaStyle )
import Data.Char
import Data.Maybe
import qualified Poly
import qualified Space

import Attr
                      
lexer = P.makeTokenParser (javaStyle
                           { P.reservedNames = [ 
                                                 -- "addr", 
                                                 "also",
                                                 "bytewise",
                                                 "constants",
                                                 "datatype",
                                                 "io",
                                                 "lsbfirst",
                                                 "many", 
                                                 "msbfirst",
                                                 "pci", 
                                                 "regarray", 
                                                 "register",
                                                 "regtype",
                                                 -- "space",
                                                 "stepwise",
                                                 "type",
                                                 "valuewise",
                                                 "device"
                                               ]

                           , P.reservedOpNames = ["*","/","+","-"]
                           })

whiteSpace = P.whiteSpace lexer 
reserved   = P.reserved lexer
identifier = P.identifier lexer
stringLit  = P.stringLiteral lexer
comma      = P.comma lexer
commaSep   = P.commaSep lexer
parens     = P.parens lexer
braces     = P.braces lexer
squares    = P.squares lexer
semiSep    = P.semiSep lexer
symbol     = P.symbol lexer
integer    = try ((P.lexeme lexer) binLiteral) 
             <|> try ((P.lexeme lexer) binOnes) 
             <|> P.integer lexer
commaSep1 = P.commaSep1 lexer

op = P.reservedOp lexer

data RegLoc = RegLoc String String Integer 
            | RegNoLoc
              deriving Show

data ArrayLoc = ArrayListLoc [ Integer ]
              | ArrayStepLoc Integer Integer
                deriving Show


bin_op name fun assoc = Infix (do { op name; return fun}) assoc

binDigit = oneOf "01"
binLiteral = do { _ <- char '0'
                ; _ <- oneOf "bB"
                ; digits <- many1 binDigit
                ; let n = foldl (\x d -> 2*x + (digitToInt d)) 0 digits
                ; seq n (return (fromIntegral n))
                }

binOnes = do { _ <- char '1'
             ; _ <- char 's'
             ; let n = -1
             ; seq n (return (fromIntegral n))
             }

data BitOrder = LSBFIRST | MSBFIRST | NOORDER
              deriving (Eq,Show)

data DeviceFile = DeviceFile AST [String]

data AST = Device String BitOrder [ AST ] String [ AST ]
--                 name  lsbfirst   args   desc   defn
         | Constants String String [ AST ] (Maybe Integer) SourcePos
         | ConstVal String Expr String SourcePos
         | RegField String Integer Attr AST String SourcePos
         | SpaceDecl Space.Rec
         | RegType String String AST SourcePos
         | DataType String String AST BitOrder Integer SourcePos
         | Register String Attr Bool RegLoc String AST SourcePos
         | RegArray String Attr Bool RegLoc ArrayLoc String AST SourcePos  
         --Register name   RO/RW also   io/at desc regfields 
         | Arg String String
         | NoBitFieldType
         | TypeRef String (Maybe String)
         | TypeDefn [ AST ]
         | Error String
         | Import String
           deriving Show 

devfile = do { whiteSpace
             ; imps <- many importdev
             ; dev <- device
             ; return (DeviceFile dev [i | (Import i) <- imps])
             }

importdev = do { reserved "import"
               ; i <- identifier
               ; _ <- symbol ";"
               ; return (Import i)
               }

device = do { reserved "device"
            ; name <- identifier 
            ; order <- option LSBFIRST bitorder 
            ; args <- parens (commaSep devarg)
            ; desc <- stringLit
            ; decls <- braces (many1 (devdecl args))
            ; _ <- symbol ";" <?> " ';' missing from end of " ++ name ++ " device specification"
            ;  return (Device name order args desc decls)
            }

bitorder = do{ reserved "lsbfirst" ; return LSBFIRST }
             <|> 
             do{ reserved "msbfirst"; return MSBFIRST }


devarg = do { tp <- devargtype  -- This must be a typename
            ; v  <- identifier  -- This is variable name 
            ; return (Arg tp v) 
            }

devargtype = do { reserved "addr"; return "addr" }
             <|> do { reserved "io"; return "io" }
             <|> do { reserved "pci"; return "pci" }

-- Extra added structure to support register comprehension 

devdecl args = register 
               <|> constants args 
               <|> spacedecl 
               <|> regtype  
               <|> dataType  
               <|> regarray

spacedecl = do { reserved "space"
               ; p <- getPosition
               ; i <- identifier 
               ; a <- parens (commaSep identifier)
               ; t <- spaceType
               ; d <- stringLit
               ; _ <- symbol ";"
               ; return (SpaceDecl (Space.make i a d t p))
               }

spaceType = do{ reserved "bytewise" ; return (Space.BYTEWISE 1) }
            <|> 
            do{ reserved "valuewise"; return Space.VALUEWISE }
            <|>
            do{ reserved "stepwise";
                s <- parens integer;
                return (Space.BYTEWISE s)
              }

register = do { reserved "register"
                   ; p      <- getPosition
                   ; i      <- identifier
                   ; a      <- option RW regAttr 
                   ; als    <- option False scanAlso  
                   ; loc    <- regLoc 
                   ; d      <- option i stringLit
                   ; f      <- format 
                   ; _      <- symbol ";"
                   ; return (Register i a als loc d f p)
              }

regarray = do { reserved "regarray"
                   ; p     <- getPosition
                   ; i     <- identifier
                   ; a     <- option RW regAttr 
                   ; als   <- option False scanAlso  
                   ; loc   <- regLoc 
                   ; aspec <- squares arraySpec 
                   ; d     <- option i stringLit
                   ; f     <- format 
                   ; _     <- symbol ";"
                   ; return (RegArray i a als loc aspec d f p)
              }

scanAlso = do{ reserved "also"
               ; return True
             }

regtype = do { reserved "regtype"
             ; p <- getPosition
             ; i <- identifier 
             ; d <- stringLit
             ; f <- typeDefn 
             ; _ <- symbol ";"
             ; return (RegType i d f p)
             }

format = typeDefn <|> typeLabel 

                 
typeDefn = do { j <- braces (many1 regField)
              ; return  (TypeDefn j) }

dataType = do { reserved "datatype"
              ; p     <- getPosition
              ; i     <- identifier
              ; (o,w) <- option (NOORDER,0) dataBitOrder 
              ; d     <- stringLit
              ; f     <- braces (many1 dataField)
              ; _     <- symbol ";"
              ; return (DataType i d (TypeDefn f) o w p)
              }

dataBitOrder = do { o <- bitorder
                  ; i <- parens integer
                  ; return (o,i)
                  }

dataField = do { p     <- getPosition
               ; i     <- identifier <|> symbol "_"
               ; width <- integer
               ; attr  <- option NOATTR dataAttr
               ; tpe   <- option NoBitFieldType typeLabel
               ; desc  <- option i stringLit
               ; _     <- symbol ";"
               ; return (RegField i width attr tpe desc p)
               }

typeLabel = do { reserved "type"
               ; i <- (parens typeReference)
               ; return i
               }

typeReference = do { i1 <- identifier
                   ; i2 <- option Nothing typeQualifier
                   ; return (case i2 of
                                Just qual -> TypeRef qual (Just i1)
                                Nothing   -> TypeRef i1   Nothing
                            )
                   }

typeQualifier = do { _  <- symbol "."
                   ; i <- identifier
                   ; return (Just i)
                   }

regField = do { p <- getPosition
              ; i <- identifier <|> symbol "_"
              ; width <- integer  
              ; attr <- option NOATTR fieldAttr
              ; tpe <- option NoBitFieldType typeLabel 
              ; desc <- option i stringLit
              ; _       <- symbol ";"
              ; return (RegField i width attr tpe desc p)
              }

numberFormat = do{ i <- integer
                 ; _ <- symbol "-"
                 ; j <- integer 
                 ; return (j - i + 1) 
                }

dataAttr = do { reserved "rw"; return RW }
           <|> do { reserved "mbz"; return MBZ }
           <|> do { reserved "mb1"; return MB1 }
           <|> do { reserved "rsvd"; return RSVD }
               
regAttr = do { reserved "rw" ; return RW }
          <|> do { reserved "ro"; return RO }
          <|> do { reserved "rc"; return RC }
          <|> do { reserved "rwc"; return RWC }
          <|> do { reserved "rw1c"; return RWC }
          <|> do { reserved "wo"; return WO }
          <|> do { reserved "rwzc"; return RWZC }

fieldAttr = regAttr
            <|> do { reserved "ros"; return ROS }
            <|> do { reserved "rwo"; return RWO }
            <|> do { reserved "rwcs"; return RWCS }
            <|> do { reserved "rw1cs"; return RWCS }
            <|> do { reserved "rws"; return RWS }
            <|> do { reserved "rwl"; return RWL }
            <|> do { reserved "mbz"; return MBZ }
            <|> do { reserved "mb1"; return MB1 }
            <|> do { reserved "rsvd"; return RSVD }

binarySpace = do { reserved "addr" ; return "addr" }
              <|> do { reserved "io" ; return "io" }
              <|> do { reserved "pci" ; return "pci" }

regLoc = do { reserved "noaddr"
            ; return RegNoLoc 
            }
         <|>
         do { sp <- binarySpace 
            ; ( base, offset ) <- parens binLoc 
            ; return ( RegLoc sp base offset )
            }
         <|> 
         do { sp <- identifier
            ; offset <- parens integer
            ; return (RegLoc sp "" offset)
            }
    
binLoc = do { e1 <- identifier 
            ; _  <- comma 
            ; e2 <- integer 
            ; return ( e1 , e2 ) }

arraySpec = try( arrayStepSpec )
            <|> try( arrayListSpec )
            <|> arrayContigSpec

arrayListSpec = do { l <- commaSep1 integer;
                   ; return ( if (length l) == 1 
                              then (ArrayStepLoc (head l) 0)
                              else (ArrayListLoc l) )
                   }

arrayStepSpec = do { base <- integer
                   ; _ <- symbol ";"
                   ; step <- integer
                   ; return ( ArrayStepLoc base step )
                   }

arrayContigSpec = do { base <- integer 
                     ; return ( ArrayStepLoc base 0 )
                     }

constants args = do { reserved "constants"
                    ; p <- getPosition
                    ; i <- identifier 
                    ; w <- option Nothing constWidth 
                    ; d <- stringLit
                    ; f <- braces (many1 (constField args))
                    ; _ <- symbol ";"
                    ; return (Constants i d f w p)
                    }

constWidth = do { _ <- reserved "width"
                ; i <- parens integer
                ; return (Just i)
                }

constField args = do { i <- identifier
                     ; p <- getPosition
                     ; _ <- symbol "="
                     ; e <- expr 
                     ; d <- option i stringLit
                     ; _ <- symbol ";"
                     ; return (ConstVal i e d p)
                     }


expr = buildExpressionParser expr_tab expr_factor 

expr_tab :: OperatorTable Char () Expr
expr_tab = [[bin_op "*" (enc_binop "*") AssocLeft
            , bin_op "/"(enc_binop "/") AssocLeft ]
           ,[bin_op "+" (enc_binop "+") AssocLeft
            , bin_op "-" (enc_binop "-") AssocLeft ]
           ]

data Expr = ExprConstant Integer 
          | ExprIdentifer String 
          | ExprBinOp String Expr Expr
          | ExprUnOp String Expr
          | ExprPoly [ (Integer, [ String ]) ]
            deriving (Show, Eq, Ord)

enc_binop :: String -> Expr -> Expr -> Expr
enc_binop o op1 op2 = ExprBinOp o op1 op2


expr_factor  
          = do { i <- parens (expr); return i }
              <|> do { i <- integer; return (ExprConstant i) }
              <|> do { i <- identifier;return (ExprIdentifer i) }


realbinop :: String -> Integer -> Integer -> Integer
realbinop "+" = (+)
realbinop "-" = (-)
realbinop "*" = (*)
realbinop "/" = div


canonicalise :: Expr -> Expr
canonicalise e = (ExprPoly (Poly.reduce (expr_to_poly (expr_to_dnf e))))

expr_to_multerms :: Expr -> [ Expr ]
expr_to_multerms (ExprBinOp "*" op1 op2) = 
    (expr_to_multerms op1) ++ (expr_to_multerms op2)
expr_to_multerms e = [e]

expr_to_poly :: Expr -> [ (Integer, [ String ]) ]
expr_to_poly (ExprBinOp "+" op1 op2) = 
    (expr_to_poly op1) ++ (expr_to_poly op2)
expr_to_poly e@(ExprBinOp "*" _ _) = 
    [ reduce_multerms (expr_to_multerms e) ]
expr_to_poly (ExprPoly p) = p
expr_to_poly (ExprConstant i) = [ (i, []) ]
expr_to_poly (ExprIdentifer i) = [ (1, [i]) ]

reduce_multerms :: [ Expr ] -> ( Integer, [ String ] )
reduce_multerms ml = (prod, varterms)
    where
      prod = foldr (*) 1 [  e | (ExprConstant e) <- ml ]
      varterms = [ i | (ExprIdentifer i) <- ml ]

--
-- Turn an expression into DNF
--
expr_to_dnf :: Expr -> Expr

-- Remove substraction
expr_to_dnf (ExprBinOp "-" e1 e2) =
    expr_to_dnf (ExprBinOp "+" (expr_to_dnf e1) 
                              (ExprBinOp "*" (ExprConstant (-1)) (expr_to_dnf e2)))

-- Distributivity of *,+,-: expand parentheses
expr_to_dnf (ExprBinOp "*" (ExprBinOp "+" e11 e12) e2) = 
    expr_to_dnf (ExprBinOp "+" 
                (expr_to_dnf (ExprBinOp "*" e11 e2))
                (expr_to_dnf (ExprBinOp "*" e12 e2)))

expr_to_dnf (ExprBinOp "*" e2 (ExprBinOp "+" e11 e12)) = 
    expr_to_dnf (ExprBinOp "+" 
                (expr_to_dnf (ExprBinOp "*" e2 e11))
                (expr_to_dnf (ExprBinOp "*" e2 e12)))
                   
-- Recurse
expr_to_dnf (ExprBinOp s e1 e2) = 
    let e1p = (expr_to_dnf e1) 
        e2p = (expr_to_dnf e2)
    in if (e1p /= e1) || (e2p /= e2) then expr_to_dnf (ExprBinOp s e1p e2p)
       else ExprBinOp s e1p e2p

-- Fall through
expr_to_dnf e = e

{- 
 A little code for independent testing of parser 
-}              
{--
main = do {
            args <- System.getArgs
            ; result <- parseFromFile devfile (head args) 
            ; case(result) of
               Left err -> print err
               Right xs -> print xs 
           }
              
--}             
