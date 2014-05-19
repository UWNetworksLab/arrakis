{- 
   CSyntax: functions for rendering C syntactic structures.  
   
  Part of Mackerel: a strawman device definition DSL for Barrelfish
   
  Copyright (c) 2007, 2008, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
-}  

module CSyntax where

import Data.List
import Text.Printf
import MackerelParser

infixr 9 >:
(>:) :: String -> [String] -> [String]
s >: [] = [s]
s >: (x:xs) = (s ++ " " ++ x) : xs

infixr 9 <:
(<:) :: [String] -> String -> [String]
[] <: s = [s]
(h:t) <: s = let (x:xs) = reverse (h:t) in
             reverse ((x ++ " " ++ s):xs )

header_file name body = 
    let sym = "__" ++ name ++ "_H" 
    in unlines [ "#ifndef " ++ sym,
                 "#define " ++ sym,
                 "",
                 body,
                 "",
                 "#endif // " ++ sym
                 ]

undef :: String -> String
undef n = "#undef " ++ n

include :: String -> String
include f = "#include <" ++ f ++ ".h>"

include_local :: String -> String
include_local f = "#include \"" ++ f ++ ".h\""

block :: [String] -> [String]
block lines = 
    ["{"] ++ (indent lines) ++ ["}"]

typedef :: String -> String -> String
typedef name typestr = "typedef " ++ typestr ++ " " ++ name ++ ";" 

packed_typedef :: String -> String -> String
packed_typedef name typestr = 
    "#if defined(__GNUC__) && (defined(__amd64) || defined(__i386)) && (__GNUC__ <= 4 && __GNUC_MINOR__ <= 3)\n#pragma pack(push,1)\n#endif\ntypedef " ++ typestr ++ " " ++ name ++ ";\n#if defined(__GNUC__) && (defined(__amd64) || defined(__i386)) && (__GNUC__ <= 4 && __GNUC_MINOR__ <= 3)\n#pragma pack(pop)\n#endif\n" 

constint :: String -> Integer -> String
constint name val = printf "static const int %s = 0x%0x;" name val

struct :: String -> [ String ] -> [ String ]
struct name fields =  structunion "struct" name fields 

struct_field n v = printf "%s\t%s;" n v

union :: String -> [ String ] -> [ String ]
union name fields =  structunion "union" name fields

union_field n v = struct_field n v

structunion :: String -> String -> [ String ] -> [ String ]
structunion su name fields = 
    (su ++ " " ++ name) >: (block fields) 

bitfields name fields = 
    ("struct " ++ name) >: (block fields) <: "__attribute__ ((packed))" 

bitfield n w t = printf "%s\t%s\t:%d;" t n w 

assertsize t s = printf "STATIC_ASSERT_SIZEOF(%s, sizeof(%s));\n" t s


enum name vals = 
    let tname = name -- ++ "_t"   
    in
      unlines ( ((printf "typedef enum %s" tname) 
                >: block [ printf "%s = %s," n v | (n, v) <- vals] )
                <: (printf "%s;" tname) )


function_proto attr rtype name args = 
    printf "%s %s %s( %s )" attr rtype name (func_args args)

function :: String -> String -> String -> [(String,String)] -> [String] -> String 
function attr rtype name args body =
    let proto = function_proto attr rtype name args 
    in
    unlines ( [ proto ++ " __attribute__ ((always_inline));",
                proto ]
             ++ (block body) )
 
inline :: String -> String -> [(String,String)] -> [String] -> String 
inline rtype name args body =
    function "static inline" rtype name args body
             
func_args:: [(String,String)] -> String
func_args alist = 
    concat (intersperse ", " [ (n ++ " " ++ v) | (n,v) <- alist ])


multi_comment str = 
    printf "\n/*\n%s */" (unlines [" * " ++ line | line <- lines str])

comment s = "// " ++ s

indent :: [String] -> [String]
indent l = [ "    " ++ line | line <- l ]

switch :: String -> [ (String, String) ] -> String -> [String]
switch disc alts dflt = 
    (printf "switch (%s)" disc) 
    >: block ( concat [ [ printf "case %s:" a, printf "%s" l ]
                            | (a,l) <- alts ]
               ++ [ "default:", printf "%s" dflt ] )

forloop :: String -> String -> String -> [String] -> [String]
forloop init iter term body = 
    (printf "for( %s; %s; %s )" init iter term)
    >: block body

--
-- Accumulating strings to print: much of the debugging code we
-- generate consists of successive calls to snprintf.
--

snprintf :: String -> [ String ]
snprintf s = snlike "snprintf" s

snlike fn arg = [ "_avail = (r > sz) ? 0 : sz-r;",
                  printf "_rc = %s(s+r, _avail, %s);" fn arg,
                  "if ( _rc > 0 && _rc < _avail) { r += _rc; }"
                ]
                  
snputs :: String -> [ String ]
snputs s = snprintf (printf "\"%%s\", %s" s)

snputsq :: String -> [ String ]
snputsq s = snprintf (printf "\"%%s\", \"%s\"" s)

--
-- Expressions
--
expression :: Expr -> String
expression (ExprConstant i) 
    = printf "(0x%0x)" i 
expression (ExprIdentifer i) 
    = "(" ++ i ++ ")"
expression (ExprBinOp op v1 v2)
    = printf "(%s %s %s)" (expression v1) op (expression v2)
expression (ExprUnOp op v) 
    = printf "(%s %s)" op (expression v)
expression (ExprPoly []) = "0"
expression (ExprPoly p) = concat (intersperse "+" (map multerm p))

multerm :: (Integer, [String] ) -> String
multerm (i, []) = printf "0x%x" i
multerm (1, sl) = "(" ++ (concat (intersperse "*" sl)) ++ ")"
multerm (i, sl) = printf "(%d%s)" i l
    where l = concat [ ("*" ++ e) | e <- sl ]

--
-- Expressions relative to the device pointer
--
dexpr (ExprConstant i) = printf "0x%0x" i 
dexpr (ExprIdentifer i) = "(dev->" ++ i ++ ")"
dexpr (ExprBinOp op v1 v2) = printf "%s %s %s" (dexpr v1) op (dexpr v2)
dexpr (ExprUnOp op v) = printf "(%s %s)" op (dexpr v)

