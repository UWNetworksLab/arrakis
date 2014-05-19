{- 
   CAbsSyntax: combinators for generating C99 syntax
   
  Part of Mackerel: a strawman device definition DSL for Barrelfish
   
  Copyright (c) 2009, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
-}  

module CAbsSyntax where

import Text.Printf
import Data.Char
import Data.List

--
-- Just enough syntax to generate files for Mackerel, etc.
--


tabstop :: String
tabstop = "    " -- How much to indent

indent_stmts :: [ Stmt ] -> [ String ] 
indent_stmts sl = [ tabstop ++ l | l <- concat [ pp_stmt s | s <- sl ] ]

-- 
-- We start with expressions
--
data Expr = NumConstant Integer         -- 123
          | HexConstant Integer         -- 0xFF
          | StringConstant String       -- "Hello!"
          | StringCat [ StrElem ]       -- "Value is " PRIu64 " bytes"
          | CharConstant Char           -- 'c'
          | Variable String             -- index
          | AddressOf Expr              -- &foo
          | DerefPtr Expr               -- *foo
          | DerefField Expr String      -- (foo)->string
          | Assignment Expr Expr        -- foo = bar
          | Unary UnOp Expr             -- -(foo)
          | Binary BinOp Expr Expr      -- (a) + (b)
          | Ternary Expr Expr Expr      -- p ? a : b
          | FieldOf Expr String         -- p.field
          | SubscriptOf Expr Expr       -- p[q]
          | Call String [ Expr ]        -- fn(a,b,c)
          | CallInd Expr [ Expr ]       -- (fn)(a,b,c)
          | SizeOf Expr                 -- sizeof(expr)
          | SizeOfT TypeSpec            -- sizeof(int)
          | Cast TypeSpec Expr          -- (foo_t)(expr)
          | PostInc Expr                -- (foo)++
          | PostDec Expr                -- (foo)++
          | Parens Expr                 -- (e)
            deriving (Show, Eq)

pp_expr :: Expr -> String
pp_expr (NumConstant i) = printf "%d" i
pp_expr (HexConstant i) = printf "0x%x" i
pp_expr (StringConstant s) = "\"" ++ (concat $ map (\x -> showLitChar x "") s) ++ "\""
pp_expr (StringCat l) = concat $ intersperse " " $ map pp_strelem l
pp_expr (CharConstant c) = "'" ++ showLitChar c "'"
pp_expr (Variable s) = s
pp_expr (AddressOf e) = "&" ++ (pp_par_expr e)
pp_expr (DerefPtr e) = "*" ++ (pp_par_expr e)
pp_expr (DerefField e s) = (pp_par_expr e) ++ "->" ++ s
pp_expr (Assignment e1 e2) = (pp_expr e1) ++ " = " ++ (pp_par_expr e2)
pp_expr (Unary o e) = (pp_unop o) ++ (pp_par_expr e)
pp_expr (Binary o e1 e2) 
    = (pp_par_expr e1) ++" " ++ (pp_binop o) ++ " "++(pp_par_expr e2)
pp_expr (Ternary e1 e2 e3) 
    = (pp_par_expr e1) ++ " ? " ++ (pp_par_expr e2) ++ " : " ++ (pp_par_expr e3)
pp_expr (FieldOf e s) = (pp_par_expr e) ++ "." ++ s
pp_expr (SubscriptOf e1 e2) = (pp_par_expr e1) ++ "[" ++ (pp_expr e2) ++ "]"
pp_expr (Call f al) 
    = f ++ "(" ++ (concat $ intersperse ", " [ pp_expr e | e <- al ]) ++ ")"
pp_expr (CallInd f al) 
    = "(" ++ (pp_expr f) ++ ")(" ++ (concat $ intersperse ", " [ pp_expr e | e <- al ]) ++ ")"
pp_expr (SizeOf e) = "sizeof(" ++ (pp_expr e) ++ ")"
pp_expr (SizeOfT t) = "sizeof(" ++ (pp_typespec t "") ++ ")"
pp_expr (Cast s e) = "(" ++ (pp_typespec s "") ++ ")(" ++ (pp_expr e) ++ ")"
pp_expr (PostInc e) = (pp_par_expr e) ++ "++"
pp_expr (PostDec e) = (pp_par_expr e) ++ "--"
pp_expr (Parens e) = "(" ++ (pp_expr e) ++ ")"

pp_par_expr :: Expr -> String
pp_par_expr (Variable s) = s
pp_par_expr e@(NumConstant _) = pp_expr e
pp_par_expr e@(HexConstant _) = pp_expr e
pp_par_expr c@(Call _ _) = pp_expr c
pp_par_expr e = "(" ++ (pp_expr e) ++ ")"

data StrElem = QStr String
             | NStr String
               deriving (Show, Eq)

pp_strelem :: StrElem -> String
pp_strelem (QStr s) = pp_expr (StringConstant s)
pp_strelem (NStr s) = s

--
-- Binary operators
--
data BinOp = Plus 
           | Minus 
           | Times 
           | Divide 
           | Modulo 
           | Equals
           | NotEquals
           | GreaterThan
           | LessThan 
           | GreaterThanEq
           | LessThanEq
           | BitwiseAnd
           | BitwiseOr
           | BitwiseXor
           | And
           | Or 
           | LeftShift
           | RightShift
             deriving (Show, Eq)

pp_binop :: BinOp -> String
pp_binop Plus = "+"
pp_binop Minus = "-"
pp_binop Times = "*"
pp_binop Divide = "/"
pp_binop Modulo = "%"
pp_binop Equals= "=="
pp_binop NotEquals= "!="
pp_binop GreaterThan= ">"
pp_binop LessThan = "<"
pp_binop GreaterThanEq= ">="
pp_binop LessThanEq= "<="
pp_binop BitwiseAnd= "&"
pp_binop BitwiseOr= "|"
pp_binop BitwiseXor= "^"
pp_binop And= "&&"
pp_binop Or = "||"
pp_binop LeftShift= "<<"
pp_binop RightShift= ">>"

--
-- Unary operators
--
data UnOp = Not | Negate | BitwiseNot 
             deriving (Show, Eq)

pp_unop :: UnOp -> String
pp_unop Not = "!"
pp_unop Negate = "-"
pp_unop BitwiseNot = "~"

--
-- Parameters to function definitions
--
data Param = Param TypeSpec String
             deriving (Show, Eq)

pp_param :: Param -> String
pp_param (Param t s) = (pp_typespec t s)

--
-- Members of an enumeration definition
--
data EnumItem = EnumItem String (Maybe Expr)
             deriving (Show, Eq)

pp_enumitem :: EnumItem -> String
pp_enumitem (EnumItem s (Just e)) = s ++ " = " ++( pp_expr e)
pp_enumitem (EnumItem s Nothing) = s


--
-- Include directives
--
data IncludePath = Standard | Local
                   deriving (Show, Eq)
pp_include :: IncludePath -> String -> String
pp_include Standard f = printf "#include <%s>" f
pp_include Local f = printf "#include \"%s\"" f

--
-- Scope of a function or variable
--
data ScopeSpec = Extern | Static | NoScope
                 deriving (Show, Eq)

pp_scopespec :: ScopeSpec -> String
pp_scopespec Extern = "extern "
pp_scopespec Static = "static "
pp_scopespec NoScope = ""

--
-- Constancy
--
data ConstSpec = Const | NonConst
                 deriving (Show, Eq)
pp_constspec :: ConstSpec -> String
pp_constspec Const = "const "
pp_constspec NonConst = ""

--
-- A Unit is a chunk of source file, i.e. top-level syntactic constructs. 
--
-- Note that we treat static inlines as their own construct.  It's easier. 
--
data Unit = Comment String
          | MultiComment [ String ]
          | TypeDef TypeSpec String
          | FunctionDef ScopeSpec TypeSpec String [ Param ] [ Stmt ]
          | StaticInline TypeSpec String [ Param ] [ Stmt ]
          | StructDecl String [ Param ]
          | UnionDecl String [ Param ]
          | EnumDecl String [ EnumItem ]
--          | FunctionDecl TypeSpec String
          | GVarDecl ScopeSpec ConstSpec TypeSpec String (Maybe Expr)
          | Define String [ String ] String
          | Undef String
          | IfDef String [ Unit ] [ Unit ]
          | IfNDef String [ Unit ] [ Unit ]
          | HashIf String [ Unit ] [ Unit ]
          | NoOp
          | Blank
          | Include IncludePath String
             deriving (Show, Eq)

pp_unit :: Unit -> [ String ] 
pp_unit (Comment s) = [ "// " ++ s ]
pp_unit (MultiComment sl) = ["/*"] ++ [ " * " ++ s | s <- sl ] ++ [ " */"] 
pp_unit (TypeDef ts s) = [ "typedef " ++ (pp_typespec ts s) ++ ";" ]
pp_unit (FunctionDef sc ts n pl body) =
    [ (pp_scopespec sc) ++ " " ++ (pp_fnhead ts n pl) ] ++ (pp_fnbody body)
pp_unit (StaticInline ts n pl body) = 
    [ hd ++ " __attribute__ ((always_inline));",
      hd ] ++ (pp_fnbody body)
    where 
      hd = "static inline " ++ (pp_fnhead ts n pl)
pp_unit (StructDecl s pl) = 
    [ printf "struct %s {" s ] ++ [ tabstop ++ (pp_param p) ++ ";" 
                                        | p <- pl ] ++ ["};"]
pp_unit (UnionDecl s pl) = 
    [ printf "union %s {" s ] ++ [ tabstop ++ (pp_param p) | p <- pl ] ++ ["}"]
pp_unit (EnumDecl s el) = 
    [ printf "enum %s {" s ] 
    ++ 
    (comma_sep_lines [ tabstop ++ (pp_enumitem e) | e <- el ])
    ++ 
    ["};"]
-- pp_unit (FunctionDecl sc ts n pl) = 
--     [ (pp_scopespec sc) ++ " " ++ (pp_fnhead ts n pl) ++ ";" ]
pp_unit (GVarDecl sc cs ts s Nothing) = 
    [ printf "%s%s%s;" (pp_scopespec sc) (pp_constspec cs) (pp_typespec ts s)]
pp_unit (GVarDecl sc cs ts s (Just e)) = 
    [ printf "%s%s%s = %s;" 
                 (pp_scopespec sc) 
                 (pp_constspec cs) 
                 (pp_typespec ts s) 
                 (pp_expr e) ]
pp_unit (Define n [] v) = [ printf "#define %s %s"  n v ]
pp_unit (Define n al v) 
    = [ printf "#define %s(%s) %s" n (concat $ intersperse "," al) v ]
pp_unit (Undef s) = [ "#undef " ++ s ]
pp_unit (IfDef s l r) = pp_cppcond "ifdef" s l r 
pp_unit (IfNDef s l r) = pp_cppcond "ifndef" s l r 
pp_unit (HashIf s l r) = pp_cppcond "if" s l r 
pp_unit NoOp = []
pp_unit Blank = [""]
pp_unit (Include s p) = [ pp_include s p ]

comma_sep_lines :: [String] -> [String] 
comma_sep_lines [] = []
comma_sep_lines [s] = [s]
comma_sep_lines (s:sl) = (s ++ ","):(comma_sep_lines sl)

pp_cppcond :: String -> String -> [ Unit ] -> [ Unit ] -> [ String ]
pp_cppcond t e l r = 
    [ "#" ++ t ++ " " ++ e ] 
    ++ 
    (concat [ pp_unit u | u <- l ])
    ++ 
    (if r == [] then [] else "#else":concat [ pp_unit u | u <- r ])
    ++ 
    [ "#endif // " ++ e ]
    

pp_fnbody :: [ Stmt ] -> [ String ] 
pp_fnbody body = [ "{" ] ++ (indent_stmts body) ++ [ "}", ""]

pp_fnhead :: TypeSpec -> String -> [ Param ] -> String
pp_fnhead ts n pl = 
    (pp_typespec ts n) ++ "(" ++ parlist ++ ")" 
    where 
      parlist = concat $ intersperse ", " [ pp_param p | p <- pl ]

-- 
-- Branches of a case statement: note that they fall through
--
data Case = Case Expr [ Stmt ]
            deriving (Show, Eq)
                   
pp_case :: Case -> [ String ]
pp_case (Case e s) 
    = [ "case " ++ (pp_expr e) ++ ":" ] ++ (indent_stmts s)

--
-- Statements.
--
data Stmt = Return Expr 
          | ReturnVoid
          | Block [ Stmt ]
          | StmtList [ Stmt ]
          | Ex Expr
          | If Expr [ Stmt ] [ Stmt ]
          | DoWhile  Expr [ Stmt ]
          | While Expr [ Stmt ] 
          | For Expr Expr Expr [ Stmt ]
          | Switch Expr [ Case ] [ Stmt ]  -- last list is default clause
          | Break
          | Continue
          | Label String
          | Goto String
          | VarDecl ScopeSpec ConstSpec TypeSpec String (Maybe Expr)
          | SComment String
            deriving (Show, Eq)

pp_stmt :: Stmt -> [ String ]
pp_stmt (Return e) = [ "return(" ++ (pp_expr e) ++ ");" ]
pp_stmt ReturnVoid = [ "return;" ]
pp_stmt (Block sl) = [ "{" ] ++ (indent_stmts sl) ++ ["}"]
pp_stmt (StmtList sl) = concat $ map pp_stmt sl
pp_stmt (Ex e) = [ (pp_expr e) ++ ";" ]
pp_stmt (If e sl []) = 
    [ "if (" ++ (pp_expr e) ++ ") {" ] ++ (indent_stmts sl) ++ ["}"]
pp_stmt (If e sl1 sl2) 
    = [ "if (" ++ (pp_expr e) ++ ") {" ] 
      ++ (indent_stmts sl1) 
      ++ ["} else {"] 
      ++ (indent_stmts sl2) ++ [ "}"]
pp_stmt (DoWhile e sl) 
    = [ "do {" ] ++ (indent_stmts sl) ++ [ "} while (" ++ (pp_expr e) ++ ");" ]
pp_stmt (While e sl) 
    = [ "while (" ++ (pp_expr e) ++ ") {" ] 
      ++ (indent_stmts sl) ++ ["}"]
pp_stmt (For e1 e2 e3 sl) 
    = ( [ "for( " ++ (pp_expr e1) ++ "; " 
          ++  (pp_expr e2) ++ "; " 
          ++ (pp_expr e3) ++ ") {" 
        ]
        ++ (indent_stmts sl) 
        ++ ["}"]
      )
pp_stmt (Switch e cl sl) 
    = ( [ "switch (" ++ (pp_expr e) ++ ") {" ] 
        ++ concat [ pp_case c | c <- cl ] 
        ++ [ "default:" ] 
        ++ (indent_stmts sl)
        ++ [ "}" ]
      )
pp_stmt Break = [ "break;" ]
pp_stmt Continue = [ "continue;" ]
pp_stmt (Label s) = [ s ++ ":" ]
pp_stmt (Goto s) = [ "goto " ++ s ++ ";" ]
pp_stmt (VarDecl sc cs ts s Nothing) = 
    [ printf "%s%s%s;" (pp_scopespec sc) (pp_constspec cs) (pp_typespec ts s)]
pp_stmt (VarDecl sc cs ts s (Just e)) = 
    [ printf "%s%s%s = %s;" 
                 (pp_scopespec sc) 
                 (pp_constspec cs) 
                 (pp_typespec ts s) 
                 (pp_expr e) ]
pp_stmt (SComment s) = [ "// " ++ s ]

--
-- Type specifiers
--
data TypeSpec = Void
              | Struct String
              | Union String
              | Enum String 
              | Ptr TypeSpec
              | Array Integer TypeSpec
              | TypeName String
              | Function ScopeSpec TypeSpec [ Param ]
                deriving (Show, Eq)

pp_typespec :: TypeSpec -> String -> String
pp_typespec Void n = "void " ++ n
pp_typespec (Struct s) n = printf "struct %s %s" s n
pp_typespec (Union s) n = printf "union %s %s" s n
pp_typespec (Enum s) n = printf "enum %s %s" s n
pp_typespec (Ptr t) n = pp_typespec t ("*" ++n)
pp_typespec (Array 0 t) n = pp_typespec t (n++"[]")
pp_typespec (Array i t) n = pp_typespec t $ printf "%s[%d]" n i
pp_typespec (TypeName s) n = printf "%s %s" s n
pp_typespec (Function sc ts pl) n 
    = (pp_scopespec sc) ++ " " ++ (pp_fnhead ts n pl)
