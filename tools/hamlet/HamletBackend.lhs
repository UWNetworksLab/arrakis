%include polycode.fmt

%if false
  Error: DSL for error definition
   
  Copyright (c) 2009 ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%if false

>  {-# OPTIONS_GHC -XBangPatterns #-}

%endif

%if false

> module HamletBackend where

> import Data.Maybe
> import Data.List
> import Data.Char
> import qualified Data.Map as Map

> import Debug.Trace

> import Semantics
> import Constructs
> import PureExpressions
> import Expressions

> import Constructs.Conditionals
> import Constructs.References
> import Constructs.Functions
> import Constructs.Enumerations
> import Constructs.Structures
> import Constructs.Unions

> import Libc.Assert

> import Libbarrelfish.HasDescendants
> import Libbarrelfish.MemToPhys

> import HamletAst

%endif


> strict x = x


> boolT :: TypeExpr
> boolT = typedef uint64T "bool"
>
> false :: PureExpr
> false = uint64 $ 0
>
> true :: PureExpr
> true = uint64 $ 1

> objtypeT :: TypeExpr
> objtypeT = typedef uint64T "enum objtype"

> ctePtrT :: TypeExpr
> ctePtrT = typedef uint64T "struct cte *"

> lvaddrT :: TypeExpr
> lvaddrT = typedef uint64T "lvaddr_t"

> genpaddrT :: TypeExpr
> genpaddrT = typedef uint64T "genpaddr_t"

> gensizeT :: TypeExpr
> gensizeT = typedef uint64T "gensize_t"

> getCap cap_pp = (readRef cap_pp >>= readRef)

> lower = map toLower

\section{|Objtype| Enumeration}

> ofObjTypeEnum :: CapName -> PureExpr
> ofObjTypeEnum (CapName x) =  CLRef Global uint64T (Provided ("ObjType_" ++ x))


> mkObjTypeEnum :: [Capability] -> Enumeration
> mkObjTypeEnum caps = enums `seq` objTypeNum `seq`
>                      objTypeNum : enums 
>     where (n,enums) = foldl' mkEnumField (0,[]) caps
>           mkEnumField (!i,!acc) !cap = i' `seq` 
>                                        (i',acc' )
>               where CapName capName = name cap 
>                     strName = "ObjType_" ++ capName
>                     acc' = strName `seq` (strName, i) : acc
>                     i' = i + 1
>           objTypeNum = ("ObjType_Num",n)


\section{Capabality Structure}

> capRightsT :: TypeExpr
> capRightsT = typedef uint8T "CapRights"


> capNameOf :: Capability -> String
> capNameOf cap = capName
>     where CapName capName = name cap

> mkCapsStruct :: Capabilities -> TFieldList
> mkCapsStruct caps = strict
>     [("type", objtypeT),
>      ("rights", capRightsT),
>      ("u", capUnionT)]
>     where capUnionT = unionST "capability_u" ((map (\cap -> (lower $ capNameOf cap, mkCapStructT cap))
>                                                  (capabilities caps)) )
>                      -- XXX: Why do I need to define types here when they are already
>                      -- defined somewhere else? Also as hamlet doesn't handle uintptr_t,
>                      --  lvaddr is defined as uint64 although that is not always the case.
>                      -- Using a type not defined here will generate "type"*
>                      -- which is obviously broken. -Akhi
>           mkCapStructT cap = structST (capNameOf cap) (map mkCapField (fields cap))
>           mkCapField (CapField typ (NameField name)) = (name, toFofType typ)
>           toFofType UInt8 = uint8T
>           toFofType UInt16 = uint16T
>           toFofType UInt32 = uint32T
>           toFofType UInt64 = uint64T
>           toFofType Int = int32T
>           toFofType GenPAddr = typedef uint64T "genpaddr_t"
>           toFofType GenSize = typedef uint64T "gensize_t"
>           toFofType LPAddr   = typedef uint64T "lpaddr_t"
>           toFofType GenVAddr = typedef uint64T "genvaddr_t"
>           toFofType LVAddr   = typedef uint64T "lvaddr_t"
>           toFofType CAddr = typedef uint32T "capaddr_t"
>           toFofType (Pointer s) = typedef uint64T (s ++ "*")
>           toFofType CapRights = capRightsT
>           toFofType CoreId = typedef uint8T "coreid_t"

> capsStructT :: Capabilities -> TypeExpr
> capsStructT cap = structST  "capability" (mkCapsStruct cap)

\section{Get address and size}

\subsection{Convert common expressions}

Generate an expression for the size of a value expressed in "bits"
i.e. {\tt (((gensize\_t)1) << bits)}

> mkSize :: PureExpr -> PureExpr
> mkSize bits = (cast gensizeT (uint64 1)) .<<. bits

> readCapAttr :: PureExpr -> String -> FoFCode PureExpr
> readCapAttr cap attr = getCap cap >>= (\c -> readStruct c attr)

Try and look up a name in a definitions and failing that generated code to look
it up in the fields of a cap.

> lookupName :: [(String, Int)] -> PureExpr -> String -> String -> FoFCode PureExpr
> lookupName defs cap capType name =
>     case (name `lookup` defs) of
>       Nothing  -> do
>                     capU <- readCapAttr cap "u"
>                     capCStruct <- readUnion capU $ lower $ capType
>                     readStruct capCStruct name
>       Just val -> return $ uint64 $ toInteger val

Generate code to calculate the result of an expression.

> exprCode :: [(String, Int)] -> PureExpr -> String -> Expr -> FoFCode PureExpr
> exprCode defs cap capType (NameExpr n) = lookupName defs cap capType n
> exprCode defs cap capType (AddExpr left right) =
>     do
>       lval <- lookupName defs cap capType left
>       rval <- lookupName defs cap capType right
>       return (lval .+. rval)

Generate code to calculate the "address" property of a cap.

> addressExprCode :: [(String, Int)] -> PureExpr -> String -> AddressExpr -> FoFCode PureExpr
> addressExprCode defs cap capType (MemToPhysOp expr) =
>     do
>       lval <- exprCode defs cap capType expr
>       mem_to_phys $ cast lvaddrT lval
> addressExprCode defs cap capType (AddressExpr expr) =
>       exprCode defs cap capType expr

Generate code to calculate the "size" property of a cap.

> sizeExprCode :: [(String, Int)] -> PureExpr -> String -> SizeExpr -> FoFCode PureExpr
> sizeExprCode defs cap capType (ZeroSize) = return $ uint64 0
> sizeExprCode defs cap capType (SizeExpr expr) = exprCode defs cap capType expr
> sizeExprCode defs cap capType (SizeBitsExpr expr) =
>     do
>       bitsExpr <- exprCode defs cap capType expr
>       return $ mkSize $ bitsExpr

\subsection{get\_address}

> get_address :: Capabilities -> FoFCode PureExpr
> get_address caps =
>     def [] "get_address"
>         (mkGetPropSwitch caps mkGetAddr)
>         genpaddrT
>         [(ptrT $ ptrT $ capsStructT caps, Just "cap")]
>     where
>         nullptr = cast genpaddrT $ uint64 0
>         mkGetAddr :: [(String, Int)] -> Capability -> PureExpr -> FoFCode PureExpr
>         mkGetAddr defines capType cap =
>             case rangeExpr capType of
>               Just expr -> mkGetAddrExpr defines capType cap $ fst expr
>               Nothing   -> returnc nullptr
>         mkGetAddrExpr defines capType cap expr = 
>           do
>             res <- addressExprCode defines cap capName expr
>             returnc res
>           where
>             capName = case name capType of (CapName s) -> s

\subsection{get\_size}

> get_size :: Capabilities -> FoFCode PureExpr
> get_size caps =
>     def [] "get_size"
>         (mkGetPropSwitch caps mkGetSize)
>         gensizeT
>         [(ptrT $ ptrT $ capsStructT caps, Just "cap")]
>     where
>         mkGetSize :: [(String, Int)] -> Capability -> PureExpr -> FoFCode PureExpr
>         mkGetSize defines capType cap =
>             case rangeExpr capType of
>               Just expr -> mkGetSizeExpr defines capType cap $ snd expr
>               Nothing   -> returnc $ cast gensizeT $ uint64 0
>         mkGetSizeExpr defines capType cap expr = 
>           do
>             res <- sizeExprCode defines cap capName expr
>             returnc res
>           where
>             capName = case name capType of (CapName s) -> s

\subsection{Generate switch on enum objtype}

\verb|mkGetPropSwitch| generates a switch statement that switches on the
objtype of the cap argument and uses mkGetProp to generate code for the
different cases.

> mkGetPropSwitch :: Capabilities
>                    -> ([(String, Int)] -> Capability -> PureExpr -> FoFCode PureExpr)
>                    -> ([PureExpr] -> FoFCode PureExpr)
> mkGetPropSwitch caps mkGetProp = get_prop_int
>       where
>         get_prop_int :: [PureExpr] -> FoFCode PureExpr
>         get_prop_int (cap : []) = 
>           do
>             let cases = map (mkGetPropCase cap) (capabilities caps)
>             capTypeInt <- readCapAttr cap "type"
>             switch capTypeInt cases ((assert false) >> (returnc $ uint64 0))
>         mkGetPropCase cap capType =
>             ((ofObjTypeEnum $ name capType), mkGetProp defineList capType cap)
>         defineList = mkDefineList $ defines caps

\section{Compare}

$get\_type\_root$ gets a number indicating which tree root of the type forest a
given type belongs to.

> get_type_root :: Capabilities -> FoFCode PureExpr
> get_type_root caps =
>     def [] "get_type_root"
>         get_type_root_int
>         uint8T
>         [(objtypeT, Just "type")]
>     where
>         get_type_root_int [typ] =
>           -- big switch over all types, each just returns the result
>           switch typ cases (assert false >> returnc false)
>         cases = map (mkCase . name) $ capTypes
>         mkCase capName = (ofObjTypeEnum capName, mkGetRoot capName)
>         -- case body just returns the calculated number
>         mkGetRoot capName = returnc $ uint8 $ fromIntegral $ fromJust $ elemIndex (typeRoot capName) rootTypeNames
>         capTypes = capabilities caps
>         -- cap name -> cap lookup list
>         capTypesLookup = map (\c -> (name c, c)) capTypes
>         -- list of names of root types. the index in this list is the final
>         -- root index
>         rootTypeNames = map name $ filter (isNothing . from) capTypes
>         typeRoot capName =
>           -- recursively walk up retype relations
>           case from $ fromJust $ lookup capName capTypesLookup of
>             Just n -> typeRoot n
>             Nothing -> capName

$compare\_caps$ returns -1, 0 or 1 indicating the ordering of the given caps.

\texttt{compare\_caps(left, right) $op$ 0} implies \texttt{left $op$ right}, where op is a numerical comparison.

> compare_caps :: Capabilities ->
>                 PureExpr ->
>                 PureExpr ->
>                 PureExpr ->
>                 FoFCode PureExpr
> compare_caps caps get_type_root get_address get_size =
>     def [] "compare_caps"
>         (compare_caps_int caps get_type_root get_address get_size)
>         int8T
>         [(ptrT $ ptrT $ capsStructT caps, Just "left"),
>          (ptrT $ ptrT $ capsStructT caps, Just "right"),
>          (boolT, Just "tiebreak")]

> compare_caps_int :: Capabilities ->
>                     PureExpr ->
>                     PureExpr ->
>                     PureExpr ->
>                     [PureExpr] ->
>                     FoFCode PureExpr
> compare_caps_int caps get_type_root get_address get_size
>                  [left_cap_pp, right_cap_pp, tiebreak] =
>     do
>       leftCap <- getCap left_cap_pp
>       rightCap <- getCap right_cap_pp
>       leftType <- readStruct leftCap "type"
>       rightType <- readStruct rightCap "type"
>       -- perform a bunch of global tests
>       sequence $ map (doCmp left_cap_pp leftCap leftType
>                             right_cap_pp rightCap rightType) tests
>       -- at this point we know the caps are the same type
>       -- also, they are at the same address and have the same size
>       -- if the cap type has additional "eq" attributes, we now compare those
>       let haveEqCaps = filter (not . null . eqFields) $ capabilities caps
>           mkCase capType = ((ofObjTypeEnum $ name capType),
>                             (mkEqCmp leftCap rightCap capType))
>           eqCases = map mkCase haveEqCaps
>       switch leftType eqCases (return false)
>       -- finally, if the tie break param is true we compare the pointers
>       ifc (return tiebreak) (mkCmp left_cap_pp right_cap_pp (.<.)) (return false)
>       returnc $ int8 0
>     where
>
>       -- type-independent tests
>       -- Note the reversed ordering on the capability size: Ordering
>       -- capabilities by descending size makes parents that have the same
>       -- starting address as their children appear before those children in
>       -- the order.
>       tests = [(getRoot, (.<.)),
>                (getAddr, (.<.)),
>                (getSize, (.>.)),
>                (getType, (.<.))]
>       getRoot cpp c ct = call get_type_root [ct]
>       getAddr cpp c ct = call get_address [cpp]
>       getSize cpp c ct = call get_size [cpp]
>       getType cpp c ct = return ct
>
>       -- comparison code generators
>       mkCmp left right op =
>         ifc (return (left .!=. right)) 
>             (returnc $ test (left `op` right) (int8 (-1)) (int8 1))
>             (return false)
>       doCmp lcpp lc lct rcpp rc rct (f, op) =
>         do
>           l <- f lcpp lc lct
>           r <- f rcpp rc rct
>           mkCmp l r op
>       mkEqCmp leftCap rightCap capType =
>         do
>           let eqs = map (\(NameField n) -> n) $ eqFields capType
>           leftCapU <- readStruct leftCap "u"
>           rightCapU <- readStruct rightCap "u"
>           let capName = case name capType of CapName n -> n
>           leftCapS <- readUnion leftCapU $ lower capName
>           rightCapS <- readUnion rightCapU $ lower capName
>           sequence $ map (doFieldCmp leftCapS rightCapS) eqs
>           return false
>       doFieldCmp lc rc n =
>         do
>           l <- readStruct lc n
>           r <- readStruct rc n
>           mkCmp l r (.<.)

\section{Is well founded}

\subsection{Compute Well-found-ness Relation}

{\tt is\_well\_founded} checks if {\tt src\_type} can be retyped to {\tt
dest\_type}.

> is_well_founded :: [Capability] ->
>                    FoFCode PureExpr
> is_well_founded caps =
>     def [] "is_well_founded"
>             (is_well_founded_int caps)
>             boolT
>             [(objtypeT, Just "src_type"),
>              (objtypeT, Just "dest_type")]

> is_well_founded_int caps (src_type : dest_type : []) =
>     do
>       switch dest_type cases (returnc false)
>     where
>       cases = map mkCase $ filter (\c -> (isJust $ from c) || (fromSelf c)) caps
>       mkCase capType = ((ofObjTypeEnum $ name capType), (checkIsParent src_type capType))
>       checkIsParent parent capType = returnc ((checkIsFrom parent capType)  .|. (checkIsFromSelf parent capType))
>       checkIsFrom parent capType =
>         case from capType of
>           Just capName -> (parent .==. (ofObjTypeEnum capName))
>           Nothing      -> false
>       checkIsFromSelf parent capType =
>         if fromSelf capType
>            then (parent .==. (ofObjTypeEnum $ name capType))
>            else false

> is_equal_types :: FoFCode PureExpr
> is_equal_types =
>     def [] "is_equal_type"
>         is_equal_int
>         boolT
>         [(objtypeT, Just "left_type"),
>          (objtypeT, Just "right_type")]
>     where is_equal_int (src_type : dest_type : []) =
>             do returnc (src_type .==. dest_type)

\section{Is revoked first}

This function queries if the given capability can be retyped in its current
state.

\subsection{Compute Revocation Paths}

\verb|revokePaths| indicates for all capability types if the type
can be retyped multiple times.

> revokePaths :: [Capability] -> 
>                [(PureExpr, Maybe Bool)]
> revokePaths caps = map revokePath caps
>     where revokePath cap = strict ( ofObjTypeEnum $ name cap, multiRet cap)
>           multiRet cap = if null (getChildren cap caps) then Nothing else Just $ multiRetype cap
>           -- this returns a list of children in the type order for the capability `cap'.
> 	    getChildren cap capabilities =
> 	        [ c | c <- capabilities, isChild c cap ] ++
> 	        (if fromSelf cap then [cap] else [])
> 	    isChild c p = if isJust (from c) then name p == (fromJust $ from c) else False

\subsection{Generate Code}

> is_revoked_first :: [Capability] ->
>                     FoFCode PureExpr
> is_revoked_first caps =
>     def [] 
>         "is_revoked_first"
>         (is_revoked_first_int caps)
>         boolT
>         [(ctePtrT, Just "src_cte"),
>          (objtypeT, Just "src_type")]

> is_revoked_first_int :: [Capability] ->
>                         [PureExpr] ->
>                         FoFCode PureExpr
> is_revoked_first_int caps (src_cte : src_type : []) =
>     do
>     let cases = map (mkRevokeCase src_cte) revokeP
>     switch src_type
>            cases
>            (do
>             returnc false)
>         -- revokeP contains all cap types that can be retyped
>         where revokeP = [(st, fromJust rp) | (st, rp) <- revokePaths caps, isJust rp]
>               mkRevokeCase cte (rType, mult) = (rType, caseCode cte mult)
>               -- return true if type of cte is multi-retypable or cte has no descendants,
>               -- else false
>  		caseCode cte mult = do if mult then returnc true
>                                              else do b <- has_descendants cte
>                                                      ifc (return b)
>                                                          (returnc false)
>                                                          (returnc true)

\section{Is copy}

Check whether two capabilities represent the same object. This is different
from \texttt{compare\_caps(left, right, false) == 0} in that it respects
general equality specifiers like \texttt{is\_always\_copy} and
\texttt{is\_never\_copy}.

> is_copy ::  Capabilities -> 
>             PureExpr ->
>             FoFCode PureExpr
> is_copy caps compare_caps =
>     def [] 
>         "is_copy" 
>         (is_copy_int caps compare_caps)
>         boolT
>         [(ptrT $ ptrT thisCapsStructT, Just "left"),
>          (ptrT $ ptrT thisCapsStructT, Just "right")]
>     where thisCapsStructT = capsStructT caps 

> is_copy_int :: Capabilities -> 
>                PureExpr ->
>                [PureExpr] ->
>                FoFCode PureExpr
> is_copy_int caps compare_caps (leftPP : rightPP : []) =
>     do 
>       -- compare types
>       leftCap <- getCap leftPP
>       rightCap <- getCap rightPP
>       leftType <- readStruct leftCap "type"
>       rightType <- readStruct rightCap "type"
>       ifc (return (leftType .!=. rightType))
>           (returnc false) (return false)
>       -- we now have equal types, check general equality for the type
>       switch leftType generalEqCases (return false)
>       -- don't have general equality, call compare_caps with tiebreak = false
>       res <- call compare_caps [leftPP, rightPP, false]
>       returnc (res .==. (int8 0))
>     where
>       -- in general equality switch, only handle cases with such an equality
>       generalEqCases = map mkGeqCase $
>                        filter (isJust . generalEquality) $
>                        capabilities caps
>       -- case body: just return the value of the equality
>       mkGeqCase capType = ((ofObjTypeEnum $ name capType),
>                            (returnc $ fofBool $ fromJust $ generalEquality capType))
>       fofBool b = if b then true else false

\section{Is Ancestor}

{\tt is\_ancestor} checks if one cap is an immediate ancestor of another (note:
if ancestor and child are of the same type, a non-immediate ancestor will also
count).

> is_ancestor :: Capabilities ->
>                PureExpr ->
>                PureExpr ->
>                PureExpr ->
>                FoFCode PureExpr
> is_ancestor caps is_well_founded get_address get_size =
>     def [] 
>         "is_ancestor" 
>         (is_ancestor_int caps is_well_founded get_address get_size)
>         boolT
>         [(ptrT $ ptrT thisCapsStructT, Just "child"),
>          (ptrT $ ptrT thisCapsStructT, Just "parent")] 
>     where thisCapsStructT = capsStructT caps 

> is_ancestor_int :: Capabilities ->
>                    PureExpr ->
>                    PureExpr ->
>                    PureExpr ->
>                    [PureExpr] ->
>                    FoFCode PureExpr
> is_ancestor_int caps is_well_founded get_address get_size (childPP : parentPP : []) =
>     do
>       child <- getCap childPP
>       parent <- getCap parentPP
>       childType <- readStruct child "type"
>       parentType <- readStruct parent "type"
>
>       -- fail if relationship is not "well founded"
>       wellFounded <- call is_well_founded [parentType, childType]
>       ifc (return $ neg $ wellFounded)
>           (returnc false) (return void)
>
>       -- compute begin and end of parent and child
>       parentAddr <- call get_address [parentPP]
>       childAddr <- call get_address [childPP]
>       parentSize <- call get_size [parentPP]
>       childSize <- call get_size [childPP]
>       parentEnd <- return $ parentAddr .+. parentSize
>       childEnd <- return $ childAddr .+. childSize
>
>       -- check that the child is inside the parent
>       ifc (return (childType .==. parentType))
>           -- for self-retypes the ancestor must be strictly greater than the child
>           (checkStrictInside parentAddr parentEnd childAddr childEnd)
>           (checkInside parentAddr parentEnd childAddr childEnd)
>
>     where
>       checkStrictInside parentAddr parentEnd childAddr childEnd =
>         do returnc (((parentAddr .<. childAddr) .&. (parentEnd .>=. childEnd))
>                     .|. ((parentAddr .<=. childAddr) .&. (parentEnd .>. childEnd)))
>       checkInside parentAddr parentEnd childAddr childEnd =
>         do returnc ((parentAddr .<=. childAddr) .&. (parentEnd .>=. childEnd))
>

\section{Back-end}

> backend :: Capabilities -> FoFCode PureExpr
> backend caps =
>     do dummy <- newEnum "objtype" enums "ObjType_Num"
>        getAddress <- get_address caps
>        getSize <- get_size caps
>        getTypeRoot <- get_type_root caps
>        compareCaps <- compare_caps caps getTypeRoot getAddress getSize
>        isWellFounded <- is_well_founded capList
>        isEqualTypes <- is_equal_types
>        isRevokedFirst <- is_revoked_first capList
>        isCopy <- is_copy caps compareCaps
>        isAncestor <- is_ancestor caps isWellFounded getAddress getSize
>        return void
>     where capList = capabilities caps
>           enums = mkObjTypeEnum capList

> userbackend :: Capabilities -> FoFCode PureExpr
> userbackend caps =
>     do dummy <- newEnum "objtype" enums "ObjType_Num"
>        getAddress <- get_address caps
>        getSize <- get_size caps
>        getTypeRoot <- get_type_root caps
>        compareCaps <- compare_caps caps getTypeRoot getAddress getSize
>        isWellFounded <- is_well_founded capList
>        isEqualTypes <- is_equal_types
>        isCopy <- is_copy caps compareCaps
>        isAncestor <- is_ancestor caps isWellFounded getAddress getSize
>        return void
>     where capList = capabilities caps
>           enums = mkObjTypeEnum capList
