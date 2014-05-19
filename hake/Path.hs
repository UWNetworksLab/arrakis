{- 
   Path: functions for manipulating (POSIX) path names.  
   
  Part of Hake: a makefile generator for Barrelfish
   
  Copyright (c) 2009, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
-}  

module Path (dirname, basename, 
             (.<.), isBelow, isInSameDirAs, 
             normalise, 
             (./.), relToDir, relToFile, 
             makeRel, makeAbs, 
             removePrefix,
             replaceSuffices, replaceSuffix, removeSuffix )
where

import Data.List

data T = T { leading :: Bool,
             parts :: [String],
             trailing :: Bool }
         deriving (Show,Eq)

make :: String -> T
make "" = T { leading = False, parts = [], trailing = False }
make s 
    | (head s) == '/' = 
        (make $ tail s) { leading = True }
    | (last s) == '/' = 
        (make (reverse $ tail $ reverse s)) { trailing = True }
    | otherwise = T { leading = False, parts = make_parts s, trailing = False }

make_parts :: String -> [String]
make_parts "" = []
make_parts s = let (l, s') = break (== '/') s
               in l : case s' of
                        [] -> []
                        (_:s'') -> make_parts s''

toStr :: T -> String
toStr n =
    (if leading n then "/" else "")
    ++
    (foldl (++) "" (intersperse "/" (parts n)))
    ++
    (if trailing n then "/" else "")

--
-- Strip non-directory suffix from a file name, designed to mimic Unix
-- dirname(1).
-- 
dirname :: String -> String
dirname n = toStr $ dirname' $ make n

dirname' :: T -> T
dirname' n 
    | ((length $ parts n) < 2) && (not $ leading n) = make "."
    | otherwise = n { parts = (init $ parts n) }

--
-- Strip directory from filenames
-- Equivalent to the 1-argument form of Unix basename(1).
--
basename :: String -> String
basename n = toStr $ basename' $ make n
    
basename' :: T -> T
basename' n = make $ last $ parts n

--
-- Return whether the two paths are in the same directory
--
isInSameDirAs :: String -> String -> Bool
isInSameDirAs f1 f2 = (dirname f1) == (dirname f2)

--
-- Remove redundant '.' and '..' from a path name
--
normalise :: String -> String 
normalise n = toStr $ normalise' $ make n

normalise' :: T -> T 
normalise' n 
    | parts n == [] = n
    | leading n = n { parts = walkAbsPath $ parts n }
    | otherwise =  n { parts = walkPath [] (parts n) }

--
-- Normalise a path starting with '/'
--
walkAbsPath :: [String] -> [String]
walkAbsPath (".":p) = walkAbsPath p
walkAbsPath ("..":p) = walkAbsPath p
walkAbsPath p = walkPath [] p

walkPath :: [String] -> [String] -> [String]
walkPath acc [] = acc
walkPath [] (".":p) = walkPath ["."] p
walkPath [] ("..":p) = walkPath [".."] p
walkPath acc (".":p) = walkPath acc p
walkPath ["."] ("..":p) = walkPath [".."] p
walkPath acc ("..":p) 
    | ".." == (last acc) = walkPath (acc ++ [".."]) p
    | otherwise = walkPath (init acc) p
walkPath acc (p:ps) = walkPath (acc ++ [p]) ps

infix 4 ./.
(./.) :: String -> String -> String
root ./. path = relToDir path root

relToDir :: String -> String -> String
relToDir path root = toStr (relToDir' (make path) (make root))

relToDir' :: T -> T -> T
relToDir' path root 
    | leading path = path
    | otherwise = normalise' root { parts = (parts root) ++ (parts path), 
                                   trailing = (trailing path) }

relToFile :: String -> String -> String
relToFile path root = toStr (relToFile' (make path) (make root))

relToFile' :: T -> T -> T
relToFile' path root 
    | leading path = path
    | trailing root = relToDir' path root
    | otherwise = normalise' root { parts = (init $ parts root) ++ (parts path), 
                                   trailing = (trailing path) }

--
-- Turn a pathname into a relative one, or an absolute one
--
makeRel :: String -> String
makeRel n = toStr ((make n) { leading = False })
makeAbs :: String -> String
makeAbs n = toStr $ normalise' ((make n) { leading = True })

--
-- Return whether one path is below another path
--
infix 9 .<.
n .<. m = isBelow m n 

isBelow :: String -> String -> Bool
isBelow n1 n2 = 
    (parts $ make n2) `isPrefixOf` (parts $ make n1)

isBelow' :: T -> T -> Bool
isBelow' n1 n2 = 
    parts n2 `isPrefixOf` parts n1

--
-- Remove a prefix
-- 
removePrefix :: String -> String -> String
removePrefix prefix f 
    | prefix .<. f =
        let f' = make f
            len = length $ parts $ make prefix
        in 
          toStr $ normalise' f' { parts = "." : drop len (parts f') }
    | otherwise = f
    


--
-- Replace one suffix with another.
--
replaceSuffices :: [ String ] -> String -> String -> [ String ]
replaceSuffices l s o = map (\x -> replaceSuffix x s o) l

replaceSuffix :: String -> String -> String -> String
replaceSuffix x s o = 
    if isSuffixOf s x
    then (take ((length x) - (length s)) x) ++ o
    else x

removeSuffix :: String -> String
removeSuffix s = 
    case (elemIndices '.' s) of
      [] -> s
      l -> take (last l) s
