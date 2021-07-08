module Compiler.PrepareCode

import Data.Maybe

import Core.Name
import Core.TT
import Compiler.VMCode
import Libraries.Data.SortedMap

%default covering

mutual
  collectConstructorNames : List VMInst -> List Name
  collectConstructorNames is = concatMap constructorNamesInst is

  constructorNamesInst : VMInst -> List Name
  constructorNamesInst (MKCON _ (Right n) _) = [n]
  constructorNamesInst (CASE _ alts def) = concatMap constructorNamesAlt alts ++ collectConstructorNames (fromMaybe [] def)
  constructorNamesInst (CONSTCASE _ alts def) = concatMap (collectConstructorNames . snd) alts ++ collectConstructorNames (fromMaybe [] def)
  constructorNamesInst _ = []

  constructorNamesAlt : (Either Int Name, List VMInst) -> List Name
  constructorNamesAlt (Left _, is) = collectConstructorNames is
  constructorNamesAlt (Right n, is) = n :: (collectConstructorNames is)

getNames : VMDef -> List Name
getNames (MkVMFun _ is) = collectConstructorNames is
getNames (MkVMError is) = collectConstructorNames is
getNames (MkVMForeign _ _ _) = empty

export
getNameMap : List VMDef -> (SortedMap Name Int)
getNameMap vms =
  let conNames = concatMap getNames vms in
      fst $ foldl insertName (empty, 1) conNames
      where
        insertName : (SortedMap Name Int, Int) -> Name -> (SortedMap Name Int, Int)
        insertName (m, i) n = case lookup n m of
                                   Just _ => (m, i)
                                   Nothing => (insert n i m, i+1)

