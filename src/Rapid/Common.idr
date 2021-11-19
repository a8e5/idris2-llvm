module Rapid.Common

public export
data GCFlavour = Zero | BDW | Statepoint

export
Show GCFlavour where
  show Zero = "zero"
  show BDW = "bdw"
  show Statepoint = "statepoint"

export
Eq GCFlavour where
  (==) Zero Zero = True
  (==) BDW BDW = True
  (==) Statepoint Statepoint = True
  (==) _ _ = False

export
encodeGCFlavourAsInt : GCFlavour -> Int
encodeGCFlavourAsInt Zero = 1
encodeGCFlavourAsInt BDW = 2
encodeGCFlavourAsInt Statepoint = 3

public export
record CompileOpts where
  constructor MkCompileOpts
  debugEnabled : Bool
  traceEnabled : Bool
  gcFlavour : GCFlavour
  targetOS : String
