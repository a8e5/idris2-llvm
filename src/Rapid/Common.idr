module Rapid.Common

public export
data GCFlavour = Zero | BDW | Statepoint

export
Show GCFlavour where
  show Zero = "zero"
  show BDW = "bdw"
  show Statepoint = "statepoint"
