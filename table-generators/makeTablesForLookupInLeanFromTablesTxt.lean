module
import MakeTablesForLookupInLeanFromTablesTxt.Common

open MakeTablesForLookupInLeanFromTablesTxt

public def main (_args : List String) : IO UInt32 := do
  generate (".."/"lib"/"data-table") (".."/"lib"/"UnicodeBasic"/"TableLookupTables")
  return 0
