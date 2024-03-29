#' @importFrom conflicted conflict_prefer
.onAttach = function(...)
{
  conflict_prefer("read_csv", "tbltools", "readr")
  conflict_prefer("write_csv", "tbltools", "readr")
  conflict_prefer("read_fst", "tbltools", "fst")
  conflict_prefer("filter", "dplyr", "stats")
}
