#' @importFrom conflicted conflict_prefer
.onAttach = function(...)
{
  conflict_prefer("read_csv", "tbltools")
}
