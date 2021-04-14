#' Extract tree structure from a data.frame
#'
#' @note For use with "Maximum Entropy Summary Tree" R Markdown document(s)
#'
#' @export
#' @importFrom tidyr replace_na
#' @importFrom dplyr rename
extract_tree <- function (input_data, form = "column_lineage", ...) {

  uid <- function (x, y = rep("", length(x))) {
    ifelse(nchar(y) == 0, x, sapply(str_c(x, y), md5))
  }

  graft <- function (left, right) {

    prev_round <- max(left$round)
    prev_data <- filter(left, round == prev_round)

    this_data <-
      data_frame(parent = prev_data$uid, label = right) %>%
      replace_na(list(label = "")) %>%
      mutate(uid = uid(parent, label), #ifelse(is_leaf, parent, uid(parent, label)),
             depth = ifelse(label == "", prev_data$depth, prev_data$depth + 1),
             round = prev_round + 1)

    bind_rows(left, this_data)

  }

  h_vars <- sort(tidyselect::vars_select(names(input_data), matches("(src|cat)_h[0-9]")))
  id_vars <- tidyselect::vars_select(names(input_data), matches("(src|cat)_id"))

  parts <- input_data %>%
    select_(.dots = c(h_vars, id_vars)) %>%
    as.list() %>% lapply(as.character)

  root <- data_frame(parent = NA_character_, label = "Bay Area", uid = md5(""), depth = 1, round = 0)
  reduced <- Reduce(graft, parts, init = root) %>% filter(label != "") %>% dplyr::select(-round)

  encode_uids <- . %>% factor(levels = unique(reduced$uid)) %>% as.integer
  reduced %>% rename(node = uid) %>% distinct() %>% mutate_at(vars(node, parent), funs(encode_uids))

}
