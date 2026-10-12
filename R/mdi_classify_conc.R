#' Classify the concordance rows (edges)

#' @description This tool classifies concordance edges into type 1:1, 1:m, m:1, or m:m. Optionally,
#' the tool also creates numbered 'components' of the m:m edges.
#' The input is a concordance data.table, with left and right columns and the function returns
#' a data.table, possibly merged with the initial table, with conctype (and optional compnum).
#'
#'
#' @param DTconc a concordance table (data.table) with two columns:
#'        given by parameters left and right.
#' @param left character ('left') is the name of the left side of the concordance
#' @param right character ('right') is the name of the right side of the concordance
#' @param mm_groups logical (TRUE) if m:m grouping (block diagonalization) is required.
#' @param mergeconc logical (FALSE) returns the conc merged with the original input
#'
#' @return Returns a data.table with columns `left`, `right`, `conctype`, `outdegree`,
#'   `indegree` (and `compnum`)
#'         where
#'         conctype gives type of match ("1:1","m:1","1:m","m:m");
#'         outdegree gives number of right nodes to which a left node goes;
#'         indegree gives number of left nodes that go to a right node;
#'         if mm_groups=TRUE, then `compnum` gives component number.
#'         If mergeconc=TRUE then mdi_classify_conc() returns the result merged with the original DTconc input.
#'
#' @examples
#' concx <- data.table::data.table(
#'   leftx = as.character(c(1, 2, 2, 3, 4, 5, 6, 6, 7, 8, 8)),
#'   rightx = c("A", "B", "C", "D", "D", "F", "E", "F", "H", "G", "H")
#' )
#' classified <- mdi_classify_conc(concx, left = "leftx", right = "rightx",
#'                                 mm_groups = TRUE, mergeconc = TRUE)
#'
#' @export
#' @import data.table
#' @importFrom igraph graph_from_data_frame induced_subgraph V components

mdi_classify_conc <- function(DTconc, left = "left", right = "right", mm_groups = TRUE, mergeconc = FALSE) {
  # -------------------------------------------------------------------------- #
  ##### Step 1: classify the concordance into 4 possible types
  # "1:1" type from left to right
  # "m:1" type from left to right
  # "1:m" type from left to right
  # "m:m" type from left to right
  # -------------------------------------------------------------------------- #

  # as.data.table() copies, so the caller's table is not changed
  conc <- as.data.table(DTconc)

  # the unique edges
  setnames(conc, c(left, right), c("left", "right"))
  conc <- unique(conc[, c("left", "right"), with = FALSE])

  # Basic Concordance type
  # make indegree and outdegree for each unique left or right node
  conc[, ("outdegree") := .N, by = "left"]
  conc[, ("indegree") := .N, by = "right"]

  # a left code with several right codes is 1:m if each of those right codes
  # has only this left code (and the other way round for m:1)
  degree_cols <- c("outdegree", "indegree")
  conc[conc[["outdegree"]] > 1, ("allin1") := .SD[["outdegree"]] == sum(.SD[["indegree"]]),
       by = "left", .SDcols = degree_cols]
  conc[conc[["indegree"]] > 1, ("allout1") := .SD[["indegree"]] == sum(.SD[["outdegree"]]),
       by = "right", .SDcols = degree_cols]

  # make the 4 types, m:m is the remainder category
  out_deg <- conc[["outdegree"]]
  in_deg <- conc[["indegree"]]
  type <- rep("", nrow(conc))
  type[out_deg == 1 & in_deg == 1] <- "1:1"
  type[out_deg > 1 & conc[["allin1"]] %in% TRUE] <- "1:m"
  type[in_deg > 1 & conc[["allout1"]] %in% TRUE] <- "m:1"
  type[type == ""] <- "m:m"
  set(conc, j = "conctype", value = type)
  conc <- conc[, c("left", "right", "conctype", "outdegree", "indegree"), with = FALSE]

  # -------------------------------------------------------------------------- #
  ##### Step 2 (if mm_groups==TRUE): add compnum for blockdiagonal components of edges
  if (mm_groups) {
    # split off non-m:m edges and give a sequential component number to each of the non-m:m groups
    conc1 <- conc[conc[["conctype"]] != "m:m"]

    # number of non-m:m groups (needed later)
    maxcmp <- 0

    if (nrow(conc1) > 0) {
      conc1[, ("compnum") := .GRP, by = "conctype"]
      maxcmp <- max(conc1[["compnum"]])
    }

    # make diagonalization for m:m edges
    # a code on both sides is two different nodes: label each by its side
    mm <- conc[conc[["conctype"]] == "m:m"]
    set(mm, j = "left", value = paste0("L:", mm[["left"]]))
    set(mm, j = "right", value = paste0("R:", mm[["right"]]))
    concgraph <- graph_from_data_frame(mm, directed = TRUE)
    # split into components
    comp <- components(concgraph)

    # the edges of each block-diagonal component of the m:m edges, numbered
    # after the 1:1, 1:m and m:1 groups; stacked once at the end
    empty <- data.table(
      from = character(), to = character(), conctype = character(),
      outdegree = integer(), indegree = integer(), compnum = integer()
    )
    edges <- lapply(seq_len(comp$no), function(i) {
      edge_table <- as.data.table(igraph::as_data_frame(induced_subgraph(concgraph, vids = V(concgraph)[comp$membership == i]), what = "edges"))
      set(edge_table, j = "compnum", value = i + maxcmp)
      edge_table
    })
    conc <- rbindlist(c(list(empty), edges), use.names = TRUE)

    setnames(conc, c("from", "to"), c("left", "right"))
    set(conc, j = "left", value = sub("^L:", "", conc[["left"]]))
    set(conc, j = "right", value = sub("^R:", "", conc[["right"]]))

    # recombine the non-m:m and m:m
    conc <- rbindlist(list(conc1, conc), use.names = TRUE, fill = TRUE)

    # cleanup
    rm(concgraph, comp, conc1, maxcmp)
  }

  setnames(conc, c("left", "right"), c(left, right))

  ### Return the dataset ----------------------------------------------------- #
  if (isTRUE(mergeconc)) {
    conc <- merge(conc, DTconc, by = c(left, right))
  }
  return(conc)
}
