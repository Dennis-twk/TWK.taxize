#' Extract a specific taxonomic rank from NCBI lineage lists
#'
#' @description
#' This function processes a list of NCBI taxonomic lineages and extracts the
#' taxonomic name at a specified rank (e.g., "phylum", "order") for each element.
#'
#' @param x A list of data.frames (or similar objects). Each element should contain
#'   taxonomic lineage information for a specific taxon, as typically obtained from
#'   NCBI classification tools (e.g., via the `taxize` package or similar). Each
#'   data.frame should at least contain the columns `rank` and `name`.
#' @param rank0 A character string. The name of the taxonomic rank to extract from
#'   each lineage in `x` (e.g., "phylum", "order", "genus").
#'
#' @returns
#' A data.frame with two columns:
#' \describe{
#'   \item{ncbi.taxid}{The names of the original list `x`, assumed to represent NCBI taxonomic IDs.}
#'   \item{NCBI.<rank0>}{The extracted taxonomic name at the specified rank for each taxon.
#'     The column name will be dynamically pasted with the value of `rank0` (e.g., "NCBI.phylum").}
#' }
#' Returns an empty data.frame (with the same column structure) if no elements in `x` contain the specified rank.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Assuming you have a list `my_lineage` obtained from NCBI classification
#'   # Example list structure (conceptual):
#'   # my_lineage <- list(
#'   #   "taxid123" = data.frame(rank = c("superkingdom", "phylum", ..., "species"),
#'   #                           name = c("Archaea", "Euryarchaeota", ..., "Thermococcus nautili")),
#'   #   "taxid456" = data.frame(rank = c("superkingdom", "phylum", ..., "species"),
#'   #                           name = c("Bacteria", "Firmicutes", ..., "Bacillus subtilis"))
#'   # )
#'   #
#'   # Extract the 'phylum' for each taxon:
#'   phylum_info <- lineage_ncbi(x = my_lineage, rank0 = "phylum")
#'   print(phylum_info)
#' }
#'
lineage_ncbi <- function(x, rank0) {
  d0 <- paste0('NCBI.', rank0)
  for (i in seq_along(x)) {
    sub <- subset(x[[i]], rank == rank0, select = name)
    if (nrow(sub)) {
      d0 <- cbind(d0, sub)
      colnames(d0)[ncol(d0)] <- names(x)[i]
    }
  }
  d1 <- t(d0)
  d2 <- data.frame(rownames(d1), d1[, 1], stringsAsFactors = FALSE)
  d3 <- d2[-1, ]
  names(d3) <- c('ncbi.taxid', paste0('NCBI.', rank0))
  d3
}

#' Retrieve full taxonomic lineage from NCBI data
#'
#' @description
#' This function takes a list of NCBI taxonomic classification data and returns
#' a complete lineage by merging taxonomic information across multiple predefined
#' ranks (from kingdom to species). It acts as a convenient wrapper around
#' `lineage_ncbi()` for multiple ranks.
#'
#' @param x A list of data.frames. Each element should contain the taxonomic
#'   lineage information for a specific taxon, typically obtained from NCBI
#'   classification tools. Each data.frame must contain at least the columns
#'   `rank` and `name`. The names of the list are assumed to be NCBI taxonomic
#'   IDs.
#'
#' @returns
#' A data.frame where each row represents a taxon from the input list `x`.
#'   The first column, `ncbi.taxid`, contains the taxonomic IDs (names of the
#'   input list). Subsequent columns correspond to the taxonomic names at each
#'   of the predefined ranks (`kingdom`, `phylum`, `class`, `order`,
#'   `superfamily`, `family`, `genus`, `species`), with column names prefixed
#'   by "NCBI." (e.g., `NCBI.kingdom`, `NCBI.phylum`). The rows are merged
#'   using `ncbi.taxid` as the key.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Assuming `my_lineage_data` is a list of NCBI lineage data.frames
#'   # names(my_lineage_data) should be taxonomic IDs
#'   full_lineage <- lineage_all_ncbi(x = my_lineage_data)
#'   head(full_lineage)
#' }
#'
lineage_all_ncbi <- function(x) {
  ranks <- c("kingdom","phylum","class","order",
             "superfamily","family","genus","species")
  lst <- lapply(ranks, function(r) lineage_ncbi(x, r))
  Reduce(function(a, b) merge(a, b, by = "ncbi.taxid", all.x = TRUE),
         lst, init = out_id_ncbi)
}
