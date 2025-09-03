#' Extract superfamily-level taxonomic information from NCBI lineage data
#'
#' @description
#' This function processes a list of NCBI taxonomic classification data frames
#' and extracts the taxonomic name at the specified rank (typically "superfamily")
#' for each input taxon. It is specifically designed to handle superfamily-level
#' classification within the NCBI taxonomy framework.
#'
#' @param x A list of data.frames containing NCBI taxonomic lineage information.
#'   Each element of the list should represent a taxon and must contain at least
#'   two columns: `rank` (character) and `name` (character). The names of the list
#'   elements are assumed to be NCBI taxonomic IDs.
#' @param rank0 A character string specifying the taxonomic rank to extract from
#'   each lineage in `x`. For superfamily-level extraction, this would typically
#'   be "superfamily".
#'
#' @returns
#' A data.frame with two columns:
#' \describe{
#'   \item{ncbi.taxid}{Character. The NCBI taxonomic IDs, taken from the names of the input list `x`.}
#'   \item{NCBI.<rank0>}{Character. The extracted taxonomic name at the specified rank for each taxon.
#'     The column name is dynamically generated based on the `rank0` parameter (e.g., "NCBI.superfamily").}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Assuming `ncbi_lineage_data` is a list of NCBI lineage data.frames:
#'   # List names should be NCBI taxonomic IDs
#'   # Each element should be a data.frame with 'rank' and 'name' columns
#'
#'   # Extract superfamily-level information
#'   superfamily_info <- lineage_superfamily_ncbi(x = ncbi_lineage_data,
#'                                               rank0 = "superfamily")
#'   head(superfamily_info)
#' }
lineage_superfamily_ncbi <- function(x, rank0) {
  d0 <- paste0('NCBI.', rank0)
  for (i in seq_along(x)) {
    sub <- subset(x[[i]], rank == rank0, select = name)
    if (NROW(sub)) {
      d0 <- cbind(d0, sub)
      colnames(d0)[ncol(d0)] <- names(x)[i]
    }
  }
  d1 <- t(d0)
  d2 <- data.frame(ncbi.taxid = rownames(d1)[-1],
                   tmp        = d1[-1, 1],
                   stringsAsFactors = FALSE)
  names(d2)[2] <- paste0('NCBI.', rank0)
  d2
}

#' Retrieve partial taxonomic lineage from NCBI data including superfamily
#'
#' @description
#' This function processes a list of NCBI taxonomic classification data frames
#' and returns a partial lineage by merging taxonomic information from kingdom
#' down to family level, specifically including the superfamily rank. It is
#' designed for cases where the full species-level lineage is not required,
#' but the higher classification including superfamily is of interest.
#'
#' @param x A list of data.frames containing NCBI taxonomic lineage information.
#'   Each element of the list should represent a taxon and must contain at least
#'   two columns: `rank` (character) and `name` (character). The names of the list
#'   elements are assumed to be NCBI taxonomic IDs.
#'
#' @returns
#' A data.frame where each row represents a taxon from the input list `x`.
#'   The first column, `ncbi.taxid`, contains the taxonomic IDs (names of the
#'   input list). Subsequent columns correspond to the taxonomic names at each
#'   of the predefined ranks (`kingdom`, `phylum`, `class`, `order`,
#'   `superfamily`, `family`), with column names prefixed by "NCBI."
#'   (e.g., `NCBI.kingdom`, `NCBI.superfamily`). The rows are merged using
#'   `ncbi.taxid` as the key.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Assuming `ncbi_lineage_data` is a list of NCBI lineage data.frames
#'   # names(ncbi_lineage_data) should be NCBI taxonomic IDs
#'   partial_lineage <- lineage_all_superfamily_ncbi(x = ncbi_lineage_data)
#'   head(partial_lineage)
#' }
#'
lineage_all_superfamily_ncbi <- function(x) {
  ranks <- c("kingdom","phylum","class","order",
             "superfamily","family")   # 这里只到 family，没有 genus/species
  lst   <- lapply(ranks, function(r) lineage_superfamily_ncbi(x, r))
  Reduce(function(a, b) merge(a, b, by = "ncbi.taxid", all.x = TRUE),
         lst, init = out_id_ncbi_superfamily)
}
