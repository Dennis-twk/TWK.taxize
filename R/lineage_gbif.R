#' Extract taxonomic information at a specific rank from GBIF lineage data
#'
#' @description
#' This function processes a list of GBIF taxonomic lineage data and extracts
#' the taxonomic name for a specified rank (e.g., "family", "genus") for each element.
#' It is designed to work with GBIF-style taxonomic classification structures.
#'
#' @param x A list of data.frames containing GBIF taxonomic lineage information.
#'   Each element of the list should represent a taxon and contain at least two
#'   columns: `rank` (character) and `name` (character). The names of the list
#'   elements are assumed to be GBIF taxonomic IDs.
#' @param rank0 A character string specifying the taxonomic rank to extract from
#'   each lineage in `x` (e.g., "phylum", "order", "genus", "species").
#'
#' @returns
#' A data.frame with two columns:
#' \describe{
#'   \item{gbif.ID}{Character. The GBIF taxonomic IDs, taken from the names of the input list `x`.}
#'   \item{GBIF.<rank0>}{Character. The extracted taxonomic name at the specified rank for each taxon.
#'     The column name is dynamically generated based on the `rank0` parameter (e.g., "GBIF.genus").}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Assuming `gbif_lineage_data` is a list of GBIF lineage data.frames:
#'   # List names should be GBIF taxonomic IDs
#'   # Each element should be a data.frame with 'rank' and 'name' columns
#'
#'   # Extract genus-level information
#'   genus_info <- lineage_gbif(x = gbif_lineage_data, rank0 = "genus")
#'   head(genus_info)
#'
#'   # Extract family-level information
#'   family_info <- lineage_gbif(x = gbif_lineage_data, rank0 = "family")
#'   head(family_info)
#' }
#'
lineage_gbif <- function(x, rank0) {
  d0 <- paste0('GBIF.', rank0)
  for (i in seq_along(x)) {
    sub <- subset(x[[i]], rank == rank0, select = name)
    if (NROW(sub)) {
      d0 <- cbind(d0, sub)
      colnames(d0)[ncol(d0)] <- names(x)[i]
    }
  }
  d1 <- t(d0)
  d2 <- data.frame(gbif.ID = rownames(d1)[-1],
                   tmp    = d1[-1, 1],
                   stringsAsFactors = FALSE)
  names(d2)[2] <- paste0('GBIF.', rank0)
  d2
}

#' Retrieve full taxonomic lineage from GBIF data
#'
#' @description
#' This function processes a list of GBIF taxonomic classification data and returns
#' a complete lineage by merging taxonomic information across multiple predefined
#' ranks (from kingdom to species). It serves as a convenient wrapper around
#' `lineage_gbif()` for multiple ranks, specifically tailored to GBIF's taxonomic
#' structure which excludes 'superfamily' compared to NCBI.
#'
#' @param x A list of data.frames. Each element should contain the taxonomic
#'   lineage information for a specific taxon, typically obtained from GBIF
#'   classification tools. Each data.frame must contain at least the columns
#'   `rank` and `name`. The names of the list are assumed to be GBIF taxonomic
#'   IDs.
#'
#' @returns
#' A data.frame where each row represents a taxon from the input list `x`.
#'   The first column, `gbif.ID`, contains the taxonomic IDs (names of the
#'   input list). Subsequent columns correspond to the taxonomic names at each
#'   of the predefined ranks (`kingdom`, `phylum`, `class`, `order`,
#'   `family`, `genus`, `species`), with column names prefixed by "GBIF."
#'   (e.g., `GBIF.kingdom`, `GBIF.phylum`). The rows are merged using `gbif.ID`
#'   as the key.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Assuming `gbif_lineage_data` is a list of GBIF lineage data.frames
#'   # names(gbif_lineage_data) should be GBIF taxonomic IDs
#'   full_lineage <- lineage_all_gbif(x = gbif_lineage_data)
#'   head(full_lineage)
#' }
#'
lineage_all_gbif <- function(x) {
  ranks <- c("kingdom","phylum","class","order",
             "family","genus","species")     # GBIF 没有 superfamily
  lst   <- lapply(ranks, function(r) lineage_gbif(x, r))
  Reduce(function(a, b) merge(a, b, by = "gbif.ID", all.x = TRUE),
         lst, init = out_id_gbif)
}
