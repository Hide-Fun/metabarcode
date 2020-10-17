#' Merging taxon in one column.
#'
#' Merging taxon in one column by deepest taxonomic level.
#' @param .identify_list table (see example).
#' @param .id logical, make id against each taxon. This is useful in makeing labels by each OTU.
#' @examples
#' identify_list <- data.frame(
#'    otu = c("otu1", "otu2", "otu3", NA),
#'    phylum = c("p1", "p2", "p3", NA),
#'    order = c("o1", "o2", "o3", NA),
#'    family = c("f1", NA, "f3", NA),
#'    genus = c(NA, NA, "g3", NA)
#' )
#' merge_taxon(.identify_list = identify_list, .id = T)
#' merge_taxon(.identify_list = identify_list, .id = F)
merge_taxon = function(
  .identify_list,
  .id = F)
{
  # make id.
  df <- .identify_list %>%
    tibble::rownames_to_column("id") %>%
    dplyr::mutate(id = readr::parse_double(id))
  # genus.
  genus <- df %>% # select genus
    dplyr::filter(!is.na(genus)) %>%
    dplyr::rename(taxon = genus) %>%
    dplyr::select(id, taxon)
  # family.
  family <- df %>% # select family
    dplyr::anti_join(genus, by = "id") %>%
    dplyr::select(id, phylum:family) %>%
    dplyr::filter(!is.na(family))  %>%
    dplyr::rename(taxon = family) %>%
    dplyr::select(id, taxon)
  # merge.
  merge_gf <- dplyr::bind_rows(genus, family)
  # order.
  order <- df %>%
    dplyr::anti_join(merge_gf, by = "id") %>%
    dplyr::select(id, phylum:family) %>%
    dplyr::filter(!is.na(order))  %>%
    dplyr::rename(taxon = order) %>%
    dplyr::select(id, taxon)
  # merge.
  merge_gfo <- merge_gf %>%
    dplyr::bind_rows(order)
  # phylum.
  phylum <- df %>%
    dplyr::anti_join(merge_gfo, by = "id") %>%
    dplyr::select(id, phylum:family) %>%
    dplyr::filter(!is.na(phylum))  %>%
    dplyr::rename(taxon = phylum) %>%
    dplyr::select(id, taxon)
  # no phylum.
  phylum_no <- df %>%
    dplyr::anti_join(merge_gfo, by = "id") %>%
    dplyr::select(id, phylum:family) %>%
    dplyr::filter(is.na(phylum))  %>%
    dplyr::rename(taxon = phylum) %>%
    dplyr::select(id, taxon)
  # merge.
  merge_gfop_na <- merge_gfo %>%
    dplyr::bind_rows(phylum, phylum_no) %>%
    dplyr::arrange(id)
  # merge.
  if(.id == T) {
    all <- df %>%
      dplyr::left_join(merge_gfop_na, by = "id") %>%
      dplyr::group_by(taxon) %>%
      dplyr::mutate(id2 = dplyr::row_number(),
                    taxon = str_c(taxon, id2, sep = "_")) %>%
      dplyr::ungroup()
  } else {
    all <- df %>%
      dplyr::left_join(merge_gfop_na, by = "id") %>%
      tibble::as_tibble() %>%
      dplyr::ungroup()
  }
  return(all)
}
