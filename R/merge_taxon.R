#' Merging Taxon
#'
#' merge taxon.
#' @param .df data.frame, tibble
#' @export
merge_taxon = function(.df) {
  # make id.
  df <- .df %>%
    rownames_to_column("id") %>%
    mutate(id = parse_double(id))
  # genus.
  genus <- df %>% # select genus
    filter(!is.na(genus)) %>%
    rename(taxon = genus) %>%
    select(id, taxon)
  # family.
  family <- df %>% # select family
    anti_join(genus_ok, by = "id") %>%
    select(id, phylum:family) %>%
    filter(!is.na(family))  %>%
    rename(taxon = family) %>%
    select(id, taxon)
  # merge.
  merge_gf <- bind_rows(genus, family)
  # order.
  order <- df %>%
    anti_join(merge_gf, by = "id") %>%
    select(id, phylum:family) %>%
    filter(!is.na(order))  %>%
    rename(taxon = order) %>%
    select(id, taxon)
  # merge.
  merge_gfo <- merge_gf %>%
    bind_rows(order)
  # phylum.
  phylum <- df %>%
    anti_join(merge_gfo, by = "id") %>%
    select(id, phylum:family) %>%
    filter(!is.na(phylum))  %>%
    rename(taxon = phylum) %>%
    select(id, taxon)
  # no phylum.
  phylum_no <- df %>%
    anti_join(merge_gfo, by = "id") %>%
    select(id, phylum:family) %>%
    filter(is.na(phylum))  %>%
    rename(taxon = phylum) %>%
    select(id, taxon)
  # merge.
  merge_gfop_na <- merge_gfo %>%
    bind_rows(phylum, phylum_no) %>%
    arrange(id)
  # merge.
  all <- df %>%
    left_join(merge_gfop_na, by = "id") %>%
    group_by(taxon) %>%
    mutate(id2 = row_number(),
           taxon = str_c(taxon, id2, sep = " ")) %>%
    filter(!is.na(taxon))
  return(all)
}
