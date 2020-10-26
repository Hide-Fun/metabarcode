#' Make summary table
#'
#' This function make summary tabel.
#' @param .id siz002
#' @param .funguild funguild
#' @param .seq clustered.fasta
#' @param .sample_info sample information
#' @param .tax_pat UNITE DB or overall_genus or another. If you specify "manual"
#' @param .taxon manual
#' @param .remove remove size.
#' @export
#' @examples
#' identify_list <- data.frame(
#' otu = c("otu1", "otu2"),
#' phylum = c("p1", "p2"),
#' order = c("o1", "o2"),
#' family = c("f1", "f2"),
#' genus = c("g1", "g2")
#' )
#'
#' otu_table <- data.frame(
#'   samplename = c("id_sample1", "id_sample2"),
#'   otu1 = c(1, 0),
#'   otu2 = c(0, 39)
#' )
#'
#' seq <- data.frame(
#'   otu = c("otu1", "otu2"),
#'   seq = c("seq1", "seq2")
#' )
#'
#' sample_info <- data.frame(
#'   samplename = c("id_sample1", "id_sample2"),
#'   sample_info = c("sample_info1", "sample_info2")
#' )
#'
#'
#' summary <- make_summary_table(
#'   .id = "id",
#'   .funguild = funguild_db(otu_table, identify_list, c("phylum", "order", "family", "genus")),
#'   .seq = seq,
#'   .tax_pat = "manual",
#'   .taxon = c("phylum", "order", "family", "genus"),
#'   .sample_info = sample_info,
#'   .remove = FALSE
#' )
#'
#' summary[[1]]
#' summary[[2]]
make_summary_table <- function(
  .id, .funguild, .seq, .sample_info, .tax_pat = "unite",
  .taxon, .remove = T)
{
  # load funguild analysis

  if("OTU" %in% colnames(.funguild)) {
    .funguild <- .funguild %>%
      dplyr::rename(otu = OTU)
  }

  if("OTU" %in% colnames(.seq)) {
    .seq <- .seq %>%
      dplyr::rename(otu = OTU)
  }

  # load representative sequences.
  seq <- .seq

  funguild <- .funguild

  sample_info <- .sample_info

  # remove size=*.
  if(.remove == T) {
    seq_clean <- seq %>%
      dplyr::mutate(otu = stringr::str_remove(otu, ";size=\\d+")) %>%
      dplyr::select(-otu)
  } else {
    seq_clean <- seq
  }

  # replace 1 to 0 and merge.
  funguild_seq <- funguild %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with(.id), ~dplyr::if_else(.== 1, 0, .))) %>%
    dplyr::left_join(seq_clean, by = "otu")

  # calculate sum of otus and sequences.
  funguild_seq_sum <- funguild_seq %>%
    dplyr::rowwise(otu) %>%
    dplyr::mutate(sequence_per_otu = sum(dplyr::c_across(dplyr::starts_with(.id))))

  funguild_seq_sum

  # calculate number of otu per sample, number of sequences per sample.
  otu_seq_num <- funguild_seq %>%
    tidyr::pivot_longer(starts_with(.id), names_to = "samplename", values_to = "seq_num") %>%
    dplyr::mutate(otu_num = dplyr::if_else(seq_num > 0, 1, 0)) %>%
    dplyr::group_by(samplename) %>%
    dplyr::summarise(otu_per_sample = sum(otu_num),
                     sequence_per_sample = sum(seq_num))
  #> `summarise()` ungrouping output (override with `.groups` argument)

  otu_seq_num

  left <- otu_seq_num %>%
    dplyr::left_join(sample_info, by ="samplename") %>%
    dplyr::arrange(sample_info)

  if(.tax_pat == "unite") {
    pattern <- left$samplename
    taxon <- c("phylum", "class",
               "order", "family",
               "genus")
    body <- funguild_seq_sum %>%
      dplyr::relocate(dplyr::all_of(pattern)) %>%
      tidyr::separate(taxonomy, into = taxon, sep = ";") %>%
      dplyr::arrange(phylum, class,
              order, family,
              genus)
  } else if(.tax_pat == "default") {
    pattern <- left$samplename
    taxon <- c("superkingdom", "kingdom", "subkingdom",
               "phylum", "class", "subclass",
               "order", "suborder", "family",
               "subfamily", "tribe", "subtribe",
               "genus", "species")
    body <- funguild_seq_sum %>%
      dplyr::relocate(dplyr::all_of(pattern)) %>%
      tidyr::separate(taxonomy, into = taxon, sep = ";") %>%
      dplyr::arrange(superkingdom, kingdom, subkingdom,
                     phylum, class, subclass,
                     order, suborder, family,
                     subfamily, tribe, subtribe,
                     genus, species)

  } else if(.tax_pat == "manual") {
    pattern <- left$samplename
    taxon <- .taxon
    body <- funguild_seq_sum %>%
      dplyr::relocate(dplyr::all_of(pattern)) %>%
      tidyr::separate(taxonomy, into = taxon, sep = ";") %>%
      dplyr::arrange(!!!rlang::syms(.taxon))
  } else {
    stop("You should set .tax_pat correct!!")
  }
  rlt <- list(body, left)
  return(rlt)
}



