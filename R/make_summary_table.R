#' Make summary table
#'
#' This function make summary tabel.
#' @param .id siz002
#' @param .funguild funguild
#' @param .seq clustered.fasta
#' @param .sample_info sample information
#' @param .unite UNITE DB or overall_genus
#' @param .remove remove size.
#' @export
make_summary_table = function(
  .id, .funguild, .seq, .sample_info, .unite = T,
  .remove = T)
{
  # load funguild analysis
  funguild <- .funguild %>%
    rename(otu = OTU)

  # load representative sequences.
  seq <- .seq

  sample_info <- .sample_info

  # remove size=*.
  if(.remove == T) {
    seq_clean <- seq %>%
      mutate(otu = str_remove(OTU, ";size=\\d+")) %>%
      select(-OTU)
  } else {
    seq_clean <- seq
  }

  # replace 1 to 0 and merge.
  funguild_seq <- funguild %>%
    mutate(across(starts_with(.id), ~if_else(.== 1, 0, .))) %>%
    left_join(seq_clean, by = "otu")

  # calculate sum of OTUs and sequences.
  funguild_seq_sum <- funguild_seq %>%
    rowwise(otu) %>%
    mutate(sequence_per_otu = sum(c_across(starts_with(.id))))

  funguild_seq_sum

  # calculate number of OTU per sample, number of sequences per sample.
  otu_seq_num <- funguild_seq %>%
    pivot_longer(starts_with(.id), names_to = "samplename", values_to = "seq_num") %>%
    mutate(otu_num = if_else(seq_num > 0, 1, 0)) %>%
    group_by(samplename) %>%
    summarise(otu_per_sample = sum(otu_num),
              sequence_per_sample = sum(seq_num))
  #> `summarise()` ungrouping output (override with `.groups` argument)

  otu_seq_num

  left <- otu_seq_num %>%
    left_join(sample_info, by ="samplename") %>%
    arrange(sample_info)

  if(.unite == T) {
    pattern <- left$samplename
    taxon <- c("phylum", "class",
               "order", "family",
               "genus")
    body <- funguild_seq_sum %>%
      relocate(all_of(pattern)) %>%
      separate(taxonomy, into = taxon, sep = ";") %>%
      arrange(phylum, class,
              order, family,
              genus)
  } else {
    pattern <- left$samplename
    taxon <- c("superkingdom", "kingdom", "subkingdom",
               "phylum", "class", "subclass",
               "order", "suborder", "family",
               "subfamily", "tribe", "subtribe",
               "genus", "species")

    body <- funguild_seq_sum %>%
      relocate(all_of(pattern)) %>%
      separate(taxonomy, into = taxon, sep = ";") %>%
      arrange(superkingdom, kingdom, subkingdom,
              phylum, class, subclass,
              order, suborder, family,
              subfamily, tribe, subtribe,
              genus, species)
  }
  rlt <- list(body, left)
  return(rlt)
}
