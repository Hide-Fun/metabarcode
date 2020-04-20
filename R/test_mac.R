test_make_index <- make_index("data/SampleSheet-1.csv", 18, "Sample_ID", "index", "index2")

test_fasta <- read_csv("index_1.fasta", col_names = F)

test_ft <- fasta_to_tibble(test_fasta)
tibble_to_fasta(test_ft)
