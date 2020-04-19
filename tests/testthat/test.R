set_formulas(
  .first_expl_var = "x",
  .other_expl_var = LETTERS[1:10],
  .resp_var = "y",
  .interaction = F
)

test_mat <- matrix(c(1, 2, 3, 4, 5, 6),
                   nrow = 2, ncol = 3,
                   byrow = TRUE)
test_mat_iris <- iris[1:4,] %>%
  mutate(row = str_c(Species, "_", 1:4)) %>%
  column_to_rownames("row") %>%
  as.matrix()

matrix_to_tbl(test_mat, .row = F)
t2 <- matrix_to_tbl(test_mat_iris, .row = T, .rowname = "row")
