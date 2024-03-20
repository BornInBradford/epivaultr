source("R/epivaultr_tools.R")
source("R/epivaultr_getters.R")
source("R/epivaultr_processing.R")

v1 <- ev_read_variables("tests/example_inputs/variables.csv")

v2 <- ev_read_variables("tests/example_inputs/variables.xlsx")

v3 <- ev_read_variables("tests/example_inputs/variables_table.xlsx")

v4 <- ev_read_variables("tests/example_inputs/variables_with_header.csv")

v5 <- ev_read_variables("tests/example_inputs/variables_as_table_with_header.csv")

v6 <- ev_read_variables("tests/example_inputs/variables_as_table_without_header.csv")
