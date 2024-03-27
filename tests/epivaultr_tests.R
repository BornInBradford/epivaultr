source("R/epivaultr_tools.R")
source("R/epivaultr_getters.R")
source("R/epivaultr_processing.R")

v1 <- read_ev_variables("tests/example_inputs/variables.csv")

v2 <- read_ev_variables("tests/example_inputs/variables.xlsx")

v3 <- read_ev_variables("tests/example_inputs/variables_table.xlsx")

v4 <- read_ev_variables("tests/example_inputs/variables_with_header.csv")

v5 <- read_ev_variables("tests/example_inputs/variables_as_table_with_header.csv")

v6 <- read_ev_variables("tests/example_inputs/variables_as_table_without_header.csv")


options(ev_server = "BHTS-RESRCH22DV")
options(ev_database = "ResearchWarehouse")

con <- ev_connect()

tabs1 <- fetch_ev_meta_tabs(con, v2)
vars1 <- fetch_ev_meta_vars(con, v1)

vars2 <- fetch_ev_meta_vars(con, v2)
cats2 <- fetch_ev_meta_vars(con, v2, cats = TRUE)

vars3 <- fetch_ev_meta_vars(con, v2, visibility = 9)
cats3 <- fetch_ev_meta_vars(con, v2, visibility = 9, cats = TRUE)

ev_disconnect(con)
