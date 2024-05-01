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

simple1 <- ev_simple_fetch(con, "BiB_CohortInfo", "person_info")
simple2 <- ev_simple_fetch(con, "BiB_CohortInfo", "person_info", visibility = 9)

simple3 <- ev_simple_fetch(con, "BiB_CohortInfo", "person_info", variables = c("BiBPersonID", "ParticipantType", "DateOfBirth", "PregnancyID"))
simple4 <- ev_simple_fetch(con, "BiB_CohortInfo", "person_info", visibility = 9,
                           variables = c("BiBPersonID", "ParticipantType", "DateOfBirth"))
simple5 <- ev_simple_fetch(con, "BiBBS_Baseline", "pregnancy_survey", variables = c("adm_medqage", "mes1*"))

simple6 <- ev_simple_fetch(con, "BiB_Metabolomics", "metms_pairm_r")
simple7 <- ev_simple_fetch(con, "BiB_Metabolomics", "metms_pairm_r", variables = c("BiBPersonID", "BiBPregNumber", "metmsr42370", "metmsr531"))

tabs1 <- fetch_ev_meta_tabs(con, v2)
vars1 <- fetch_ev_meta_vars(con, v1)

vars2 <- fetch_ev_meta_vars(con, v2)
cats2 <- fetch_ev_meta_vars(con, v2, cats = TRUE)

vars3 <- fetch_ev_meta_vars(con, v2, visibility = 9)
cats3 <- fetch_ev_meta_vars(con, v2, visibility = 9, cats = TRUE)

dat1 <- fetch_ev_data(con, v2)
dat2 <- fetch_ev_data(con, v2, visibility = 9)

write_ev_data(dat2, 
              path = "H:/MyDocuments/R/dev",
              name = "ev_data_test",
              format = "stata",
              metadata = TRUE)

write_ev_data(dat2, 
              path = "H:/MyDocuments/R/dev",
              name = "ev_data_test",
              format = "csv",
              metadata = TRUE)

ev_disconnect(con)
