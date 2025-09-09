exp_types <- d_enc %>%
  mutate(
    nesttreatment_year = paste(nest_treatment, exp_year, sep = "_"),
    individualtreatment_year = paste(individual_treatment, exp_year, sep = "_")) %>%
  select(nesttreatment_year, individualtreatment_year)

table1 <- table(exp_types$nesttreatment_year)
table2 <- table(exp_types$individualtreatment_year)
write.csv(table1, "nesttreatment.csv")
write.csv(table2, "indtreatment.csv")