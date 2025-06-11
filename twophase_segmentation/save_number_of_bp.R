library(tidyr)

nb_of_bp_df <- bind_rows(readRDS("results/whole_jul_2024/nb_of_bp_c57_f.rds"),
                         readRDS("results/whole_jul_2024/nb_of_bp_c57_m.rds"),
                         readRDS("results/whole_jul_2024/nb_of_bp_akrj_f.rds"))

write_csv(nb_of_bp_df, file = "results/whole_jul_2024/number_of_breakpoints.csv")
