## code to prepare `DATASET` dataset goes here

usethis::use_data("DATASET")


usethis::use_data(gp_type, gp_c_type, gp_t_type,
                  gp_source, gp_c_source, gp_t_source,
                  gp_phase, gp_c_phase, gp_t_phase)

Survey <- pseudo
Survey$MDR <- ifelse(Survey$mdr, "MDR", "DS")
usethis::use_data(Survey, overwrite = T)
