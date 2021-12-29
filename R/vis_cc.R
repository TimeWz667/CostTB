#' Visualise catastropoic patient costs by age and sex
#'
#' @param catas A CataCosts object
#' @param by size of breaks of population size
#'
#' @return age pyramid of cases with catastrophic patient costs
#' @export
#'
#' @examples
#' Costs <- as_costs(Survey)
#' Catas <- calc_cc(Costs, sp = "all spe", indirect = "hc")
#' 
#' vis_cc_age(Catas)
#' vis_cc_age_mdr(Catas)
#' 
vis_cc_age <- function(catas, by = 50) {
  require(tidyr)
  require(dplyr)
  require(ggplot2)
  cas <- tibble::as_tibble(catas$CCs) %>%
    group_by(Sex, AgeGrp) %>%
    summarise(N = dplyr::n(), CC = sum(has_cc), Pr = round(CC / N * 100))
  
  mx <- (round(max(cas$N) / by) + 1) * by
  breaks <- seq(0, mx, by)
  breaks <- c(rev(-breaks), breaks[-1])
  
  cas_f <- subset(cas, Sex == "Female")
  cas_m <- subset(cas, Sex == "Male")
  
  ggplot(data = cas, aes(x = AgeGrp)) +
    geom_bar(data = cas_f, aes(y = - N, fill = "Female"), alpha = 0.7, stat = "identity") +
    geom_bar(data = cas_f, aes(y = - CC, fill = "CC"), width = 0.6, stat = "identity") +
    geom_text(data = cas_f, aes(y = - CC, label = paste0(Pr, "%")), hjust = 1) +
    geom_bar(data = cas_m, aes(y = N, fill = "Male"), alpha = 0.7, stat = "identity") +
    geom_bar(data = cas_m, aes(y = CC, fill = "CC"), width = 0.6, stat = "identity") +
    geom_text(data = cas_m, aes(y = CC, label = paste0(Pr, "%")), hjust = 0) +
    geom_abline(intercept = 0, slope = 0) +
    scale_y_continuous("TB patients", breaks = breaks, labels = abs(breaks)) +
    scale_x_discrete("Age group") + 
    scale_fill_discrete("") +
    coord_flip() +
    theme(legend.position = "bottom")
}


#' @rdname vis_cc_agestr
#' @export
vis_cc_age_mdr <- function(catas, by = 50) {
  require(tidyr)
  require(dplyr)
  require(ggplot2)
  cas <- tibble::as_tibble(catas$CCs) %>%
    group_by(Sex, AgeGrp, MDR) %>%
    summarise(N = dplyr::n(), CC = sum(has_cc), Pr = round(CC / N * 100))
  
  mx <- (round(max(cas$N) / by) + 1) * by
  breaks <- seq(0, mx, by)
  breaks <- c(rev(-breaks), breaks[-1])
  
  cas_f <- subset(cas, Sex == "Female")
  cas_m <- subset(cas, Sex == "Male")
  
  ggplot(data = cas, aes(x = AgeGrp)) +
    geom_bar(data = cas_f, aes(y = - N, fill = "Female"), alpha = 0.7, stat = "identity") +
    geom_bar(data = cas_f, aes(y = - CC, fill = "CC"), width = 0.6, stat = "identity") +
    geom_text(data = cas_f %>% filter(Pr > 1), aes(y = - CC, label = paste0(Pr, "%")), hjust = 1) +
    geom_bar(data = cas_m, aes(y = N, fill = "Male"), alpha = 0.7, stat = "identity") +
    geom_bar(data = cas_m, aes(y = CC, fill = "CC"), width = 0.6, stat = "identity") +
    geom_text(data = cas_m %>% filter(Pr > 1), aes(y = CC, label = paste0(Pr, "%")), hjust = 0) +
    geom_abline(intercept = 0, slope = 0) +
    scale_y_continuous("TB patients", breaks = breaks, labels = abs(breaks)) +
    scale_x_discrete("Age group") + 
    scale_fill_discrete("") +
    facet_wrap(.~MDR, nrow = 2) +
    coord_flip() +
    theme(legend.position = "bottom")
}
