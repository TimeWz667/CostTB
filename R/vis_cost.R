#' Visualise the types of costs by age group
#'
#' @param catas 
#' @param scale 
#' @param xlab 
#'
#' @return
#' @export
#'
#' @examples
vis_type_age <- function(catas, scale = 1E3, xlab = "Costs per patient (NTD in thousand)") {
  require(tidyr)
  require(dplyr)
  require(ggplot2)
  
  cas <- tibble::as_tibble(catas$CCs) %>%
    group_by(Sex, AgeGrp) %>%
    summarise(`Medical` = mean(out_med), `Non-Medical` = mean(out_nmed), `Indirect` = mean(out_indir)) %>%
    gather("Type", "Cost", c("Medical", "Non-Medical", "Indirect"))
  
  cas_f <- subset(cas, Sex == "Female")
  cas_m <- subset(cas, Sex == "Male")
  max_f <- max((cas_f %>% group_by(AgeGrp, Sex) %>% summarise(s = sum(Cost)))$s)
  max_m <- max((cas_m %>% group_by(AgeGrp, Sex) %>% summarise(s = sum(Cost)))$s)
  
  ggplot(data = cas, aes(x = AgeGrp)) +
    geom_bar(data = cas_f, aes(y = - Cost, fill = Type), stat = "identity") +
    geom_bar(data = cas_m, aes(y = Cost, fill = Type), stat = "identity") + 
    geom_abline(intercept = 0, slope = 0) +
    scale_y_continuous(xlab, labels = function(x) abs(x) / scale) +
    scale_x_discrete("Age group") + 
    scale_fill_discrete("") +
    coord_flip() +
    labs(subtitle = "Female / Male") +
    theme(legend.position = "bottom")
}


#' @rdname vis_type_agestr
#' @export
vis_type_age_mdr <- function(catas, scale = 1E3, xlab = "Costs per patient (NTD in thousand)") {
  require(tidyr)
  require(dplyr)
  require(ggplot2)
  
  cas <- tibble::as_tibble(catas$CCs) %>%
    group_by(Sex, AgeGrp, MDR) %>%
    summarise(`Medical` = mean(out_med), `Non-Medical` = mean(out_nmed), `Indirect` = mean(out_indir)) %>%
    gather("Type", "Cost", c("Medical", "Non-Medical", "Indirect"))
  
  cas_f <- subset(cas, Sex == "Female")
  cas_m <- subset(cas, Sex == "Male")
  max_f <- max((cas_f %>% group_by(AgeGrp, Sex) %>% summarise(s = sum(Cost)))$s)
  max_m <- max((cas_m %>% group_by(AgeGrp, Sex) %>% summarise(s = sum(Cost)))$s)
  
  ggplot(data = cas, aes(x = AgeGrp)) +
    geom_bar(data = cas_f, aes(y = - Cost, fill = Type), stat = "identity") +
    geom_bar(data = cas_m, aes(y = Cost, fill = Type), stat = "identity") + 
    geom_abline(intercept = 0, slope = 0) +
    scale_y_continuous(xlab, labels = function(x) abs(x) / scale) +
    scale_x_discrete("Age group") + 
    scale_fill_discrete("") +
    facet_wrap(.~MDR, nrow = 2) +
    coord_flip() +
    labs(subtitle = "Female / Male") +
    theme(legend.position = "bottom")
}
