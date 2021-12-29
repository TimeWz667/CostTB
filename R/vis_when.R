#' Visualise time to catastropoic patient costs
#'
#' @param ttcc A TimeToCataCosts object
#' @param fill fill stacked bar to one or not
#' @param ylab label for y axis
#'
#' @return stacked bar plot of CC occurance
#' @export
#'
#' @examples
#' Costs <- as_costs(Survey)
#' TTCC <- calc_time_to_cc(Costs, sp = "all spe", indirect = "hc")
#' 
#' vis_cc_phase(TTCC, fill = F)
#' vis_cc_phase(TTCC, fill = T)
#'
vis_cc_phase <- function(ttcc, fill = F, ylab = "Patients with catastrophic costs (%)") {
  require(tidyr)
  require(ggplot2)
  
  dat <- tibble::as_tibble(summary(TTCC)$tab) %>%
    gather(Phase, P, c(P_Before, P_Int, P_Con))
  
  
  g <- ggplot(data=dat) + 
    scale_y_continuous(ylab, limits=c(0, 1), labels=scales::percent) +
    scale_x_discrete("Type of TB") +
    scale_fill_discrete("Phase", label=c("Before treatment", "Intensive", "Continuation")) + 
    theme(legend.position="bottom")
  
  if (fill) {
    g <- g + geom_bar(aes(x=Group, y=P, fill=Phase), 
                      width=0.8, stat="identity", position=position_fill(reverse=T))
  } else {
    g <- g + geom_bar(aes(x=Group, y=P, fill=Phase), 
                      width=0.8, stat="identity", position=position_stack(reverse=T))
  }                 
  
  return(g)
}


#' @rdname vis_cc_phase
#' @export
vis_acc_by_time <- function(ttcc, xlab = "Time since treatment initiation (month)", 
                            ylab = "Proportaion of patients with catastrophic costs (%)") {
  require(tidyr)
  require(dplyr)
  require(ggplot2)
  
  thres <- ttcc$Threshold
  burden <- ttcc$BurdenByTime
  
  tab_gp <- tibble::as_tibble(burden) %>%
    group_by(Time, MDR) %>%
    summarise(Pr = mean(burden > thres))
  
  tab <- tibble::as_tibble(burden) %>%
    group_by(Time) %>%
    summarise(Pr = mean(burden > thres))
  
  ggplot(data = tab_gp) + 
    geom_line(aes(x = Time, y = Pr, colour = MDR)) +
    geom_line(data = tab, aes(x = Time, y = Pr, colour = "All")) +
    scale_y_continuous(ylab, 
                       limits = c(0, 1), labels = scales::percent,
                       breaks = seq(0, 1, by = 0.2)) +
    scale_x_continuous(xlab) + 
    guides(colour=guide_legend(title = "TB Type")) + 
    theme(legend.position="bottom") 
}


#' @rdname vis_cc_phase
#' @export
vis_acc_by_pr <- function(ttcc, xlab = "Time since treatment initiation (month)", 
                          ylab = "Percentage of time in each phase") {
  require(tidyr)
  require(dplyr)
  require(ggplot2)
  
  thres <- ttcc$Threshold
  burden <- ttcc$BurdenByPr
  
  tab_gp <- tibble::as_tibble(burden) %>%
    group_by(Pr, MDR) %>%
    summarise(Prop = mean(burden > thres))
  
  tab <- tibble::as_tibble(burden) %>%
    group_by(Pr) %>%
    summarise(Prop = mean(burden > thres))
  
  ggplot(data = tab_gp) +
    geom_line(aes(x = Pr, y = Prop, colour = MDR)) +
    geom_line(data = tab, aes(x = Pr, y = Prop, colour = "All")) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    annotate(geom = "text", x = 0.5, y = 0.95, label = "Intensive\nphase") +
    annotate(geom = "text", x = 1.5, y = 0.95, label = "Continuation\nphase") +
    scale_y_continuous(
      "Proportaion of patients with catastrophic costs (%)",
      limits = c(0, 1),
      labels = scales::percent,
      breaks = seq(0, 1, by = 0.2)
    ) +
    scale_x_continuous("Percentage of time in each phase") +
    guides(colour = guide_legend(title = "TB Type")) +
    theme(legend.position = "bottom")
  
}


#' @rdname vis_cc_phase
#' @export
vis_ts_by_time <- function(ttcc, xlab = "Time since treatment initiation (month)", 
                           ylab1 = "Proportaion of patients with catastrophic costs (%)",
                           ylab2 = "Patient costs over 0.2 annuel household income") {
  require(tidyr)
  require(dplyr)
  require(ggplot2)
  
  thres <- ttcc$Threshold
  burden <- ttcc$BurdenByTime
  
  tab_gp <- tibble::as_tibble(burden) %>%
    group_by(Time, MDR) %>%
    summarise(Prop = mean(burden > thres))
  
  tab <- tibble::as_tibble(burden) %>%
    group_by(Time) %>%
    summarise(Prop = mean(burden > thres))

  tab$MDR = "All"
  tab <- dplyr::bind_rows(list(tab, tab_gp))
  
  ggplot(data = burden) + 
    geom_line(aes(x = Time, y = burden, group = X_id), alpha=0.1) +
    geom_line(data = tab, aes(x = Time, y = Prop, colour = "Proportion", size = 1.5)) +
    facet_wrap(~MDR) +
    scale_y_continuous(ylab1, 
                       limits = c(0, 1), labels=scales::percent,
                       breaks = seq(0, 1, by =0.2),
                       sec.axis = sec_axis(~ ., name = ylab2, 
                                           breaks=seq(0, 1, by = 0.2))) +
    scale_x_continuous(xlab) + 
    theme(legend.position="none") 
}


#' @rdname vis_cc_phase
#' @export
vis_ts_by_pr <- function(ttcc, xlab = "Percentage of time in each phase", 
                         ylab1 = "Proportaion of patients with catastrophic costs (%)",
                         ylab2 = "Patient costs over 0.2 annuel household income") {
  require(tidyr)
  require(dplyr)
  require(ggplot2)
  
  thres <- ttcc$Threshold
  burden <- ttcc$BurdenByPr
  
  tab_gp <- tibble::as_tibble(burden) %>%
    group_by(Pr, MDR) %>%
    summarise(Prop = mean(burden > thres))
  
  tab <- tibble::as_tibble(burden) %>%
    group_by(Pr) %>%
    summarise(Prop = mean(burden > thres))
  tab$MDR = "All"
  tab <- dplyr::bind_rows(list(tab, tab_gp))
  
  
  ggplot(data = burden) + 
    geom_line(aes(x = Pr, y= burden, group = X_id), alpha=0.1) +
    geom_line(data = tab, aes(x = Pr, y = Prop, colour = "Proportion", size = 1.5)) +
    annotate(geom = "text", x = 0.5, y = 0.95, label = "Intensive\nphase") +
    annotate(geom = "text", x = 1.5, y = 0.95, label = "Continuation\nphase") + 
    geom_vline(xintercept = 1, linetype="dashed") +
    facet_wrap(~MDR) +
    scale_y_continuous(ylab1, 
                       limits = c(0, 1), labels=scales::percent,
                       breaks = seq(0, 1, by=0.2),
                       sec.axis = sec_axis(~ ., name = ylab2, 
                                          breaks=seq(0, 1, by = 0.2))) +
    scale_x_continuous(xlab) + 
    theme(legend.position="none")
}
