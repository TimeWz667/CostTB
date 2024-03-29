

find_timing <- function(ccs, thres = 0.2) {
  cata_t_int <- with(ccs, {
    ti <- (c_before - thres * i_before) / (thres * rate_i_int - rate_c_int)
    ti[is.na(ti)] <- 0
    ti[ti < 0] <- NA
    ti <- ifelse(ti < dur_int, ti, NA)
    ti
  })
  
  cata_t_con <- with(ccs, {
    ti <- (c_int - thres * i_int) / (thres * rate_i_con - rate_c_con)
    ti[is.na(ti)] <- 0
    ti[ti < 0] <- NA
    ti <- ifelse(ti < dur_con, ti + dur_int, NA)
    ti
  })
  
  cata_before <- (ccs$c_before / ccs$i_before) > thres
  cata_before[is.na(cata_before)] <- T
  
  ifelse(cata_before, 0, 
         ifelse(
           is.na(cata_t_int), 
           cata_t_con,
           cata_t_int
         ))
}


calc_pct_ti <- function(ccs, ti, max_value=2) {
  if (ti <= 0) {
    pct <- with(ccs, {
      c_before / i_before
    })
  } else {
    ti <- pmin(ti, ccs$dur_int + ccs$dur_con)
    
    pct <- ifelse(
      ti <= ccs$dur_int,
      with(ccs, {
        (c_before + rate_c_int * ti) / (i_before + rate_i_int * ti)
      }),
      with(ccs, {
        (c_int + rate_c_con * ti) / (i_int + rate_i_con * ti)
      })
    )
  } 
  
  pct[is.na(pct)] <- max_value
  pct <- pmax(pmin(pct, max_value), 0)
  pct
}


calc_pct_pr <- function(ccs, phase, pr=NA, max_value=2) {
  pr <- max(min(pr, 1), 0)
  
  if (phase == 0) {
    pct <- with(ccs, { c_before / i_before })
  } else if (phase == 1){
    pct <- with(ccs, {
      ti <- pr * dur_int
      (c_before + rate_c_int * ti) / (i_before + rate_i_int * ti)
    }) 
  } else {
    pct <- with(ccs, {
      ti <- pr * dur_con
      (c_int + rate_c_con * ti) / (i_int + rate_i_con * ti)
    })
  }
  
  pct[is.na(pct)] <- max_value
  pct <- pmax(pmin(pct, max_value), 0)
  pct
}



calc_pct_matrix_ti <- function(ccs, max_value = 2) {
  ti_max <- max(ccs$dur)
  
  times <- seq(0, ti_max, 0.25)
  res <- matrix(0, length(times), nrow(ccs))
  
  for (i in 1:length(times)) {
    ti <- times[i]
    res[i, ] <- calc_pct_ti(ccs, ti, max_value=max_value)
  }
  colnames(res) <- ccs$X_id
  rownames(res) <- times
  res <- data.frame(as.table(res))
  colnames(res) <- c("Time", "X_id", "burden")
  res$Time <- as.numeric(as.character(res$Time))
  res <- merge(res, ccs[c("X_id", "MDR")])
  return(res)
}


calc_pct_matrix_pr <- function(ccs, max_value = 2) {
  prs <- seq(0, 1, 0.05)
  
  res <- matrix(0, length(prs) * 2 - 1, nrow(ccs))
  
  res[1, ] <- calc_pct_pr(ccs, 0, max_value = max_value)
  
  for (i in 2:length(prs)) {
    res[i, ] <- calc_pct_pr(ccs, phase = 1, pr = prs[i], max_value = max_value)
    res[i + length(prs) - 1, ] <- calc_pct_pr(ccs, phase = 2, pr = prs[i], max_value = max_value)
  }
  
  colnames(res) <- ccs$X_id
  rownames(res) <- c(prs, 1 + prs[-1])
  res <- data.frame(as.table(res))
  colnames(res) <- c("Pr", "X_id", "burden")
  res$Pr <- as.numeric(as.character(res$Pr))
  res <- merge(res, ccs[c("X_id", "MDR")])
  return(res)
}


#' Calculate Time to Catastrophic Costs
#'
#' @param cs an object of Costs
#' @param sp option for specifying social protection
#' @param sp_sen proportion of social protection are TB-sensitive
#' @param indirect option for specifying indiriect cost
#' @param indirect_hc weight of indiriect cost in human capital
#' @param threshold threshold of catastrophic costs
#'
#' @return an object of TimeToCataCosts
#' @export
#'
#' @examples
calc_time_to_cc <- function(cs, 
                            sp=c("none", "all spe", "all sen", "prop", "explicit"), sp_sen=1,
                            indirect=c("hc", "dinc", "prop"), indirect_hc=1, 
                            threshold=0.2) {
  
  res <- list(costs=cs)
  
  if (threshold < 0) stop("Threshold should larger than zero")
  res$Threshold <- threshold 
  
  res$SocialProtection <- sp <- match.arg(sp)
  if (sp == "prop") sp_sen <- min(max(sp_sen, 0), 1)
  
  res$IndirectCost <- indirect <- match.arg(indirect)
  if (indirect == "prop") indirect_hc <- min(max(indirect_hc, 0), 1)
  
  ccs <- cs$source[c("X_id", "MDR", "Sex", "AgeGrp")]
  
  
  sp_sen <- switch(sp, none = 0, 
                   prop=sp_sen * cs$source$c_spvoucher, 
                   "all spe" = 0,
                   "all sen" = cs$source$c_spvoucher, 
                   explicit = cs$source$c_sp_sen)
  
  sp_spe <- switch(sp, none = 0, 
                   prop=(1 - sp_sen) * cs$source$c_spvoucher, 
                   "all spe" = cs$source$c_spvoucher,
                   "all sen" = 0, 
                   explicit = cs$source$c_sp_spe)
  
  ccs$dur_int <- cs$source$treat_duration_int
  ccs$dur_con <- cs$source$treat_duration_con
  ccs$dur <- dur <- ccs$dur_int + ccs$dur_con
  
  ccs$c_before <- cs$costs_phase$c_before
  ccs$i_before <- cs$source$income_hh_pre_annual
  
  idir_int <- switch(indirect,
                     hc = cs$costs_phase$c_indirect_int, 
                     dinc = cs$source$income_diff * ccs$dur_int / dur,
                     prop = indirect_hc * cs$costs_phase$c_indirect_int + 
                       (1-indirect_hc) * cs$source$income_diff * ccs$dur_int / dur) 
  out_int <- cs$costs_phase$c_med_int + cs$costs_phase$c_nmed_int + idir_int
  ccs$rate_c_int <- out_int / ccs$dur_int - sp_spe / dur 
  ccs$rate_i_int <- sp_sen / dur
  ccs$c_int <- ccs$c_before + out_int - sp_spe * ccs$dur_int / dur
  ccs$i_int <- ccs$i_before + sp_sen * ccs$dur_int / dur
  
  idir_con <- switch(indirect,
                     hc = cs$costs_phase$c_indirect_con, 
                     dinc = cs$source$income_diff * ccs$dur_con / dur,
                     prop = indirect_hc * cs$costs_phase$c_indirect_con + 
                       (1-indirect_hc) * cs$source$income_diff * ccs$dur_con / dur) 
  out_con <- cs$costs_phase$c_med_con + cs$costs_phase$c_nmed_con + idir_con
  ccs$rate_c_con <- out_con / ccs$dur_con - sp_spe / dur
  ccs$rate_i_con <- sp_sen / dur
  ccs$c_all <- ccs$c_before + out_int + out_con - sp_spe
  ccs$i_all <- ccs$i_before + sp_sen
  
  ccs$has_cc <- (ccs$c_all / ccs$i_all > threshold) + 0 
  
  ccs$time_to_cc <- find_timing(ccs, thres = threshold)
  ccs$prog_to_cc <- ifelse(is.na(ccs$time_to_cc), "None", 
                           ifelse(ccs$time_to_cc == 0, "Before", 
                                  ifelse(ccs$time_to_cc < ccs$dur_int, "Int", "Con"))
  )
  res$CCs <- ccs
  
  surv <- ccs[c("X_id", "MDR", "Sex", "AgeGrp", "time_to_cc")]
  surv$event <- (!is.na(surv$time_to_cc)) + 0
  surv$time <- ifelse(surv$event, surv$time_to_cc, dur)
  res$Survival <- surv
  
  res$BurdenByTime <- calc_pct_matrix_ti(ccs, 2)
  res$BurdenByPr <- calc_pct_matrix_pr(ccs, 2)
  
  class(res) <- "TimeToCataCosts"
  return(res)
}


#' @rdname calc_time_to_cc
#' @export
summary.TimeToCataCosts <- function(ttcc) {
  ttcc <- TTCC
  when <- ttcc$CCs
  
  
  tab <- cbind(table(when$prog_to_cc), table(when$prog_to_cc, when$MDR))
  tab <- tab[c("Before", "Int", "Con"),]
  tab <- rbind(c(nrow(when), table(when$MDR)), tab)
  tab <- t(tab)
  colnames(tab) <- c("N", "N_Before", "N_Int", "N_Con")
  rownames(tab) <- NULL
  
  summary_when <- data.frame(Group=c("All", "DS", "MDR"))
  summary_when <- cbind(summary_when, tab)
  summary_when$N_All <- rowSums(summary_when[, c("N_Before", "N_Int", "N_Con")])
  
  summary_when$P_Before <- summary_when$N_Before / summary_when$N
  summary_when$P_Int <- summary_when$N_Int / summary_when$N
  summary_when$P_Con <- summary_when$N_Con / summary_when$N
  summary_when$P_All <- summary_when$N_All / summary_when$N
  res <- list(tab = summary_when)
  class(res) <- "summaryTimeToCataCosts"
  return(res)
}


#' @rdname calc_time_to_cc
#' @export
print.summaryTimeToCataCosts <- function(res) {
  print(res$tab)
}

