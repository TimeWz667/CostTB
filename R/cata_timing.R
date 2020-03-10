

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
  dur <- ccs$dur_int + ccs$dur_con
  
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


calculate_pct_pr <- function(cs, phase, pr=NA, max_value=2) {
  pr <- max(min(pr, 1), 0)
  
  if (phase == 0) {
    pct <- with(cs, {
      c_before / income_hh_now_annual
    })
  } else if (phase == 1){
    pct <- with(cs, {
      cb <- c_before
      dc <- c_int
      ds <- c_spvoucher * treat_duration_int / treat_duration
      i <- income_hh_now_annual
      
      (cb + dc * pr) / (i + ds * pr)
    }) 
  } else {
    pct <- with(cs, {
      cb <- c_before + c_int
      dc <- c_con
      ds <- c_spvoucher * treat_duration_cont / treat_duration
      i <- income_hh_now_annual + c_spvoucher * treat_duration_int / treat_duration

      (cb + dc * pr) / (i + ds * pr)
    })
  }
  
  pct[is.na(pct)] <- max_value
  pct <- pmin(pct, max_value)
  pct
}


calculate_pct_matrix_ti <- function(cs, thres=0.2, max_value=2) {
  ti_max <- max(cs$treat_duration)
  
  ti <- 0
  res <- data.frame(who=cs$X_id, 
                    MDR=ifelse(cs$mdr==1, "MDR", "DS"),
                    When=0,
                    pct=calculate_pct_ti(cs, 0, max_value=max_value))
  
  for (ti in seq(0, ti_max, 0.25)[-1]) {
    res <- rbind(res, 
                 data.frame(who=cs$X_id, 
                            MDR=ifelse(cs$mdr==1, "MDR", "DS"),
                            When=ti,
                            pct=calculate_pct_ti(cs, ti, max_value=max_value)))
  }
  
  prop_all <- tapply(res$pct > thres, res$When, mean, na.rm=T)
  prop_all <- data.frame(When=as.numeric(names(prop_all)), Prop=prop_all, MDR="All")
  prop <- prop_all[order(prop_all$When), ] 
  
  prop_gp <- tapply(res$pct > thres, list(res$When, res$MDR), mean, na.rm=T)
  prop_ds <- data.frame(When=as.numeric(names(prop_gp[, 1])), Prop=prop_gp[, 1], MDR="DS")
  prop <- rbind(prop, prop_ds[order(prop_ds$When), ]) 
  prop_mdr <- data.frame(When=as.numeric(names(prop_gp[, 2])), Prop=prop_gp[, 2], MDR="MDR")
  prop <- rbind(prop, prop_mdr[order(prop_mdr$When), ]) 
  
  list(
    TS=res,
    Prop=prop
  )
}


calculate_pct_matrix_pr <- function(cs, thres=0.2, max_value=2) {
  res <- data.frame(who=cs$X_id, 
                    MDR=ifelse(cs$mdr==1, "MDR", "DS"),
                    When=0,
                    pct=calculate_pct_pr(cs, 0, max_value=max_value))
  
  for (pr in seq(0, 1, 0.05)[-1]) {
    res <- rbind(res, 
                 data.frame(who=cs$X_id, 
                            MDR=ifelse(cs$mdr==1, "MDR", "DS"),
                            When=pr,
                            pct=calculate_pct_pr(cs, phase=1, pr=pr, max_value=max_value)))
  }
  
  for (pr in seq(0, 1, 0.05)[-1]) {
    res <- rbind(res, 
                 data.frame(who=cs$X_id, 
                            MDR=ifelse(cs$mdr==1, "MDR", "DS"),
                            When=pr + 1,
                            pct=calculate_pct_pr(cs, phase=2, pr=pr, max_value=max_value)))
  }
  
  prop_all <- tapply(res$pct > thres, res$When, mean, na.rm=T)
  prop_all <- data.frame(When=as.numeric(names(prop_all)), Prop=prop_all, MDR="All")
  prop <- prop_all[order(prop_all$When), ] 
  
  prop_gp <- tapply(res$pct > thres, list(res$When, res$MDR), mean, na.rm=T)
  prop_ds <- data.frame(When=as.numeric(names(prop_gp[, 1])), Prop=prop_gp[, 1], MDR="DS")
  prop <- rbind(prop, prop_ds[order(prop_ds$When), ]) 
  prop_mdr <- data.frame(When=as.numeric(names(prop_gp[, 2])), Prop=prop_gp[, 2], MDR="MDR")
  prop <- rbind(prop, prop_mdr[order(prop_mdr$When), ]) 
  
  list(
    TS=res,
    Prop=prop
  )
}

