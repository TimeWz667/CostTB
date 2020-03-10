#' Formulate survey data to a cost object
#'
#' @param dat source data
#' @param inc_guard include guardian cost or not
#' @param explt.fn function for expolation if data missed, median as default
#' @param adjust_anchor the key layer of cost structure
#'
#' @return
#' @export
#'
#' @examples
#' 
as_costs <- function(dat, inc_guard=T, explt.fn="Median", adjust_anchor="G3") {
  if (class(explt.fn) != "function") {
    if (explt.fn == "Median") {
      explt.fn <- function(x) median(x, na.rm=T)
    } else if (explt.fn == "Mean") {
      explt.fn <- function(x) mean(x, na.rm=T)
    } else if (is.numeric(explt.fn)) {
      trim <- min(max(explt.fn, 0), 1)
      explt.fn <- function(x) mean(x, trim = trim / 2, na.rm = T)
    } else {
      stop("Can find the matched function for extrapolation")
    }
  }
  
  med <- explt(dat, explt.fn, inc_guard=inc_guard)
  
  costs <- specify_costs(dat, med, inc_guard)
  
  if (is.na(adjust_anchor)) {
    css <- fill_bottom_up(costs, gp_source, inc_guard=inc_guard)
  } else {
    adj_anchor <- ifelse(is.numeric(adjust_anchor), 
                         colnames(gp_source)[adjust_anchor], 
                         adjust_anchor)
    css <- adjust_anchor(costs, gp_source, anchor=adjust_anchor, inc_guard=inc_guard)
  }
  
  cst <- fill_bottom_up(css, gp_type, inc_guard=inc_guard)
  csp <- fill_bottom_up(css, gp_phase, inc_guard=inc_guard)
  
  res <- list(
    source = dat,
    costs_raw = costs,
    costs_source = css,
    costs_type = cst,
    costs_phase = csp,
    inc_guard = inc_guard,
    explts = med,
    explt.fn = explt.fn
  )
  
  class(res) <- "Costs"
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

#' Calculate catastrophic costs
#'
#' @param cs an object of Costs
#' @param sp option for specifying social protection
#' @param sp_sen proportion of social protection are TB-sensitive
#' @param indirect option for specifying indiriect cost
#' @param indirect_hc weight of indiriect cost in human capital
#' @param hhi use annual household income or that of treatment period
#' @param threshold threshold of catastrophic costs
#'
#' @return an object of CataCosts concluding catastrophic costs
#' @export
#'
#' @examples
calc_cc <- function(cs, 
                    sp=c("none", "all spe", "all sen", "prop", "explicit"), sp_sen=1,
                    indirect=c("hc", "dinc", "prop"), indirect_hc=1, 
                    hhi=c("annual", "treatment"), threshold=0.2) {
  
  res <- list(costs=cs)
  
  if (threshold < 0) stop("Threshold should larger than zero")
  res$Threshold <- threshold 
  
  res$SocialProtection <- sp <- match.arg(sp)
  if (sp == "prop") sp_sen <- min(max(sp_sen, 0), 1)
  
  res$IndirectCost <- indirect <- match.arg(indirect)
  if (indirect == "prop") indirect_hc <- min(max(indirect_hc, 0), 1)
  
  res$IncomeBasis <- hhi <- match.arg(hhi)
  
  ccs <- cs$source[c("X_id", "MDR", "Sex", "AgeGrp")]
  
  ccs$sp_sen <- switch(sp, none = 0, 
                       prop=sp_sen * cs$source$c_spvoucher, 
                       "all spe" = 0,
                       "all sen" = cs$source$c_spvoucher, 
                       explicit = cs$source$c_sp_sen)
  
  ccs$sp_spe <- switch(sp, none = 0, 
                       prop=(1 - sp_sen) * cs$source$c_spvoucher, 
                       "all spe" = cs$source$c_spvoucher,
                       "all sen" = 0, 
                       explicit = cs$source$c_sp_spe)
  
  ccs$out_med <- cs$costs_type$c_med
  ccs$out_nmed <- cs$costs_type$c_nmed
  ccs$out_dir <- ccs$out_med + ccs$out_nmed
  ccs$out_indir <- switch(indirect,
                          hc = cs$costs_type$c_indirect, 
                          dinc = cs$source$income_diff,
                          prop = indirect_hc * cs$costs_type$c_indirect + 
                          (1-indirect_hc) * cs$source$income_diff) 
    
  ccs$outcome <- ccs$out_dir + ccs$out_indir - ccs$sp_spe
  if (hhi == "treatment") {
    ccs$in_hhi <- cs$source$income_hh_pre_annual * cs$source$treat_duration / 12
  } else {
    ccs$in_hhi <- cs$source$income_hh_pre_annual
  }

  ccs$income <- ccs$in_hhi + ccs$sp_sen 
  ccs$burden <- ccs$outcome / ccs$income
  ccs$has_cc <- cc <- (ccs$burden >= threshold) + 0
  
  ccs$add_sp_sen <- pmax(ccs$outcome / threshold - ccs$income, 0)
  ccs$add_sp_spe <- pmax(ccs$outcome - threshold * ccs$income, 0)
  
  res$CCs <- ccs
  
  res$Summary <- c("N.All" = sum(cc), 
                   "P.All" = mean(cc),
                   "P.DS" = mean(cc[ccs$mdr==0]),
                   "P.MDR" = mean(cc[ccs$mdr==1]))

  class(res) <- "CataCosts"
  
  return(res)
}


#' @rdname calc_cc
#' @export
summary.CataCosts <- function(ccs) {
  ccs <- ccs$CCs
  cc <- ccs$has_cc
  res <- list()
  res$Summary <- c("N.All" = sum(cc), 
    "P.All" = mean(cc),
    "P.DS" = mean(cc[ccs$mdr==0]),
    "P.MDR" = mean(cc[ccs$mdr==1]))
  
  class(res) <- "summaryCataCosts"
  return(res)
}


#' @rdname calc_cc
#' @export
print.summaryCataCosts <- function(obj) {
  print(res$Summary)
} 


