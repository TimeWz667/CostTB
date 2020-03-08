specify_costs <- function(dat, med, inc_guard=T) {
  if (inc_guard) {
    dat <- include_guardians(dat)
  }
  
  dat[is.na(dat)] <- 0
  wage <- dat$hourly_wage
  wage[is.na(wage)] <- 0
  c_wage <- dat$caregiver_wage
  c_wage[is.na(c_wage)] <- 0
  
  int_ds <- dat$phase == 1 & dat$mdr == 0
  int_mdr <- dat$phase == 1 & dat$mdr == 1
  con_ds <- dat$phase == 2 & dat$mdr == 0
  con_mdr <- dat$phase == 2 & dat$mdr == 1
  
  
  res <- dat[c("X_id", "phase", "mdr", "income_hh_pre_annual", "income_diff", "c_spvoucher")]
  
  res$c_med_before <- dat$c_med_before
  
  res$c_travel_before <- dat$c_travel_nmed_before
  res$c_food_before <- dat$c_food_nmed_before
  res$c_nutri_before <- dat$c_nutri_nmed_before
  res$c_othernmed_before <- dat$c_othernmed_nmed_before
  res$c_nmed_before <- dat$c_nmed_before
  
  res$c_traveltime_before <- dat$c_traveltime_before
  res$c_visittime_before <- dat$c_visittime_before
  res$c_time_before <- dat$c_time_before
  
  res$c_direct_before <- dat$c_direct_before
  res$c_indirect_before <- dat$c_indirect_before
  res$c_before <- dat$c_before
  
  res$t_travel_before <- dat$t_travel_before
  res$t_visit_before <- dat$t_visit_before
  res$t_time_before <- dat$t_time_before
  res$t_before <- dat$t_before
  
  
  for (col in c(
    "c_med_before",
    "c_travel_before", "c_food_before", "c_nutri_before", "c_othernmed_before", "c_nmed_before",
    "c_direct_before", "c_indirect_before", "c_before",
    "t_travel_before", "t_visit_before", "t_time_before", "t_before"
  )) {
    res[con_ds, col] <- med[[paste0("m_", col, "_ds")]]
    res[con_mdr, col] <- med[[paste0("m_", col, "_mdr")]]
  }
  
  res[con_ds, "c_traveltime_before"] <- med$m_t_travel_before_ds * wage[con_ds]
  res[con_mdr, "c_traveltime_before"] <- med$m_t_travel_before_mdr * wage[con_mdr]
  res[con_ds, "c_visittime_before"] <- med$m_t_visit_before_ds * wage[con_ds]
  res[con_mdr, "c_visittime_before"] <- med$m_t_visit_before_mdr * wage[con_mdr]
  res[con_ds, "c_time_before"] <- med$m_t_before_ds * wage[con_ds]
  res[con_mdr, "c_time_before"] <- med$m_t_before_mdr * wage[con_mdr]
  
  ## After treatment (inpatient)
  
  res$c_med_after_int <- dat$s_c_med_current
  
  res$c_travel_after_int <- dat$s_c_travel_nmed_current
  res$c_food_after_int <- dat$s_c_food_nmed_current
  res$c_nutri_after_int <- dat$s_c_nutri_nmed_current
  res$c_othernmed_after_int <- dat$s_c_othernmed_nmed_current
  res$c_nmed_after_int <- dat$s_c_nmed_current
  
  res$c_traveltime_after_int <- dat$s_c_traveltime_current
  res$c_staytime_after_int <- dat$s_c_staytime_current
  res$c_time_after_int <- dat$s_c_time_current
  
  res$c_indirect_after_int <- dat$s_c_indirect_current
  res$c_direct_after_int <- dat$s_c_direct_current
  res$c_after_int <- dat$s_c_current
  
  res$t_travel_after_int <- dat$s_t_travel_current
  res$t_stay_after_int <- dat$s_t_stay_current
  res$t_time_after_int <- dat$s_t_time_current
  res$t_after_int <- dat$s_t_current
  
  
  for (col in c(
    "c_med_after_int",
    "c_travel_after_int", "c_food_after_int", "c_nutri_after_int", "c_othernmed_after_int", "c_nmed_after_int", 
    "c_indirect_after_int", "c_direct_after_int", "c_after_int",
    "t_travel_after_int", "t_stay_after_int", "t_time_after_int", "t_after_int"
  )) {
    res[con_ds, col] <- med[[paste0("m_", col, "_ds")]]
    res[con_mdr, col] <- med[[paste0("m_", col, "_mdr")]]
  }
  
  res[con_ds, "c_traveltime_after_int"] <- med$m_t_travel_after_int_ds * wage[con_ds]
  res[con_mdr, "c_traveltime_after_int"] <- med$m_t_travel_after_int_mdr * wage[con_mdr]
  res[con_ds, "c_staytime_after_int"] <- med$m_t_stay_after_int_ds * wage[con_ds]
  res[con_mdr, "c_staytime_after_int"] <- med$m_t_stay_after_int_mdr * wage[con_mdr]
  res[con_ds, "c_time_after_int"] <- med$m_t_time_after_int_ds * wage[con_ds]
  res[con_mdr, "c_time_after_int"] <- med$m_t_time_after_int_mdr * wage[con_mdr]
  
  if (inc_guard) {
    res$c_guard_after_int <- dat$s_c_guard_hosp
    res$t_guard_after_int <- dat$s_t_guard_hosp
    
    res[con_ds, "c_guard_after_int"] <- med$m_t_guard_after_int_ds * c_wage[con_ds]
    res[con_mdr, "c_guard_after_int"] <- med$m_t_guard_after_int_mdr * c_wage[con_mdr]
    res[con_ds, "t_guard_after_int"] <- med$m_t_guard_after_int_ds
    res[con_mdr, "t_guard_after_int"] <- med$m_t_guard_after_int_mdr
  }
  
  
  res$c_med_after_con <- dat$s_c_med_current
  
  res$c_travel_after_con <- dat$s_c_travel_nmed_current
  res$c_food_after_con <- dat$s_c_food_nmed_current
  res$c_nutri_after_con <- dat$s_c_nutri_nmed_current
  res$c_othernmed_after_con <- dat$s_c_othernmed_nmed_current
  res$c_nmed_after_con <- dat$s_c_nmed_current
  
  res$c_traveltime_after_con <- dat$s_c_traveltime_current
  res$c_staytime_after_con <- dat$s_c_staytime_current
  res$c_time_after_con <- dat$s_c_time_current
  
  res$c_indirect_after_con <- dat$s_c_indirect_current
  res$c_direct_after_con <- dat$s_c_direct_current
  res$c_after_con <- dat$s_c_current
  
  res$t_travel_after_con <- dat$s_t_travel_current
  res$t_stay_after_con <- dat$s_t_stay_current
  res$t_time_after_con <- dat$s_t_time_current
  res$t_after_con <- dat$s_t_current
  
  
  for (col in c(
    "c_med_after_con",
    "c_travel_after_con", "c_food_after_con", "c_nutri_after_con", "c_othernmed_after_con", "c_nmed_after_con", 
    "c_direct_after_con", "c_indirect_after_con", "c_after_con",
    "t_travel_after_con", "t_stay_after_con", "t_time_after_con", "t_after_con"
  )) {
    res[int_ds, col] <- med[[paste0("m_", col, "_ds")]]
    res[int_mdr, col] <- med[[paste0("m_", col, "_mdr")]]
  }
  
  res[int_ds, "c_traveltime_after_con"] <- med$m_t_travel_after_con_ds * wage[int_ds]
  res[int_mdr, "c_traveltime_after_con"] <- med$m_t_travel_after_con_mdr * wage[int_mdr]
  res[int_ds, "c_staytime_after_con"] <- med$m_t_stay_after_con_ds * wage[int_ds]
  res[int_mdr, "c_staytime_after_con"] <- med$m_t_stay_after_con_mdr * wage[int_mdr]
  res[int_ds, "c_time_after_con"] <- med$m_t_time_after_con_ds * wage[int_ds]
  res[int_mdr, "c_time_after_con"] <- med$m_t_time_after_con_mdr * wage[int_mdr]
  
  if (inc_guard) {
    res$c_guard_after_con <- dat$s_c_guard_hosp
    res$t_guard_after_con <- dat$s_t_guard_hosp
    res[int_ds, "c_guard_after_con"] <- med$m_t_guard_after_con_ds * c_wage[int_ds]
    res[int_mdr, "c_guard_after_con"] <- med$m_t_guard_after_con_mdr * c_wage[int_mdr]
    res[int_ds, "t_guard_after_con"] <- med$m_t_guard_after_con_ds
    res[int_mdr, "t_guard_after_con"] <- med$m_t_guard_after_con_mdr
  }
  
  ## Follow-up
  wts_int <- rep(1, nrow(dat))
  wts_int[con_ds] <- med$m_s_fu_int_ds / dat$s_fu[con_ds]
  wts_int[con_mdr] <- med$m_s_fu_int_mdr / dat$s_fu[con_mdr]
  
  res$c_med_fu_int <- dat$s_c_med_fu * wts_int
  
  res$c_travel_fu_int <- dat$s_c_travel_nmed_fu * wts_int
  res$c_othernmed_fu_int <- dat$s_c_othernmed_nmed_fu * wts_int
  res$c_nmed_fu_int <- dat$s_c_nmed_fu * wts_int

  res$c_time_fu_int <- dat$s_c_time_fu * wts_int
  res$c_indirect_fu_int <- dat$s_c_indirect_fu * wts_int
  res$c_direct_fu_int <- dat$s_c_direct_fu * wts_int
  res$c_fu_int <- dat$s_c_fu * wts_int
  
  res$t_time_fu_int <- dat$s_t_time_fu * wts_int
  res$t_fu_int <- dat$s_t_fu * wts_int
  
  if (inc_guard) {
    res$c_guard_fu_int <- dat$s_c_guard_fu * wts_int
    res$t_guard_fu_int <- dat$s_t_guard_fu * wts_int
  }
  
  
  wts_con <- rep(1, nrow(dat))
  wts_con[int_ds] <- med$m_s_fu_con_ds / dat$s_fu[int_ds]
  wts_con[int_mdr] <- med$m_s_fu_con_mdr / dat$s_fu[int_mdr]
  
  res$c_med_fu_con <- dat$s_c_med_fu * wts_con
  
  res$c_travel_fu_con <- dat$s_c_travel_nmed_fu * wts_con
  res$c_othernmed_fu_con <- dat$s_c_othernmed_nmed_fu * wts_con
  res$c_nmed_fu_con <- dat$s_c_nmed_fu * wts_con
  
  res$c_time_fu_con <- dat$s_c_time_fu * wts_con
  res$c_indirect_fu_con <- dat$s_c_indirect_fu * wts_con
  res$c_direct_fu_con <- dat$s_c_direct_fu * wts_con
  res$c_fu_con <- dat$s_c_fu * wts_con
  
  res$t_time_fu_con <- dat$s_t_time_fu * wts_con
  res$t_fu_con <- dat$s_t_fu * wts_con
  
  if (inc_guard) {
    res$c_guard_fu_con <- dat$s_c_guard_fu * wts_con
    res$t_guard_fu_con <- dat$s_t_guard_fu * wts_con
  }
  
  ## DOTS
  
  dur <- dat$dur_dot_int + dat$dur_dot_con
  p_int <- dat$dur_dot_int / dur
  p_con <- dat$dur_dot_con / dur
  
  res$c_med_dot_int <- dat$c_med_dot * p_int
  
  res$c_travel_dot_int <- dat$c_travel_nmed_dot * p_int
  res$c_food_dot_int <- dat$c_food_nmed_dot * p_int
  res$c_nmed_dot_int <- dat$c_nmed_dot * p_int
  
  res$c_time_dot_int <- dat$c_time_dot * p_int
  res$c_indirect_dot_int <- dat$c_indirect_dot * p_int
  res$c_direct_dot_int <- dat$c_direct_dot * p_int
  res$c_dot_int<- dat$c_dot * p_int
  
  res$t_time_dot_int <- dat$t_time_dot * p_int
  res$t_dot_int <- dat$t_dot * p_int
  
  if (inc_guard) {
    res$c_guard_dot_int <- dat$c_guard_dot * p_int
    res$t_guard_dot_int <- dat$t_guard_dot * p_int
  }
  
  res$c_med_dot_con <- dat$c_med_dot * p_con
  
  res$c_travel_dot_con <- dat$c_travel_nmed_dot * p_con
  res$c_food_dot_con <- dat$c_food_nmed_dot * p_con
  res$c_nmed_dot_con <- dat$c_nmed_dot * p_con
  
  res$c_time_dot_con <- dat$c_time_dot * p_con
  res$c_indirect_dot_con <- dat$c_indirect_dot * p_con
  res$c_direct_dot_con <- dat$c_direct_dot * p_con
  res$c_dot_con <- dat$c_dot * p_con
  
  res$t_time_dot_con <- dat$t_time_dot * p_con
  res$t_dot_con <- dat$t_dot * p_con
  
  if (inc_guard) {
    res$c_guard_dot_con <- dat$c_guard_dot * p_con
    res$t_guard_dot_con <- dat$t_guard_dot * p_con
  }
  
  res$c_med_dot <- dat$c_med_dot
  
  res$c_travel_dot <- dat$c_travel_nmed_dot
  res$c_food_dot <- dat$c_food_nmed_dot
  res$c_nmed_dot <- dat$c_nmed_dot
  
  res$c_time_dot <- dat$c_time_dot
  res$c_indirect_dot <- dat$c_indirect_dot
  res$c_direct_dot <- dat$c_direct_dot
  res$c_dot <- dat$c_dot
  
  res$t_time_dot <- dat$t_time_dot
  res$t_dot <- dat$t_dot 
  
  if (inc_guard) {
    res$c_guard_dot <- dat$c_guard_dot
    res$t_guard_dot <- dat$t_guard_dot
  }
  
  
  ## Validate indirect costs
  
  if (inc_guard) {
    res$c_indirect_before[con_ds | con_mdr] <- res$c_time_before[con_ds | con_mdr] 
    res$c_before[con_ds | con_mdr] <- res$c_indirect_before[con_ds | con_mdr] + res$c_direct_before[con_ds | con_mdr]
    
    res$c_indirect_after_int[con_ds | con_mdr] <- res$c_time_after_int[con_ds | con_mdr] + res$c_guard_after_int[con_ds | con_mdr]
    res$c_after_int[con_ds | con_mdr] <- res$c_indirect_after_int[con_ds | con_mdr] + res$c_direct_after_int[con_ds | con_mdr]
    
    res$c_indirect_after_con[int_ds | int_mdr] <- res$c_time_after_con[int_ds | int_mdr] + res$c_guard_after_con[int_ds | int_mdr]
    res$c_after_con[int_ds | int_mdr] <- res$c_indirect_after_con[int_ds | int_mdr] + res$c_direct_after_con[int_ds | int_mdr]
  }
   
  res$c_caregiver <- dat$c_guard_tot
  res$t_caregiver <- dat$t_guard_tot
  
  ## Food costs
  
  dur <- dat$treat_duration_int + dat$treat_duration_con
  p_int <- dat$treat_duration_int / dat$treat_duration
  p_con <- dat$treat_duration_con / dat$treat_duration
  
  res$c_foodnutri_ext_int <- dat$c_food_nutri * p_int
  res$c_foodadd_ext_int <- dat$c_food_add * p_int
  res$c_food_ext_int <- sum_selected(res, c("c_foodnutri_ext_int", "c_foodadd_ext_int")) * p_int
  
  res$c_foodnutri_ext_con <- dat$c_food_nutri * p_con
  res$c_foodadd_ext_con <- dat$c_food_add * p_con
  res$c_food_ext_con <- sum_selected(res, c("c_foodnutri_ext_con", "c_foodadd_ext_con")) * p_con
  
  res[is.na(res)] <- 0
  
  res
}
