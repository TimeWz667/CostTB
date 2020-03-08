include_guardians <- function(dat) {
  dat$s_c_indirect_current <- dat$s_c_indirect_current + dat$s_c_guard_hosp
  dat$s_c_current <- dat$s_c_current + dat$s_c_guard_hosp
  dat$s_t_current <- dat$s_t_current + dat$s_t_guard_hosp
  
  dat$s_c_indirect_fu <- dat$s_c_indirect_fu + dat$s_c_guard_fu
  dat$s_c_fu <- dat$s_c_fu + dat$s_c_guard_fu
  dat$s_t_fu <- dat$s_t_fu + dat$s_t_guard_fu
  
  dat$c_indirect_dot <- dat$c_indirect_dot + dat$c_guard_dot
  dat$c_dot <- dat$c_dot + dat$c_guard_dot
  dat$t_dot <- dat$t_dot + dat$t_guard_dot
  
  dat
}


#' @export
explt <- function(dat, fn, inc_guard=T) {
  if (inc_guard) {
    dat <- include_guardians(dat)
  }
  
  med <- list()
  
  
  ## DS-TB in the intensive phase
  temp <- subset(dat, phase == 1 & mdr == 0)
  
  ## Before treatment
  med$m_c_med_before_ds <- fn(temp$c_med_before)
  
  med$m_c_travel_before_ds <- fn(temp$c_travel_nmed_before)
  med$m_c_food_before_ds <- fn(temp$c_food_nmed_before)
  med$m_c_nutri_before_ds <- fn(temp$c_nutri_nmed_before)  
  med$m_c_othernmed_before_ds <- fn(temp$c_othernmed_nmed_before)
  med$m_c_nmed_before_ds <- fn(temp$c_nmed_before)
  
  med$m_c_traveltime_before_ds <- fn(temp$c_traveltime_before)
  med$m_c_visittime_before_ds <- fn(temp$c_visittime_before)
  med$m_c_time_before_ds <- fn(temp$c_time_before)
  
  med$m_c_direct_before_ds <- fn(temp$c_direct_before)
  med$m_c_indirect_before_ds <- fn(temp$c_indirect_before)
  med$m_c_before_ds <- fn(temp$c_before)
  
  med$m_t_travel_before_ds <- fn(temp$t_travel_before)
  med$m_t_visit_before_ds <- fn(temp$t_visit_before)
  med$m_t_time_before_ds <- fn(temp$t_time_before)
  med$m_t_before_ds <- fn(temp$t_before)
  
  ## During treatment
  med$m_c_med_after_int_ds <- fn(temp$s_c_med_current) #already scalared
  
  med$m_c_travel_after_int_ds <- fn(temp$s_c_travel_nmed_current)
  med$m_c_food_after_int_ds <- fn(temp$s_c_food_nmed_current)
  med$m_c_nutri_after_int_ds <- fn(temp$s_c_nutri_nmed_current)
  med$m_c_othernmed_after_int_ds <- fn(temp$s_c_othernmed_nmed_current)
  med$m_c_nmed_after_int_ds <- fn(temp$s_c_nmed_current)  #scalared
  
  med$m_c_traveltime_after_int_ds <- fn(temp$s_c_traveltime_current)
  med$m_c_staytime_after_int_ds <- fn(temp$s_c_staytime_current)
  med$m_c_time_after_int_ds <- fn(temp$s_c_time_current)
  
  med$m_c_indirect_after_int_ds <- fn(temp$s_c_indirect_current)
  med$m_c_direct_after_int_ds <- fn(temp$s_c_direct_current)
  med$m_c_after_int_ds <- fn(temp$s_c_current)
  
  med$m_t_travel_after_int_ds <- fn(temp$s_t_travel_current)
  med$m_t_stay_after_int_ds <- fn(temp$s_t_stay_current)
  med$m_t_time_after_int_ds <- fn(temp$s_t_time_current)
  med$m_t_after_int_ds <- fn(temp$s_t_current)
    
  if (inc_guard) {
    med$m_c_guard_after_int_ds <- fn(temp$s_c_guard_hosp)
    med$m_t_guard_after_int_ds <- fn(temp$s_t_guard_hosp)
  }
  
  
  ## Follow-up
  med$m_s_fu_int_ds <- fn(temp$s_fu) 
  
  
  # MDR-TB in the intensive phase
  temp <- subset(dat, phase == 1 & mdr == 1)
  
  ## Before treatment
  med$m_c_med_before_mdr <- fn(temp$c_med_before)
  
  med$m_c_travel_before_mdr <- fn(temp$c_travel_nmed_before)
  med$m_c_food_before_mdr <- fn(temp$c_food_nmed_before)
  med$m_c_nutri_before_mdr <- fn(temp$c_nutri_nmed_before)  
  med$m_c_othernmed_before_mdr <- fn(temp$c_othernmed_nmed_before)
  med$m_c_nmed_before_mdr <- fn(temp$c_nmed_before)
  
  med$m_c_traveltime_before_mdr <- fn(temp$c_traveltime_before)
  med$m_c_visittime_before_mdr <- fn(temp$c_visittime_before)
  med$m_c_time_before_mdr <- fn(temp$c_time_before)
  
  med$m_c_direct_before_mdr <- fn(temp$c_direct_before)
  med$m_c_indirect_before_mdr <- fn(temp$c_indirect_before)
  med$m_c_before_mdr <- fn(temp$c_before)
  
  med$m_t_travel_before_mdr <- fn(temp$t_travel_before)
  med$m_t_visit_before_mdr <- fn(temp$t_visit_before)
  med$m_t_time_before_mdr <- fn(temp$t_time_before)
  med$m_t_before_mdr <- fn(temp$t_before)
  
  ## During treatment
  med$m_c_med_after_int_mdr <- fn(temp$s_c_med_current) #already scalared
  
  med$m_c_travel_after_int_mdr <- fn(temp$s_c_travel_nmed_current)
  med$m_c_food_after_int_mdr <- fn(temp$s_c_food_nmed_current)
  med$m_c_nutri_after_int_mdr <- fn(temp$s_c_nutri_nmed_current)
  med$m_c_othernmed_after_int_mdr <- fn(temp$s_c_othernmed_nmed_current)
  med$m_c_nmed_after_int_mdr <- fn(temp$s_c_nmed_current)  #scalared
  
  med$m_c_traveltime_after_int_mdr <- fn(temp$s_c_traveltime_current)
  med$m_c_staytime_after_int_mdr <- fn(temp$s_c_staytime_current)
  med$m_c_time_after_int_mdr <- fn(temp$s_c_time_current)
  
  med$m_c_indirect_after_int_mdr <- fn(temp$s_c_indirect_current)
  med$m_c_direct_after_int_mdr <- fn(temp$s_c_direct_current)
  med$m_c_after_int_mdr <- fn(temp$s_c_current)
  
  med$m_t_travel_after_int_mdr <- fn(temp$s_t_travel_current)
  med$m_t_stay_after_int_mdr <- fn(temp$s_t_stay_current)
  med$m_t_time_after_int_mdr <- fn(temp$s_t_time_current)
  med$m_t_after_int_mdr <- fn(temp$s_t_current)

  if (inc_guard) {
    med$m_c_guard_after_int_mdr <- fn(temp$s_c_guard_hosp)
    med$m_t_guard_after_int_mdr <- fn(temp$s_t_guard_hosp)
  }
    
  ## Follow-up
  med$m_s_fu_int_mdr <- fn(temp$s_fu)
  
  
  # DS-TB in continuation phase
  temp <- subset(dat, phase == 2 & mdr == 0)

  ## During treatment
  med$m_c_med_after_con_ds <- fn(temp$s_c_med_current) #already scalared
  
  med$m_c_travel_after_con_ds <- fn(temp$s_c_travel_nmed_current)
  med$m_c_food_after_con_ds <- fn(temp$s_c_food_nmed_current)
  med$m_c_nutri_after_con_ds <- fn(temp$s_c_nutri_nmed_current)
  med$m_c_othernmed_after_con_ds <- fn(temp$s_c_othernmed_nmed_current)
  med$m_c_nmed_after_con_ds <- fn(temp$s_c_nmed_current)  #scalared
  
  med$m_c_traveltime_after_con_ds <- fn(temp$s_c_traveltime_current)
  med$m_c_staytime_after_con_ds <- fn(temp$s_c_staytime_current)
  med$m_c_time_after_con_ds <- fn(temp$s_c_time_current)
  
  med$m_c_indirect_after_con_ds <- fn(temp$s_c_indirect_current)
  med$m_c_direct_after_con_ds <- fn(temp$s_c_direct_current)
  med$m_c_after_con_ds <- fn(temp$s_c_current)
  
  med$m_t_travel_after_con_ds <- fn(temp$s_t_travel_current)
  med$m_t_stay_after_con_ds <- fn(temp$s_t_stay_current)
  med$m_t_time_after_con_ds <- fn(temp$s_t_time_current)
  med$m_t_after_con_ds <- fn(temp$s_t_current)
  
  if (inc_guard) {
    med$m_c_guard_after_con_ds <- fn(temp$s_c_guard_hosp)
    med$m_t_guard_after_con_ds <- fn(temp$s_t_guard_hosp)
  }
  
  ## Follow-up
  med$m_s_fu_con_ds <- fn(temp$s_fu) 
  
  
  # MDR-TB in continuation phase
  temp <- subset(dat, phase == 2 & mdr == 1)
  
  ## During treatment
  med$m_c_med_after_con_mdr <- fn(temp$s_c_med_current) #already scalared
  
  med$m_c_travel_after_con_mdr <- fn(temp$s_c_travel_nmed_current)
  med$m_c_food_after_con_mdr <- fn(temp$s_c_food_nmed_current)
  med$m_c_nutri_after_con_mdr <- fn(temp$s_c_nutri_nmed_current)
  med$m_c_othernmed_after_con_mdr <- fn(temp$s_c_othernmed_nmed_current)
  med$m_c_nmed_after_con_mdr <- fn(temp$s_c_nmed_current)
  
  med$m_c_traveltime_after_con_mdr <- fn(temp$s_c_traveltime_current)
  med$m_c_staytime_after_con_mdr <- fn(temp$s_c_staytime_current)
  med$m_c_time_after_con_mdr <- fn(temp$s_c_time_current)
  
  med$m_c_indirect_after_con_mdr <- fn(temp$s_c_indirect_current)
  med$m_c_direct_after_con_mdr <- fn(temp$s_c_direct_current)
  med$m_c_after_con_mdr <- fn(temp$s_c_current)
  
  med$m_t_travel_after_con_mdr <- fn(temp$s_t_travel_current)
  med$m_t_stay_after_con_mdr <- fn(temp$s_t_stay_current)
  med$m_t_time_after_con_mdr <- fn(temp$s_t_time_current)
  med$m_t_after_con_mdr <- fn(temp$s_t_current)
  
  if (inc_guard) {
    med$m_c_guard_after_con_mdr <- fn(temp$s_c_guard_hosp)
    med$m_t_guard_after_con_mdr <- fn(temp$s_t_guard_hosp)
  }
  
  ## Follow-up
  med$m_s_fu_con_mdr <- fn(temp$s_fu)
  
  nm <- names(med)[is.na(med)]
  for (n in nm) {
    med[[n]] <- 0
  }
  med
}
