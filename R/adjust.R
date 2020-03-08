fill_bottom_up <- function(dat, gp, inc_guard=T) {
  if (!inc_guard) {
    gp <- gp[!startsWith(gp[, 1], "c_guard"), ]
    gp <- gp[!startsWith(gp[, 1], "t_guard"), ]
  }
  
  lvs <- names(gp)
  lvs <- lvs[startsWith(lvs, "G")]
  
  for (i in 1:(length(lvs)-1)){
    g1 <- gp[, i]
    g2 <- gp[, i+1]
    g2_names <- unique(g2)
    
    for (g_t in g2_names) {
      g_fs <- unique(g1[g2 == g_t])
      if (!(g_t %in% names(dat))) {
        ss <- sum_selected(dat, g_fs)
        ss[is.na(ss)] <- 0
        dat[, g_t] <- ss
      }
    }
  }
  dat[, c("X_id", unique(c(as.matrix(gp))))]
}


adjust_bottom_up <- function(dat, gp, inc_guard=T) {
  if (!inc_guard) {
    gp <- gp[!startsWith(gp[, 1], "c_guard"), ]
    gp <- gp[!startsWith(gp[, 1], "t_guard"), ]
  }
  
  lvs <- names(gp)
  lvs <- lvs[startsWith(lvs, "G")]
  
  for (i in 1:(length(lvs)-1)){
    g1 <- gp[, i]
    g2 <- gp[, i+1]
    g2_names <- unique(g2)
    
    for (g_t in g2_names) {
      g_fs <- unique(g1[g2 == g_t])
      if (any(g_fs != g_t)) {
        dat[g_t] <- sum_selected(dat, g_fs)
        dat[is.na(dat[g_t]), g_t] <- 0
      }
    }
  }
  dat
}


adjust_top_down <- function(dat, gp, inc_guard=T) {
  if (!inc_guard) {
    gp <- gp[!startsWith(gp[, 1], "c_guard"), ]
    gp <- gp[!startsWith(gp[, 1], "t_guard"), ]
  }
  
  lvs <- names(gp)
  lvs <- lvs[startsWith(lvs, "G")]
  
  for (i in 1:(length(lvs)-1)){
    g1 <- gp[, i]
    g2 <- gp[, i+1]
    g2_names <- unique(g2)
    
    for (g_t in g2_names) {
      g_fs <- unique(g1[g2 == g_t])
      if (!(g_t %in% names(dat))) {
        ss <- sum_selected(dat, g_fs)
        ss[is.na(ss)] <- 0
        dat[, g_t] <- ss
      }
    }
  }
  
  for (i in (length(lvs)-1):1){
    g1 <- gp[, i]
    g2 <- gp[, i+1]
    g2_names <- unique(g2)
    
    for (g_t in g2_names) {
      g_fs <- unique(g1[g2 == g_t])

      if (length(g_fs) == 1) {
        if (any(g_fs != g_t)) {
          dat[g_fs] <- dat[g_t]
        }
      } else {
        rat <- sum_selected(dat, g_fs)
        rat[is.na(rat)] <- 0
        rat <- dat[, g_t] / rat
        rat[is.na(rat)] <- 0
        for (g_f in g_fs) {
          dat[, g_f] <- ifelse(rat > 0, dat[, g_f] * rat, 0) 
        }
      }
    }
  }
  dat
}


adjust_anchor <- function(dat, gp, anchor="G3", inc_guard=T) {
  if (!inc_guard) {
    gp <- gp[!startsWith(gp[, 1], "c_guard"), ]
    gp <- gp[!startsWith(gp[, 1], "t_guard"), ]
  }
  
  lvs <- names(gp)
  lvs <- lvs[startsWith(lvs, "G")]
  n_lvs <- length(lvs)
  
  i_anc <- which(lvs == anchor)
  i_anc <- min(max(i_anc, 1), n_lvs)
  
  if (i_anc > 1) {
    # Forward fill-up, ensuring every variable exists
    dat <- adjust_top_down(dat, gp[1:i_anc], inc_guard=T)
  }
  
  if (i_anc < n_lvs) {
    # Forward adjustment
    dat <- adjust_bottom_up(dat, gp[i_anc:n_lvs], inc_guard=T)
  }  
  dat[, c("X_id", unique(c(as.matrix(gp))))]
} 
