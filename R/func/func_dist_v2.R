##name of script: func_dist_v2.R
#cat("version_number= ",v_nr,"\n")
#purpose: correction of midpoints which represent line segments
#function used in: "sequence of lines.R"
#author: Joachim Höhle
#GNU General Public License (GPL)

dist_v2 <- function(i, b_angle_df, all_PC) { #calculation of vector with distances
  np2 <<- length(all_PC[[i]]$idx)
  x_dist <- rep(NA, np2)
  j1 <- 1
  
  while (j1 <= np2) {
    dx <- all_PC[[i]]$x[j1] - all_PC[[i]]$x[1]
    dy <- all_PC[[i]]$y[j1] - all_PC[[i]]$y[1]
    x_dist[j1] <- sqrt(dx^2 + dy^2)
    j1 <- j1 + 1
  } #end loop j1
  
  return(x_dist)
} #end function

#end of script "func_dist_v2.R"
