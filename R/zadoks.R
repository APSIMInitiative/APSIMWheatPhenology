# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   02:45 PM Thursday, 27 March 2014
# * Copyright: AS IS
# *


#' Calculate the Zadoks and APSIM stages for Wheat Module 
#' 
#' @export
zadoks <- function(stages)
{
#     # target <- seq(1, 100)
#     map_zadoks <- c(30.0, 40.0, 65.0, 71.0, 87.0, 90.0, 100.0)
#     map_stages <- c(4.9, 5.4, 6.0, 7.0, 8.0, 9.0, 10.0)
#     values <- interpolationFunction(map_zadoks, map_stages, target)

#     
#     pos <- target <= 5
#     values[pos] <- 1 + target[pos] / 5
#     pos <- target > 5 & target <= 10
#     values[pos] <- 2 + (target[pos] - 5) / 5
#     pos <- target > 10 & target <= 30
#     values[pos] <- NA
#     return(data.frame(stages = values, zadoks = target))
#     
    map_zadoks <- c(30.0, 40.0, 65.0, 71.0, 87.0, 90.0, 100.0)
    map_stages <- c(4.9, 5.4, 6.0, 7.0, 8.0, 9.0, 10.0)
    values <- interpolationFunction(map_stages, map_zadoks, stages)
    values[stages < 4.9] <- NA
    values
}

