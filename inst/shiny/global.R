analyze_motility <- function(data, cutoff){

  #calculates average velocity
  velocity_average <- data %>%
    dplyr::filter(Distance >= cutoff) %>%
    summarize(average_velocity = mean(AvgSpeed),
              moving_filaments = n())

  # setDT(data)
  # velocity_average <- data[Distance >= cutoff, .(moving_filaments = .N,
  #                                                average_velocity = mean(AvgSpeed))

  #counts total number of filaments
  all_filament <- data %>%
    summarize(total_filaments = n())
 #data[, .N]

  #calculate percent moving
 right_join(velocity_average, all_filament) %>%
    mutate(percent_moving = moving_filaments/total_filaments) %>%
    replace_na(list(average_velocity = 0, moving_filaments = 0, percent_moving = 0))


}


#get the mean function (drm self-starters) in the 'drc' package
drc_mean_fun <- drc::getMeanFunctions()
mean_fun_abrv <- sapply(drc_mean_fun, "[[", 1)
mean_fun_desc <- lapply(drc_mean_fun, "[[", 2)
names(mean_fun_desc) <- mean_fun_abrv

#get the built-in datasets in the 'drc' package
datasets <- as.data.frame(data(package = "drc")$results)
datasets$Item <- as.character(datasets$Item)


#the 'aomisc' package has additional mean function/self-starts for drm()
aomisc_starters <- lsf.str("package:aomisc")
is_drc <- str_detect(aomisc_starters, "DRC.")
aomisc_starters <- aomisc_starters[which(is_drc == TRUE)]

#get 'biophysr' drc starters
motr_starters <- lsf.str("package:motr")
is_drc2 <- str_detect(motr_starters, "drc")
motr_starters <- motr_starters[which(is_drc2 == TRUE)]

#combine names of all self-starters
all_drc_starters <- list(motr =  motr_starters,
                         aomisc = aomisc_starters,
                         drc = mean_fun_abrv)

#make id generator function
generate_id <- function(x, data, input){
  paste0(x, seq_along(unique(data[[input]])))
}

