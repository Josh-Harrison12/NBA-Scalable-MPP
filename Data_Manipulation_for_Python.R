###Loading and Separating Data
library(readr)
ModelData <- read_csv("ModelData.csv")
mod_pbp_2022_season <- ModelData[which(ModelData$season==2022 & ModelData$seasont==2),]
mod_pbp_2023_season <- ModelData[which(ModelData$season==2023 & ModelData$seasont==2),]

MD_noReb <- ModelData[-which(ModelData$ptype==4 | ModelData$ptype==25),]
#Need to adjust Marks so that we have max as 39
MD_noReb$ptype[which(MD_noReb$ptype>4 & MD_noReb$ptype<25)] <- MD_noReb$ptype[which(MD_noReb$ptype>4 & MD_noReb$ptype<25)] - 1
MD_noReb$ptype[which(MD_noReb$ptype>25)] <- MD_noReb$ptype[which(MD_noReb$ptype>25)] - 2

#
mod_pbp_2022_season <- MD_noReb[which(MD_noReb$season==2022 & MD_noReb$seasont==2),]
mod_pbp_2023_season <- MD_noReb[which(MD_noReb$season==2023 & MD_noReb$seasont==2),]

###Validation Set Creator
ValSetMaker <- function(mod_pbp_season_spec){
  games_list <- split(mod_pbp_season_spec, mod_pbp_season_spec$game_id)
  g_by_date <- games_list[order(sapply(games_list, function(df) min(df$game_date)))]
  val_games <- list()
  limbo <- list()
  Gameweeks <- list()
  for(i in 1:2){
    Gameweeks[[paste0("g_", i)]] <- list()
  }
  num_assigned <- 0
  while(num_assigned < 30){
    n <- length(g_by_date)
    g_oi <- as.data.frame(g_by_date[[n]])
    g_by_date <- g_by_date[-n]
    t_involved <- unique(g_oi[,5])
    hteam <- t_involved[1]
    ateam <- t_involved[2]
    Assigned <- FALSE
    for(j in 1:2){
      in_gw <- any(unlist(sapply(Gameweeks[[j]],function(x) (t_involved %in% unique(x[,5])))))
      if(!in_gw){
        Gameweeks[[j]][[length(Gameweeks[[j]])+1]] <- g_oi
        val_games[[length(val_games)+1]] <- g_oi
        Assigned = TRUE
        num_assigned = num_assigned+1
        break
      }
    }
    if(Assigned == FALSE){
      limbo[[length(limbo)+1]] <- g_oi
    }
  }
  polished_data <- unlist(list(g_by_date,limbo), recursive = FALSE)
  polished_data <- lapply(polished_data, function(x) as.data.frame(x))
  g_by_date <- polished_data[order(sapply(polished_data, function(df) min(df$game_date)))]
  g_by_date <- unlist(list(g_by_date,val_games), recursive = FALSE)
  return(g_by_date)
}
g_by_date_2022 <- ValSetMaker(mod_pbp_2022_season)
g_by_date_2023 <- ValSetMaker(mod_pbp_2023_season)

###Split Into Quarters
QuarterMaker <- function(to_split){
  all_sequences <- list()
  for(i in to_split){
    interevent <- rep(0,dim(i)[1])
    i <- cbind(i,interevent)
    i$interevent <- c(0,diff(i$EventTimes))
    i$period[which(i$period>4)] <- 4
    quarters <- split(i, i$period)
    if(length(quarters)>4){print(i[which(!(i$period %in% c(1:4))),'period'])}
    if((length(quarters)<4) & (length(quarters) >2)){
      quarters[[4]] <- quarters[[3]] 
    }
    for(j in 1:4){
      quarters[[j]]$interevent[1] <- 0
      #Some negative values for where overtime begins
      quarters[[j]]$interevent[which(quarters[[j]]$interevent<0)] <- 0.5
      quarters[[j]]$EventTimes <- cumsum(quarters[[j]]$interevent)
    }
    all_sequences <- append(all_sequences,quarters)
  }
  return(all_sequences)
}
Q2022 <- QuarterMaker(g_by_date_2022)
Q2023 <- QuarterMaker(g_by_date_2023)
####Overtime is now included in 4th quarter for simplicity

###Saving To Drive
library(jsonlite)
QuarterToDrive <- function(all_seq,save_as){
  times_list <- lapply(all_seq, function(df) df$EventTimes)
  marks_list <- lapply(all_seq, function(df) df$ptype)
  zones_list <- lapply(all_seq, function(df) df$FinZone)
  teams_list <- lapply(all_seq, function(df) df$teamID)
  
  write_json(times_list, paste("C:/Users/joshh/GitFolder/NBAScalableMPP/noreb_timestamps_","_final.json",sep = save_as))
  write_json(marks_list, paste("C:/Users/joshh/GitFolder/NBAScalableMPP/noreb_marklist_","_final.json",sep = save_as))
  write_json(teams_list, paste("C:/Users/joshh/GitFolder/NBAScalableMPP/noreb_teamlist_","_final.json",sep = save_as))
  write_json(zones_list, paste("C:/Users/joshh/GitFolder/NBAScalableMPP/noreb_zonelist_","_final.json",sep = save_as))
}
QuarterToDrive(Q2022,"2022")
QuarterToDrive(Q2023,"2023")