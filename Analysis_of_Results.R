####DOCUMENT for Analysing Results

###Teams
#East Conference
Atlantic <- list(2,18,20,17,28)
Central <- list(5,11,15,8,4)
Southeast <- list(19,14,1,30,27)
#West Conference
Northwest <- list(25,7,16,22,26)
Pacific <- list(13,12,9,23,21)
Southwest <- list(29,10,6,24,3)
tnames <- c("ATL Hawks", "BOS Celtics", "NO Pelicans", "CHI Bulls", "CLE Cavs", "DAL Mavs", "DEN Nuggets", "DET Pistons", "GS Warriors", "HOU Rockets", "IND Pacers", "LA Clippers", "LA Lakers", "MIA Heat", "MIL Bucks", "MIN Twolves", "BRO Nets", "NY Knicks", "ORL Magic", "PHI 76ers", "PHE Suns", "POR TBlazers", "SAC Kings", "SA Spurs", "OKL CityT", "UTA Jazz", "WAS Wizards", "TOR Raptors", "MEM Grizz", "CHA Hornets")
TeamsDF <- data.frame(id= c(1:30), name=tnames)

library(dplyr)
east_conference <- c(2,18,20,17,28,5,11,15,8,4,19,14,1,30,27)
east_standings <- c(51,37,51,44,48,44,25,51,23,46,22,53,43,43,35)
east_standings_23 <- c(57,47,54,45,41,51,35,58,17,40,34,44,41,27,35)

west_conference <- c(25,7,16,22,26,13,12,9,23,21,29,10,6,24,3)
west_standings <- c(24,48,46,27,49,33,42,53,30,64,56,20,52,34,36)
west_standings_23 <- c(40,52,42,33,37,43,44,44,48,45,51,22,38,22,42)

east_table <- data.frame(team=east_conference,wins=east_standings)
east_table <- east_table %>% arrange(wins)
east_table_23 <- data.frame(team=east_conference, wins=east_standings_23)
east_table_23 <- east_table_23 %>% arrange(wins)

west_table <- data.frame(team=west_conference,wins=west_standings)
west_table <- west_table %>% arrange(wins)
west_table_23 <- data.frame(team=west_conference,wins=west_standings_23)
west_table_23 <- west_table_23 %>% arrange(wins)



full_teams <- c(east_conference,west_conference)
full_standings <- c(east_standings,west_standings)
full_standings_23 <- c(east_standings_23,west_standings_23)


nba_table <- data.frame(team=full_teams, Wins=full_standings)
nba_table <- nba_table %>% arrange(team)
nba_table_23 <- data.frame(team=full_teams, Wins=full_standings_23)
nba_table_23 <- nba_table_23 %>% arrange(team)

###Tile Plots For Abilities
library(ggplot2)
tile_plot <- function(omegas,nba_table){
  levels_displayed <- c(1,10,20,30,39)
  mark_dev = list()
  mark_sig <- list()
  for(i in 1:(U_dim-1)){
    mark_dev[[paste0("m_", i)]] <- list()
    mark_sig[[paste0("h_", i)]] <- list()
  }
  points <- nba_table$Wins
  for(w in 1:80){
    for(u in 1:(U_dim-1)){
      o_oi <- omegas[u,w,] #30
      c <- cor(o_oi, points, method = "spearman")
      t <- cor.test(o_oi, points, method = "spearman", exact = FALSE)
      mark_dev[[u]][[length(mark_dev[[u]])+1]] <- c
      mark_sig[[u]][[length(mark_sig[[u]])+1]] <- t$p.value
    }
  }
  
  scc <- unlist(mark_dev)
  marks <- rep(c(1:(U_dim-1)),each=80)
  week <- rep(c(1:80), times=(U_dim-1))
  plot_df <- data.frame(w = week, u = marks, scc = scc)
  #print(plot_df[c(1:5),])
  p <- ggplot(plot_df, aes(x=w, y=factor(u), fill=scc))+
    geom_tile(colour="white", size=0.25) + theme_minimal() +
    labs(title="Spearman's Correlation Coefficient",x="Gameweek", y="Mark") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_gradient(low = "hotpink", high = "skyblue") + scale_y_discrete(breaks = levels_displayed)
  print(p)
  avgs <- sapply(mark_sig, function(x) mean(unlist(x)))
  print(avgs)
}

###Loading Parameters
library(RcppCNPy)
x0_2022 <- npyLoad("2022x0_noreb.npy")
x0_2023 <- npyLoad("2023x0_noreb.npy")

U_dim = 40
Z_dim = 2
num_teams = 30
size_alpha_betas = 1 + Z_dim*(U_dim^2)
size_phis = Z_dim * U_dim * (U_dim - 1)
size_deltas = Z_dim * U_dim
#Defining Indices
m_betas_ind = size_alpha_betas
sigma_betas_ind = 2*size_alpha_betas
m_phis_ind = sigma_betas_ind + size_phis
sigma_phis_ind = m_phis_ind + size_phis
m_omega_all_ind = sigma_phis_ind + (U_dim-1) * num_teams
sigma_omega_all_ind = m_omega_all_ind + (U_dim/2) * num_teams
rho_ind = sigma_omega_all_ind + (U_dim/2) * num_teams
omegas_all_ind = rho_ind + (U_dim-1) * num_teams * 80

omegas_2022 <- array(x0_2022[rho_ind+1:omegas_all_ind],dim=c(80,(U_dim-1),num_teams))
omegas_2022 <- aperm(omegas_2022, c(2, 1, 3)) #Now 41x80x30
tile_plot(omegas_2022,nba_table)

omegas_2023 <- array(x0_2023[rho_ind+1:omegas_all_ind],dim=c(80,(U_dim-1),num_teams))
omegas_2023 <- aperm(omegas_2023, c(2, 1, 3)) #Now 41x80x30
tile_plot(omegas_2023,nba_table_23)

phis_2022 <- x0_2022[c((m_phis_ind+1):sigma_phis_ind)]
phis_2022 <- array(phis_2022,dim = c(41,50,2))
phis_2022 <- aperm(phis_2022, c(2, 1, 3))



###Time Series Plot
#Given Omegas we want mark u for each team for each gameweek
#Will Plot East and West conference separately

#Pick a mark - 36 (home successful free throw)
StandTSeries <- function(u, east, west, east_table, west_table,om){
  east_series <- data.frame(matrix(rep(0,80*15),nrow=80,ncol=15))
  east_conference <- east_table$team
  colnames(east_series) <- east
  
  west_series <- data.frame(matrix(rep(0,80*15),nrow=80,ncol=15))
  west_conference <- west_table$team
  colnames(west_series) <- west
  
  c <- 0
  for(t in east){
    c <- c+1
    for(w in 1:80){
      east_series[w,c]<- om[u,w,t]
    }
  }
  c <- 0
  for(t in west){
    c <- c+1
    for(w in 1:80){
      west_series[w,c]<- om[u,w,t]
    }
  }
  
  east_time_data <- data.frame(
    date = rep(c(1:80),times=15), 
    omega = unlist(east_series),  
    team = rep(east, each = 80),
    wins = rep(east_table$wins, each=80)
  )
  west_time_data <- data.frame(
    date = rep(c(1:80),times=15),  
    omega = unlist(west_series),  
    team = rep(west, each = 80),
    wins = rep(west_table$wins, each=80)
  )
  ts_plot1 <- ggplot(east_time_data, aes(x=date, y=omega, color=wins, group=team)) + 
    geom_line() + labs(title = paste("Time Series Plot For Ability In Mark",mark_vec[u],sep = " : "), x="Gameweek", y="Omega") + theme_minimal() + geom_line(size = 1) +
    scale_color_gradient(low = "red", high = "skyblue")
  
  ts_plot2 <- ggplot(west_time_data, aes(x=date, y=omega,colour=wins,group=team)) + 
    geom_line() + labs(title = paste("Time Series Plot For Ability In Mark",mark_vec[u],sep = " : "), x="Gameweek", y="Omega") + theme_minimal() + geom_line(size = 1) +
    scale_color_gradient(low = "red", high = "skyblue")
  
  print(ts_plot1)
  print(ts_plot2)
}

StandTSeries(12, east_conference,west_conference,east_table,west_table,omegas_2022)
StandTSeries(34, east_conference,west_conference,east_table,west_table,omegas_2022)

StandTSeries(12, east_conference,west_conference,east_table_23,west_table_23,omegas_2023)
StandTSeries(34, east_conference,west_conference,east_table_23,west_table_23,omegas_2023)


###Omega Time Series plot
mark_vec_norebs <- mark_vec[-c(5,26)]

omega_tsplot <- function(omegas,n_gw,n_teams,u){
  teams <- c(1:n_teams)
  mark_array <- omegas[u,(1:n_gw),]
  mark_v <- c(mark_array)
  #linetypes <- c(rep("solid",9),"dashed",rep("solid",10),"dashed",rep("solid",9))
  linetypes <- c(rep("solid",7),"dashed",rep("solid",6),"dashed",rep("solid",15))
  sim_time_data <- data.frame(
    date = rep(c(1:n_gw),times=n_teams),  
    ability = mark_v,
    team = rep(tnames, each = n_gw),
    ltypes = rep(linetypes, each=n_gw)
  )
  
  omega_plot <- ggplot(sim_time_data, aes(x=date, y=ability, group=team, color=team,linetype = ltypes)) + geom_line(size = 1) + labs(title = paste("Time Series Plot For Ability In Mark",mark_vec_norebs[u],sep = " : "), x="Gameweek", y="Omega") + theme_minimal() + theme(plot.title = element_text(margin = margin(b = 10)))  +
    theme(legend.position = "none") +
    theme(plot.title = element_text(hjust = 0.5)) 
  return(omega_plot)
}

team_baseline <- function(omegas,team){
  baseline <- omegas[,1:80,team]
  omega_adj <- array(rep(0,(U_dim-1)*80*30),dim=c((U_dim-1),80,30))
  for(i in 1:30){
    omega_adj[,,i] <- omegas[,1:80,i] - baseline
  }
  return(omega_adj)
}

omega_adj_2022 <- team_baseline(omegas_2022,1)
for(u in 1:(U_dim-1)){
  print(omega_tsplot(omega_adj_2022,80,30,u))
}

omega_adj_2023 <- team_baseline(omegas_2023,1)
for(u in 1:(U_dim-1)){
  print(omega_tsplot(omega_adj_2023,80,30,u))
}



###Individual Time Plots
#Going to investigate time plots for individual teams:
ind_tsplot <- function(omegas,n_gw,team,u){
  mark_array <- omegas[u,(1:n_gw),team]
  mark_vec <- c(mark_array)
  
  sim_time_data <- data.frame(
    date = c(1:n_gw),  
    ability = mark_vec
  )
  
  omega_plot <- ggplot(sim_time_data, aes(x=date, y=ability)) + geom_line(size = 1) + labs(title = paste("Time Series Plot For Ability In Mark",u,sep = " : "), x="Gameweek", y=paste("Omega for team", team, sep = " : ")) + theme_minimal()
  
  return(omega_plot)
}

t <- 24
ind_tsplot(omegas_2022,80,t,7)
ind_tsplot(omegas_2022,80,t,30)




##### Going to investigate raw frequencies  ###
A_DefR_list <- ModelData[which(ModelData$ptype==2),'game_id']
count_list <- list()
tally <- 1
for(i in unique(A_DefR_list)){
  tally <- tally +1
  count <- sum(A_DefR_list==i)
  count_list[[tally]] <- count
}

### SCC Averages
scc_averages <- function(omegas,nba_table){
  mark_dev = list()
  for(i in 1:(U_dim-1)){
    mark_dev[[paste0("m_", i)]] <- list()
  }
  points <- nba_table$Wins
  for(w in 1:80){
    for(u in 1:(U_dim-1)){
      o_oi <- omegas[u,w,] #30
      c <- cor(o_oi, points, method = "spearman")
      mark_dev[[u]][[length(mark_dev[[u]])+1]] <- c
    }
  }
  
  scc_avg <- c()
  for(i in 1:(U_dim-1)){
    to_sum <- unlist(mark_dev[[i]])
    temp <- sum(to_sum)/80
    scc_avg <- c(scc_avg,temp)
  }
  
  return(scc_avg)
}
avg_2022_scc <- scc_averages(omegas_2022,nba_table)
avg_2023_scc <- scc_averages(omegas_2023,nba_table_23)

### Q Plots
Q_data <- data.frame(f1=c(25.56, 25.41  , 25.42 ), Q=c(1,2,3), times =c(3281.6191 , 5366.7090,6594.1248  ))
Q_plot_2022 <- ggplot(Q_data,aes(Q,f1))+geom_point(size=2)+geom_line(col="red",lwidth=2)+theme_minimal()+labs(title = "Effect of Cutoff on Accuracy", y="F1 Score")+
  theme(plot.title = element_text(hjust = 0.5))
Q_plot_2022
Q_plot_2022_2 <- ggplot(Q_data,aes(Q,times))+geom_point(size=2)+geom_line(col="blue")+theme_minimal() +labs(title = "Effect of Cutoff on Training Time", y="Training Time")+
  theme(plot.title = element_text(hjust = 0.5))
Q_plot_2022_2

###Raw Frequency Plot
rf_plot <- function(df,year,n_teams,u){
  teams <- c(1:n_teams)
  df <- df[which(df$season==year & df$seasont==2),]
  df <- df[order(df$game_date), ]
  raw_freq <- list()
  
  for(i in 1:n_teams){
    team_data <- df[which(df$teamID==i),]
    game_numbers <- unique(team_data$game_id)
    raw_freq[[i]] <- list()
    for(j in 1:80){
      df_temp <- team_data[which(team_data$game_id==game_numbers[j] & team_data$ptype==u ),]
      num <- dim(df_temp)[1]
      raw_freq[[i]][[j]] <- num
    }
  }
  #mark_array <- omegas[u,(1:80),]
  #mark_v <- c(mark_array)
  rf_vec <- unlist(raw_freq)
  print(length(rf_vec))
  sim_time_data <- data.frame(
    date = rep(c(1:80),times=n_teams),  
    ability = rf_vec,
    team = rep(tnames, each = 80)
  )
  
  rf <- ggplot(sim_time_data, aes(x=date, y=ability, group=team, color=team)) + geom_line(size = 1) + labs(title = paste("Time Series Plot For Raw Frequencies In Mark",mark_vec_norebs[u],sep = " : "), x="Gameweek", y="Raw Frequency") + theme_minimal() + theme(plot.title = element_text(margin = margin(b = 10)))  + theme(legend.position = "none") + theme(plot.title = element_text(hjust = 0.5)) 
  return(rf)
}
rf_2022 <- rf_plot(ModelData,"2022",30,36)
print(rf_2022)