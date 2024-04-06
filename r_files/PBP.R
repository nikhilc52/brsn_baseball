library(baseballr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(gt)
library(gtExtras)
#-----------------------------------------------------
#Getting all the games from 2012 onwards
pbp <- ncaa_schedule_info(team_id = 167, year = 2012)

for (x in 2013:2024){
  try(pbp <- rbind(pbp, ncaa_schedule_info(team_id = 167, year=x)))
  print(x)
}

#renaming
games <- pbp
rm(pbp)

write.csv(games, file="games.csv")


#----------------------------------
#Getting the PBP data for each game
games <- read_csv("games.csv")

#remove unreadable data
games <- games |> 
  filter(!is.na(game_info_url))
  

#set up dataframe
data <- data.frame()
data <- rbind(data, ncaa_pbp(game_info_url = as.character(games[1,21])))
#errors, used to check
errors <- data.frame()
#errors <- rbind(errors, as.character(games[x,21]))

for (x in 2:nrow(games)){
  tryCatch(data <- rbind(data, ncaa_pbp(game_info_url = as.character(games[x,21]))),
           error=function(e){errors <- rbind(errors, as.character(games[x,21]))})
  print(paste("Date: ",as.character(games[x,3])))
}
write.csv(data,"data.csv")

print(x)

#--------------------------------
#Getting the lineup data for each game

#set up dataframe
lineups <- data.frame()
lineups <- rbind(lineups, ncaa_lineups(game_info_url = as.character(games[1,21])))

for (x in 2:nrow(games)){
  tryCatch(lineups <- rbind(lineups, ncaa_lineups(game_info_url = as.character(games[x,21]))),
           error=function(e){})
  print(paste("Date: ",as.character(games[x,3])))
}

write.csv(lineups,"lineups.csv")
#------------------------------
#Work with data

#remove duplicates/empty data
pbp <- read_csv("data.csv")
pbp <- pbp[,-1]
pbp <- pbp[,-3]
pbp <- pbp[-(1:94),]

#add columns to represent scores
cleaned_pbp <- pbp |> 
  mutate(away_score = as.integer(sub("-.*", "", score))) |> 
  mutate(home_score = as.integer(sub(".*-(.*)", "\\1", score))) |> 
  mutate(away_runs_scored = ifelse(lag(game_pbp_id)!=game_pbp_id,0,(away_score - lag(away_score)))) |> 
  mutate(home_runs_scored = ifelse(lag(game_pbp_id)!=game_pbp_id,0,(home_score - lag(home_score))))

#clean descriptions
cleaned_pbp <- cleaned_pbp[!grepl("^H:", cleaned_pbp$description), ]
cleaned_pbp <- cleaned_pbp[!grepl("^R:", cleaned_pbp$description), ]
cleaned_pbp <- cleaned_pbp[!grepl("^LOB:", cleaned_pbp$description), ]

#clean scoring columns for errors (can't score negative runs)
cleaned_pbp <- cleaned_pbp |> 
  filter(home_runs_scored > -1) |> 
  filter(away_runs_scored > -1)

#--------------------------
#Find interesting moments

clutch_home <- cleaned_pbp |> 
  filter(inning >= 9) |> 
  filter(batting == "Cornell") |> 
  filter((home_score-home_runs_scored) <= away_score & home_score > away_score)

#------------
#Get all the rosters/player stats from 2012 onwards
full_roster <- ncaa_roster(team_id = 167, year=2012)

for (x in 2013:2024){
  try(full_roster <- rbind(full_roster, ncaa_roster(team_id = 167, year=x)))
  print(x)
}

#write.csv(games, file="full_roster.csv")

team_player_stats <- ncaa_team_player_stats(team_id = 167, year=2013, type = "batting")

for (x in 2014:2024){
  try(team_player_stats <- rbind(team_player_stats, ncaa_team_player_stats(team_id = 167, year=x, type = "batting")))
  print(x)
}

write.csv(team_player_stats, "team_player_stats.csv")

#------------------------
#Import the data to find players that are interesting
team_player_stats <- read_csv("player_stats.csv")

team_player_stats <- team_player_stats |> 
  filter(!is.na(GP))

team_player_stats[is.na(team_player_stats)] <- 0

player_stats <- team_player_stats |> 
  filter(!is.na(BA)) |> 
  group_by(player_id, year) |> 
  summarize(
    seasons = n(),
    GP = sum(GP),
    AB = sum(AB),
    player_name = first(player_name),
    BA = mean(BA),
    OBPct = mean(OBPct),
    SlgPct = mean(SlgPct),
    RBI = sum(RBI),
    HR = sum(HR),
    Doubles = sum(Doubles),
    Triples = sum(Triples),
    H = sum(H)
  ) |> 
  filter(AB > 70) |> 
  mutate(Singles = H-Doubles-Triples-HR) |> 
  mutate(player_year = paste(player_name, ",", year)) |> 
  mutate(HRPct = HR/H) |> 
  mutate(HRPctAB = HR/AB)


#----------------------------------
#Scatter plot SLG/BA
ggplot(player_stats, aes(x=BA, y=SlgPct, color=player_name=="Cruz, Chris"))+
  scale_color_manual(values = c("grey", "red"),
                    labels=c('TRUE'='Chris Cruz', 'FALSE'='Not Chris Cruz'), name="")+
  geom_point(size=3)+
  scale_x_continuous(breaks=scales::pretty_breaks(n=8))+
  theme(panel.background = element_rect(fill="white", color="white"))+
  theme(legend.background = element_rect(fill="white", color="white"))+
  theme(legend.box.background = element_rect(fill="white", color="white"))+
  theme(plot.background = element_rect(fill="white", color="white"))+
  theme(legend.key = element_rect(fill = "white"))+
  theme(plot.title = element_text(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = element_text(size = 16, hjust =0.5), 
        panel.grid.major.x = element_line("lightgrey", linetype="dashed"),
        panel.grid.major.y = element_line("lightgrey", linetype="dashed"))+
  annotate("text", x=.2, y=.55, label="Chris Cruz in 2013")+
  annotate("text", x=.26, y=.7, label="Chris Cruz in 2012")+
  annotate("text", x=.23, y=.43, label="Chris Cruz in 2014")+
  xlab("Batting Average")+
  ylab("Slugging %")+
  labs(
    title="Season Slugging Percentages and Batting Averages Since 2012",
    subtitle="For all Cornell hitters with more than 70 at-bats in a season",
    caption="Nikhil Chinchalkar for BRSN | Data: baseballR"
  )+
  geom_vline(xintercept=mean(player_stats$BA), linetype="dashed")+
  geom_hline(yintercept=mean(player_stats$SlgPct), linetype="dashed")

print(mean(player_stats$BA))

+
  annotate("text", x=.33, y=.3, label="Frequent, soft hits", color="darkred", size=4)+
  annotate("text", x=.33, y=.6, label="Frequent, hard hits", color="darkgreen", size=4)+
  annotate("text", x=.17, y=.3, label="Infrequent, soft hits", color="darkred", size=4)+
  annotate("text", x=.17, y=.6, label="Infrequent, hard hits", color="darkred", size=4)


#-------------------------------------
#Bar graph SLG
ggplot(player_stats, aes(y = reorder(player_year, SlgPct), x = SlgPct, fill=player_name == "Cruz, Chris"))+ 
  scale_fill_manual(values = c("darkgrey", "red"),
                    labels=c('TRUE'='Chris Cruz', 'FALSE'='Not Chris Cruz'), name="")+
  geom_bar(stat="identity")+
  xlab("Slugging %")+
  ylab("Player , Year")+
  labs(
    title="Season Slugging Percentages Since 2012",
    subtitle="For all Cornell hitters with more than 70 at-bats in a season",
    caption="Nikhil Chinchalkar for BRSN | Data: baseballR"
  )+
  scale_x_continuous(breaks=scales::pretty_breaks(n=8))+
  theme(panel.background = element_rect(fill="white"))+
  theme(plot.background = element_rect(fill="white", color="white"))+
  theme(legend.background = element_rect(fill="white", color="white"))+
  theme(legend.box.background = element_rect(fill="white", color="white"))+
  theme(plot.title = element_text(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = element_text(size = 16, hjust =0.5), 
        axis.text.y = element_text(size=6),
        panel.grid.major.x = element_line("lightgrey", linetype = "dashed"))

#-----------------
#Scatter plot HR/BA
ggplot(player_stats, aes(x=BA, y=HR, color=player_name=="Cruz, Chris"))+
  scale_color_manual(values = c("grey", "red"),
                     labels=c('TRUE'='Chris Cruz', 'FALSE'='Not Chris Cruz'), name="")+
  geom_vline(xintercept=mean(player_stats$BA), linetype="dashed")+
  geom_point(size=3)+
  scale_x_continuous(breaks=scales::pretty_breaks(n=8))+
  theme(panel.background = element_rect(fill="white", color="white"))+
  theme(legend.background = element_rect(fill="white", color="white"))+
  theme(legend.box.background = element_rect(fill="white", color="white"))+
  theme(plot.background = element_rect(fill="white", color="white"))+
  theme(legend.key = element_rect(fill = "white"))+
  theme(plot.title = element_text(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = element_text(size = 16, hjust =0.5), 
        panel.grid.major.x = element_line("lightgrey", linetype="dashed"),
        panel.grid.major.y = element_line("lightgrey", linetype="dashed"))+
  xlab("Batting Average")+
  ylab("Home Runs")+
  labs(
    title="Season Home Runs and Batting Averages Since 2012",
    subtitle="For all Cornell hitters with more than 70 at-bats in a season",
    caption="Nikhil Chinchalkar for BRSN | Data: baseballR"
  )+
  annotate("text", x=.19, y=4, label="Chris Cruz in 2013")+
  annotate("text", x=.26, y=12.5, label="Chris Cruz in 2012")+
  annotate("text", x=.23, y=6, label="Chris Cruz in 2014")

#------------------------
#scatter HR/SlgPct
ggplot(player_stats, aes(x=SlgPct, y=HR, color=player_name=="Cruz, Chris"))+
  scale_color_manual(values = c("grey", "red"),
                     labels=c('TRUE'='Chris Cruz', 'FALSE'='Not Chris Cruz'), name="")+
  geom_point(size=3)+
  scale_x_continuous(breaks=scales::pretty_breaks(n=8))+
  theme(panel.background = element_rect(fill="white", color="white"))+
  theme(legend.background = element_rect(fill="white", color="white"))+
  theme(legend.box.background = element_rect(fill="white", color="white"))+
  theme(plot.background = element_rect(fill="white", color="white"))+
  theme(legend.key = element_rect(fill = "white"))+
  theme(plot.title = element_text(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = element_text(size = 16, hjust =0.5), 
        panel.grid.major.x = element_line("lightgrey", linetype="dashed"),
        panel.grid.major.y = element_line("lightgrey", linetype="dashed"))+
  xlab("Slugging %")+
  ylab("Home Runs")+
  labs(
    title="Season Home Runs and Slugging Since 2012",
    subtitle="For all Cornell hitters with more than 70 at-bats in a season",
    caption="Nikhil Chinchalkar for BRSN | Data: baseballR"
  )+
  annotate("text", x=.65, y=12.5, label="Chris Cruz in 2012")

#-------------------------------------
#Bar graph HR/AB
player_stats_100 <- player_stats |> 
  filter(AB > 100)

ggplot(player_stats_100, aes(y = reorder(player_year, HRPctAB), x = HRPctAB, fill=player_name == "Cruz, Chris"))+ 
  scale_fill_manual(values = c("darkgrey", "red"),
                    labels=c('TRUE'='Chris Cruz', 'FALSE'='Not Chris Cruz'), name="")+
  geom_bar(stat="identity")+
  xlab("Home Run %")+
  ylab("Player , Year")+
  labs(
    title="Percent of all at bats that were HRs in a season",
    subtitle="For all Cornell hitters with more than 100 at-bats in a season",
    caption="Nikhil Chinchalkar for BRSN | Data: baseballR"
  )+
  scale_x_continuous(breaks=scales::pretty_breaks(n=8))+
  theme(panel.background = element_rect(fill="white"))+
  theme(plot.background = element_rect(fill="white", color="white"))+
  theme(legend.background = element_rect(fill="white", color="white"))+
  theme(legend.box.background = element_rect(fill="white", color="white"))+
  theme(plot.title = element_text(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = element_text(size = 16, hjust =0.5), 
        axis.text.y = element_text(size=6),
        panel.grid.major.x = element_line("lightgrey", linetype = "dashed"))


#---------------------------------
#Bar graph attendance
lineups <- read_csv("lineups.csv")

summary_lineups <- lineups |> 
  group_by(game_date) |> 
  summarize(
    location=first(location),
    year=first(year),
    opponent = first(team_name),
    attendance = mean(attendance)
  ) |> 
  filter(location == "Ithaca, N.Y.") |> 
  filter(year <= 2022) |> 
  mutate(opponent_date = paste(opponent, "on", game_date))

ggplot(summary_lineups, aes(y = reorder(opponent_date, attendance), x = (attendance), fill=(year==2012&opponent=="Dartmouth"&attendance>200)))+ 
  scale_fill_manual(values = c("darkgrey", "red"),
                    labels=c('TRUE'='Dartmouth Championship', 'FALSE'='Not Dartmouth Championship'), name="")+
  geom_bar(stat="identity")+
  xlab("People in Attendance")+
  ylab("Game, Date")+
  labs(
    title="Attendance of all games at Hoy Field",
    subtitle="2012-2022",
    caption="Nikhil Chinchalkar for BRSN | Data: baseballR"
  )+
  scale_x_continuous(breaks=scales::pretty_breaks(n=8))+
  theme(panel.background = element_rect(fill="white"))+
  theme(plot.background = element_rect(fill="white", color="white"))+
  theme(legend.background = element_rect(fill="white", color="white"))+
  theme(legend.box.background = element_rect(fill="white", color="white"))+
  theme(plot.title = element_text(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = element_text(size = 16, hjust =0.5), 
        axis.text.y = element_text(size=6),
        panel.grid.major.x = element_line("lightgrey", linetype = "dashed"))

#----------------------------------
#season info
seasons <- games |> 
  group_by(year) |> 
  summarize(
    cornell_win = sum(ifelse((home_team=="Cornell"&home_team_score>away_team_score) |
                           (away_team=="Cornell"&away_team_score>home_team_score),1,0)),
    cornell_loss_tie = n()-cornell_win
  )

#scatter plot wins
ggplot(seasons, aes(x=year, y=cornell_win, color=year=="2012"))+
  scale_color_manual(values = c("grey", "red"),
                     labels=c('TRUE'='2012 Season', 'FALSE'='Not 2012 Season'), name="")+
  geom_point(size=3)+
  scale_x_continuous(breaks=scales::pretty_breaks(n=8))+
  theme(panel.background = element_rect(fill="white", color="white"))+
  theme(legend.background = element_rect(fill="white", color="white"))+
  theme(legend.box.background = element_rect(fill="white", color="white"))+
  theme(plot.background = element_rect(fill="white", color="white"))+
  theme(legend.key = element_rect(fill = "white"))+
  theme(plot.title = element_text(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = element_text(size = 16, hjust =0.5), 
        panel.grid.major.x = element_line("lightgrey", linetype="dashed"),
        panel.grid.major.y = element_line("lightgrey", linetype="dashed"))+
  xlab("Year")+
  ylab("Wins")+
  labs(
    title="Number of wins in each season",
    subtitle="Since 2012",
    caption="Nikhil Chinchalkar for BRSN | Data: baseballR"
  )

#------------------------------------
#Bar graph HR/H

ggplot(player_stats, aes(y = reorder(player_year, HRPct), x = HRPct, fill=player_name == "Cruz, Chris"))+ 
  scale_fill_manual(values = c("darkgrey", "red"),
                    labels=c('TRUE'='Chris Cruz', 'FALSE'='Not Chris Cruz'), name="")+
  geom_bar(stat="identity")+
  xlab("Home Run %")+
  ylab("Player , Year")+
  labs(
    title="Percent of all hits that were HRs in a season",
    subtitle="For all Cornell hitters with more than 70 at-bats in a season",
    caption="Nikhil Chinchalkar for BRSN | Data: baseballR"
  )+
  scale_x_continuous(breaks=scales::pretty_breaks(n=8))+
  theme(panel.background = element_rect(fill="white"))+
  theme(plot.background = element_rect(fill="white", color="white"))+
  theme(legend.background = element_rect(fill="white", color="white"))+
  theme(legend.box.background = element_rect(fill="white", color="white"))+
  theme(plot.title = element_text(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = element_text(size = 16, hjust =0.5), 
        axis.text.y = element_text(size=6),
        panel.grid.major.x = element_line("lightgrey", linetype = "dashed"))

#-------------------------------------
#history table
history <- read.csv("C:\\Users\\nikhi\\Downloads\\championship_history.csv")


table <- history |> 
  select(Year, Winner, Loser) |> 
  gt() |> 
  opt_align_table_header("center") |> 
  cols_align("center") |> 
  tab_source_note("Nikhil Chinchalkar for BRSN | Data: Wikipedia") |> 
  tab_header(title = "Ivy Championship Recent History", subtitle="Excluding 2012, 2020, 2021") |> 
  opt_align_table_header(align = "center") |> 
  opt_table_font(
    font = list(
      google_font(name = "Helvetica")
    )) |>  
  tab_style(
    style = cell_text(
      size = "bigger",
      weight = "bold",
      transform = "uppercase"
    ),
    location = cells_title(groups = "title")
  ) |> 
  tab_style(
    style = list(
      cell_fill(color = "darkgreen"),
      cell_text(weight = "bold", color = "white")
    ),
    locations = cells_body(
      columns = Winner,
      rows = Winner == "Dartmouth"
    )
  ) |> 
  tab_style(
    style = list(
      cell_fill(color = "darkgreen"),
      cell_text(weight = "bold", color = "white")
    ),
    locations = cells_body(
      columns = Loser,
      rows = Loser == "Dartmouth"
    )
  ) |> 
  tab_style(
    style = list(
      cell_fill(color = "red"),
      cell_text(weight = "bold", color = "white")
    ),
    locations = cells_body(
      columns = Winner,
      rows = Winner == "Cornell"
    )
  ) |> 
  tab_style(
    style = list(
      cell_fill(color = "red"),
      cell_text(weight = "bold", color = "white")
    ),
    locations = cells_body(
      columns = Loser,
      rows = Loser == "Cornell"
    )
  )

gtsave(table,"history_table.png")
