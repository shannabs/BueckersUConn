library(readxl)
library(tidyverse)
library(gridExtra)

#-------- import all datasets ---------

#dataset of Paige's sum stats for 4 years
Totals <- read_excel("Documents/SIE532 Final/PB_Totals.xlsx")

#average stats per game per year
PerGameAvgY <- read_excel("Documents/SIE532 Final/PB_PerGame.xlsx")

#advanced totals 
Advanced_totals <- read_excel("Documents/SIE532 Final/Advanced_totals.xlsx")

#per 40 stats
PG_Per40 <- read_excel("Downloads/PG_Per40.xlsx")

#all games Paige played 2020-21 stats
GS_20_21 <- read_excel("Documents/SIE532 Final/PB_2020_21_GameStats.xlsx")

#all games Paige played 2021-22
GS_21_22 <- read_excel("Documents/SIE532 Final/PB_2021_22_GameStats.xlsx")

#all games Paige played 2023-24 
GS_23_24 <- read_excel("Documents/SIE532 Final/PB_2023_24_GameStats.xlsx")

#all games Paige played 2024-25 
GS_24_25 <- read_excel("Documents/SIE532 Final/PB_2024_25_GameStats.xlsx")

#total games UConn WBB played 2024-25
Uconn2425 <- read_excel("Documents/SIE532 Final/Uconn2425.xls")

#total games UConn WBB played 2023-24
Uconn2324 <- read_excel("Downloads/Uconn2324.xls")

#this is the season Paige didn't play but total games UConn WBB played 2022-23
Uconn2223 <- read_excel("Documents/SIE532 Final/Uconn2223.xls")

#total games UConn WBB played 2021-22
Uconn2122 <- read_excel("Documents/SIE532 Final/Uconn2122.xls")

#total games UConn WBB played 2020-21
Uconn20_21 <- read_excel("Documents/SIE532 Final/Uconn20_21.xls")

#----------------------------------

#Injury Dates 
#---- 1/21/21 rolled right ankle
#---- 12/5/21 left knee tibial plateau fracture and meniscus tear 
#-----------returned 12 weeks later on 2/25/22
#---- 8/1/22 Left Knee ACL Tear
#---- 10/13/2023 left thumb sprain
#---- 1/5/25 left knee/ankle injury 

#--------------------- cleaning data ---------------------
#renaming columns
#need to rename to acceptable names 
#can't start with number or have special characters
Totals <- Totals %>% rename(Perc3 = '3P%', P3 = '3P', PA3 = '3PA',
                                Perc2 = '2P%', P2 = '2P', PA2 = '2PA',
                                FGPerc = 'FG%', eFGPerc = 'eFG%', FTPerc = 'FT%')

Per40 <- PG_Per40 %>% rename(Perc3 = '3P%', P3 = '3P', PA3 = '3PA',
                             Perc2 = '2P%', P2 = '2P', PA2 = '2PA',
                             FGPerc = 'FG%', eFGPerc = 'eFG%', FTPerc = 'FT%')

GS_20_21 <- GS_20_21 %>% rename(Perc3 = '3P%', P3 = '3P', PA3 = '3PA',
                                Perc2 = '2P%', P2 = '2P', PA2 = '2PA',
                                FGPerc = 'FG%', eFGPerc = 'eFG%', FTPerc = 'FT%')

GS_21_22 <- GS_21_22 %>% rename(Perc3 = '3P%', P3 = '3P', PA3 = '3PA',
                                Perc2 = '2P%', P2 = '2P', PA2 = '2PA',
                                FGPerc = 'FG%', eFGPerc = 'eFG%', FTPerc = 'FT%')

GS_23_24 <- GS_23_24 %>% rename(Perc3 = '3P%', P3 = '3P', PA3 = '3PA',
                                Perc2 = '2P%', P2 = '2P', PA2 = '2PA',
                                FGPerc = 'FG%', eFGPerc = 'eFG%', FTPerc = 'FT%')

GS_24_25 <- GS_24_25 %>% rename(Perc3 = '3P%', P3 = '3P', PA3 = '3PA',
                                Perc2 = '2P%', P2 = '2P', PA2 = '2PA',
                                FGPerc = 'FG%', eFGPerc = 'eFG%', FTPerc = 'FT%')

#adding class column to separate seasons
Class = 'FR'
GS_20_21['Class'] <- Class
GS_20_21

Class2 = 'SO'
GS_21_22['Class'] <- Class2
GS_21_22

Class3 = 'JR'
GS_23_24['Class'] <- Class3
GS_23_24

Class4 = 'SR'
GS_24_25['Class'] <- Class4
GS_24_25

#creating new dataframe with all 4 years of Paige playing
PG_AllGames <- rbind(GS_20_21, GS_21_22, GS_23_24, GS_24_25)

#------------------------------------------------------------------
#looking at totals for different seasons

#Paige's overall FGPerc over her 4 years at UConn bar chart 
ggplot(Totals, aes(x = Season, y = FGPerc)) + geom_col(fill = 'steelblue') +
  labs (y = 'Field Goal Percentage') + geom_text(aes(label = FGPerc), vjust = 1.5,
                                                     position = position_dodge(width = 0.9),
                                                 color = 'white') + ylim(c(0,.75))
#shooting percentage in 2020-22 season 
# 1/21/21 rolled right ankle
ggplot(GS_20_21, aes(Date, FGPerc)) + geom_point(color = 'steelblue')
ggplot(GS_21_22, aes(Date, FGPerc)) + geom_point(color = 'red') 

#ggplot() + geom_point(data = GS_20_21, aes( x = Date, y = FGPerc), color = 'steelblue') + geom_point(data = GS_21_22, aes(Date, FGPerc), color = 'red') 

#-------------
#Analyze overall shooting across 4 years
ggplot(PG_AllGames, aes(Date, FGPerc)) + geom_point()


#Analyze minutes played JR yr vs other years 
#chart shows minutes played per season --> sophmore season least amount of minutes
ggplot(Totals, aes(x = Season, y = MP)) + geom_col(fill = 'steelblue') +
  labs(y = 'Minutes Played') + geom_text(aes(label = MP), nudge_y = 50) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())

#shows total games Paige played each season 
ggplot(Totals, aes(x = Season, y = G)) + geom_col(fill = 'steelblue') +
  labs(y = 'Games Played') + geom_text(aes(label = G), nudge_y = -2, color = 'white')

#---------------------------------------------------------------------------------
#Looking at the Sophmore season of injury that occured on 2021-12-05 and returned 2022-02-25
#GS_21-22 df

#grouping games before 2021 injury of 21-22 season
Before_Injury1 <- GS_21_22 %>% 
  filter(Date <= '2021-12-05')

#grouping games after 2021 injury of 21-22 season
After_Injury1 <- GS_21_22 %>%
  filter(Date > '2021-12-05')

#AVG_MP_Before1 <- GS_21_22 %>%

#total minutes played before vs after injury
tot_mp_before1 = sum(Before_Injury1$MP)
tot_mp_after1 = sum(After_Injury1$MP)

#average minutes played before vs after injury
avg_mp_before1 = tot_mp_before1/6
avg_mp_after1 = tot_mp_after1/11

#minimum minutes played before vs after
min_mp_before1 = min(Before_Injury1$MP)
min_mp_after1 = min(After_Injury1$MP)

#max minutes played before vs after
max_mp_before1 = max(Before_Injury1$MP)
max_mp_after1 = max(After_Injury1$MP)

#Gtm26-31

#looking at the six games returning from injury
games6_back <- After_Injury1 %>%  
  filter(Gtm <= 31)

#six games return and total minutes played summed
totmp_6_after1 = sum(games6_back$MP)

#average minutes played six games return from injury 
avg_game6_mpafter = totmp_6_after1/6

#last five games of 2021-22 injury after 6 games return 
last5_back <- After_Injury1 %>%  
  filter(Gtm > 31)
totmp_last5_after1 = sum(last5_back$MP)
avg_last5_mpafter = totmp_last5_after1/5

#minutes played per game before injury
ggplot(Before_Injury1, aes(x = Gtm, y = MP)) + geom_col(fill = 'steelblue') + ylim(0,50)

#minutes played per game after injury
ggplot(After_Injury1, aes(x = Gtm, y = MP)) + geom_col(fill = 'darkblue') + ylim(0,50)

#looking at line graphs but don't like that as much as bar
ggplot(Before_Injury1, aes(x = Gtm, y = MP)) + geom_line(color = 'steelblue')+
  ylim(0,50)
ggplot(After_Injury1, aes(x = Gtm, y = MP)) + geom_line(color = 'darkblue') + ylim(0,50)

                      
#i need to create a dataframe with both before and after but label before and after for each so i can
#have a label showing the differences to create new column Injury in before labeled 'before
#create new column Injury labeled after in after
#create new combined dataframe then do ggplot showing those with MP,

#creating variable BI = before 
#adding variable to each row in the before_injury df and labeling column as Injury
BI = 'Before'
Before_Injury1['Injury'] <- BI

#creating new variable After as AI and adding that to each row in After_Injury and labeling as Injury
AI = 'After'
After_Injury1['Injury'] <- AI

#creating game_id so i can compare first games of season and first games back from injury
Before_Injury1 <- Before_Injury1 %>%
  mutate(game_id = row_number())

After_Injury1 <- After_Injury1 %>%
  mutate(game_id = row_number())

#combining Before/After Injury df
Soph_Year <- rbind(Before_Injury1, After_Injury1)

#comparing before vs after injury minutes played in one bar chart 
#--------------MINUTES PLAYED COMPARRISON----------------
ggplot() + geom_bar(stat='identity', data = Before_Injury1, aes(x = game_id, y = MP, fill = Injury)) +
  geom_bar(stat='identity', data = After_Injury1, aes(x = game_id, y = MP, fill = Injury)) +
  labs(x = 'Games Played', y = "Minutes Played")+
  scale_fill_manual(values = c("steelblue", "darkgrey"))

#comparing before vs after injury minutes played in one line graph
#comparing first games each on top of each other
ggplot() + geom_line( data = Before_Injury1, aes(x = game_id, y = MP, color = Injury)) +
  geom_line( data = After_Injury1, aes(x = game_id, y = MP, color = Injury)) +
  labs(x = 'Games Played', y = "Minutes Played", fill = 'Injury')+
  scale_color_manual(values = c("steelblue", "darkgrey"))

#comparing before/after by number of games played line graph
#doesn't show dates 
ggplot() + geom_line( data = Soph_Year, aes(x = Rk, y = MP, color = Injury)) +
  geom_line( data = Soph_Year, aes(x = Rk, y = MP, color = Injury)) +
  labs(x = 'Games Played', y = "Minutes Played", fill = 'Injury')+
  scale_fill_manual(values = c("steelblue", "darkgrey"))

#comparing before/after by number of games played bar chart
#doesn't show dates 
ggplot() + geom_col( data = Soph_Year, aes(x = Rk, y = MP, color = Injury, fill = Injury)) +
  geom_col( data = Soph_Year, aes(x = Rk, y = MP, color = Injury, fill = Injury)) +
  labs(x = 'Games Played', y = "Minutes Played", fill = 'Injury')+
  scale_color_manual(values = c("steelblue", "darkgrey")) +
  scale_fill_manual(values = c("steelblue", "darkgrey"))
  


#------------------- performance of games before and after 2021-22 --------------

#going to compare eFGPerc before and after in 2021-22
#dot plot of games played before versus after injury and the effective field goal percentage
ggplot() + geom_point( data = Before_Injury1, aes(x = Rk, y = eFGPerc, color = Injury), shape = 19) +
  geom_point( data = After_Injury1, aes(x = Rk, y = eFGPerc, color = Injury)) +
  labs(x = 'Games Played', y = "eField Goal Percent")+
  scale_color_manual(values = c("steelblue", "navy")) 

# bar plot of games played before versus after injury and the effective field goal percentage
ggplot() + geom_col( data = Before_Injury1, aes(x = Rk, y = eFGPerc, fill = Injury), shape = 19) +
  geom_col( data = After_Injury1, aes(x = Rk, y = eFGPerc, fill = Injury)) +
  labs(x = 'Games Played', y = "eField Goal Percent")+
  scale_fill_manual(values = c("steelblue", "navy"))

#           comparing Game Score Value before vs after injury
#---------------------------------------------------------------------
ggplot() + geom_line( data = Soph_Year, aes(x = Rk, y = GmSc, color = Injury)) +
  geom_line( data = Soph_Year, aes(x = Rk, y = GmSc, color = Injury)) +
  labs(x = 'Games Played', y = "Game Score Value")+
  scale_color_manual(values = c("steelblue", "navy")) 

ggplot() + geom_col( data = Soph_Year, aes(x = Rk, y = GmSc, fill = Injury)) +
  geom_col( data = Soph_Year, aes(x = Rk, y = GmSc, fill = Injury)) +
  labs(x = 'Games Played', y = "Game Score Value")+
  scale_fill_manual(values = c("steelblue", "navy"))
#-------------------------------------------------------------------------------

#----------------------- 2024-25 Season ------------------------------------
GS_24_25
#missed games 16 and 17 --- Xavier and Georgetown with Uconn winning by 20+ points
#last game gtm = 15
#BACK ON gtm = 18

#lets look at the 5 games before and after injury 
injury_25 <- GS_24_25 %>%
  filter( Gtm >= 10, Gtm <= 22) %>%
  select (!GS)

ggplot() + geom_col(data = injury_25, aes (x= Gtm, y = Perc2), fill = 'steelblue') + 
  labs(x = 'Game Number', y = "Two Shooting Percentage")

#graph is showing 5 games before vs after small injury and showing two point field goal percentage
ggplot() + geom_point(data = injury_25, aes (x= Gtm, y = Perc2), color = 'steelblue') + 
  geom_line(data = injury_25, aes (x= Gtm, y = Perc2), color = 'steelblue') +
  labs(x = 'Game Number', y = "Two Shooting Percentage" ) +
  geom_point(data = injury_25, aes(x = Gtm, y = Perc3), color = 'navy') + 
  geom_line(data = injury_25, aes(x = Gtm, y = Perc3), color = 'navy')

#three point shooting percentage before vs after injury 24-25 injury 
ggplot() + geom_point(data = injury_25, aes(x = Gtm, y = Perc3), color = 'darkgray') + 
  geom_line(data = injury_25, aes(x = Gtm, y = Perc3), color = 'darkgray')

ggplot() + geom_col(data = injury_25, aes(x = Gtm, y = Perc3)) 

#---------- shooting performance each year ---------------
#using totals 
Totals
library(kableExtra)
library(knitr)

sp_totals <- Totals %>%
  select(Season, MP, Perc2, Perc3, FGPerc, eFGPerc)%>%
  rename( Minutes_Played = MP,
          Two_Point_Perc = Perc2,
          Three_Point_Perc = Perc3,
          Field_Goal_Perc = FGPerc,
          Effective_Field_Goal_Perc = eFGPerc)


table(sp_totals, col.names = c('Season', 'Minutes_Played', 'Two_Point_Perc', 
                                     'Three_Point_Perc', 'Field_Goal_Perc',
                                     'Effective_Field_Goal_Perc' ))

#------------------------------------------------------------------------------

#blocks steals Defensive rebounds totals for all seasons
defensive_tots <- Totals %>%
  select(Season, DRB, STL, BLK) %>%
  rename(Defensive_Rebounds = DRB,
         Steals = STL,
         Blocks = BLK)

#want to create a combine df with totals and advanced totals
# Season, DRBPerc , STLPerc, BLKPerc, DWS 
ad_tots <- Advanced_totals %>%
  select(DRBPerc, STLPerc, BLKPerc, DWS)

tot_def_stats <- cbind(defensive_tots, ad_tots)



#---------------------------- analyze all games with minutes played ---------------
ggplot(PG_AllGames, aes( x = Gcar, y = MP, color = Class)) + geom_point() +
  labs(x = 'Career Games', y = "Minutes Played")+
  scale_color_manual(values = c("steelblue", "#98D7D8", "#7E868C", "#004369")) + theme_light() 



#-------------------- Game Score --------------
ggplot() + geom_point(data = PG_AllGames, aes(x = Gcar, y = GmSc, color = Class)) +
  scale_color_manual(values = c("steelblue", "#98D7D8", "#7E868C", "#004369")) +
  labs(x = "Career Game Number", y = 'Game Score')

ggplot() + geom_line(data = PG_AllGames, aes(x = Gcar, y = GmSc, color = Class)) +
  scale_color_manual(values = c("steelblue", "#98D7D8", "#7E868C", "#004369")) +
  labs(x = "Career Game Number", y = 'Game Score')

#calc total games where Game Score > = 10 
tot_above_GmSc <- PG_AllGames %>%
  select(GmSc) %>%
  filter(GmSc >= 10) %>%
  tally()

#calc total games where Game Score is below 10
tot_below_GmSc <- PG_AllGames %>%
  select(GmSc) %>%
  filter(GmSc < 10) %>%
  tally()


