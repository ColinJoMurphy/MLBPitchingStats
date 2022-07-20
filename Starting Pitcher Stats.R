
##### Load Required Packages #####

# Install packaaes if needed
if (!('stringi' %in% rownames(installed.packages()))){
  install.packages('stringi')
}
if (!('data.table' %in% rownames(installed.packages()))){
  install.packages('data.table')
}

# Load packages
library(data.table)
library(stringi)



##### Load Data #####
# Read in data
setwd('YOUR DIRIECTORY HERE')
teamd <- fread('MLB_Final5.csv')
playerd <- fread('game_logs_player_level_sportreference.csv')

# Format column names
setnames(playerd, names(playerd), sapply(names(playerd), tolower))
setnames(teamd, names(teamd), sapply(names(teamd), tolower))

# Remove periods from player nams and set them to upper case
playerd[, name := toupper(stri_replace_all_fixed(name, pattern = '.', replacement = ''))]

# Remove duplicates from 'playerd' table
playerd <- playerd[!duplicated(playerd)]

# Add season variable to 'playerd' table
playerd[, season := as.numeric(substr(game.id, 4, 7))]



##### Reorganize Data #####
# 'playerd' table contains all pitchers who played in any MLB game. The 'teamd' table 
# contians only starters for every team and game. So we subset the starting pitchers 
# from 'playerd' using the names and game ids from 'teamd'. Additionally, we extract variables
# needed for calculations from 'teamd'.

# Generate table of starters from 'teamd' by combining home_starting_pitcher and away_starting_pitcher
starters <- melt(teamd[, .(game_id, datetime, home_starting_pitcher, away_starting_pitcher)], 
                 id.vars = c('game_id', 'datetime'),
                 variable.name = 'home_or_away',
                 value.name = 'pitcher')

# Set 'home_or_away' variable to 'home' or 'away'
starters[, home_or_away := fifelse(stri_detect_fixed(home_or_away, 'home'),
                                   'home',
                                   'away')]

# Generate table of days between outings
dbo <- melt(teamd[, .(game_id, home_starter_days_between_outing, away_starter_days_between_outing)], 
                        id.vars = c('game_id'),
                        variable.name = 'home_or_away',
                        value.name = 'days_between_outing')

# Set 'home_or_away' variable to 'home' or 'away'
dbo[, home_or_away := fifelse(stri_detect_fixed(home_or_away, 'home'),
                                   'home',
                                   'away')]


# Generate table of runs in the first five innings
runs <- melt(teamd[, .(game_id, home_runs_in_inning1to5, away_runs_in_inning1to5)], 
            id.vars = c('game_id'),
            variable.name = 'home_or_away',
            value.name = 'runs_in_inning1to5')

# Set 'home_or_away' variable to 'home' or 'away'
runs[, home_or_away := fifelse(stri_detect_fixed(home_or_away, 'home'),
                              'home',
                              'away')]

# Join 'starters' table with 'dbo' to add days between_outings_variable
starters <- runs[starters, on = c('game_id' = 'game_id', 'home_or_away' = 'home_or_away')]

# Join 'starters' table with 'dbo' to add days between_outings_variable
starters <- dbo[starters, on = c('game_id' = 'game_id', 'home_or_away' = 'home_or_away')]

# Join 'starters' table with 'playerd' to get player stat table for pitchers who 
# started games
starters <- playerd[starters, on = c('game.id' = 'game_id', 'name' = 'pitcher'), nomatch = 0]


# Set NAs in days_between_outings as zero as to not mess up the cumsum() function used below
setnafill(starters, type = 'const', fill = 0, cols = 'days_between_outing')


##### Pitches Per Days Rest Differential #####

# Generate 'last_outing' variables for pitches thrown and days between outings
starters <- starters[order(datetime)]
starters[, c('last_outing', 'lo1_pit', 'lo2_pit', 'lo_dbo') := 
           .(shift(game.id, type = 'lag', fill = 0), 
             shift(pit, type = 'lag', fill = 0), 
             shift(pit, n = 2L, type = 'lag', fill = 0), 
             shift(days_between_outing, type = 'lag', fill = 0)),
         by = .(name, season)]

# Calculate Pitches Per Days Rest Differential Current Season (pdr_season)
starters[, 
         pdr_season_term1 := (lo1_pit + lo2_pit)/(lo_dbo + days_between_outing)
         ][, 
           pdr_season_term2 := cumsum(pit)/cumsum(days_between_outing), 
           by = .(name, season)
           ][, 
             pdr_season := pdr_season_term1 - pdr_season_term2]


# Calculate Pitches Per Days Rest Differential Current Season (pdr_career)
# Note: the first term is the same as 'pdr_season_term1', so 'pdr_season_term1' is used
starters[,
         pdr_career_term2 := cumsum(pit)/cumsum(days_between_outing), 
         by = .(name)
           ][,
             pdr_career := pdr_season_term1 - pdr_career_term2]



##### Ground Balls Allowed Per Batters Faced #####
#' The 'starters' table from the previous section is used here as it already 
#' contains all the stats needed

# Generate 'last_outing' variables for ground balls allowed and batters faced
starters <- starters[order(datetime)]
starters[, c('lo1_gb', 'lo2_gb', 'lo1_bf', 'lo2_bf') := 
           .(shift(gb, n = 1L, type = 'lag', fill = 0), 
             shift(gb, n = 2L, type = 'lag', fill = 0),
             shift(bf, n = 1L, type = 'lag', fill = 0), 
             shift(bf, n = 2L, type = 'lag', fill = 0)),
         by = .(name, season)]

# Calculate ground balls allowed per batters faced over each season  (gbf_season)
starters[, 
         gbf_season_term1 := (lo1_gb + lo2_gb)/(lo1_bf + lo2_bf)
         ][,
           gbf_season_term2 := cumsum(gb)/cumsum(bf),
           by = .(name, season)
           ][,
             gbf_season := gbf_season_term1 - gbf_season_term2
           ]

# Calculate ground balls allowed per batters faced over the players' career  (gbf_career)
# Note: the first term is the same as 'gbf_season_term1', so 'gbf_season_term1' is used
starters[, 
         gbf_career_term2 := cumsum(gb)/cumsum(bf),
         by = .(name)
         ][,
           gbf_career := gbf_season_term1 - gbf_career_term2
            ]



##### Strike Outs Per Batters Faced #####
#' The 'starters' table from the previous section is used here as it already 
#' contains all the stats needed

# Generate last outing variables for strikeouts and batters faced
# Note: the last two outing variables for batters faced were generated in the 
#       previous section so they are not generated again here
starters <- starters[order(datetime)]
starters[, c('lo1_so', 'lo2_so') := 
           .(shift(so, n = 1L, type = 'lag', fill = 0), 
             shift(so, n = 2L, type = 'lag', fill = 0)),
         by = .(name, season)]

# Calculate strike outs per batters faced over each season (sbf_season)
starters[, 
         sbf_season_term1 := (lo1_so + lo2_so)/(lo1_bf + lo2_bf)
         ][,
           sbf_season_term2 := cumsum(so)/cumsum(bf),
           by = .(name, season)
           ][,
             sbf_season := sbf_season_term1 - sbf_season_term2
             ]   
             
# Calculate strike outs per batters faced over players' career (sbf_career)
# Note: the first term is the same as 'sbf_season_term1', so 'sbf_season_term1' is used             
starters[, 
         sbf_career_term2 := cumsum(so)/cumsum(bf),
         by = .(name)
         ][,
           sbf_career := sbf_season_term1 - sbf_career_term2
           ]   

             
             

##### Home Runs Per Batters Faced #####
#' The 'starters' table from the previous section is used here as it already 
#' contains all the stats needed

# Generate last outing variables for home runs allowed and batters faced
# Note: the last two outing variables for batters faced were generated in a 
#       previous section so they are not generated again here
starters <- starters[order(datetime)]
starters[, c('lo1_hr', 'lo2_hr') := 
           .(shift(hr, n = 1L, type = 'lag', fill = 0), 
             shift(hr, n = 2L, type = 'lag', fill = 0)),
         by = .(name, season)]

# Calculate strike outs per batters faced over each season (hbf_season)
starters[, 
         hbf_season_term1 := (lo1_hr + lo2_hr)/(lo1_bf + lo2_bf)
         ][,
           hbf_season_term2 := cumsum(hr)/cumsum(bf),
           by = .(name, season)
           ][,
             hbf_season := hbf_season_term1 - hbf_season_term2
             ]   

# Calculate home runs allowed per batters faced over players' career (hbf_career)
# Note: the first term is the same as 'hbf_season_term1', so 'hbf_season_term1' is used             
starters[, 
         hbf_career_term2 := cumsum(hr)/cumsum(bf),
         by = .(name)
         ][,
          hbf_career := hbf_season_term1 - hbf_career_term2
          ]   




##### Walks Allowed Per Batters Faced #####
#' The 'starters' table from the previous section is used here as it already 
#' contains all the stats needed

# Generate last outing variables for walks allowed and batters faced
# Note: the last two outing variable for batters faced were generated in a 
#       previous section so they are not generated again here
starters <- starters[order(datetime)]
starters[, c('lo1_bb', 'lo2_bb') := 
           .(shift(bb, n = 1L, type = 'lag', fill = 0), 
             shift(bb, n = 2L, type = 'lag', fill = 0)),
         by = .(name, season)]

# Calculate walks allowed per batters faced over each season (bbf_season)
starters[,
         bbf_season_term1 := (lo1_bb + lo2_bb)/(lo1_bf + lo2_bf)
         ][,
           bbf_season_term2 := cumsum(bb)/cumsum(bf),
           by = .(name, season)
           ][,
             bbf_season := bbf_season_term1 - bbf_season_term2
             ]   

# Calculate walks allowed per batters faced over players' career (bbf_career)
# Note: the first term is the same as 'bbf_season_term1', so 'bbf_season_term1' is used             
starters[, 
         bbf_career_term2 := cumsum(bb)/cumsum(bf),
         by = .(name)
         ][,
           bbf_career := bbf_season_term1 - bbf_career_term2
           ]   



##### First Five Inning Win Percentage #####

# Generate indicator variable for first five inning wins
starters[, 
         inning1to5_win := fifelse(runs_in_inning1to5 > min(runs_in_inning1to5),
                                     TRUE,
                                     FALSE),
         by = .(game.id)
         ]


# Calculate first five inning win percentage for current season
starters[, 
         inning1to5_win_perc_season := cumsum(inning1to5_win)/.SD[,.I],
         by = .(name, season)]


# Calculate first five inning win percentage for players' career
starters[, 
         inning1to5_win_perc_career := cumsum(inning1to5_win)/.SD[,.I],
         by = .(name)]



##### Add New Stats back in with old Stats #####

# Change the names of the newly calculated stats to match naming convention of 'teamd'
newstats <- c('pdr_season',
              'pdr_career',
              'gbf_season',
              'gbf_career',
              'sbf_season',
              'sbf_career',
              'hbf_season',
              'hbf_career',
              'bbf_season',
              'bbf_career',
              'inning1to5_win_perc_season',
              'inning1to5_win_perc_career')

# Format new names
newnames <- c('Pitches Per Days Rest Differential Current Season',
              'Pitches Per Days Rest Differential Career',
              'GB Per BF Trending Current Season',
              'GB Per BF Trending Career',
              'SO Per BF Trending Current Season',
              'SO Per BF Trending Career',
              'HR Per BF Trending Current Season',
              'HR Per BF Trending Career',
              'BB Per BF Trending Current Season',
              'BB Per BF Trending Career',
              'inning1to5 win percentage season',
              'inning1to5 win percentage career')
newnames <- stri_replace_all_fixed(stri_trans_tolower(newnames), pattern = ' ', replacement = '_' ) 

# Extract new stat columns from 'starters' and rename them
todate <- starters[, c('game.id', 'name', 'home_or_away', ..newstats)]
setnames(todate, newstats, newnames)

# Set NaN values to NA
setnafill(todate, type = 'const', nan = NA, cols = 4:ncol(todate))

# Split the 'todate' table into two tables, one with home pitcher data, the other 
# with away pitcher data
todate <- dcast(todate, ... ~ home_or_away, value.var = 'name')
setnames(todate, c('home','away'), c('home_starting_pitcher', 'away_starting_pitcher'))
todate.home <- todate[!is.na(home_starting_pitcher)][, away_starting_pitcher := NULL]
todate.away <- todate[!is.na(away_starting_pitcher)][, home_starting_pitcher := NULL]


# Rename stat variables to reflect the 'home_starter_xxx' naming convention
setnames(todate.away, newnames, stri_c('away_starter_', newnames))
setnames(todate.home, newnames, stri_c('home_starter_', newnames))

# Join the home pitcher table with the away pitcher table along 'game.id'
pitchers <- todate.away[todate.home, on = c('game.id' = 'game.id')]

# Join 'pitcher' table and 'teamd' table 
teamd <- pitchers[teamd, on = c('game.id' = 'game_id',
                                   'home_starting_pitcher' = 'home_starting_pitcher',
                                   'away_starting_pitcher' = 'away_starting_pitcher')]
setnames(teamd, 'game.id', 'game_id')

# Reorder cols so new ones are on the far right of 'teamd'
stats <- names(pitchers)[-c(1, 14, 27)]
nameorder <- c(names(teamd[, -..stats]), stats)
teamd <- teamd[, ..nameorder]

teamd[, 997:1020]
# Save file
fwrite(teamd, 'MLB_Final5.csv')




