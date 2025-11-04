# Import Dataset 
atp<-read.csv("atp_matches_2024.csv")
atp
# View names of columns 
names(atp)
# See the number of rows and columns 
dim(atp)
# Investigate the classes of the features
sapply(atp,class)
# Load Tidyverse package 
library(tidyverse)
# Add Specific match id to rows
atp <- mutate(atp, match_id = row_number())
# View section of the dataset
tibble(atp)
# Investigate if there are rows containing NA values 
sum(is.na(atp))
summary(is.na(atp))
# Investigate if there are rows where the aces are NA values
sum(is.na(atp$w_ace))
# Remove rows where serve stats had NA values
atp<- atp[!is.na(atp$w_ace),]
atp <- atp[!is.na(atp$w_SvGms),]
atp <- atp[!is.na(atp$l_SvGms),]
# Create Percentage columns for serve statistics 
atp$w_1stPercentage <- (atp$w_1stIn/atp$w_svpt) * 100 
atp$w_1stWonPercentage <- (atp$w_1stWon/atp$w_1stIn) * 100
atp$w_2ndWonPercentage <- (atp$w_2ndWon/(atp$w_svpt-atp$w_1stIn)) *100
atp$l_1stPercentage <- (atp$l_1stIn/atp$l_svpt) * 100 
atp$l_1stWonPercentage <- (atp$l_1stWon/atp$l_1stIn) * 100
atp$l_2ndWonPercentage <- (atp$l_2ndWon/(atp$l_svpt-atp$l_1stIn)) *100
# Create columns for serve and return points won 
atp$w_svptsWon <- (atp$w_1stWon + atp$w_2ndWon)
atp$l_svptsWon <- (atp$l_1stWon + atp$l_2ndWon)
atp$w_returnWon <- (atp$l_svpt -(atp$l_1stWon + atp$l_2ndWon))
atp$l_returnWon <- (atp$w_svpt - (atp$w_1stWon + atp$w_2ndWon))
# Create total points column 
atp$total_match_points <- (atp$w_svpt + atp$l_svpt)
# Create total points won for winner and loser 
atp$w_totalpointsWon <- (atp$w_svptsWon + atp$w_returnWon)
atp$l_totalpointsWon <- (atp$l_svptsWon + atp$l_returnWon)
# Create columns for return percentage points won 
atp$w_returnpointsWonPercentage <- (atp$w_returnWon/atp$l_svpt) * 100 
atp$l_returnpointsWonPercentage <- (atp$l_returnWon/atp$w_svpt) * 100
# Create a break point conversion ratio column 
w_numerator <- atp$l_bpFaced-atp$l_bpSaved 
w_denominator <- atp$l_bpFaced
atp$w_bpconverted <- paste0(w_numerator, "/", w_denominator)
l_numerator <- atp$w_bpFaced-atp$w_bpSaved
l_denominator <- atp$w_bpFaced
atp$l_bpconverted <- paste0(l_numerator, "/", l_denominator)
# Investigate for duplicated values 
table(duplicated(atp))
# Investigate summary statistics 
summary(atp)
# Look at names of all the players that won 
unique(atp$winner_name)
# Only allow percentages equal or less than 100
atp <- atp[atp$w_2ndWonPercentage <=100,] 
# Investigate any NA values in tourney ID
sum(is.na(atp$tourney_id))
# Remove rows containing NA values 
atp<- atp[!is.na(atp$tourney_id),]
atp<- atp[!is.na(atp$l_2ndWonPercentage),]
# Separating the break points converted columns into integers and then into a ratio for analysis 
atp <- atp %>% 
  separate(w_bpconverted, into = c("w_bpconverted", "w_bptotal"),sep = "/", convert =TRUE) %>%
  separate(l_bpconverted, into = c("l_bpconverted", "l_bptotal"), sep = "/", convert = TRUE) %>%
  mutate(
    w_bp_conversion_percentage = ifelse(w_bptotal == 0 &  w_bpconverted == 0, 0, w_bpconverted/w_bptotal * 100),
    l_bp_conversion_percentage = ifelse(l_bptotal == 0 & l_bpconverted == 0, 0, l_bpconverted/l_bptotal * 100)
  )
# Add total Aces column 
atp <- atp %>% 
  mutate(total_aces = w_ace +l_ace)
# Add Total Double Faults column 
atp <- atp %>%
  mutate(total_dfs = w_df + l_df)
# Load Libraries for data wrangling 
library(dplyr)
library(tidyr)
# Import ggplot for data visualisations 
library(ggplot2)
# Create a bar chart for number of matches on each surface 
ggplot(atp, aes(x = surface, fill = surface))+
  geom_bar() +  
  labs(title = "Count of Matches by Surface",
       x = "Surface",
       y = "Number of Matches") +
  theme_minimal()

# Create a boxplot for the height of players 
atp$loser_ht[atp$match_id == 2917] <- 180 
height_data <- atp %>% 
  dplyr::select(winner_ht, loser_ht) %>%
  pivot_longer(cols = everything(), values_to = "height") %>%
  filter(!is.na(height))
ggplot(height_data, aes(y = height )) + 
  geom_boxplot(fill = "skyblue", color = "black") + 
  labs(title = "Distribution of Player Heights",
       y = "Height (cm)") +
  theme_minimal()

# Boxplot for Age 
age_data <- atp %>%
  dplyr::select(winner_age, loser_age) %>%
  pivot_longer(cols = everything(), values_to = "age") %>% 
  filter(!is.na(age))
ggplot(age_data, aes(y = age)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Player Ages",
       y = "Age") +
  theme_minimal()

# Create table to see what countries have the most players and most wins 
match_wins <- atp %>% 
  count(winner_ioc, name = "matches_won") %>%
  arrange(desc(matches_won))
match_wins
players <- atp %>% 
  dplyr::select(winner_name, winner_ioc) %>%
  rename(player = winner_name, country = winner_ioc) %>%
  bind_rows(
    atp %>%
      dplyr::select(loser_name, loser_ioc) %>%
      rename(player = loser_name, country = loser_ioc)
  ) %>%
  distinct()
player_freq <- players %>%
  count(country, name = "player_count") %>%
  arrange(desc(player_count))
player_freq
country_stats <- full_join(match_wins, player_freq, by = c("winner_ioc" = "country")) %>%
  rename(country = winner_ioc) %>%
  replace_na(list(matches_won = 0, player_count = 0))
country_stats

# Long format for grouped bar chart
country_stats_long <- country_stats %>%
  pivot_longer(cols = c(matches_won, player_count),
               names_to = "metric",
               values_to = "count") %>%
  slice(1:20)
country_stats_long

# grouped bar chart to show countries with most wins and player count 
ggplot(country_stats_long, aes(x = reorder(country, -count), y = count, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Matches Won and Player Count by Country",
       x = "Country",
       y = "Count",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1))

# Create boxplots for Serve statistics 
boxplot(atp$w_1stPercentage,atp$l_1stPercentage, col = c("green","red"),main = "Boxplot of Winner 1st Serve % vs Loser 1st Serve %")
boxplot(atp$w_1stWonPercentage,atp$l_1stWonPercentage, col = c("green","red"),main = "Boxplot of Winner 1st Serve % Points Won vs Loser 1st Serve % Points Won")
boxplot(atp$w_2ndWonPercentage,atp$l_2ndWonPercentage, col = c("green","red"),main = "Boxplot of Winner 2nd Serve % Points Won vs Loser 2nd Serve % Points Won")

# Create a table for 1st Serve % by surface 
first_serve_pct <- atp %>%
  dplyr::select(surface,w_1stPercentage,l_1stPercentage) %>%
  pivot_longer(cols = c(w_1stPercentage,l_1stPercentage), names_to = "player_type", values_to = "conversion_rate")

# Create bar chart for Winner 1st Serve % by Surface 
first_serve_pct %>%
  group_by(surface, player_type) %>%
  summarise(mean_rate = mean(conversion_rate, na.rm = TRUE)) %>%
  ggplot(aes(x = surface, y = mean_rate, fill = player_type)) +
  geom_col(position = position_dodge()) +
  labs(title = "Mean 1st Serve % by Surface and Player Type",
       x = "Surface",
       y = "%") +
  theme_minimal()
# Create table to view 1st Serve % Points Won by Surface 
first_serve <- atp %>%
  dplyr::select(surface,w_1stWonPercentage,l_1stWonPercentage) %>%
  pivot_longer(cols = c(w_1stWonPercentage,l_1stWonPercentage), names_to = "player_type", values_to = "conversion_rate")
first_serve 
# Create Boxplot for 1st Serve % Points Won by Surface 
ggplot(first_serve, aes(x = surface, y = conversion_rate, fill = player_type)) +
  geom_boxplot(position = position_dodge()) +
  labs(title = "1st Serve Points Won by Surface and Player Type",
       x = "Surface",
       y = "% Points Won") +
  theme_minimal()
# Create bar chart to view Mean 1st Serve % Points Won by Surface 
first_serve %>%
  group_by(surface, player_type) %>%
  summarise(mean_rate = mean(conversion_rate, na.rm = TRUE)) %>%
  ggplot(aes(x = surface, y = mean_rate, fill = player_type)) +
  geom_col(position = position_dodge()) +
  labs(title = "Mean 1st Serve Points Won by Surface and Player Type",
       x = "Surface",
       y = "% Points Won") +
  theme_minimal()

# Create table for 2nd Serve % Points Won by Surface 
second_serve <- atp %>%
  dplyr::select(surface,w_2ndWonPercentage,l_2ndWonPercentage) %>%
  pivot_longer(cols = c(w_2ndWonPercentage,l_2ndWonPercentage), names_to = "player_type", values_to = "conversion_rate") 
# Create boxplot for 2nd Serve % Points Won by Surface 
ggplot(second_serve, aes(x = surface, y = conversion_rate, fill = player_type)) +
  geom_boxplot(position = position_dodge()) +
  labs(title = "2nd Serve Points Won by Surface and Player Type",
       x = "Surface",
       y = "% Points Won") +
  theme_minimal()
# Create bar chart for 2nd Serve % Points Won by Surface 
second_serve %>%
  group_by(surface, player_type) %>%
  summarise(mean_rate = mean(conversion_rate, na.rm = TRUE)) %>%
  ggplot(aes(x = surface, y = mean_rate, fill = player_type)) +
  geom_col(position = position_dodge()) +
  labs(title = "Mean 2nd Serve Points Won by Surface and Player Type",
       x = "Surface",
       y = "% Points Won") +
  theme_minimal()

# Create table for Mean Total Aces by Surface 
aces_by_surface <- atp %>%
  group_by(surface) %>%
  summarise(mean_total_aces = mean(total_aces, na.rm = TRUE))
# Create Bar Chart for Mean Total Aces per Match by Surface 
ggplot(aces_by_surface, aes(x = surface, y = mean_total_aces, fill = surface)) +
  geom_col() +
  labs(title = "Mean Total Aces per Match by Surface",
       x = "Surface", y = "Mean Total Aces") +
  theme_minimal()
# Create table for Mean Total Double Faults by Surface 
dfs_by_surface <- atp %>%
  group_by(surface) %>%
  summarise(mean_total_dfs = mean(total_dfs, na.rm = TRUE))
# Create Bar Chart for Mean Total Double Faults by Surface 
ggplot(dfs_by_surface, aes(x= surface, y = mean_total_dfs, fill = surface)) + 
  geom_col() + 
  labs(title = "Mean Total Double Faults per Match by Surface",
       x = "Surface", y = "Mean Total Double Faults") + 
  theme_minimal()

# Create density graphs with aces for Winners and Losers 
ggplot() +
  geom_density(aes(x = atp$w_ace, color = "Winner"), adjust = 1.5) +
  geom_density(aes(x = atp$l_ace, color = "Loser"), adjust = 1.5) +
  scale_color_manual(values = c("Winner" = "blue", "Loser" = "red")) +
  labs(title = "Density of Aces: Winner vs Loser",
       x = "Number of Aces",
       y = "Density",
       color = "Player") +
  theme_minimal()
# Create Density graphs for double faults for Winners and Losers
ggplot() +
  geom_density(aes(x = atp$w_df, color = "Winner"), adjust = 2) +
  geom_density(aes(x = atp$l_df, color = "Loser"), adjust = 2) +
  scale_color_manual(values = c("Winner" = "blue", "Loser" = "red")) +
  labs(title = "Density of Double Faults: Winner vs Loser",
       x = "Number of Double Faults",
       y = "Density",
       color = "Player") +
  theme_minimal()
# Look at summary aces statistics 
summary(atp$w_ace)
summary(atp$l_ace)

# Producing a longer format to establish breakpoint conversion percentage for the winner and loser and the value 
bp_long <- atp %>% 
  dplyr::select(surface,w_bp_conversion_percentage,l_bp_conversion_percentage) %>%
  pivot_longer(cols = c(w_bp_conversion_percentage,l_bp_conversion_percentage), names_to = "player_type", values_to = "conversion_percentage") 
bp_long

# Boxplot to show breakpoint conversion percentage of winner and losers. 
ggplot(bp_long, aes(x = player_type, y= conversion_percentage, fill = player_type)) +
  geom_boxplot() +
  labs(title = "Break Point Conversion: Winner vs Loser",
       x = "", y ="Conversion %") + 
  theme_minimal()
# Create a boxplot to view Breakpoint Conversion percentage by Surface 
ggplot(bp_long, aes(x = surface, y = conversion_percentage, fill = player_type)) +
  geom_boxplot(position = position_dodge()) +
  labs(title = "Break Point Conversion by Surface and Player Type",
       x = "Surface",
       y = "Conversion %") +
  theme_minimal()
# Create a bar chart to view Mean Break Point Conversion by Surface 
bp_long %>%
  group_by(surface, player_type) %>%
  summarise(mean_rate = mean(conversion_percentage, na.rm = TRUE)) %>%
  ggplot(aes(x = surface, y = mean_rate, fill = player_type)) +
  geom_col(position = position_dodge()) +
  labs(title = "Mean Break Point Conversion by Surface and Player Type",
       x = "Surface",
       y = "Mean Conversion %") +
  theme_minimal()

# Producing a longer format to establish breakpoints converted for the winner and loser and the value 
bp_converted <- atp %>% 
  dplyr::select(surface,w_bpconverted,l_bpconverted) %>%
  pivot_longer(cols = c(w_bpconverted,l_bpconverted), names_to = "player_type", values_to = "converted")
bp_converted

# Boxplot to show break point converted for winner and losers. 
ggplot(bp_converted, aes(x = player_type, y= converted, fill = player_type)) +
  geom_boxplot() +
  labs(title = "Break Points Converted: Winner vs Loser",
       x = "", y ="Breaks") + 
  theme_minimal()
# Create a boxplot to view Break Points Converted by Surface 
ggplot(bp_converted, aes(x = surface, y = converted, fill = player_type)) +
  geom_boxplot(position = position_dodge()) +
  labs(title = "Break Points Converted by Surface and Player Type",
       x = "Surface",
       y = "Breaks") +
  theme_minimal()
# Create a bar chart to view Mean Break Point Converted by Surface 
bp_converted %>%
  group_by(surface, player_type) %>%
  summarise(mean_rate = mean(converted, na.rm = TRUE)) %>%
  ggplot(aes(x = surface, y = mean_rate, fill = player_type)) +
  geom_col(position = position_dodge()) +
  labs(title = "Mean Break Points Converted by Surface and Player Type",
       x = "Surface",
       y = "Breaks") +
  theme_minimal()

# Producing a longer format to establish breakpoint chances for the winner and loser and the value 
bp_chances <- atp %>% 
  dplyr::select(surface,w_bptotal,l_bptotal) %>%
  pivot_longer(cols = c(w_bptotal,l_bptotal), names_to = "player_type", values_to = "chances")
bp_chances

# Boxplot to show break point chances for winner and losers. 
ggplot(bp_chances, aes(x = player_type, y= chances, fill = player_type)) +
  geom_boxplot() +
  labs(title = "Break Point Chances: Winner vs Loser",
       x = "", y ="Chances") + 
  theme_minimal()
# Create a boxplot to view Break Points Chances by Surface 
ggplot(bp_chances, aes(x = surface, y = chances, fill = player_type)) +
  geom_boxplot(position = position_dodge()) +
  labs(title = "Break Point Chances by Surface and Player Type",
       x = "Surface",
       y = "Break Chances") +
  theme_minimal()
# Create a bar chart to view Mean Break Point Chances by Surface 
bp_chances %>%
  group_by(surface, player_type) %>%
  summarise(mean_rate = mean(chances, na.rm = TRUE)) %>%
  ggplot(aes(x = surface, y = mean_rate, fill = player_type)) +
  geom_col(position = position_dodge()) +
  labs(title = "Mean Break Point Chances by Surface and Player Type",
       x = "Surface",
       y = "Break Chances") +
  theme_minimal()
# Data Modelling 
# Set seed for reproducibility 
set.seed(1)
# New format for modelling 
atp_balanced <- atp %>%
  filter(!is.na(winner_name), !is.na(loser_name)) %>%
  filter(!is.na(minutes), minutes >= 43) %>%
  filter(!is.na(winner_rank), !is.na(loser_rank)) %>%
  mutate(flip = sample(c(TRUE, FALSE), n(), replace = TRUE)) %>%
  transmute(
    outcome = ifelse(flip, 1, 0),
    surface = factor(surface),
    duration = as.numeric(minutes),
    match_date = as.Date(as.character(tourney_date), format = "%Y%m%d"),
    
    # Assign player 1 and player 2 stats based on flip 
    player1_total_points_won = ifelse(flip, w_totalpointsWon, l_totalpointsWon),
    player2_total_points_won = ifelse(flip, l_totalpointsWon, w_totalpointsWon),
    total_match_points,
    
    player1_ace = ifelse(flip, w_ace, l_ace),
    player2_ace = ifelse(flip, l_ace, w_ace),
    
    player1_df = ifelse(flip, w_df, l_df),
    player2_df = ifelse(flip, l_df, w_df),
    
    player1_1st_pct = ifelse(flip, w_1stPercentage, l_1stPercentage),
    player2_1st_pct = ifelse(flip, l_1stPercentage, w_1stPercentage),
    
    player1_1st_won_pct = ifelse(flip, w_1stWonPercentage, l_1stWonPercentage),
    player2_1st_won_pct = ifelse(flip, l_1stWonPercentage, w_1stWonPercentage),
    
    player1_2nd_won_pct = ifelse(flip, w_2ndWonPercentage, l_2ndWonPercentage),
    player2_2nd_won_pct = ifelse(flip, l_2ndWonPercentage, w_2ndWonPercentage),
    
    player1_return_won_pct = ifelse(flip, w_returnpointsWonPercentage, l_returnpointsWonPercentage),
    player2_return_won_pct = ifelse(flip, l_returnpointsWonPercentage, w_returnpointsWonPercentage),
    
    player1_bp_converted = ifelse(flip, w_bpconverted, l_bpconverted),
    player2_bp_converted = ifelse(flip, l_bpconverted, w_bpconverted),
    
    player1_bp_conversion_pct = ifelse(flip, w_bp_conversion_percentage, l_bp_conversion_percentage),
    player2_bp_conversion_pct = ifelse(flip, l_bp_conversion_percentage, w_bp_conversion_percentage),
    
    player1_bp_chances = ifelse(flip, w_bptotal, l_bptotal),
    player2_bp_chances = ifelse(flip, l_bptotal, w_bptotal),
    
    player1_rank = ifelse(flip, winner_rank, loser_rank),
    player2_rank = ifelse(flip, loser_rank, winner_rank),
    
    # Differences between variables 
    aces_diff = ifelse(flip, w_ace - l_ace, l_ace - w_ace),
    double_faults_diff = ifelse(flip, w_df - l_df, l_df - w_df),
    first_serve_pct_diff = ifelse(flip, w_1stPercentage - l_1stPercentage, l_1stPercentage - w_1stPercentage),
    first_serve_won_pct_diff = ifelse(flip, w_1stWonPercentage - l_1stWonPercentage, l_1stWonPercentage - w_1stWonPercentage),
    second_serve_won_pct_diff = ifelse(flip, w_2ndWonPercentage - l_2ndWonPercentage, l_2ndWonPercentage - w_2ndWonPercentage),
    return_points_won_pct_diff = ifelse(flip, w_returnpointsWonPercentage - l_returnpointsWonPercentage, l_returnpointsWonPercentage - w_returnpointsWonPercentage),
    break_points_converted_diff = ifelse(flip, w_bpconverted - l_bpconverted, l_bpconverted - w_bpconverted),
    break_points_conversion_pct_diff = ifelse(flip, w_bp_conversion_percentage - l_bp_conversion_percentage, l_bp_conversion_percentage - w_bp_conversion_percentage),
    break_points_chances_diff = ifelse(flip, w_bptotal - l_bptotal, l_bptotal - w_bptotal),
    ranking_diff = ifelse(flip, loser_rank - winner_rank, winner_rank - loser_rank)
  ) 

# View new format
glimpse(atp_balanced)
# Investiagte number of rows for each outcome
table(atp_balanced$outcome)
# Set minimum value 
min_size <- min(table(atp_balanced$outcome))
# Adapt dataset to remove extra losses 
atp_balanced <- atp_balanced %>%
  group_by(outcome) %>%
  slice_sample(n = min_size) %>%
  ungroup()
# View to ensure equal measure of outcomes
table(atp_balanced$outcome)
# Create match date number for order 
atp_balanced$match_date_num <- as.numeric(atp_balanced$match_date - as.Date("2024-01-01"))
# Model with all variables 
model1_ind <- glm(
  outcome ~ surface + duration + match_date_num +
    player1_ace + player2_ace +
    player1_df + player2_df +
    player1_1st_pct + player2_1st_pct +
    player1_1st_won_pct + player2_1st_won_pct +
    player1_2nd_won_pct + player2_2nd_won_pct +
    player1_bp_converted + player2_bp_converted +
    player1_bp_conversion_pct + player2_bp_conversion_pct + 
    player1_bp_chances + player2_bp_chances +
    player1_rank + player2_rank,
  data = atp_balanced,
  family = binomial()
)

summary(model1_ind)
# Import car to check multicolinearity 
library(car)

# Check for co-linearity and correlations 
vif(model1_ind)
cor(atp_balanced$outcome, atp_balanced$player1_bp_converted)
cor(atp_balanced$outcome, atp_balanced$player1_bp_conversion_pct)
cor(atp_balanced$outcome,atp_balanced$player1_bp_chances)
cor(atp_balanced$outcome, atp_balanced$player1_1st_won_pct, use = "complete.obs")
cor(atp_balanced$player1_1st_won_pct, atp_balanced$player1_bp_converted)
cor(atp_balanced$player1_rank, atp_balanced$player2_rank)
cor(atp_balanced$player1_bp_converted, atp_balanced$player1_bp_conversion_pct, use = "complete.obs")
cor(atp_balanced$player1_bp_chances, atp_balanced$player1_bp_converted, use = "complete.obs")
# Focus on Break Points converted as first Break point variable 
model2_ind <- glm(
  outcome ~ surface + duration + match_date_num +
    player1_ace + player2_ace +
    player1_df + player2_df +
    player1_1st_pct + player2_1st_pct +
    player1_1st_won_pct + player2_1st_won_pct +
    player1_2nd_won_pct + player2_2nd_won_pct +
    player1_bp_converted + player2_bp_converted +
    player1_rank + player2_rank,
  data = atp_balanced,
  family = binomial()
)

summary(model2_ind)
# Check for co-linearity 
vif(model2_ind)

# Change rank of players to ranking difference 
model3_ind <- glm(
  outcome ~ surface + duration + match_date_num +
    player1_ace + player2_ace +
    player1_df + player2_df +
    player1_1st_pct + player2_1st_pct +
    player1_1st_won_pct + player2_1st_won_pct +
    player1_2nd_won_pct + player2_2nd_won_pct +
    player1_bp_converted + player2_bp_converted +
    ranking_diff,
  data = atp_balanced,
  family = binomial()
)

summary(model3_ind)

# Change rank difference to player rank 
model3.5_ind <- glm(
  outcome ~ surface + duration + match_date_num +
    player1_ace + player2_ace +
    player1_df + player2_df +
    player1_1st_pct + player2_1st_pct +
    player1_1st_won_pct + player2_1st_won_pct +
    player1_2nd_won_pct + player2_2nd_won_pct +
    player1_bp_converted + player2_bp_converted +
    player1_rank,
  data = atp_balanced,
  family = binomial()
)

summary(model3.5_ind)

# Incorporate a quadratic term for match date in the model
model4_ind <- glm(
  outcome ~ surface + duration + match_date_num + I(match_date_num^2) +
    player1_ace + player2_ace +
    player1_df + player2_df +
    player1_1st_pct + player2_1st_pct +
    player1_1st_won_pct + player2_1st_won_pct +
    player1_2nd_won_pct + player2_2nd_won_pct +
    player1_bp_converted + player2_bp_converted + 
    player1_rank,
  data = atp_balanced,
  family = binomial()
)

summary(model4_ind)

# Add a cubic term for match date to the model
model5_ind <- glm(
  outcome ~ surface + duration + match_date_num + I(match_date_num^2) + I(match_date_num^3) +
    player1_ace + player2_ace +
    player1_df + player2_df +
    player1_1st_pct + player2_1st_pct +
    player1_1st_won_pct + player2_1st_won_pct +
    player1_2nd_won_pct + player2_2nd_won_pct +
    player1_bp_converted + player2_bp_converted +
    player1_rank,
  data = atp_balanced,
  family = binomial()
)

summary(model5_ind)

# Add these a quadratic term for duration to the model
model6_ind <- glm(
  outcome ~ surface + duration + I(duration^2) + 
    match_date_num + 
    player1_ace + player2_ace +
    player1_df + player2_df +
    player1_1st_pct + player2_1st_pct +
    player1_1st_won_pct + player2_1st_won_pct +
    player1_2nd_won_pct + player2_2nd_won_pct +
    player1_bp_converted + player2_bp_converted +
    player1_rank,
  data = atp_balanced,
  family = binomial()
)

summary(model6_ind)

# Add a cubic term for duration to the model
model7_ind <- glm(
  outcome ~ duration + I(duration^2) + I(duration^3) +
    match_date_num + I(match_date_num^2) + I(match_date_num^3) +
    player1_ace + player2_ace +
    player1_df + player2_df +
    player1_1st_pct + player2_1st_pct +
    player1_1st_won_pct + player2_1st_won_pct +
    player1_2nd_won_pct + player2_2nd_won_pct +
    player1_bp_converted + player2_bp_converted +
    player1_rank,
  data = atp_balanced,
  family = binomial()
)

summary(model7_ind)

# Change the baseline of surface to Hard 
atp_balanced$surface <- relevel(factor(atp_balanced$surface), ref = "Hard")

model9_ind <- glm(
  outcome ~ surface + duration + match_date_num +
    player1_ace + player2_ace + 
    player1_df + player2_df + 
    player1_1st_pct + player2_1st_pct +
    player1_1st_won_pct + player2_1st_won_pct +
    player1_2nd_won_pct + player2_2nd_won_pct +
    player1_bp_converted + player2_bp_converted +
    player1_rank,
  data = atp_balanced,
  family = binomial()
)

summary(model9_ind)

# Change the baseline of surface to Grass 
atp_balanced$surface <- relevel(factor(atp_balanced$surface), ref = "Grass") 

model10_ind <- glm(
  outcome ~ surface + duration + 
    match_date_num +
    player1_ace + player2_ace + 
    player1_df + player2_df + 
    player1_1st_pct + player2_1st_pct +
    player1_1st_won_pct + player2_1st_won_pct +
    player1_2nd_won_pct + player2_2nd_won_pct +
    player1_bp_converted + player2_bp_converted +
    player1_rank,
  data = atp_balanced,
  family = binomial()
)

summary(model10_ind)

# Remove surface, duration, match date number, aces and opponent double faults 
model11_ind <- glm(
  outcome ~ player1_df + 
    player1_1st_pct + player2_1st_pct +
    player1_1st_won_pct + player2_1st_won_pct +
    player1_2nd_won_pct + player2_2nd_won_pct +
    player1_bp_converted + player2_bp_converted +
    player1_rank,
  data = atp_balanced,
  family = binomial()
)

summary(model11_ind)

# Load sjplot for clean table to add to report 
install.packages("sjPlot")
library(sjPlot)

tab_model(model11_ind, show.ci = FALSE, show.se = TRUE, show.std = TRUE, transform = NULL,
          dv.labels = "Match Outcome (Win = 1)")

# Investigate use of break point conversion rate on all variables 
model12_ind <- glm(
  outcome ~ surface + duration + match_date_num +
    player1_ace + player2_ace +
    player1_df + player2_df +
    player1_1st_pct + player2_1st_pct +
    player1_1st_won_pct + player2_1st_won_pct +
    player1_2nd_won_pct + player2_2nd_won_pct +
    player1_bp_conversion_pct + player2_bp_conversion_pct +
    player1_rank,
  data = atp_balanced,
  family = binomial()
)

summary(model12_ind)

# Remove surface, duration, match date, aces, opponent double faults
model12.5_ind <- glm(
  outcome ~ player1_df + 
    player1_1st_pct + player2_1st_pct +
    player1_1st_won_pct + player2_1st_won_pct +
    player1_2nd_won_pct + player2_2nd_won_pct +
    player1_bp_conversion_pct + player2_bp_conversion_pct + 
    player1_rank,
  data = atp_balanced,
  family = binomial()
)

summary(model12.5_ind)

tab_model(model12.5_ind, show.ci = FALSE, show.se = TRUE, show.std = TRUE, transform = NULL,
         dv.labels = "Match Outcome (Win = 1)")

cor(atp_balanced$outcome,atp_balanced$player1_bp_converted)
cor(atp_balanced$outcome,atp_balanced$player1_bp_conversion_pct)
cor(atp_balanced$player1_bp_converted,atp_balanced$player1_1st_won_pct)
cor(atp_balanced$player1_1st_won_pct,atp_balanced$player1_bp_conversion_pct)
#Investigate use of break point chances with all variables 
model13_ind <- glm(
  outcome ~ surface + duration + 
    match_date_num +
    player1_ace + player2_ace +
    player1_df + player2_df +
    player1_1st_pct + player2_1st_pct +
    player1_1st_won_pct + player2_1st_won_pct +
    player1_2nd_won_pct + player2_2nd_won_pct +
    player1_bp_chances + player2_bp_chances +
    player1_rank,
  data = atp_balanced,
  family = binomial()
)

summary(model13_ind)
cor(atp_balanced$outcome,atp_balanced$player2_bp_chances)

# Remove duration, match date, opponent double faults and player aces
model14_hard_ind <- glm(
  outcome ~ 
    player2_ace + surface + player1_df +
    player1_1st_pct + player2_1st_pct +
    player1_1st_won_pct + player2_1st_won_pct +
    player1_2nd_won_pct + player2_2nd_won_pct +
    player1_bp_chances + player2_bp_chances +
    player1_rank,
  data = atp_balanced,
  family = binomial()
)
summary(model14_hard_ind)
# Change baseline to Grass
atp_balanced$surface <- relevel(factor(atp_balanced$surface), ref = "Grass") 
model14_grass_ind <- glm(
  outcome ~ 
    player2_ace + surface + player1_df + 
    player1_1st_pct + player2_1st_pct +
    player1_1st_won_pct + player2_1st_won_pct +
    player1_2nd_won_pct + player2_2nd_won_pct +
    player1_bp_chances + player2_bp_chances +
    player1_rank,
  data = atp_balanced,
  family = binomial()
)
summary(model14_grass_ind)

# Change baseline to Clay
atp_balanced$surface <- relevel(factor(atp_balanced$surface), ref = "Clay")

model14_clay_ind <- glm(
  outcome ~ 
    player2_ace + surface + player1_df +
    player1_1st_pct + player2_1st_pct +
    player1_1st_won_pct + player2_1st_won_pct +
    player1_2nd_won_pct + player2_2nd_won_pct +
    player1_bp_chances + player2_bp_chances +
    player1_rank,
  data = atp_balanced,
  family = binomial()
)

summary(model14_clay_ind)
tab_model(model14_clay_ind, show.ci = FALSE, show.se = TRUE, show.std = TRUE, transform = NULL,
          dv.labels = "Match Outcome (Win = 1)")
cor(atp_balanced$player2_df, atp_balanced$player2_bp_chances)

# Remove duration, match date, surface, opponent double faults and aces
model14_ind <- glm(
  outcome ~ 
    player1_df +
    player1_1st_pct + player2_1st_pct +
    player1_1st_won_pct + player2_1st_won_pct +
    player1_2nd_won_pct + player2_2nd_won_pct +
    player1_bp_chances + player2_bp_chances +
    player1_rank,
  data = atp_balanced,
  family = binomial()
)
summary(model14_ind)

model14_noserve_ind <- glm(
  outcome ~ 
    player1_df +
    player1_bp_chances + player2_bp_chances,
  data = atp_balanced,
  family = binomial()
)
summary(model14_noserve_ind)
tab_model(model14_noserve_ind, show.ci = FALSE, show.se = TRUE, show.std = TRUE, transform = NULL,
          dv.labels = "Match Outcome (Win = 1)")

# Load ggeffects for marginal effects 
install.packages("ggeffects")
library(ggeffects)
# Create marginal effects for breakpoint chances with serve statistics 
pred_with_serve <- ggeffect(model14_ind, "player1_bp_chances")
pred_with_serve
# Create marginal effects for breakpoint chances without serve statistics
pred_no_serve <- ggeffect(model14_noserve_ind, "player1_bp_chances")
pred_no_serve
pred_no_serve$model <- "Without Serve stats"
pred_with_serve$model <- "With Serve stats"
# Bind tables together 
pred_all <- rbind(pred_no_serve, pred_with_serve)
# Produce a graph for marginal effects 
ggplot(pred_all, aes(x = x, y = predicted, color = model)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = model), alpha = 0.2, color = NA) +
  labs(x = "Player 1 Break Point Chances",
       y = "Predicted Probability of Winning",
       title = "Effect of Break Points with vs without Serve Stats") +
  theme_minimal()
# Variable importance for each final model 
varImp(model11_ind)
imp_bp_converted <- varImp(model11_ind)
varImp(model12.5_ind)
imp_bp_con_pct <- varImp(model12.5_ind)
varImp(model14_clay_ind)
imp_bp_chances <- varImp(model14_clay_ind)  

# Investigate differences between player in new variables, add a return % points won variable

model1_diff <- glm(outcome ~ aces_diff + double_faults_diff + first_serve_pct_diff +
               first_serve_won_pct_diff + second_serve_won_pct_diff + break_points_converted_diff + 
               ranking_diff + surface + duration + 
               match_date_num,
             data = atp_balanced,
             family = binomial)

summary(model1_diff)

# Investigate co linearity and correlations 
vif(model1_diff)

# Remove duration, and match date number 
model2_diff <- glm(outcome ~ aces_diff + double_faults_diff + first_serve_pct_diff +
                     first_serve_won_pct_diff + second_serve_won_pct_diff + break_points_converted_diff + 
                     ranking_diff + surface, 
                   data = atp_balanced,
                   family = binomial)

summary(model2_diff)

# Check for co-linearity 
vif(model2_diff)

# Remove surface and change ranking differnece to player rank 
model3_diff <- glm(outcome ~ aces_diff + double_faults_diff + first_serve_pct_diff +
                     first_serve_won_pct_diff + second_serve_won_pct_diff + break_points_converted_diff +
                     player1_rank, 
                   data = atp_balanced,
                   family = binomial)

summary(model3_diff)
cor(atp_balanced$outcome_num, atp_balanced$break_points_converted_diff)
cor(atp_balanced$outcome_num, atp_balanced$first_serve_won_pct_diff)

# Check for co-linearity 
vif(model3_diff)
cor(atp_balanced$aces_diff, atp_balanced$break_points_converted_diff)

# Remove aces and double faults 
model4_diff <- glm(outcome ~ first_serve_pct_diff +
                     first_serve_won_pct_diff + second_serve_won_pct_diff + break_points_converted_diff + 
                     player1_rank,
                     data = atp_balanced,
                   family = binomial)

summary(model4_diff)
varImp(model4_diff)

# Investigate with break points rate instead of break points converted with all variables 

model6_diff <- glm(outcome ~ aces_diff + double_faults_diff + first_serve_pct_diff + 
                     first_serve_won_pct_diff + second_serve_won_pct_diff + break_points_conversion_pct_diff + 
                     player1_rank + surface, 
                   data = atp_balanced,
                   family = binomial)

summary(model6_diff)
cor(atp_balanced$outcome_num, atp_balanced$break_points_rate_diff)

# Remove aces, double faults, and surface and player ranking 

model7_diff <- glm(outcome ~ first_serve_pct_diff +
                     first_serve_won_pct_diff + second_serve_won_pct_diff + 
                     break_points_conversion_pct_diff, 
                   data = atp_balanced,
                   family = binomial)

summary(model7_diff)

# Investigate with break point chances 

model8_diff <- glm(outcome ~ aces_diff + double_faults_diff + first_serve_pct_diff +
                     first_serve_won_pct_diff + second_serve_won_pct_diff + break_points_chances_diff + 
                     player1_rank + surface, 
                   data = atp_balanced,
                   family = binomial)

summary(model8_diff)

# Remove double faults 

model9_diff <- glm(outcome ~ aces_diff + first_serve_pct_diff +
                     first_serve_won_pct_diff + second_serve_won_pct_diff +
                     break_points_chances_diff + player1_rank + surface, 
                   data = atp_balanced,
                   family = binomial)

summary(model9_diff)

# Remove surface

model10_diff <- glm(outcome ~ aces_diff + first_serve_pct_diff + first_serve_won_pct_diff +
                      second_serve_won_pct_diff + break_points_chances_diff +
                      player1_rank,
                   data = atp_balanced,
                   family = binomial)

summary(model10_diff)

varImp(model10_diff)

# Create a function  for probability changes 

simulate_probability_changes <- function(model, data, change = 1, units = "abs") {
  coefs <- coef(model)
  predictor_names <- names(coefs)[-1]  # exclude intercept
  
  # Create baseline profile with means for numeric and mode for factors
  baseline <- data %>%
    dplyr::summarise(across(all_of(predictor_names), ~ {
      if (is.numeric(.)) mean(., na.rm = TRUE)
      else names(sort(table(.), decreasing = TRUE))[1]
    })) %>%
    as.data.frame()
  
  results <- lapply(predictor_names, function(var) {
    new_data <- baseline
    
    # Increase predictor by 'change' if numeric
    if (is.numeric(data[[var]])) {
      new_data[[var]] <- new_data[[var]] + change
    } else {
    }
    
    # Predict probabilities before and after the change
    prob_before <- predict(model, newdata = baseline, type = "response")
    prob_after  <- predict(model, newdata = new_data, type = "response")
    
    change_val <- prob_after - prob_before
    
    # Convert to percentage if units == "pct"
    if (units == "pct") change_val <- change_val * 100
    
    data.frame(
      Variable = var,
      Beta = coefs[var],
      Prob_Change = change_val
    )
  })
  
  do.call(rbind, results)
}
# Summary serve statistics check 
summary(atp_balanced$player1_1st_won_pct)
summary(atp_balanced$player1_2nd_won_pct)
# Probability change for all 3 final models  
sim_model11_ind <- simulate_probability_changes(model11_ind, atp_balanced, change = 1, units = "pct")
view(sim_model11_ind)
sim_model12.5_ind <- simulate_probability_changes(model12.5_ind, atp_balanced, change = 1, units = "pct")
view(sim_model12.5_ind)
sim_model14_ind <- simulate_probability_changes(model14_ind, atp_balanced, change = 1, units = "pct")
view(sim_model14_ind)

# Cross Validation for all Outcome final models 
# Set seed for reproducibility 
set.seed(123)
# Set cross validation control 
cv_control <- trainControl(method = "cv", number = 10, savePredictions = TRUE,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
# Make outcome variable a factor with lose and win 
atp_balanced$outcome <- factor(ifelse(atp_balanced$outcome == 1, "win", "lose"),
                               levels = c("lose", "win"))
# Making sure it is recorded as a factor 
atp_balanced$outcome <- as.factor(atp_balanced$outcome)

# CV for end model with break points converted 

model1_cv <- train(outcome ~
                     player1_df + 
                     player1_1st_pct + player2_1st_pct +
                     player1_1st_won_pct + player2_1st_won_pct +
                     player1_2nd_won_pct + player2_2nd_won_pct +
                     player1_bp_converted + player2_bp_converted +
                     player1_rank,
                   data = atp_balanced,
                   method = "glm",
                   family = "binomial",
                   trControl = cv_control,
                   metric = "ROC")

print(model1_cv)


# CV for end model with break point conversion rate 
model2_cv <- train(outcome ~
                     player1_df + 
                     player1_1st_pct + player2_1st_pct +
                     player1_1st_won_pct + player2_1st_won_pct +
                     player1_2nd_won_pct + player2_2nd_won_pct +
                     player1_bp_conversion_pct + player2_bp_conversion_pct + 
                     player1_rank,
                   data = atp_balanced,
                   method = "glm",
                   family = "binomial",
                   trControl = cv_control,
                   metric = "ROC")

print(model2_cv)
varImp(model2_cv)

# CV for end model with break point chances 
model3_cv <- train(outcome ~ player2_ace + player1_df + surface +
                     player1_1st_pct + player2_1st_pct +
                     player1_1st_won_pct + player2_1st_won_pct +
                     player1_2nd_won_pct + player2_2nd_won_pct +
                     player1_bp_chances + player2_bp_chances + 
                     player1_rank,
                   data = atp_balanced,
                   method = "glm",
                   family = "binomial",
                   trControl = cv_control,
                   metric = "ROC")

print(model3_cv)
varImp(model3_cv)


# CV for end model with difference variables and break point conversion 
model1_cv_diff <- train(outcome ~ first_serve_pct_diff +
                          first_serve_won_pct_diff + second_serve_won_pct_diff + break_points_converted_diff +
                          player1_rank,
                        data = atp_balanced,
                        method = "glm",
                        family = "binomial",
                        trControl = cv_control,
                        metric = "ROC")

print(model1_cv_diff)
varImp(model1_cv_diff)

# CV for end model with difference variables and break point conversion rate 
model2_cv_diff <- train(outcome ~ first_serve_pct_diff +
                          first_serve_won_pct_diff + second_serve_won_pct_diff + break_points_conversion_pct_diff +
                          player1_rank,
                        data = atp_balanced,
                        method = "glm",
                        family = "binomial",
                        trControl = cv_control,
                        metric = "ROC")

print(model2_cv_diff)
varImp(model2_cv_diff)


# CV for end model with difference variables and break point chances 
model3_cv_diff <- train(outcome ~ aces_diff + first_serve_pct_diff + first_serve_won_pct_diff +
                          second_serve_won_pct_diff + break_points_chances_diff +
                          player1_rank,
                        data = atp_balanced,
                        method = "glm",
                        family = "binomial",
                        trControl = cv_control,
                        metric = "ROC")

print(model3_cv_diff)
varImp(model3_cv_diff)

# install pROC for cross validated roc curve 
install.packages("pROC")
library(pROC)
model1_cv$pred$outcome <- as.factor(model1_cv$pred$obs)
head(model1_cv$pred)
roc_obj <- roc(model1_cv$pred$obs, model1_cv$pred$win) 
plot(roc_obj, main = "Cross-Validated ROC Curve")
auc(roc_obj)

#Change cbind to a matrix 
atp_balanced <- atp_balanced %>%
  mutate(p1_points_matrix = cbind(player1_total_points_won, player2_total_points_won))

# Create null model for propotion of points won model
model_null <- glm(
  p1_points_matrix ~
    1,
  data = atp_balanced, 
  family = binomial(link = "logit")
)
summary(model_null)

# All variables with breakpoints converted
model_pct <- glm(
  p1_points_matrix ~
    player1_ace + player2_ace +
    player1_df + player2_df +
    player1_1st_pct + player2_1st_pct +
    player1_1st_won_pct + player2_1st_won_pct +
    player1_2nd_won_pct + player2_2nd_won_pct +
    player1_bp_converted + player2_bp_converted +
    player1_rank,
  data = atp_balanced,
  family = binomial(link = "logit")
)
summary(model_pct)

#load pscl for enhanced analysis 
install.packages("pscl")
library(pscl)
# Extra metrics 
pR2(model_pct)
# Check correlations and multicolinearity 
vif(model_pct)
cor(atp_balanced$p1_points_matrix, atp_balanced$player1_bp_converted)
cor(atp_balanced$p1_points_matrix, atp_balanced$player1_ace)
cor(atp_balanced$p1_points_matrix, atp_balanced$player1_bp_chances)
cor(atp_balanced$p1_points_matrix, atp_balanced$player1_1st_won_pct)
# Remove any non statistically significant variables
model2_pct <- glm(
  p1_points_matrix ~ 
    player1_1st_pct + player2_1st_pct +
    player1_1st_won_pct + player2_1st_won_pct +
    player1_2nd_won_pct + player2_2nd_won_pct +
    player1_bp_converted + player2_bp_converted, 
  data = atp_balanced,
  family = binomial(link = "logit")
)
summary(model2_pct)
pR2(model2_pct)

# Change break points converted to a rate 
model3_pct <- glm(
  p1_points_matrix ~
    player1_1st_pct + player2_1st_pct +
    player1_1st_won_pct + player2_1st_won_pct +
    player1_2nd_won_pct + player2_2nd_won_pct +
    player1_bp_conversion_pct + player2_bp_conversion_pct,
  data = atp_balanced,
  family = binomial(link = "logit")
)
summary(model3_pct)

# Change break points converted rate to break point chances 
model4_pct <- glm(
  p1_points_matrix ~ 
    player1_1st_pct + player2_1st_pct +
    player1_1st_won_pct + player2_1st_won_pct +
    player1_2nd_won_pct + player2_2nd_won_pct +
    player1_bp_chances + player2_bp_chances,
  data = atp_balanced,
  family = binomial(link = "logit")
)
summary(model4_pct)

# Look at percentage difference stats with break point converted
model5_pct <- glm(
  p1_points_matrix ~
    first_serve_pct_diff + first_serve_won_pct_diff +
    second_serve_won_pct_diff + break_points_converted_diff,
  data = atp_balanced,
  family = binomial(link = "logit")
)
summary(model5_pct)

# Look at percentage difference stats with break point converted percentage 
model6_pct <- glm(
  p1_points_matrix ~
    first_serve_pct_diff + first_serve_won_pct_diff +
    second_serve_won_pct_diff + break_points_conversion_pct_diff,
  data = atp_balanced,
  family = binomial(link = "logit")
)
summary(model6_pct)
pR2(model6_pct)
tab_model(model6_pct, show.ci = FALSE, show.se = TRUE, show.std = TRUE, transform = NULL,
          dv.labels = "Proportion of points won")

# Look at percentage difference stats with break point chances
model7_pct <- glm(
  p1_points_matrix ~
    first_serve_pct_diff + first_serve_won_pct_diff +
    second_serve_won_pct_diff + break_points_chances_diff,
  data = atp_balanced,
  family = binomial(link = "logit")
)
summary(model7_pct)

# Create a function for marginal change for each variable 
marginal_pp_change_binomial_mean <- function(model, delta = 1, units = "pct") {
  # Extract model data frame
  data <- model.frame(model)
  
  # Get baseline profile (means for numeric, mode for factors)
  baseline <- data %>% 
    summarise(across(everything(), ~ {
      if (is.numeric(.)) mean(., na.rm = TRUE) else names(sort(table(.), decreasing = TRUE))[1]
    }))
  
  results <- list()
  
  # Loop over numeric predictors in the model
  for (var in names(baseline)) {
    if (!is.numeric(baseline[[var]])) next
    
    new_data <- baseline
    new_data[[var]] <- new_data[[var]] + delta
    
    # Predict before and after
    prob_before <- predict(model, newdata = baseline, type = "response")
    prob_after  <- predict(model, newdata = new_data, type = "response")
    
    change <- prob_after - prob_before
    if (units == "pct") change <- change * 100
    
    results[[var]] <- change
  }
  
  return(data.frame(
    variable = names(results),
    change   = unlist(results)
  ))
}

# Chose which model to select 
marginal_results_mean <- marginal_pp_change_binomial_mean(
  model6_pct,
  delta = 10,  
  units = "pct"
)
print(marginal_results_mean)

# Load boot for bootstrapping 
library(boot)

# RMSE function for binomial glm with cbind()
rmse_fn <- function(data, indices, model) {
  d <- data[indices, ]
  
  # Predicted probabilities of points won
  preds <- predict(model, type = "response")
  
  # Actual proportion of points won
  actual <- d$player1_total_points_won / d$total_match_points
  
  sqrt(mean((preds - actual)^2))
}

# MAE function
mae_fn <- function(data, indices, model) {
  d <- data[indices, ]
  
  preds <- predict(model, type = "response")
  actual <- d$player1_total_points_won / d$total_match_points
  
  mean(abs(preds - actual))
}

# Bootstrap RMSE
set.seed(123)
cv_results_rmse <- boot(
  data = atp_balanced, 
  statistic = rmse_fn, 
  model = model6_pct,
  R = 100)
# Mean RMSE from original data
cv_results_rmse$t0
# Mean RMSE from bootstraps
mean(cv_results_rmse$t) 

# Bootstrap MAE
set.seed(123)
cv_results_mae <- boot(
  data = atp_balanced,
  statistic = mae_fn,
  model = model6_pct,
  R = 100)
# Mean MAE from original data
cv_results_mae$t0     
# Mean MAE from bootstraps
mean(cv_results_mae$t) 

# 95% CI for RMSE
boot.ci(cv_results_rmse, type = c("perc"))

# 95% CI for MAE
boot.ci(cv_results_mae, type = c("perc"))

# Regularisation 
library(glmnet)
# Prepare data for breakpoints converted model
X <- model.matrix(outcome ~ duration + match_date_num + player1_ace + player2_ace +
                    player1_df + player2_df +
                    player1_1st_pct + player2_1st_pct +
                    player1_1st_won_pct + player2_1st_won_pct +
                    player1_2nd_won_pct + player2_2nd_won_pct +
                    player1_bp_converted + player2_bp_converted +
                    player1_rank + player2_rank,data = atp_balanced)[,-1]
y <- atp_balanced$outcome

# Fit Lasso model
cvfit <- cv.glmnet(X, y, family = "binomial", alpha = 1)
plot(cvfit)
plot(cvfit$glmnet.fit, xvar = "lambda", label = TRUE)
best_lambda <- cvfit$lambda.min
print(best_lambda)
lasso_model <- glmnet(X, y, alpha = 1, lambda = best_lambda, family = "binomial")
coef(lasso_model)

# Fit Ridge model
cvfit_ridge <- cv.glmnet(X, y, alpha = 0, family = "binomial")
best_lambda_ridge <- cvfit_ridge$lambda.min
print(best_lambda_ridge)
ridge_model <- glmnet(X,y, alpha = 0, lambda = best_lambda_ridge, family = "binomial")
coef(ridge_model)

# Select the predictors only (excluding outcome)
predictors <- atp_balanced[, c( "player1_df", "player1_1st_pct", "player2_1st_pct", 
                                "player1_1st_won_pct", "player2_1st_won_pct", 
                                "player1_2nd_won_pct", "player2_2nd_won_pct", 
                                "player1_bp_converted", "player2_bp_converted", 
                                "player1_rank")]

# Standardise predictors
predictors_scaled <- as.data.frame(scale(predictors))


# Create model matrix (including intercept automatically)
X_scaled <- model.matrix(~ player1_df + 
                           player1_1st_pct + player2_1st_pct +
                           player1_1st_won_pct + player2_1st_won_pct +
                           player1_2nd_won_pct + player2_2nd_won_pct +
                           player1_bp_converted + player2_bp_converted +
                           player1_rank, data = predictors_scaled)[,-1]

y <- atp_balanced$outcome

# Lasso scaled 
cvfit_scaled <- cv.glmnet(X_scaled, y, alpha = 1, family = "binomial")
best_lambda_scaled <- cvfit_scaled$lambda.min
print(best_lambda_scaled)
lasso_model_scaled <- glmnet(X_scaled, y, alpha = 1, lambda = best_lambda_scaled, family = "binomial")
coef(lasso_model_scaled)
plot(cvfit_scaled$glmnet.fit, xvar = "lambda", label = TRUE)

# Ridge scaled 
cvfit_ridge <- cv.glmnet(X_scaled, y, alpha = 0, family = "binomial")
best_lambda_ridge <- cvfit_ridge$lambda.min
print(best_lambda_ridge)
ridge_model_scaled <- glmnet(X_scaled,y, alpha = 0, lambda = best_lambda_ridge, family = "binomial")
coef(ridge_model_scaled)

# Extract coefficients (excluding intercept)
coef_df_bp_converted <- as.data.frame(as.matrix(coef(lasso_model_scaled)))
coef_df_bp_converted <- coef_df_bp_converted[-1, , drop=FALSE]  # remove intercept row
coef_df_bp_converted$variable <- rownames(coef_df_bp_converted)
colnames(coef_df_bp_converted)[1] <- "coefficient"

# Calculate absolute coefficients for importance
coef_df_bp_converted$importance <- abs(coef_df_bp_converted$coefficient)

# Order variables by importance descending
coef_df_bp_converted <- coef_df_bp_converted[order(coef_df_bp_converted$importance, decreasing = TRUE), ]

# Plot graph for variable importance using lasso scaled coefficients 
ggplot(coef_df_bp_converted, aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance from Lasso Model",
       x = "Variable",
       y = "Absolute Coefficient") +
  theme_minimal()

# Repeat Lasso and ridge for model with break point chances
X <- model.matrix(outcome ~ player1_ace +
                    player2_ace + player1_df + 
                    player1_1st_pct + player2_1st_pct +
                    player1_1st_won_pct + player2_1st_won_pct +
                    player1_2nd_won_pct + player2_2nd_won_pct +
                    player1_bp_chances + player2_bp_chances +
                    player1_rank, data = atp_balanced)[,-1]
y <- atp_balanced$outcome

# Fit Lasso model
cvfit_bp_chance <- cv.glmnet(X, y, family = "binomial", alpha = 1)
plot(cvfit_bp_chance)
plot(cvfit_bp_chance$glmnet.fit, xvar = "lambda", label = TRUE)
best_lambda_bp_chance <- cvfit_bp_chance$lambda.min
print(best_lambda_bp_chance)
lasso_model_bp_chance <- glmnet(X, y, alpha = 1, lambda = best_lambda_bp_chance, family = "binomial")
coef(lasso_model_bp_chance)

# Fit Ridge model
cvfit_ridge_bp_chance<- cv.glmnet(X, y, alpha = 0, family = "binomial")
best_lambda_ridge_bp_chance <- cvfit_ridge_bp_chance$lambda.min
print(best_lambda_ridge_bp_chance)
ridge_model_bp_chance <- glmnet(X,y, alpha = 0, lambda = best_lambda_ridge_bp_chance, family = "binomial")
coef(ridge_model_bp_chance)

# Select the predictors only (excluding outcome)
predictors_bp_chance <- atp_balanced[, c( "player2_ace", "player1_df","player1_1st_pct", "player2_1st_pct", 
                                          "player1_1st_won_pct", "player2_1st_won_pct", 
                                          "player1_2nd_won_pct", "player2_2nd_won_pct",
                                          "player1_bp_chances", "player2_bp_chances",
                                          "player1_rank")]

# Standardise predictors
predictors_scaled_bp_chance <- as.data.frame(scale(predictors_bp_chance))

# Create model matrix (including intercept automatically)
X_scaled_bp_chance <- model.matrix(~ 
                                     player2_ace + player1_df + 
                                     player1_1st_pct + player2_1st_pct +
                                     player1_1st_won_pct + player2_1st_won_pct +
                                     player1_2nd_won_pct + player2_2nd_won_pct +
                                     player1_bp_chances + player2_bp_chances +
                                     player1_rank, data = predictors_scaled_bp_chance)[,-1]

y <- atp_balanced$outcome

# Lasso model scaled 
cvfit_scaled_bp_chance <- cv.glmnet(X_scaled_bp_chance, y, alpha = 1, family = "binomial")  # change family if needed
best_lambda_scaled_bp_chance <- cvfit_scaled_bp_chance$lambda.min
print(best_lambda_scaled_bp_chance)
lasso_model_scaled_bp_chance <- glmnet(X_scaled_bp_chance, y, alpha = 1, lambda = best_lambda_scaled_bp_chance, family = "binomial")
coef(lasso_model_scaled_bp_chance)
plot(cvfit_scaled_bp_chance$glmnet.fit, xvar = "lambda", label = TRUE)

# Ridge model scaled 
cvfit_ridge_bp_chance <- cv.glmnet(X_scaled_bp_chance, y, alpha = 0, family = "binomial")
best_lambda_ridge_bp_chance <- cvfit_ridge_bp_chance$lambda.min
print(best_lambda_ridge_bp_chance)
ridge_model_scaled_bp_chance <- glmnet(X_scaled_bp_chance,y, alpha = 0, lambda = best_lambda_ridge_bp_chance, family = "binomial")
coef(ridge_model_scaled_bp_chance)

# Extract coefficients (excluding intercept)
coef_df_bp_chance <- as.data.frame(as.matrix(coef(lasso_model_bp_chance)))
coef_df_bp_chance <- coef_df_bp_chance[-1, , drop=FALSE]  # remove intercept row
coef_df_bp_chance$variable <- rownames(coef_df_bp_chance)
colnames(coef_df_bp_chance)[1] <- "coefficient"

# Calculate absolute coefficients for importance
coef_df_bp_chance$importance <- abs(coef_df_bp_chance$coefficient)

# Order variables by importance descending
coef_df_bp_chance <- coef_df_bp_chance[order(coef_df_bp_chance$importance, decreasing = TRUE), ]

# Plot graph for variable importance using lasso coefficients 
ggplot(coef_df_bp_chance, aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance from Lasso Model",
       x = "Variable",
       y = "Absolute Coefficient") +
  theme_minimal()

# Lasso and ridge for model with percentage points won 
# Prepare data
X <- model.matrix(~ first_serve_pct_diff + first_serve_won_pct_diff + 
                    second_serve_won_pct_diff + break_points_conversion_pct_diff,
                  data = atp_balanced)[,-1]
y <- cbind(atp_balanced$player1_total_points_won, atp_balanced$player2_total_points_won)

# Fit Lasso model
cvfit_pct <- cv.glmnet(X, y, family = "binomial", alpha = 1)
plot(cvfit_pct)
plot(cvfit_pct$glmnet.fit, xvar = "lambda", label = TRUE)
best_lambda_pct <- cvfit_pct$lambda.min
print(best_lambda_pct)
lasso_model_pct <- glmnet(X, y, alpha = 1, lambda = best_lambda_pct, family = "binomial")
coef(lasso_model_pct)

# Fit Ridge model
cvfit_ridge_pct <- cv.glmnet(X, y, alpha = 0, family = "binomial")
plot(cvfit_ridge_pct)
plot(cvfit_ridge_pct$glmnet.fit, xvar = "lambda", label = TRUE)
best_lambda_ridge_pct <- cvfit_ridge_pct$lambda.min
print(best_lambda_ridge_pct)
ridge_model_pct <- glmnet(X,y, alpha = 0, lambda = best_lambda_ridge_pct, family = "binomial")
coef(ridge_model_pct)


# Select the predictors only (excluding outcome)
predictors_pct <- atp_balanced[, c("first_serve_pct_diff","first_serve_won_pct_diff", 
                                            "second_serve_won_pct_diff", "break_points_conversion_pct_diff")]

# Standardise predictors
predictors_scaled_pct <- as.data.frame(scale(predictors_pct))

# Create model matrix (including intercept automatically)
X_scaled_pct <- model.matrix(~ 
                              first_serve_pct_diff + first_serve_won_pct_diff + 
                              second_serve_won_pct_diff + break_points_conversion_pct_diff, data = predictors_scaled_pct)[,-1]

y <- cbind(atp_balanced$player1_total_points_won, atp_balanced$player2_total_points_won)

# Lasso model scaled 
cvfit_scaled_pct <- cv.glmnet(X_scaled_pct, y, alpha = 1, family = "binomial")
best_lambda_scaled_pct <- cvfit_scaled_pct$lambda.min
print(best_lambda_scaled_pct)
lasso_model_scaled_pct <- glmnet(X_scaled_pct, y, alpha = 1, lambda = best_lambda_scaled_pct, family = "binomial")
coef(lasso_model_scaled_pct)
plot(cvfit_scaled_pct$glmnet.fit, xvar = "lambda", label = TRUE)

# Ridge model scaled 
cvfit_ridge_pct <- cv.glmnet(X_scaled_pct, y, alpha = 0, family = "binomial")
best_lambda_ridge_pct <- cvfit_ridge_pct$lambda.min
print(best_lambda_ridge_pct)
ridge_model_scaled_pct <- glmnet(X_scaled_pct,y, alpha = 0, lambda = best_lambda_ridge_pct, family = "binomial")
coef(ridge_model_scaled_pct)

# Extract coefficients (excluding intercept)
coef_df_pct <- as.data.frame(as.matrix(coef(lasso_model_scaled_pct)))
coef_df_pct <- coef_df_pct[-1, , drop=FALSE]  # remove intercept row
coef_df_pct$variable <- rownames(coef_df_pct)
colnames(coef_df_pct)[1] <- "coefficient"

# Calculate absolute coefficients for importance
coef_df_pct$importance <- abs(coef_df_pct$coefficient)

# Order variables by importance descending
coef_df_pct <- coef_df_pct[order(coef_df_pct$importance, decreasing = TRUE), ]

# Plot variable importance using lasso scaled coefficients 
ggplot(coef_df_pct, aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance from Lasso Model",
       x = "Variable",
       y = "Absolute Coefficient") +
  theme_minimal()

# General Additive Models 
# Load mgcv to investiagte non-linear models 
library(mgcv)
# initial gam model with breapoints converted 
base_gam_model<- gam(
  outcome ~ s(match_date_num) + s(duration) + surface +
    s(player1_ace) + s(player2_ace) + 
    s(player1_df) + s(player2_df) +
    s(player1_1st_pct) + s(player2_1st_pct) +
    s(player1_1st_won_pct) + s(player2_1st_won_pct) +
    s(player1_2nd_won_pct) + s(player2_2nd_won_pct) +
    s(player1_bp_converted) + s(player2_bp_converted) +
    s(player1_rank),
  data = atp_balanced,
  family = binomial
)
summary(base_gam_model)
# Using only significant variables 
gam_model1_ind <- gam(
  outcome ~ s(player1_df) +
    s(player1_1st_pct) + s(player2_1st_pct) +
    s(player1_1st_won_pct) + s(player2_1st_won_pct) +
    s(player1_2nd_won_pct) + s(player2_2nd_won_pct) +
    s(player1_bp_converted) + s(player2_bp_converted) +
    s(player1_rank),
  data = atp_balanced,
  family = binomial
)
summary(gam_model1_ind)
# Compare AIC with glm model 
AIC(model11_ind,gam_model1_ind)
# Analysis of deviance table 
anova(model11_ind,gam_model1_ind)
# Plot to see non-linear variable relationships 
plot(gam_model1_ind, pages = 1, shade = TRUE)
# Final model removing splines for linear variables 
final_model1_ind <- gam(
  outcome ~ player1_df + 
    player1_1st_pct + player2_1st_pct +
    player1_1st_won_pct + s(player2_1st_won_pct) +
    player1_2nd_won_pct + player2_2nd_won_pct +
    s(player1_bp_converted) + s(player2_bp_converted) +
    player1_rank,
  data = atp_balanced,
  family = binomial
)
summary(final_model1_ind)
AIC(model11_ind,final_model1_ind)
anova(model11_ind,final_model1_ind)
plot(final_model1_ind, pages = 1, shade = TRUE)

# Gam for breakpoint chances 
gam_model2_ind <- gam(
  outcome ~ s(player2_ace) + s(player1_df) + surface +
    s(player1_1st_pct) + s(player2_1st_pct) +
    s(player1_1st_won_pct) + s(player2_1st_won_pct) +
    s(player1_2nd_won_pct) + s(player2_2nd_won_pct) +
    s(player1_bp_chances) + s(player2_bp_chances) +
    s(player1_rank),
  data = atp_balanced,
  family = binomial
)
summary(gam_model2_ind)
AIC(model14_ind,gam_model2_ind)
anova(model14_ind,gam_model2_ind)
plot(gam_model2_ind, pages = 1, shade = TRUE)

# removed splines for linear variables 
gam_model3_ind <- gam(
  outcome ~ player2_ace + player1_df + surface +
    s(player1_1st_pct) + player2_1st_pct +
    s(player1_1st_won_pct) + s(player2_1st_won_pct) +
    player1_2nd_won_pct + s(player2_2nd_won_pct) +
    player1_bp_chances + s(player2_bp_chances) +
    player1_rank,
  data = atp_balanced,
  family = binomial
)
summary(gam_model3_ind)
# Check diagnostics 
gam.check(gam_model3_ind)
# Final model with breakpoint chances 
final_model3_ind <- gam(
  outcome ~ player2_ace + player1_df + surface +
    s(player1_1st_pct) + player2_1st_pct +
    s(player1_1st_won_pct) + s(player2_1st_won_pct) +
    player1_2nd_won_pct + s(player2_2nd_won_pct) +
    player1_bp_chances + player2_bp_chances +
    player1_rank,
  data = atp_balanced,
  family = binomial
)
summary(final_model3_ind)
gam.check(final_model3_ind)
AIC(model14_ind,final_model3_ind)
anova(model14_ind,final_model3_ind)
plot(final_model3_ind, pages = 1, shade = TRUE)

# Final model with breakpoint conversion percentage 
final_model4_ind <- gam(
  outcome ~
    s(player1_1st_pct) + s(player2_1st_pct) +
    s(player1_1st_won_pct) + s(player2_1st_won_pct) +
    player1_2nd_won_pct + s(player2_2nd_won_pct) +
    s(player1_bp_conversion_pct) + s(player2_bp_conversion_pct) +
    player1_rank,
  data = atp_balanced,
  family = binomial
)
summary(final_model4_ind)
gam.check(final_model4_ind)
AIC(model12.5_ind,final_model4_ind)
anova(model12.5_ind,final_model4_ind)
plot(final_model4_ind, pages = 1, shade = TRUE)

# Final model with proportion of points won with breakpoint chnaces 
final_model3_pct <- gam(
  p1_points_matrix ~ 
    player1_1st_pct + player2_1st_pct +
    s(player1_1st_won_pct) + s(player2_1st_won_pct) +
    s(player1_2nd_won_pct) + s(player2_2nd_won_pct) +
    player1_bp_chances + player2_bp_chances,
  data = atp_balanced,
  family = binomial()
)
summary(final_model3_pct)
gam.check(final_model3_pct)
AIC(model4_pct,final_model3_pct)
anova(model4_pct,final_model3_pct)
plot(final_model3_pct, pages = 1, shade = TRUE)

# Investigate non-linear relationships with breakpoints conversion percentage differences
gam_model_pct_diff <- gam(
  p1_points_matrix ~ 
    s(first_serve_pct_diff) + 
    s(first_serve_won_pct_diff) +s(second_serve_won_pct_diff) +
    s(break_points_conversion_pct_diff) + player1_rank,
  data = atp_balanced,
  family = binomial
)
summary(gam_model_pct_diff)
AIC(model6_pct,gam_model_pct_diff)
plot(gam_model_diff, pages = 1, scheme = 1, shade = TRUE, seWithMean = TRUE)

# Bayesian Logistic regression predicting outcome 
# Load brms for Bayesian analysis 
install.packages("brms")
library(brms)
# Bayesian analysis for final model with breakpoints converted
final_model1_bayes <- brm(
  outcome ~  player1_df +
    player1_1st_pct + player2_1st_pct +
    player1_1st_won_pct + player2_1st_won_pct +
    player1_2nd_won_pct + player2_2nd_won_pct +
    s(player1_bp_converted) + s(player2_bp_converted) +
    player1_rank,
  data = atp_balanced,
  family = bernoulli(),
  prior = c(
    prior(normal(0,2), class ="b"),
    prior(normal(0,5), class = "Intercept")
  ),
  chains = 4, cores = 4, iter = 2000, seed = 123,
  control = list(adapt_delta = 0.8)
)

summary(final_model1_bayes)
# LOO-cv analysis 
loo_fit <- loo(final_model1_bayes)
print(loo_fit)
plot(loo_fit)
hist(loo_fit$diagnostics$pareto_k, main = "Pareto k values", xlab= "k")
# Graphs for non-linear variables and behaviour 
for (term in names(conditional_effects(final_model1_bayes))) {
  p <- plot(conditional_effects(final_model1_bayes), effects = term)[[1]]
  print(p)
}
# Bayesian analysis for Final model for breakpoint chances 
final_model2_bayes <- brm(
  outcome ~ player2_ace + player1_df + surface +
    player1_1st_pct + player2_1st_pct +
    s(player1_1st_won_pct) + s(player2_1st_won_pct) +
    player1_2nd_won_pct + s(player2_2nd_won_pct) +
    player1_bp_chances + player2_bp_chances +
    s(player1_rank),
  data = atp_balanced,
  family = bernoulli(),
  prior = c(
    prior(normal(0,2), class ="b"),
    prior(normal(0,5), class = "Intercept")
  ),
  chains = 4, cores = 4, iter = 2000, seed = 123,
  control = list(adapt_delta = 0.8)
)

summary(final_model2_bayes)
loo_fit <- loo(final_model2_bayes)
print(loo_fit)
loo_compare(loo(final_model1_bayes),loo(final_model2_bayes))

for (term in names(conditional_effects(final_model2_bayes))) {
  p <- plot(conditional_effects(final_model1_bayes), effects = term)[[1]]
  print(p)
}
# Bayesian analysis for final model with proportion of points won with breakpoints conversion percentage differences 
final_model_pct_diff_bayes <- brm(
  p1_points_matrix ~
    first_serve_pct_diff + 
    first_serve_won_pct_diff +second_serve_won_pct_diff +
    break_points_conversion_pct_diff,
  data = atp_balanced,
  family = bernoulli(),
  prior = c(
    prior(normal(0,2), class ="b"),
    prior(normal(0,5), class = "Intercept")
  ),
  chains = 4, cores = 4, iter = 2000, seed = 123,
  control = list(adapt_delta = 0.8)
)
summary(final_model_pct_diff_bayes)

# Get posterior summary
post_sum <- as.data.frame(posterior_summary(final_model1_bayes))
post_sum$term <- rownames(post_sum)

# Smooth terms (sds...)
smooth_df <- post_sum %>%
  filter(grepl("^sds", term)) %>%
  rename(Estimate = Estimate,
         l95 = Q2.5,
         u95 = Q97.5)

# Linear terms (b_...)
linear_df <- post_sum %>%
  filter(grepl("^b_", term)) %>%
  filter(term != "b_Intercept") %>% 
  rename(Estimate = Estimate,
         l95 = Q2.5,
         u95 = Q97.5)

# Plot smooth terms
p1 <- ggplot(smooth_df, aes(x = term, y = Estimate)) +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = l95, ymax = u95), width = 0.2, color = "blue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Smooth Terms (Wiggliness)",
       y = "Estimate", x = "Term")

# Plot linear terms
p2 <- ggplot(linear_df, aes(x = term, y = Estimate)) +
  geom_point(color = "red") +
  geom_errorbar(aes(ymin = l95, ymax = u95), width = 0.2, color = "red") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Linear Terms",
       y = "Estimate", x = "Term")

# Show plots
p1
p2

# Time Series, Predictive Modelling 
# Load slider for rolling operations 
install.packages("slider")
library(slider)

# Reshape data long so every row is a player-match
player_long <- bind_rows(
  atp %>%
    transmute(
      player_name = winner_name,
      opponent_name = loser_name,
      player_rank = winner_rank,
      opponent_rank = loser_rank,
      tourney_date,
      match_num,
      minutes,
      outcome = 1,  # win
      w_ace, w_df, w_1stPercentage, w_1stWonPercentage, w_2ndWonPercentage, w_bptotal # add stats
    ) %>%
    rename_with(~ gsub("w_", "", .x), starts_with("w_")),
  
  atp %>%
    transmute(
      player_name = loser_name,
      opponent_name = winner_name,
      player_rank = loser_rank,
      opponent_rank = winner_rank,
      tourney_date,
      match_num,
      minutes,
      outcome = 0,  # loss
      l_ace, l_df, l_1stPercentage, l_1stWonPercentage, l_2ndWonPercentage, l_bptotal # add stats
    ) %>%
    rename_with(~ gsub("l_", "", .x), starts_with("l_"))
)

# Order by player and match
player_long <- player_long %>%
  mutate(order_key = as.numeric(tourney_date) * 1000 + match_num) %>%
  arrange(player_name, order_key)

# Compute rolling averages over last 5 matches for each player
player_long <- player_long %>%
  group_by(player_name) %>%
  mutate(across(
    c(ace, df, `1stPercentage`, `1stWonPercentage`, `2ndWonPercentage`, bptotal, minutes), 
    ~ slide_dbl(.x, mean, .before = 4, .complete = TRUE, na.rm = TRUE), 
    .names = "avg_{.col}_last5"
  ),
  outcome_char = ifelse(outcome == 1, "W", "L"),
  form_last4 = lag(slide_chr(outcome_char, ~ paste0(.x, collapse = ""), .before = 3, .complete = TRUE)),
  wins_last4 = ifelse(is.na(form_last4), NA, str_count(form_last4, "W")),
  weighted_form = ifelse(
    is.na(form_last4), NA,
    vapply(
      strsplit(form_last4, ""),
      function(x) sum(rev(ifelse(x == "W", 1, 0)) * 1:4),
      numeric(1)
    )
  ),
  streak_length = ifelse(
    is.na(form_last4), NA,
    vapply(
      strsplit(form_last4, ""),
      function(x) {
        if (length(x) == 0 || all(is.na(x))) return(NA_real_)
        r <- rle(rev(x))
        if (length(r$values) == 0) return(NA_real_)
        if(r$values[1] == "W") r$lengths[1] else 0 
      },
      numeric(1)
    )
  )
) %>%
  ungroup()

# Combine player averages
atp_with_avg <- atp %>%
  mutate(order_key = as.numeric(tourney_date) * 1000 + match_num) %>%
  left_join(
    player_long %>% select(player_name, order_key, starts_with("avg_"), wins_last4, weighted_form, streak_length),
    by = c("winner_name" = "player_name", "order_key")
  ) %>%
  left_join(
    player_long %>% select(player_name, order_key, starts_with("avg_"), wins_last4, weighted_form, streak_length),
    by = c("loser_name" = "player_name", "order_key"),
    suffix = c("_winner", "_loser")
  )

# Filter to remove matches without enough history
atp_with_avg <- atp_with_avg %>%
  filter(!is.na(avg_ace_last5_winner),!is.na(avg_ace_last5_loser))

# For each match, create two rows: one for winner, one for loser
player_match_data <- atp_with_avg %>%
  # Winner row
  transmute(
    player = winner_name,
    opponent = loser_name,
    outcome = 1, 
    tourney_date,
    match_num,
    surface,
    player_rank = winner_rank,
    opponent_rank = loser_rank,
    avg_minutes_last5 = avg_minutes_last5_winner,
    avg_ace_last5 = avg_ace_last5_winner,
    avg_df_last5 = avg_df_last5_winner,
    avg_1stPercentage_last5 = avg_1stPercentage_last5_winner,
    avg_1stWonPercentage_last5 = avg_1stWonPercentage_last5_winner,
    avg_2ndWonPercentage_last5 = avg_2ndWonPercentage_last5_winner,
    avg_bptotal_last5 = avg_bptotal_last5_winner,
    wins_last4 = wins_last4_winner,
    streak_length = streak_length_winner,
    weighted_form = weighted_form_winner,
    opp_avg_minutes_last5 = avg_minutes_last5_loser,
    opp_avg_ace_last5 = avg_ace_last5_loser,
    opp_avg_df_last5 = avg_df_last5_loser,
    opp_avg_1stPercentage_last5 = avg_1stPercentage_last5_loser,
    opp_avg_1stWonPercentage_last5 = avg_1stWonPercentage_last5_loser,
    opp_avg_2ndWonPercentage_last5 = avg_2ndWonPercentage_last5_loser,
    opp_avg_bptotal_last5 = avg_bptotal_last5_loser,
    opp_wins_last4 = wins_last4_loser,
    opp_streak_length = streak_length_loser,
    opp_weighted_form = weighted_form_loser,
    match_id = row_number()
  ) %>%
  bind_rows(
    atp_with_avg %>%
      # Loser row
      transmute(
        player = loser_name,
        opponent = winner_name,
        outcome = 0,
        tourney_date,
        match_num,
        surface,
        player_rank = loser_rank,
        opponent_rank = winner_rank,
        avg_minutes_last5 = avg_minutes_last5_loser,
        avg_ace_last5 = avg_ace_last5_loser,
        avg_df_last5 = avg_df_last5_loser,
        avg_1stPercentage_last5 = avg_1stPercentage_last5_loser,
        avg_1stWonPercentage_last5 = avg_1stWonPercentage_last5_loser,
        avg_2ndWonPercentage_last5 = avg_2ndWonPercentage_last5_loser,
        avg_bptotal_last5 = avg_bptotal_last5_loser,
        wins_last4 = wins_last4_loser,
        streak_length = streak_length_loser,
        weighted_form = weighted_form_loser,
        opp_avg_minutes_last5 = avg_minutes_last5_winner,
        opp_avg_ace_last5 = avg_ace_last5_winner,
        opp_avg_df_last5 = avg_df_last5_winner,
        opp_avg_1stPercentage_last5 = avg_1stPercentage_last5_winner,
        opp_avg_1stWonPercentage_last5 = avg_1stWonPercentage_last5_winner,
        opp_avg_2ndWonPercentage_last5 = avg_2ndWonPercentage_last5_winner,
        opp_avg_bptotal_last5 = avg_bptotal_last5_winner,
        opp_wins_last4 = wins_last4_winner,
        opp_streak_length = streak_length_winner,
        opp_weighted_form = weighted_form_winner,
        match_id = row_number()
      ) 
  )
# Investigate significant predictors 
model_ts <- glm(
  outcome ~ avg_minutes_last5 + avg_ace_last5 + avg_df_last5 +
  avg_1stPercentage_last5 + avg_1stWonPercentage_last5 +
  avg_2ndWonPercentage_last5 + avg_bptotal_last5 +
  wins_last4 + weighted_form + 
  opp_avg_minutes_last5 + opp_avg_ace_last5 + opp_avg_df_last5 +
  opp_avg_1stPercentage_last5 + opp_avg_1stWonPercentage_last5 + 
  opp_avg_2ndWonPercentage_last5 + opp_avg_bptotal_last5 + 
  opp_wins_last4 + opp_weighted_form,
  data = player_match_data,
  family=binomial()
)
summary(model_ts)

# Remove non-significant variables 
model1_ts <- glm(
  outcome ~ avg_minutes_last5 + avg_ace_last5 +
    avg_1stPercentage_last5 + avg_1stWonPercentage_last5 +
    avg_2ndWonPercentage_last5 + avg_bptotal_last5 +
    wins_last4 + 
    opp_avg_minutes_last5 + opp_avg_ace_last5 +
    opp_avg_1stPercentage_last5 + opp_avg_1stWonPercentage_last5 + 
    opp_avg_2ndWonPercentage_last5 + opp_avg_bptotal_last5 + 
    opp_wins_last4,
  data = player_match_data,
  family=binomial()
)
summary(model1_ts)
# Load sandwich and lmtest to examine regression diagnostics 
install.packages("sandwich")
library(sandwich)
library(lmtest)

clustered_se <- vcovCL(model1_ts, cluster = ~match_id )
coeftest(model1_ts, vcov = clustered_se)
tab_model(model1_ts, show.ci = FALSE, show.se = TRUE, show.std = TRUE, transform = NULL,
          dv.labels = "Match Outcome (Win = 1)")
# Load lme4 for miex model analysis 
library(lme4)
# Mixed model with match ID
model_mixed <- glmer(
  outcome ~ avg_minutes_last5 + avg_ace_last5 + 
    avg_1stPercentage_last5 + avg_1stWonPercentage_last5 +
    avg_2ndWonPercentage_last5 + avg_bptotal_last5 +
    wins_last4 + opp_avg_minutes_last5 +
    opp_avg_ace_last5 + 
    opp_avg_1stPercentage_last5 + opp_avg_1stWonPercentage_last5 + 
    opp_avg_2ndWonPercentage_last5 + opp_avg_bptotal_last5 + 
    opp_wins_last4 + 
    (1 | match_id),
  data = player_match_data,
  family = binomial()
)
summary(model_mixed)
# Mixed model with player 
model1_mixed <- glmer(
  outcome ~ avg_minutes_last5 + avg_ace_last5 + 
    avg_1stPercentage_last5 + avg_1stWonPercentage_last5 +
    avg_2ndWonPercentage_last5 + avg_bptotal_last5 +
    wins_last4 + opp_avg_minutes_last5 +
    opp_avg_ace_last5 + 
    opp_avg_1stPercentage_last5 + opp_avg_1stWonPercentage_last5 + 
    opp_avg_2ndWonPercentage_last5 + opp_avg_bptotal_last5 + 
    opp_wins_last4 + 
    (1 | player),
  data = player_match_data,
  family = binomial(),
  control = ctrl
)
summary(model1_mixed)
# Ensure predictors are standardised 
dat <- player_match_data %>%
  mutate(
    across(c(avg_minutes_last5, avg_ace_last5, avg_df_last5,
             avg_1stPercentage_last5, avg_1stWonPercentage_last5,
             avg_2ndWonPercentage_last5, avg_bptotal_last5,
             opp_avg_minutes_last5, opp_avg_ace_last5, opp_avg_df_last5,
             opp_avg_1stPercentage_last5, opp_avg_1stWonPercentage_last5,
             opp_avg_2ndWonPercentage_last5, opp_avg_bptotal_last5),
           ~ scale(.) %>% as.numeric()),
  )
# Removes warning issues 
ctrl <- glmerControl(optimizer = "bobyqa",
                     optCtrl = list(maxfun = 2e5))
# Mixed model with player and match ID
model2_mixed <- glmer(
  outcome ~ avg_minutes_last5 + avg_ace_last5 + 
    avg_1stPercentage_last5 + avg_1stWonPercentage_last5 +
    avg_2ndWonPercentage_last5 + avg_bptotal_last5 +
    wins_last4 + opp_avg_minutes_last5 +
    opp_avg_ace_last5 + 
    opp_avg_1stPercentage_last5 + opp_avg_1stWonPercentage_last5 + 
    opp_avg_2ndWonPercentage_last5 + opp_avg_bptotal_last5 + 
    opp_wins_last4 + 
    (1 | player),
  data = dat,
  family = binomial()
)
summary(model2_mixed)
# Mixed model with player and match ID
model3_mixed <- glmer(
  outcome ~ avg_minutes_last5 + avg_ace_last5 + 
    avg_1stPercentage_last5 + avg_1stWonPercentage_last5 +
    avg_2ndWonPercentage_last5 + avg_bptotal_last5 +
    wins_last4 + opp_avg_minutes_last5 +
    opp_avg_ace_last5 + 
    opp_avg_1stPercentage_last5 + opp_avg_1stWonPercentage_last5 + 
    opp_avg_2ndWonPercentage_last5 + opp_avg_bptotal_last5 + 
    opp_wins_last4 + 
    (1 | player) + (1|match_id),
  data = dat,
  family = binomial()
)
summary(model3_mixed)

# Set seeed 
set.seed(123)

# Function for rolling forward predictions 
rolling_forward_cumulative_cm <- function(data, min_train_matches = 5) {
  
  results <- list()
  cumulative_history <- data.frame(match_num = integer(), cumulative_auc = numeric(), cumulative_accuracy = numeric())
  
  # Sort so matches are in chronological order
  data <- data %>%
    arrange(tourney_date, match_num, player)
  
  match_ids <- unique(data$match_id)
  
  all_preds <- data.frame()  
  
  for (i in seq_along(match_ids)) {
    current_match_id <- match_ids[i]
    
    # Training = all matches before this one
    train <- data %>% filter(match_id %in% match_ids[1:(i-1)])
    test  <- data %>% filter(match_id == current_match_id)
    
    if (nrow(train) < min_train_matches) next
    if (length(unique(train$outcome)) < 2) next 
    
    train <- train %>% drop_na(
      avg_ace_last5, avg_minutes_last5, avg_1stPercentage_last5, avg_1stWonPercentage_last5,
      avg_2ndWonPercentage_last5, avg_bptotal_last5,
      wins_last4, opp_avg_ace_last5,opp_avg_minutes_last5,
      opp_avg_1stPercentage_last5, opp_avg_1stWonPercentage_last5,
      opp_avg_2ndWonPercentage_last5, opp_avg_bptotal_last5,
      opp_wins_last4
    )
    if (nrow(train) < min_train_matches) next
    
    model <- glm(
      outcome ~ avg_ace_last5 + avg_minutes_last5 + 
        avg_1stPercentage_last5 + avg_1stWonPercentage_last5 +
        avg_2ndWonPercentage_last5 + avg_bptotal_last5 +
        wins_last4 +
        opp_avg_ace_last5 + opp_avg_minutes_last5 + 
        opp_avg_1stPercentage_last5 + opp_avg_1stWonPercentage_last5 + 
        opp_avg_2ndWonPercentage_last5 + opp_avg_bptotal_last5 +
        opp_wins_last4,
      data = train,
      family = binomial
    )
    
    test <- test %>% drop_na(
      avg_ace_last5, avg_minutes_last5, avg_1stPercentage_last5, avg_1stWonPercentage_last5,
      avg_2ndWonPercentage_last5, avg_bptotal_last5,
      wins_last4, opp_avg_ace_last5, opp_avg_minutes_last5,
      opp_avg_1stPercentage_last5, opp_avg_1stWonPercentage_last5,
      opp_avg_2ndWonPercentage_last5, opp_avg_bptotal_last5,
      wins_last4
    )
    if (nrow(test) == 0) next
    
    test$predicted_prob <- predict(model, newdata = test, type = "response")
    test$pred_class <- ifelse(test$predicted_prob >= 0.5, 1, 0)
    
    results[[length(results) + 1]] <- test
    all_preds <- bind_rows(all_preds, test)
    
    # Compute cumulative metrics
    cum_auc <- auc(all_preds$outcome, all_preds$predicted_prob)
    cum_acc <- mean(all_preds$pred_class == all_preds$outcome)
    
    cumulative_history <- rbind(cumulative_history, data.frame(
      match_num = i,
      cumulative_auc = as.numeric(cum_auc),
      cumulative_accuracy = cum_acc
    ))
  }
  
  all_results <- bind_rows(results)
  
  # Cumulative confusion matrix
  cm <- confusionMatrix(
    factor(all_preds$pred_class, levels = c(0, 1)),
    factor(all_preds$outcome, levels = c(0, 1))
  )
  
  roc_obj <- roc(all_preds$outcome, all_preds$predicted_prob)
  auc_val <- auc(roc_obj)
  
  # Plot cumulative AUC and Accuracy
  cum_plot <- ggplot(cumulative_history %>% filter(!is.na(cumulative_auc)), aes(x = match_num)) +
    geom_line(aes(y = cumulative_auc, color = "Cumulative AUC")) +
    geom_line(aes(y = cumulative_accuracy, color = "Cumulative Accuracy")) +
    labs(
      title = "Cumulative Rolling Forward Validation Performance",
      x = "Match Number",
      y = "Metric",
      color = "Metric"
    ) +
    theme_minimal()
  
  list(
    predictions = all_results,
    cumulative_history = cumulative_history,
    confusion_matrix = cm,
    auc = auc_val,
    cum_plot = cum_plot
  )
}

# Run results for function 
results <- rolling_forward_cumulative_cm(player_match_data)
# View cumulative plot 
results$cum_plot
# View confusion matrix 
results$confusion_matrix
# View AUC maximum value 
results$auc





