#######################################################################
##########################  TENNIS PROJECT  ###########################
#######################################################################

# Libraries
library(MASS) ; library(ggplot2) ; library(scales) ; library(tidyverse) ; library(corrplot) 
library(corrplot) ; library(car) ; library(caret) ; library(readxl) ; library(e1071) ;
library(randomForest); library(dplyr)

####
#### Reading in data.
####

# Load last 20 years of ATP tour level matches
matches2023 <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2023.csv')
matches2022 <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2022.csv')
matches2021 <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2021.csv')
matches2020 <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2020.csv')
matches2019 <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2019.csv')
matches2018 <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2018.csv')
matches2017 <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2017.csv')
matches2016 <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2016.csv')
matches2015 <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2015.csv')
matches2014 <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2014.csv')
matches2013 <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2013.csv')
matches2012 <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2012.csv')
matches2011 <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2011.csv')
matches2010 <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2010.csv')
matches2009 <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2009.csv')
matches2008 <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2008.csv')
matches2007 <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2007.csv')
matches2006 <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2006.csv')
matches2005 <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2005.csv')
matches2004 <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2004.csv')
matches2003 <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2003.csv')

# Load ATP Player Data -> decided not to use
#atp_players <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_players.csv')

# Load Ranking Data (per decade) -> decided not to use.
#atp_rankings_00s <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_rankings_00s.csv')
#atp_rankings_10s <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_rankings_10s.csv')
#atp_rankings_20s <- read.csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_rankings_20s.csv')

# Combine matches into one data frame
atp_matches <- rbind(matches2003, matches2004, matches2005, matches2006, matches2007,
                     matches2008, matches2009, matches2010, matches2011, matches2012,
                     matches2013, matches2014, matches2015, matches2016, matches2017,
                     matches2018, matches2019, matches2020, matches2021, matches2022,
                     matches2023) 
# 61932 observations

# remove after combining to de-clutter
rm(matches2003, matches2004, matches2005, matches2006, matches2007,
   matches2008, matches2009, matches2010, matches2011, matches2012,
   matches2013, matches2014, matches2015, matches2016, matches2017,
   matches2018, matches2019, matches2020, matches2021, matches2022,
   matches2023)

####
#### Data Exploration & Manipulation
####

#instead of listing seed or NA for unseeded, create binary seeded variable
atp_matches$winner_seeded <- ifelse(is.na(atp_matches$winner_seed), "No", "Yes")
atp_matches$loser_seeded <- ifelse(is.na(atp_matches$loser_seed),"No", "Yes")

names(atp_matches)
str(atp_matches)
levels(as.factor(atp_matches$surface))
levels(as.factor(atp_matches$winner_entry))

#creating outcome variable for winners and losers
match_winners = atp_matches
match_winners$outcome = 1

match_losers = atp_matches
match_losers$outcome = 0

#renaming columns to prep for consolidating winners and losers

match_winners = match_winners %>% dplyr::rename(
  id = winner_id,
  seed = winner_seed,
  entry = winner_entry,
  name = winner_name,
  hand = winner_hand,
  ht = winner_ht,
  ioc = winner_ioc,
  age = winner_age,
  opponent_id = loser_id,
  opponent_seed = loser_seed,
  opponent_entry = loser_entry,
  opponent_name = loser_name,
  opponent_hand = loser_hand,
  opponent_ht = loser_ht,
  opponent_ioc = loser_ioc,
  opponent_age = loser_age,
  ace = w_ace,
  df = w_df,
  svpt = w_svpt,
  firstIn = w_1stIn,
  firstWon = w_1stWon,
  secWon = w_2ndWon,
  SvGms = w_SvGms,
  bpSaved = w_bpSaved,
  bpFaced = w_bpFaced,
  opponent_ace = l_ace,
  opponent_df = l_df,
  opponent_svpt = l_svpt,
  opponent_firstIn = l_1stIn,
  opponent_firstWon = l_1stWon,
  opponent_secWon = l_2ndWon,
  opponent_SvGms = l_SvGms,
  opponent_bpSaved = l_bpSaved,
  opponent_bpFaced = l_bpFaced,
  rank = winner_rank,
  rank_points = winner_rank_points,
  opponent_rank = loser_rank,
  opponent_rank_points = loser_rank_points,
  seeded = winner_seeded,
  opponent_seeded = loser_seeded
)

match_losers = match_losers %>% dplyr::rename(
  id = loser_id,
  seed = loser_seed,
  entry = loser_entry,
  name = loser_name,
  hand = loser_hand,
  ht = loser_ht,
  ioc = loser_ioc,
  age = loser_age,
  opponent_id = winner_id,
  opponent_seed = winner_seed,
  opponent_entry = winner_entry,
  opponent_name = winner_name,
  opponent_hand = winner_hand,
  opponent_ht = winner_ht,
  opponent_ioc = winner_ioc,
  opponent_age = winner_age,
  ace = l_ace,
  df = l_df,
  svpt = l_svpt,
  firstIn = l_1stIn,
  firstWon = l_1stWon,
  secWon = l_2ndWon,
  SvGms = l_SvGms,
  bpSaved = l_bpSaved,
  bpFaced = l_bpFaced,
  opponent_ace = w_ace,
  opponent_df = w_df,
  opponent_svpt = w_svpt,
  opponent_firstIn = w_1stIn,
  opponent_firstWon = w_1stWon,
  opponent_secWon = w_2ndWon,
  opponent_SvGms = w_SvGms,
  opponent_bpSaved = w_bpSaved,
  opponent_bpFaced = w_bpFaced,
  rank = loser_rank,
  rank_points = loser_rank_points,
  opponent_rank = winner_rank,
  opponent_rank_points = winner_rank_points,
  seeded = loser_seeded,
  opponent_seeded = winner_seeded
)

# combine match_winners and losers
match_outcomes <- rbind(match_winners, match_losers)
rm(match_winners,match_losers)

# additional variables
match_outcomes$ht_dif = match_outcomes$ht - match_outcomes$opponent_ht
match_outcomes$age_dif = match_outcomes$age - match_outcomes$opponent_age
match_outcomes$rank_dif = match_outcomes$rank - match_outcomes$opponent_rank
names(match_outcomes)

# create averages of player match outcomes
match_outcomes <- match_outcomes %>%
  group_by(id) %>%
  mutate(
    avg_ace = mean(ace, na.rm = TRUE),
    avg_df = mean(df, na.rm = TRUE),
    avg_svpt = mean(svpt, na.rm = TRUE),
    avg_firstIn = mean(firstIn, na.rm = TRUE),
    avg_firstWon = mean(firstWon, na.rm = TRUE),
    avg_secWon = mean(secWon, na.rm = TRUE),
    avg_SvGms = mean(SvGms, na.rm = TRUE),
    avg_bpSaved = mean(bpSaved, na.rm = TRUE),
    avg_bpFaced = mean(bpFaced, na.rm = TRUE)
  )

match_outcomes <- match_outcomes %>%
  group_by(opponent_id) %>%
  mutate(    
    avg_opponent_ace = mean(opponent_ace, na.rm = TRUE),
    avg_opponent_df = mean(opponent_df, na.rm = TRUE),
    avg_opponent_svpt = mean(opponent_svpt, na.rm = TRUE),
    avg_opponent_firstIn = mean(opponent_firstIn, na.rm = TRUE),
    avg_opponent_firstWon = mean(opponent_firstWon, na.rm = TRUE),
    avg_opponent_secWon = mean(opponent_secWon, na.rm = TRUE),
    avg_opponent_SvGms = mean(opponent_SvGms, na.rm = TRUE),
    avg_opponent_bpSaved = mean(opponent_bpSaved, na.rm = TRUE),
    avg_opponent_bpFaced = mean(opponent_bpFaced, na.rm = TRUE)
  )


match_outcomes$surface_clay <- ifelse(match_outcomes$surface == "Clay",1,0)
match_outcomes$surface_grass <- ifelse(match_outcomes$surface == "Grass",1,0)
match_outcomes$surface_hard <- ifelse(match_outcomes$surface == "Hard",1,0)
str(match_outcomes)
summary(match_outcomes)

## match_outcomes has all of my variables ##

# new DF for modeling
tennis_df = match_outcomes

# Removing pointless variables
tennis_df = select(tennis_df, -seed, -entry, -opponent_seed, -opponent_entry,
                   -tourney_id,-tourney_name,-tourney_date,-match_num,-name,-ioc,
                   -opponent_name,-opponent_ioc,-score,-round,-best_of,-surface)

# Removing variables related to match outcome
tennis_df = select(tennis_df,-minutes,-ace,-df,-svpt,-firstIn,-firstWon,-secWon,-SvGms,-bpSaved,
                   -bpFaced,-opponent_ace,-opponent_df,-opponent_svpt,-opponent_firstIn,
                   -opponent_firstWon,-opponent_secWon,-opponent_SvGms,-opponent_bpSaved,
                   -opponent_bpFaced)

# Removing height, age, rank since I have the differences instead. also rank points
tennis_df = select(tennis_df,-ht,-age,-rank,-rank_points,-opponent_ht,-opponent_age,-opponent_rank,
                   -opponent_rank_points)

# Removing IDs
tennis_df = ungroup(tennis_df)
tennis_df = select(tennis_df,-id,-opponent_id)

#coding U (unknown) hand as NAs
tennis_df$hand = ifelse(tennis_df$hand == 'U',NA, tennis_df$hand)
tennis_df$opponent_hand = ifelse(tennis_df$opponent_hand == 'U',NA, tennis_df$opponent_hand)


# remove rows containing missing values
tennis_df = na.omit(tennis_df) #123,864 to 116,166

# Looking at the relationships between the numeric variables
tennis_df_num <- dplyr::select_if(tennis_df, is.numeric)
corrplot(cor(tennis_df_num), method = c("number"), type = c("lower"),
         number.cex=0.65, tl.cex = 0.65,tl.col="black")
#pairs(tennis_df_num)
summary(tennis_df_num)

high_correlation <- which(cor(tennis_df_num) > 0.8 & cor(tennis_df_num) < 1, arr.ind = TRUE)
print(high_correlation)

# bp faced & saved are highly correlated (greater than 0.9), will remove faced
tennis_df = select(tennis_df,-avg_bpFaced,-avg_opponent_bpFaced)
# service points and service games are highly correlated (0.88) will remove games
tennis_df = select(tennis_df,-avg_SvGms,-avg_opponent_SvGms)

# Look again
tennis_df_num <- dplyr::select_if(tennis_df, is.numeric)
corrplot(cor(tennis_df_num), method = c("number"), type = c("lower"),
         number.cex=0.65, tl.cex = 0.65,tl.col="black")

# Data Type Changes 
#tennis_df$tourney_date = as.Date(as.character(tennis_df$tourney_date),format = "%Y%m%d")
#tennis_df$surface = as.factor(tennis_df$surface)
tennis_df$tourney_level = as.factor(tennis_df$tourney_level)
tennis_df$hand = as.factor(tennis_df$hand)
tennis_df$opponent_hand = as.factor(tennis_df$opponent_hand)
tennis_df$seeded = as.factor(tennis_df$seeded)
tennis_df$opponent_seeded = as.factor(tennis_df$opponent_seeded)
tennis_df$outcome = as.factor(tennis_df$outcome)
tennis_df$surface_clay = as.factor(tennis_df$surface_clay)
tennis_df$surface_grass = as.factor(tennis_df$surface_grass)
tennis_df$surface_hard = as.factor(tennis_df$surface_hard)

str(tennis_df)
names(tennis_df)


### Splitting into train and test
set.seed(12)
index = sample(nrow(tennis_df), 0.8*nrow(tennis_df), replace = F) # 80/20 split
tennis_train = tennis_df[index,]
tennis_test = tennis_df[-index,]

#checking for balance
table(tennis_train$outcome)
#0     1 
#46477 46455
table(tennis_test$outcome)
#0     1 
#11606 11628
str(tennis_train)


#frequency table for tennis_train$outcome
train_outcome_table <- table(tennis_train$outcome)

# frequency table for tennis_test$outcome
test_outcome_table <- table(tennis_test$outcome)

# Create a dataframe for visualization
outcome_data <- data.frame(
  dataset = rep(c("Train", "Test"), each = 2),
  outcome = rep(c("0", "1"), times = 2),
  count = c(train_outcome_table, test_outcome_table)
)
# Create bar graph using ggplot
ggplot(outcome_data, aes(x = outcome, y = count, fill = dataset)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Distribution of Outcome in Train and Test Datasets",
       x = "Outcome",
       y = "Count",
       fill = "Dataset") +
  theme_minimal()


# Histograms to understand skew
dev.new(width = 1500, height = 1000, unit = "px")
par(mfrow = c(3, 4))
hist(tennis_train$draw_size, main = "Distribution of Draw Size", xlab = "")
hist(tennis_train$ht_dif, main = "Distribution of Height Difference", xlab = "")
hist(tennis_train$age_dif, main = "Distribution of Age Difference", xlab = "")
hist(tennis_train$rank_dif, main = "Distribution of Rank Difference", xlab = "")
hist(tennis_train$avg_ace, main = "Distribution of Average Aces", xlab = "") #right skew
hist(tennis_train$avg_df, main = "Distribution of Average Double Faults", xlab = "") #right skew
hist(tennis_train$avg_svpt, main = "Distribution of Average Serve Points", xlab = "")
hist(tennis_train$avg_ace, main = "Distribution of Average Aces", xlab = "")
hist(tennis_train$avg_firstIn, main = "Distribution of Avg 1st Serve In", xlab = "")
hist(tennis_train$avg_firstWon, main = "Distribution of Avg 1st Serve Won", xlab = "")
hist(tennis_train$avg_secWon, main = "Distribution of Avg 2nd Serve Won", xlab = "") 
hist(tennis_train$avg_bpSaved, main = "Distribution of Avg BP Saved", xlab = "")
library(ggplot2)

# Prettier Histograms
ggplot(tennis_train, aes(x = draw_size)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Draw Size", x = "") +
  theme_minimal() +
  scale_y_continuous(NULL)

ggplot(tennis_train, aes(x = ht_dif)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Height Difference", x = "") +
  theme_minimal() +
  scale_y_continuous(NULL)

ggplot(tennis_train, aes(x = age_dif)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Age Difference", x = "") +
  theme_minimal() +
  scale_y_continuous(NULL)

ggplot(tennis_train, aes(x = rank_dif)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Rank Difference", x = "") +
  theme_minimal() +
  scale_y_continuous(NULL)

ggplot(tennis_train, aes(x = avg_ace)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Average Aces", x = "") +
  theme_minimal() +
  scale_y_continuous(NULL)

ggplot(tennis_train, aes(x = avg_df)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Average Double Faults", x = "") +
  theme_minimal() +
  scale_y_continuous(NULL)

ggplot(tennis_train, aes(x = avg_svpt)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Average Serve Points", x = "") +
  theme_minimal() +
  scale_y_continuous(NULL)

ggplot(tennis_train, aes(x = avg_ace)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Average Aces", x = "") +
  theme_minimal() +
  scale_y_continuous(NULL)

ggplot(tennis_train, aes(x = avg_firstIn)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Avg 1st Serve In", x = "") +
  theme_minimal() +
  scale_y_continuous(NULL)

ggplot(tennis_train, aes(x = avg_firstWon)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Avg 1st Serve Won", x = "") +
  theme_minimal() +
  scale_y_continuous(NULL)

ggplot(tennis_train, aes(x = avg_secWon)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Avg 2nd Serve Won", x = "") +
  theme_minimal() +
  scale_y_continuous(NULL)

ggplot(tennis_train, aes(x = avg_bpSaved)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Avg BP Saved", x = "") +
  theme_minimal() +
  scale_y_continuous(NULL)


####
#### Which variables impact whether a tennis player wins a match?
####

#Logistic Regression
log.all = glm(outcome ~ .,data = tennis_train, family = binomial)
summary(log.all)
vif(log.all) # multicollinearity in the model: first removing avg_opponent_firstIn (43.77)

log.all = glm(outcome ~ . -avg_opponent_firstIn,data = tennis_train, family = binomial)
summary(log.all)
vif(log.all) # multicollinearity in the model: next removing avg_firstIn (43.414)

log.all = glm(outcome ~ . -avg_opponent_firstIn -avg_firstIn,data = tennis_train, family = binomial)
summary(log.all)
vif(log.all) # multicollinearity in the model: next removing avg_svpt (15.61)

log.all = glm(outcome ~ . -avg_opponent_firstIn -avg_firstIn -avg_svpt,
              data = tennis_train, family = binomial)
summary(log.all)
vif(log.all) # multicollinearity in the model: next removing avg_opponent_svpt (15.38)

log.all = glm(outcome ~ . -avg_opponent_firstIn -avg_firstIn - avg_svpt -avg_opponent_svpt,
              data = tennis_train, family = binomial)
summary(log.all)
vif(log.all) # multicollinearity in the model: surface_hard (13.44)

log.all = glm(outcome ~ . -avg_opponent_firstIn -avg_firstIn - avg_svpt 
              -avg_opponent_svpt -surface_hard,
              data = tennis_train, family = binomial)
summary(log.all)
vif(log.all) # multicollinearity in the model: tourney_level (10.14)

log.all = glm(outcome ~ . -avg_opponent_firstIn -avg_firstIn - avg_svpt 
              -avg_opponent_svpt -surface_hard -tourney_level,
              data = tennis_train, family = binomial)
summary(log.all)
vif(log.all) # no multicollinearity :)


predprob_log <- predict.glm(log.all, tennis_test, type = "response")
predclass_log = ifelse(predprob_log >= 0.5, yes = 1, 0)
caret::confusionMatrix(as.factor(predclass_log), tennis_test$outcome, positive = "1")
# Accuracy    : 0.6596  
# Sensitivity : 0.6660         
# Specificity : 0.6529  

#Logistic Regression with Stepwise Selection
null_model = glm(outcome ~ 1, data = tennis_train, family = binomial) 
full_model = log.all

step.model.AIC = step(null_model, scope = list(upper = full_model),
                      direction = "both", test = "Chisq", trace = F) 
summary(step.model.AIC) 

# Best model based on stepwise 
log.sel <- glm(outcome ~ rank_dif + avg_opponent_bpSaved + avg_bpSaved + opponent_seeded +
                 seeded + age_dif + avg_firstWon + avg_ace + avg_secWon + avg_opponent_firstWon +
                 avg_opponent_ace + avg_opponent_secWon + hand + ht_dif + avg_df,
               tennis_train, family = binomial)
summary(log.sel)

# predictions based on stepwise model
logistic_pred2 <- predict(log.sel, newdata = tennis_test, type = "response")
logistic_pred_class2 <- ifelse(logistic_pred2 > 0.5, yes = 1,0)
caret::confusionMatrix(as.factor(logistic_pred_class2), tennis_test$outcome, positive = "1")

# Accuracy :    0.6592     
# Sensitivity : 0.6659        
# Specificity : 0.6525 


#LDA
lda_model = lda(outcome ~ draw_size + hand + opponent_hand + seeded + opponent_seeded + 
                  ht_dif + age_dif + rank_dif + avg_ace + avg_df +  avg_firstWon + avg_secWon + 
                  avg_bpSaved + avg_opponent_ace + avg_opponent_df + avg_opponent_firstWon + 
                  avg_opponent_secWon + avg_opponent_bpSaved +  surface_clay + surface_grass,
                data = tennis_train)
lda_model
predictions.lda = predict(lda_model, tennis_test)
caret::confusionMatrix(as.factor(predictions.lda$class), tennis_test$outcome)
# Accuracy    : 0.6584 
# Sensitivity : 0.6521       
# Specificity : 0.6647 

#QDA
qda_model = qda(outcome ~ draw_size + hand + opponent_hand + seeded + opponent_seeded + ht_dif + 
                  age_dif + rank_dif + avg_ace + avg_df +  avg_firstWon + avg_secWon + avg_bpSaved + 
                  avg_opponent_ace + avg_opponent_df + avg_opponent_firstWon + avg_opponent_secWon + 
                  avg_opponent_bpSaved +  surface_clay + surface_grass, data = tennis_train)
qda_model
predictions.qda = predict(qda_model, tennis_test)
caret::confusionMatrix(as.factor(predictions.qda$class), tennis_test$outcome)
# Accuracy    : 0.6511
# Sensitivity : 0.6533       
# Specificity : 0.6489 

#Random Forest
set.seed(29)
rf <- randomForest(outcome ~ ., data = tennis_train, importance = TRUE)
rf
rf.preds = predict(rf, tennis_test,type="class")
caret::confusionMatrix(as.factor(rf.preds), tennis_test$outcome, positive = "1")
# Accuracy    : 0.6503
# Sensitivity : 0.6482       
# Specificity : 0.6525


data <- data.frame(model = c("log.all", "log.sel", "LDA", "QDA", "RF"),
                   acc = c(0.6596, 0.6592, 0.6584, 0.6511, 0.6503))

custom_colors <- c("log.all" = "#5B9F9A", "log.sel" = "#7DAFCA", "LDA" = "#E9909D", 
                   "QDA" = "#AABAE4", "RF" = "#D2C3EE")

ggplot(data, aes(x = model, y = acc, fill = model)) +
  geom_bar(stat = "identity", width = 0.5, fill = custom_colors) +
  geom_text(aes(label = acc), vjust = -0.5, size = 3) +
  labs(title = "Test Accuracy Comparison of Models", x = "", y = "Accuracy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
        legend.position = "none")




####
#### Do player age and height influence winning a match?
####
tennis_df2 = match_outcomes
tennis_df2 = dplyr::select(tennis_df2, -seed, -entry, -opponent_seed, -opponent_entry,
                          -tourney_id,-tourney_name,-match_num,-name,-opponent_name,-score)
tennis_df2$hand = ifelse(tennis_df2$hand == 'U',NA, tennis_df2$hand)
tennis_df2$opponent_hand = ifelse(tennis_df2$opponent_hand == 'U',NA, tennis_df2$opponent_hand)
# Data Type Changes
tennis_df2$tourney_date = as.Date(as.character(tennis_df2$tourney_date),format = "%Y%m%d")
tennis_df2$surface = as.factor(tennis_df2$surface)
tennis_df2$tourney_level = as.factor(tennis_df2$tourney_level)
tennis_df2$hand = as.factor(tennis_df2$hand)
tennis_df2$opponent_hand = as.factor(tennis_df2$opponent_hand)
tennis_df2$seeded = as.factor(tennis_df2$seeded)
tennis_df2$opponent_seeded = as.factor(tennis_df2$opponent_seeded)
tennis_df2$outcome = as.factor(tennis_df2$outcome)
str(tennis_df2)
# remove rows containing missing values
tennis_df2 = na.omit(tennis_df2) #123,864 to 107,536

# log for just these two vars
age_ht_log = glm(outcome ~ age + ht,tennis_df2, family = binomial)
summary(age_ht_log)
vif(age_ht_log)

#age and height are significant

log.dif <- glm(outcome ~ age_dif + ht_dif, tennis_train, family = binomial)
summary(log.dif)


####
#### Do top players face fewer break points?
####
#let's consider top players as ranked in the top 25 in the world
tennis_df3 = select(ungroup(tennis_df2),id,surface,rank,bpFaced,ioc)
str(tennis_df3)
tennis_df3$top_player <- ifelse(tennis_df3$rank <= 25, "Yes", "No")
tennis_df3$top_player <- as.factor(tennis_df3$top_player)


# Check for Normality
qqnorm(tennis_df3$bpFaced)
qqline(tennis_df3$bpFaced)
# the qq plot does not look normal

# Because not normal, will use Wilcoxon-Mann Whitney test
wilcox.test(bpFaced ~ top_player, data = tennis_df3,int = TRUE)
#p-value is 2.2e-16, less than 0.05, therefore significant
# There is a statistically significant difference between top player vs other players
#in terms of how many break points they face.

# median break points faced for top players vs other players
median(tennis_df3$bpFaced[tennis_df3$top_player == "Yes"])
median(tennis_df3$bpFaced[tennis_df3$top_player == "No"])
#median is 5 for top layers, 7 for others
#top players face fewer break points


####
#### Which countries produce top tennis players?
####
str(tennis_df3)
tennis_df4 = select(tennis_df3, -surface, -rank, -bpFaced)
tennis_df4 = distinct(tennis_df4)
#chi square test 
chisq.test(table(tennis_df4$top_player, tennis_df4$ioc))
#p-value p-value = 0.9768

player_country = as.data.frame.matrix(table(tennis_df4$ioc,tennis_df4$top_player))
print(player_country)
player_country %>% filter(Yes >= 10)

# Spain, USA, Argentina, France, Russia

####
#### Do the variables that impact whether a tennis player wins a match depend on the surface type?
####

# Subset based on surface type
clay_subset <- tennis_df[tennis_df$surface_clay == 1, ]
grass_subset <- tennis_df[tennis_df$surface_grass == 1, ]
hard_subset <- tennis_df[tennis_df$surface_hard == 1, ]

clay_subset = select(clay_subset,-surface_clay,-surface_grass,-surface_hard)
grass_subset = select(grass_subset,-surface_clay,-surface_grass,-surface_hard)
hard_subset = select(hard_subset,-surface_clay,-surface_grass,-surface_hard)

### Splitting into train and test
set.seed(12)
#clay
index = sample(nrow(clay_subset), 0.8*nrow(clay_subset), replace = F) # 80/20 split
clay_train = clay_subset[index,]
clay_test = clay_subset[-index,]
#grass
index = sample(nrow(grass_subset), 0.8*nrow(grass_subset), replace = F) # 80/20 split
grass_train = grass_subset[index,]
grass_test = grass_subset[-index,]
#hard
index = sample(nrow(hard_subset), 0.8*nrow(hard_subset), replace = F) # 80/20 split
hard_train = hard_subset[index,]
hard_test = hard_subset[-index,]
#checking for balance
table(clay_train$outcome)
table(clay_test$outcome)
table(grass_train$outcome)
table(grass_test$outcome)
table(hard_train$outcome)
table(hard_test$outcome)



#Logistic Regression: CLAY
log.clay = glm(outcome ~ .,data = clay_train, family = binomial)
summary(log.clay)
vif(log.clay) # multicollinearity in the model: first removing avg_opponent_firstIn & avg_firstIn

log.clay = glm(outcome ~ . -avg_opponent_firstIn -avg_firstIn,data = clay_train, family = binomial)
summary(log.clay)
vif(log.clay) # multicollinearity in the model: next removing tourney level & avg_svpt & avg_opponent_svpt

log.clay = glm(outcome ~ . -avg_opponent_firstIn -avg_firstIn - avg_svpt 
               -avg_opponent_svpt -tourney_level,data = clay_train, family = binomial)
summary(log.clay)
vif(log.clay) # no multicollinearity

predprob_log_clay <- predict.glm(log.clay, clay_test, type = "response")
predclass_log_clay = ifelse(predprob_log_clay >= 0.5, yes = 1, 0)
caret::confusionMatrix(as.factor(predclass_log_clay), clay_test$outcome, positive = "1")
# Accuracy    : 0.6582 
# Sensitivity : 0.6524         
# Specificity : 0.6441  

#Logistic Regression with Stepwise Selection
null_model_clay = glm(outcome ~ 1, data = clay_train, family = binomial) 
full_model_clay = log.clay

step.model.AIC.clay = step(null_model_clay, scope = list(upper = full_model_clay),
                      direction = "both", test = "Chisq", trace = F) 
summary(step.model.AIC.clay) 

# Best model based on stepwise 
log.sel.clay <- glm(outcome ~ rank_dif + opponent_seeded + seeded + age_dif + avg_opponent_bpSaved +
                      avg_opponent_ace + avg_opponent_firstWon + avg_ace + avg_firstWon + avg_opponent_secWon +
                      avg_secWon + ht_dif + opponent_hand + hand,
               clay_train, family = binomial)
summary(log.sel.clay)

# predictions based on stepwise model
logistic_pred2_clay <- predict(log.sel.clay, newdata = clay_test, type = "response")
logistic_pred_class2_clay <- ifelse(logistic_pred2_clay > 0.5, yes = 1,0)
caret::confusionMatrix(as.factor(logistic_pred_class2_clay), clay_test$outcome, positive = "1")

# Accuracy :    0.6451    
# Sensitivity : 0.6426        
# Specificity : 0.6476  

# Logistic Regression: GRASS
log.grass <- glm(outcome ~ ., data = grass_train, family = binomial)
summary(log.grass)
vif(log.grass)  # Check for multicollinearity in the model

log.grass = glm(outcome ~ . -avg_opponent_firstIn -avg_firstIn,data = grass_train, family = binomial)
summary(log.grass)
vif(log.grass)

log.grass = glm(outcome ~ . -avg_opponent_firstIn -avg_firstIn -tourney_level -avg_firstWon
                -avg_opponent_firstWon,data = grass_train, family = binomial)
summary(log.grass)
vif(log.grass) # no multicollinearity

# Prediction on test set
predprob_log_grass <- predict.glm(log.grass, grass_test, type = "response")
predclass_log_grass <- ifelse(predprob_log_grass >= 0.5, yes = 1, 0)
caret::confusionMatrix(as.factor(predclass_log_grass), grass_test$outcome, positive = "1")

# Accuracy :    0.6811    
# Sensitivity : 0.6940        
# Specificity : 0.6675 

#Logistic Regression with Stepwise Selection
null_model_grass = glm(outcome ~ 1, data = grass_train, family = binomial) 
full_model_grass = log.grass

step.model.AIC.grass = step(null_model_grass, scope = list(upper = full_model_grass),
                           direction = "both", test = "Chisq", trace = F) 
summary(step.model.AIC.grass) 

# Best model based on stepwise 
log.sel.grass <- glm(outcome ~ rank_dif + avg_opponent_bpSaved + avg_bpSaved + opponent_seeded + seeded +
                       avg_opponent_secWon + avg_secWon + ht_dif + avg_df + avg_svpt + age_dif + avg_opponent_svpt + 
                       avg_opponent_df + opponent_hand + hand,
                    grass_train, family = binomial)
summary(log.sel.grass)

# predictions based on stepwise model
logistic_pred2_grass <- predict(log.sel.grass, newdata = grass_test, type = "response")
logistic_pred_class2_grass <- ifelse(logistic_pred2_grass > 0.5, yes = 1,0)
caret::confusionMatrix(as.factor(logistic_pred_class2_grass), grass_test$outcome, positive = "1")

# Accuracy :    0.6803     
# Sensitivity : 0.6924         
# Specificity : 0.6675


# Logistic Regression: HARD
log.hard <- glm(outcome ~ ., data = hard_train, family = binomial)
summary(log.hard)
vif(log.hard)  # Check for multicollinearity in the model

log.hard = glm(outcome ~ . -avg_firstIn -avg_opponent_firstIn -avg_firstWon
               -avg_opponent_firstWon,data = hard_train, family = binomial)
summary(log.hard)
vif(log.hard) #no multicolienarity 

# Prediction on test set
predprob_log_hard <- predict.glm(log.hard, hard_test, type = "response")
predclass_log_hard <- ifelse(predprob_log_hard >= 0.5, yes = 1, 0)
caret::confusionMatrix(as.factor(predclass_log_hard), hard_test$outcome, positive = "1")

# Accuracy :    0.6666    
# Sensitivity : 0.6645        
# Specificity : 0.6687

#Logistic Regression with Stepwise Selection
null_model_hard = glm(outcome ~ 1, data = hard_train, family = binomial) 
full_model_hard = log.hard

step.model.AIC.hard = step(null_model_hard, scope = list(upper = full_model_hard),
                            direction = "both", test = "Chisq", trace = F) 
summary(step.model.AIC.hard) 

# Best model based on stepwise 
log.sel.hard <- glm(outcome ~ rank_dif + avg_bpSaved + avg_opponent_bpSaved + seeded + opponent_seeded +
                       age_dif + avg_secWon + avg_opponent_secWon + avg_opponent_df + avg_ace + avg_svpt + 
                      avg_opponent_ace + avg_opponent_svpt + avg_df + hand + opponent_hand + ht_dif,
                     hard_train, family = binomial)
summary(log.sel.hard)

# predictions based on stepwise model
logistic_pred2_hard <- predict(log.sel.hard, newdata = hard_test, type = "response")
logistic_pred_class2_hard <- ifelse(logistic_pred2_hard > 0.5, yes = 1,0)
caret::confusionMatrix(as.factor(logistic_pred_class2_hard), hard_test$outcome, positive = "1")

# Accuracy :    0.6672     
# Sensitivity : 0.6648         
# Specificity : 0.6696  

#Common Variables: Some variables appear consistently across different surface types, such as:
#rank_dif, seeded, opponent_seeded, avg_secWon, avg_opponent_secWon, and age_dif. 
#This suggests that these variables have a consistent impact on match outcomes regardless of the surface type.

#Differing Variables:
#In the clay model, avg_ace has a negative coefficient, indicating that a higher average number of aces is associated with a lower probability of winning.
#In the grass model, avg_ace has a positive coefficient, suggesting that a higher average number of aces is associated with a higher probability of winning on grass.
#Similarly, other variables like ht_dif, avg_df, and avg_svpt also have coefficients that vary across surface types.



####
#### Does match length depend on the surface type?
####
levels(as.factor(match_outcomes$surface))

surface_subset <- match_outcomes[match_outcomes$surface != "Carpet", ]
levels(as.factor(surface_subset$best_of))
surface_bestof3 <- surface_subset[surface_subset$best_of == 3, ]
surface_bestof5 <- surface_subset[surface_subset$best_of == 5, ]

lm_surface3 <- lm(minutes ~ surface, data = surface_bestof3)
summary(lm_surface3)

# FOR BEST OF 3:
#The intercept (for matches played on clay) is estimated to be 100.7297 minutes.
#Matches played on Grass surface have, on average, 8.26 minutes shorter duration compared to matches played on clay.
#Matches played on Hard surface have, on average, 3.4627 minutes shorter duration compared to matches played on clay.

lm_surface5 <- lm(minutes ~ surface, data = surface_bestof5)
summary(lm_surface5)

# FOR BEST OF 5:
#The intercept (for matches played on clay) is estimated to be 151.931 minutes.
#Matches played on Grass surface have, on average 8.89 minutes shorter duration than matches played on clay.
#Matches played on Hard surface have, on average 0.5864 minutes shorter duration than matches played on clay (NOT SIGNIFICANT)
