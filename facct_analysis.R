### FAccT Analysis
### Brooke Perreault
### 1/11/24

############### Read in all of the CSVs (1 per day) and combine them ###################
files <- list.files(path="facct_results", pattern='.*\\.csv', full.names=TRUE)
data <- read.csv(files[1], header=TRUE)

for (i in 2:length(files)) {
  df <- read.csv(files[i], header=TRUE)
  data <- rbind(data, df)
}

# Add query category information
query_cats <- read.csv("query_categories_final.csv", header=TRUE)
categories <- rep(NA, nrow(data))
for (i in 1:nrow(data)) {
  q <- data[i,"query"]
  cat <- query_cats$Category[query_cats$Query==q]
  cat <- ifelse(cat=="Ballots", "Voting Logistics", cat)  # combine ballots and voting logistics
  categories[i] <- cat
}
data$query_category <- categories

## REMOVE ERROR ROWS (Location did not change properly; coordinates wrong)
error_file <- read.csv("wrong-locations.csv")
errors <- error_file[-which(error_file$state=="Ohio"),]
bad_ids <- which(data$state=="Ohio") 
data <- data[-bad_ids,] # drop all of Ohio - too much error

# drop remaining locations with errors
drop_ids <- c()
for (i in 1:nrow(errors)) {
  ids <- which(data$state==errors[i,1] & data$place==errors[i,2])
  #print(length(ids))
  drop_ids <- c(drop_ids, ids)
}
data_clean <- data[-drop_ids,]

# # Drop "Issues" queries
# issues_ids <- which(data_clean$query_category=="Issues")
# data_clean <- data_clean[-issues_ids,]

# Final dimensions
dim(data_clean)

# number of final locations
nrow(unique(data_clean[,c("state", "place")]))

########################### Initial Exploration #####################
# summary(data_clean$gov_count)
# data[which.max(data$gov_count),]

# Number of non-federal govt
sum(data_clean$gov_count-data_clean$federal_count)  # matches Python rank analysis

# How many have zero govt or govt maintained results (fed, state, county, local, native)
ids_zero <- which(data_clean$gov_count==0) #737355
pct_zero <- length(ids_zero)/nrow(data_clean) #21.3% don't have any govt

# So, about 78.6% of SERPs have at least one govt or govt maintained domain (including federal) and this is pretty consistent across states.

# % of SERPs by category that have at least 1 govt or govt maintained result
table(data_clean$query_category[-ids_zero])/as.array(table(data_clean$query_category))

# % of SERPs that have at least 1 state, county, or local govt website
ids_non_fed <- which(data_clean$state_count>0 | data_clean$county_count>0 | data_clean$local_count>0)
length(ids_non_fed)/nrow(data_clean)
# 69% of SERPs have at least 1 state, county, or local govt/govt maintained result


#### Senator and Represenative sources ####
# Number of SERPs where a senator source shows up
nrow(data_clean[data_clean$sen_count>0,])

# for which queries do senator sources show up
table(data_clean$query[data_clean$sen_count>0])

# how many appearances in organic results
sum(data_clean$sen_count)

# Number of SERPs where a represenative source shows up
nrow(data_clean[data_clean$rep_count>0,])

# for which queries do representative sources show up
table(data_clean$query_category[data_clean$rep_count>0])

# how many appearances in organic results
sum(data_clean$rep_count)

## Summary of number of govt results per SERP
# Ballots
# summary(data_clean$gov_count[data_clean$query_category=="Ballots"])
# summary(data_clean$federal_count[data_clean$query_category=="Ballots"])
# summary(data_clean$state_count[data_clean$query_category=="Ballots"])
# summary(data_clean$county_count[data_clean$query_category=="Ballots"])
# summary(data_clean$local_count[data_clean$query_category=="Ballots"])

# Voting Logistics
summary(data_clean$gov_count[data_clean$query_category=="Voting Logistics"])
summary(data_clean$federal_count[data_clean$query_category=="Voting Logistics"])
summary(data_clean$state_count[data_clean$query_category=="Voting Logistics"])
summary(data_clean$county_count[data_clean$query_category=="Voting Logistics"])
summary(data_clean$local_count[data_clean$query_category=="Voting Logistics"])

# Candidates and issues
summary(data_clean$gov_count[data_clean$query_category=="Candidates and Issues"])
summary(data_clean$federal_count[data_clean$query_category=="Candidates and Issues"])
summary(data_clean$state_count[data_clean$query_category=="Candidates and Issues"])
summary(data_clean$county_count[data_clean$query_category=="Candidates and Issues"])
summary(data_clean$local_count[data_clean$query_category=="Candidates and Issues"])

# General Election Topics
summary(data_clean$gov_count[data_clean$query_category=="General Election Topics"])
summary(data_clean$federal_count[data_clean$query_category=="General Election Topics"])
summary(data_clean$state_count[data_clean$query_category=="General Election Topics"])
summary(data_clean$county_count[data_clean$query_category=="General Election Topics"])
summary(data_clean$local_count[data_clean$query_category=="General Election Topics"])


# Boxplots of number of gov results per serp by state
par(mfrow=c(1,1))
boxplot(gov_count~state, data=data_clean, las=2)

######################### FUNCTIONS ###########################################
counts <- function(category) {
  "Calculate appearances for each type of government source per given category."
  fed <- sum(data_clean$federal_count[data_clean$query_category==category])/sum(data_clean$total_results[data_clean$query_category==category]) # federal
  
  state <- sum(data_clean$state_count[data_clean$query_category==category])/sum(data_clean$total_results[data_clean$query_category==category]) # state
  
  county <- sum(data_clean$county_count[data_clean$query_category==category])/sum(data_clean$total_results[data_clean$query_category==category]) # county
  
  local <- sum(data_clean$local_count[data_clean$query_category==category])/sum(data_clean$total_results[data_clean$query_category==category]) # local
  
  rep <- sum(data_clean$rep_count[data_clean$query_category==category])/sum(data_clean$total_results[data_clean$query_category==category]) # representative
  
  sen <- sum(data_clean$sen_count[data_clean$query_category==category])/sum(data_clean$total_results[data_clean$query_category==category]) # senator
  total_app_no_fed <- state+county+local+rep+sen
  total <- total_app_no_fed + fed
  
  return (c(fed, state, county, local, rep, sen, total_app_no_fed, total))
}

correct <- function(category) {
  "calculate % correct appearances for state, county, local, rep, sen govt sources for given category"
  state_correct <- sum(data_clean$State_correct[data_clean$query_category==category])/sum(data_clean$total_results[data_clean$query_category==category]) # state
  county_correct <- sum(data_clean$County_correct[data_clean$query_category==category])/sum(data_clean$total_results[data_clean$query_category==category]) # county
  local_correct <- sum(data_clean$Local_correct[data_clean$query_category==category])/sum(data_clean$total_results[data_clean$query_category==category]) # local
  rep_correct <- sum(data_clean$rep_correct[data_clean$query_category==category])/sum(data_clean$total_results[data_clean$query_category==category]) # representative
  sen_correct <-  sum(data_clean$sen_correct[data_clean$query_category==category])/sum(data_clean$total_results[data_clean$query_category==category]) # senator
  return (c(state_correct, county_correct, local_correct, rep_correct, sen_correct))
}

mistargeting_rate <- function(state, county, local, rep, sen, state_correct, county_correct, local_correct, rep_correct, sen_correct) {
  "Calculate % mistargeted state, county, local govt sources for one category."
  state_mis <- (state-state_correct)/state
  county_mis <- (county-county_correct)/county
  local_mis <- (local-local_correct)/local
  rep_mis <- (rep-rep_correct)/rep
  sen_mis <- (sen-sen_correct)/sen
  overall_mis <- ((state+county+local+rep+sen)-(state_correct+county_correct+local_correct+rep_correct+sen_correct))/(state+county+local+rep+sen)
  
  return (c(state_mis, county_mis, local_mis, rep_mis, sen_mis, overall_mis))
}

in_state <- function(category) {
  "Calculate % of mistargeted appearances that are in correct state for given category"
  # County
  county_instate <- sum(data_clean$County_instate[data_clean$query_category==category]-data_clean$County_correct[data_clean$query_category==category])
  mis_instate_county <- county_instate/(sum(data_clean$county_count[data_clean$query_category==category]))
  
  # Local
  local_instate <- sum(data_clean$Local_instate[data_clean$query_category==category]-data_clean$Local_correct[data_clean$query_category==category])
  mis_instate_local <- local_instate/(sum(data_clean$local_count[data_clean$query_category==category]))
  
  # rep
  rep_instate <- sum(data_clean$rep_instate[data_clean$query_category==category]-data_clean$rep_correct[data_clean$query_category==category])
  mis_instate_rep <- rep_instate/(sum(data_clean$rep_count[data_clean$query_category==category]))
  
  return (c(mis_instate_county, mis_instate_local, mis_instate_rep))
}

do_all_one_cat <- function(category) {
  "Do all calculations for one category"
  cat_counts <- counts(category) #fed, state, county, local, rep, sen, total_app_no_fed, total
  cat_correct <- correct(category) #c(state_correct, county_correct, local_correct, rep_correct, sen_correct)
  
  # Input: state, county, local, rep, sen, state_correct, county_correct, local_correct, rep_correct, sen_correct
  cat_mistarget <- mistargeting_rate(cat_counts[2], cat_counts[3],cat_counts[4],
                                     cat_counts[5], cat_counts[6],cat_correct[1],
                                     cat_correct[2], cat_correct[3], cat_correct[4],
                                     cat_correct[5])
  # output: c(state_mis, county_mis, local_mis, rep_mis, sen_mis, overall_mis)
  
  cat_instate <- in_state(category) #c(mis_instate_county, mis_instate_local, mis_instate_rep)

  return (list(cat_counts, cat_correct, cat_mistarget, cat_instate))
}

all_queries <- function(data_clean) {
  "Calculate overall proprotions across all queries"
  all_fed <- sum(data_clean$federal_count)/sum(data_clean$total_results)
  all_state <- sum(data_clean$state_count)/sum(data_clean$total_results)
  all_county <-sum(data_clean$county_count)/sum(data_clean$total_results)
  all_local <- sum(data_clean$local_count)/sum(data_clean$total_results)
  all_rep <- sum(data_clean$rep_count)/sum(data_clean$total_results)
  all_sen <- sum(data_clean$sen_count)/sum(data_clean$total_results)
  all_counts <- c(all_fed, all_state, all_county, all_local, all_rep, all_sen,
                  all_state+all_county+all_local+all_rep+all_sen, 
                  all_state+all_county+all_local+all_fed+all_rep+all_sen)
  
  # Correct - all queries
  all_state_correct <- sum(data_clean$State_correct)/sum(data_clean$total_results)
  all_county_correct  <-sum(data_clean$County_correct)/sum(data_clean$total_results)
  all_local_correct  <- sum(data_clean$Local_correct)/sum(data_clean$total_results)
  all_rep_correct <- sum(data_clean$rep_correct)/sum(data_clean$total_results)
  all_sen_correct <- sum(data_clean$sen_correct)/sum(data_clean$total_results)
  all_correct <- c(all_state_correct, all_county_correct, all_local_correct, 
                   all_rep_correct, all_sen_correct)
  
  # Mistargeting - all queries
  all_state_mis <- (all_state-all_state_correct)/all_state
  all_county_mis <- (all_county-all_county_correct)/all_county
  all_local_mis <- (all_local-all_local_correct)/all_local
  all_rep_mis <- (all_rep-all_rep_correct)/all_rep
  all_sen_mis <- (all_sen-all_sen_correct)/all_sen
  
  all_mis <- ((all_state+all_county+all_local+all_rep+all_sen)-(all_state_correct+all_county_correct+all_local_correct+all_rep_correct+all_sen_correct))/(all_state+all_county+all_local+all_rep+all_sen)
  all_mis_vector <- c(all_state_mis, all_county_mis, all_local_mis, 
                      all_rep_mis, all_sen_mis, all_mis)
  
  # Mistargeted in state - all queries
  total_county_instate <- sum(data_clean$County_instate-data_clean$County_correct)
  county_instate_prop <- total_county_instate/sum(data_clean$county_count)
  total_local_instate <- sum(data_clean$Local_instate-data_clean$Local_correct)
  local_instate_prop <- total_local_instate/sum(data_clean$local_count)
  total_rep_instate <- sum(data_clean$rep_instate-data_clean$rep_correct)
  rep_instate_prop <- total_rep_instate/sum(data_clean$rep_count)
  
  instate <- c(county_instate_prop, local_instate_prop, rep_instate_prop)
  
  return (list(all_counts, all_correct, all_mis_vector, instate))
}

clean <- function(input) {
  pct <- input*100
  return (round(pct, 2))
}

############################## CALL FUNCTIONS #######################
ballot_output <- do_all_one_cat("Ballots")  # Not included in final paper
voting_output <- do_all_one_cat("Voting Logistics")
general_output <- do_all_one_cat("General Election Topics")
candidate_output <- do_all_one_cat("Candidates and Issues")
issue_output <- do_all_one_cat("Issues")  # Not included in final paper
all_output <- all_queries(data_clean)

############ MAKE MATRICES ######################

categories_list <- c("Ballots", "Voting Logistics", "General Election Topics", "Candidates", "Issues", "All Queries")
source_type_list <- c("Federal", "State", "County", "Local", "Rep.", "Senator", "Total excl. Fed", "Total")

apps_matrix <- matrix(data=c(ballot_output[[1]], voting_output[[1]], general_output[[1]], candidate_output[[1]],issue_output[[1]], all_output[[1]]), 
                      nrow=6, ncol=8, byrow=TRUE, dimnames=list(categories_list, source_type_list))

correct_matrix <- matrix(data=c(ballot_output[[2]], voting_output[[2]], general_output[[2]], candidate_output[[2]], issue_output[[2]], all_output[[2]]), 
                      nrow=6, ncol=5, byrow=TRUE, dimnames=list(categories_list, c("State Correct", "County Correct", "Local Correct", "Rep. Correct", "Senator Correct")))

mistarget_matrix <- matrix(data=c(ballot_output[[3]], voting_output[[3]], general_output[[3]], candidate_output[[3]], issue_output[[3]], all_output[[3]]), 
                           nrow=6, ncol=6, byrow=TRUE, dimnames=list(categories_list, c("State Mistargeted", "County Mistargeted", "Local Mistargeted", "Rep. Mistargeted", "Sen. Mistargeted","Overall Mistargeted")))

instate_matrix <- matrix(data=c(ballot_output[[4]], voting_output[[4]], general_output[[4]], candidate_output[[4]], issue_output[[4]], all_output[[4]]), 
                         nrow=6, ncol=3, byrow=TRUE, dimnames=list(categories_list, c("County Mis. - In State", "Local Mis. - In State", "Rep. Mis - In State")))

################## FINAL MATRICES FOR BREAKDOWN BY QUERY #####################
apps_matrix  # proportion of appearances by source type per query category 
correct_matrix  # proportion correct appearances by source type and query category
mistarget_matrix  # porportion of mistargeted results per source type per query category
instate_matrix  # proportion of mistargeted results in the correct state


# Combine matrices, convert to percentages with two decimals; save to CSV
final_matrix_cats <- cbind(apps_matrix, correct_matrix, mistarget_matrix, instate_matrix)
final_matrix_cats_clean <- apply(final_matrix_cats, MARGIN=2, FUN=clean)

write.csv(final_matrix_cats_clean, "results_by_query_category.csv")

# Barplot
# All sources
# barplot(final_matrix_cats_clean[,c(2,9,3,10,4,11)], beside=TRUE, col=c("darkred", "darkblue", "darkgreen", "purple3", "grey"), main="% appearances and % correct by source type", ylab="% of organic results", )
# legend("topright", c("Ballots", "Voting Logistics", "General Election Topics", "Candidates and Issues", "All Queries"), pch=15, bty="n", col=c("darkred", "darkblue", "darkgreen", "purple3", "grey"))

# by query category, stacked bar charts for state, county, local sources 
# State
df <- as.data.frame(final_matrix_cats_clean[,c(2,9,3,10,4,11)])
df$state_mistargeted <- df$State-df$`State Correct`
df$county_mistargeted <- df$County-df$`County Correct`
df$local_mistargeted <- df$Local-df$`Local Correct`
state_mat <- t(as.matrix(df[,c(2,7)]))
county_mat <- t(as.matrix(df[,c(4,8)]))
local_mat <- t(as.matrix(df[,c(6,9)]))

#par(mfrow=c(3,1))
barplot(state_mat, legend.text=c("Correct", "Mistargeted"), col=c("darkgreen", "darkred"), ylab="% appearances in organic", main="State Sources", xlab="Query Category", names.arg=c("Ballots", "Voting Logistics", "General", "Candidates", "All Queries"))
barplot(county_mat,  legend.text=c("Correct", "Mistargeted"), col=c("darkgreen", "darkred"), ylab="% appearances in organic", main="County Sources", xlab="Query Category",names.arg=c("Ballots", "Voting Logistics", "General", "Candidates", "All Queries"))
barplot(local_mat,  legend.text=c("Correct", "Mistargeted"), col=c("darkgreen", "darkred"), ylab="% appearances in organic", main="Local Sources", xlab="Query Category",names.arg=c("Ballots", "Voting Logistics", "General", "Candidates", "All Queries"))


# Barplots that include in-state
df2 <- as.data.frame(final_matrix_cats_clean)
# Add in state for county and local
df2$county_instate_pct <- df2$County*(df2$`County Mis. - In State`/100)
df2$local_instate_pct <- df2$Local*(df2$`Local Mis. - In State`/100)
df2$county_mistarget <- df2$County - df2$`County Correct`-df2$county_instate_pct
df2$local_mistarget <- df2$Local - df2$`Local Correct`-df2$local_instate_pct

county_mat2 <- t(as.matrix(df2[,c(10,23,25)]))
local_mat2 <- t(as.matrix(df2[,c(11,24,26)]))

# county sources
barplot(county_mat2,  legend.text=c("Correct","In State", "Mistargeted"), col=c("darkgreen","darkgoldenrod1", "darkred"), ylab="% appearances in organic", main="County Sources", xlab="Query Category",names.arg=c("Ballots", "Voting Logistics", "General", "Candidates", "All Queries"))

# local sources
barplot(local_mat2,  legend.text=c("Correct","In State", "Mistargeted"), col=c("darkgreen","darkgoldenrod1", "darkred"), ylab="% appearances in organic", main="Local Sources", xlab="Query Category",names.arg=c("Ballots", "Voting Logistics", "General", "Candidates", "All Queries"))


############### FUNCTIONS FOR BREAKDOWN BY STATE #######################
counts_state <- function(this_state) {
  "Calculate appearances for each type of government source per given state."
  fed <- sum(data_clean$federal_count[data_clean$state==this_state])/sum(data_clean$total_results[data_clean$state==this_state])
  state <- sum(data_clean$state_count[data_clean$state==this_state])/sum(data_clean$total_results[data_clean$state==this_state])
  county <- sum(data_clean$county_count[data_clean$state==this_state])/sum(data_clean$total_results[data_clean$state==this_state])
  local <- sum(data_clean$local_count[data_clean$state==this_state])/sum(data_clean$total_results[data_clean$state==this_state])
  total_app_no_fed <- state+county+local
  total <- total_app_no_fed + fed
  
  return (c(fed, state, county, local, total_app_no_fed, total))
}

correct_state <- function(this_state) {
  "calculate % correct appearances for state, county, and local govt sources for given state"
  state_correct <- sum(data_clean$State_correct[data_clean$state==this_state])/sum(data_clean$total_results[data_clean$state==this_state]) # state
  county_correct <- sum(data_clean$County_correct[data_clean$state==this_state])/sum(data_clean$total_results[data_clean$state==this_state]) # county
  local_correct <- sum(data_clean$Local_correct[data_clean$state==this_state])/sum(data_clean$total_results[data_clean$state==this_state]) # local
  return (c(state_correct, county_correct, local_correct))
}

mistargeting_rate_state <- function(state, county, local, state_correct, county_correct, local_correct) {
  "Calculate % mistargeted state, county, local govt sources for one category."
  state_mis <- (state-state_correct)/state
  county_mis <- (county-county_correct)/county
  local_mis <- (local-local_correct)/local
  overall_mis <- ((state+county+local)-(state_correct+county_correct+local_correct))/(state+county+local)
  
  return (c(state_mis, county_mis, local_mis, overall_mis))
}

in_state_by_state <- function(this_state) {
  "Calculate % of mistargeted appearances that are in correct state for given state"
  # County
  county_instate <- sum(data_clean$County_instate[data_clean$state==this_state]-data_clean$County_correct[data_clean$state==this_state])
  mis_instate_county <- county_instate/(sum(data_clean$county_count[data_clean$state==this_state]))
  
  # Local
  local_instate <- sum(data_clean$Local_instate[data_clean$state==this_state]-data_clean$Local_correct[data_clean$state==this_state])
  mis_instate_local <- local_instate/(sum(data_clean$local_count[data_clean$state==this_state]))
  
  return (c(mis_instate_county, mis_instate_local))
}

do_all_one_state <- function(this_state) {
  "Do all calculations for one category"
  cat_counts <- counts_state(this_state) #fed, state, county, local, total_app_no_fed, total
  cat_correct <- correct_state(this_state) #c(state_correct, county_correct, local_correct)
  
  # Input: state, county, local, state_correct, county_correct, local_correct
  cat_mistarget <- mistargeting_rate_state(cat_counts[2], cat_counts[3],cat_counts[4], 
                                     cat_correct[1], cat_correct[2], cat_correct[3])
  # output: c(state_mis, county_mis, local_mis, overall_mis)
  
  cat_instate <- in_state_by_state(this_state) #c(mis_instate_county, mis_instate_local)
  
  return (list(cat_counts, cat_correct, cat_mistarget, cat_instate))
}


state_labels <- unique(data_clean$state)
state_labels <- state_labels[order(state_labels)]  # alphabetical order state labels

do_all_states <- function(state_labels) {
  counts_by_state <- c() # proportions appearances
  correct_by_state <- c()  # proportions correct
  mistarget_by_state <- c() # proportions mistargeted
  instate_prop_by_state <- c() # proportions of mistargeted that are in state
  
  for (i in 1:length(state_labels)) {
    this_output <- do_all_one_state(state_labels[i])
    counts_by_state <- c(counts_by_state, this_output[[1]])
    correct_by_state <- c(correct_by_state, this_output[[2]])
    mistarget_by_state <- c(mistarget_by_state, this_output[[3]])
    instate_prop_by_state <- c(instate_prop_by_state, this_output[[4]])
    # list(cat_counts, cat_correct, cat_mistarget, cat_instate)
  }
  
  return (list(counts_by_state, correct_by_state, mistarget_by_state, instate_prop_by_state))
}

make_matrix <- function(output) {
  # Overall proportions by state
  app_by_state <- matrix(data=output[[1]], 
                         nrow=25, ncol=6, byrow=TRUE, dimnames=list(state_labels, c("Federal", "State", "County", "Local", " Total excl. Fed", "Total")))
  
  # proportions correct by state
  correct_matrix_state <- matrix(data=output[[2]], 
                                 nrow=25, ncol=3, byrow=TRUE, dimnames=list(state_labels, c("State Correct", "County Correct", "Local Correct")))
  
  # Mistargeting rate by state
  mistarget_matrix_state <- matrix(data=output[[3]], 
                                   nrow=25, ncol=4, byrow=TRUE, dimnames=list(state_labels, c("State Mistargeted", "County Mistargeted", "Local Mistargeted", "Overall Mistargeted")))
  
  # Proportions mistargeted instate by state
  instate_matrix_state <- matrix(data=output[[4]], 
                                 nrow=25, ncol=2, byrow=TRUE, dimnames=list(state_labels, c("County Mis. - In State", "Local Mis. - In State")))
  
  # Combine all state matrices
  final_by_state <- cbind(app_by_state, correct_matrix_state, mistarget_matrix_state, instate_matrix_state)
  final_by_state_clean <- apply(final_by_state, MARGIN=2, FUN=clean)
  
  return(final_by_state_clean)
}


barplots_correct_mis <- function(matrix, category) {
  df_state1 <- as.data.frame(matrix)
  df_state1$state_mistarget <- df_state1$State - df_state1$`State Correct`
  df_state1$county_mistarget <- df_state1$County - df_state1$`County Correct`
  df_state1$local_mistarget <- df_state1$Local - df_state1$`Local Correct`
  # Make matrices for barplots
  mat_state1 <- t(as.matrix(df_state1[,c(7,16)]))
  mat_county1 <- t(as.matrix(df_state1[,c(8,17)]))
  mat_local1 <- t(as.matrix(df_state1[,c(9,18)]))
  
  par(mar = c(1.1, 4.1, 1.6, 2.1), mfrow=c(3,1))
  barplot(mat_state1, col=c("palegreen3", "tomato3"), ylab="% organic", main="State Government Domains", las=2, xaxt="n")
  barplot(mat_county1, col=c("palegreen3","tomato3"), ylab="% organic", main="County Government Domains",las=2, xaxt="n")
  par(mar = c(7.1, 4.1, 1.6, 2.1))
  barplot(mat_local1, col=c("palegreen3", "tomato3"), ylab="% in organic", main="Local Government Domains", las=2)
}


barplot_in <- function(matrix) {
  df_state <- as.data.frame(matrix)
  df_state$state_mistarget <- df_state$State - df_state$`State Correct`
  # Add in state for county and local
  df_state$county_instate_pct <- df_state$County*(df_state$`County Mis. - In State`/100)
  df_state$local_instate_pct <- df_state$Local*(df_state$`Local Mis. - In State`/100)
  df_state$county_mistarget <- df_state$County - df_state$`County Correct`-df_state$county_instate_pct
  df_state$local_mistarget <- df_state$Local - df_state$`Local Correct`-df_state$local_instate_pct
  
  # Make matrices for barplots
  mat_state <- t(as.matrix(df_state[,c(7,16)]))
  mat_county <- t(as.matrix(df_state[,c(8,17,19)]))
  mat_local <- t(as.matrix(df_state[,c(9,18,20)]))
  
  # Barplots [need to fix legend placement]
  par(mar = c(1.1, 4.1, 1.6, 7.1), mfrow=c(3,1))
  barplot(mat_state, col=c("palegreen3", "tomato3"), ylab="% in organic", main="State Government Domains", las=2, xaxt="n")
  # legend("right", inset=c(-.16,0),legend=c("Mistargeted", "In State", "Correct"), fill=c("tomato3", "khaki1", "palegreen3"), xpd=TRUE)
  barplot(mat_county, col=c("palegreen3", "khaki1","tomato3"), ylab="% in organic", main="County Government Domains",las=2, xaxt="n", ylim=c(0,20))
  par(mar = c(7.1, 4.1, 1.6, 7.1))
  legend("right", inset=c(-0.16,0),legend=c("Mistargeted", "In State", "Correct"), fill=c("tomato3", "khaki1", "palegreen3"), xpd=TRUE)
  barplot(mat_local, col=c("palegreen3", "khaki1","tomato3"), ylab="% in organic", main="Local Government Domains", las=2)
  
}

##################### CALL FUNCTIONS ####################

output <- do_all_states(state_labels)
final_by_state_clean <- make_matrix(output)

write.csv(as.data.frame(final_by_state_clean), "results_by_state.csv") 
barplots_correct_mis(final_by_state_clean)
barplot_in(final_by_state_clean)

# legend("topright", inset=c(-0.28,0),legend=c("Mistargeted", "In State", "Correct"), fill=c("tomato3", "khaki1", "palegreen3"), xpd=TRUE)

### Correlation between mistargeting and # of local and county governments present
census <- read.csv("gov_census/local_gov_census.csv")
census$AMOUNT <- as.numeric(gsub(",", "", census$AMOUNT))
census_subset <- census[census$AGG_DESC_TTL=="Total Local Government Units",]
county_subset <- census[census$AGG_DESC_TTL=="Total Local Government Units - County Governments",]


state_mis_df <- as.data.frame(final_by_state_clean)

lcs <- c()
ccs <- c()
for (i in 1:length(state_labels)) {
  #print(state_labels[i])
  lc <- census_subset$AMOUNT[census_subset$GEO_TTL==state_labels[i]]
  cc <- county_subset$AMOUNT[county_subset$GEO_TTL==state_labels[i]]
  lcs <- c(lcs, lc)
  ccs <- c(ccs, cc)
}

# Correlation between instate mistargeting and number of local governments
cor(lcs, state_mis_df$`Local Mis. - In State`)
plot(lcs, state_mis_df$`Local Mis. - In State`)

# Moderately strong, positive correlation between the number of local governments in a state and local mistargeting in that state
cor(lcs, state_mis_df$`Local Mistargeted`)

# Correlation between county mistargeting and number of county governments
cor(ccs, state_mis_df$`County Mistargeted`)



############ BARPLOTS - CORRECT AND MISTARGETED ONLY #####################
# # Make matrices for barplots
# mat_state1 <- t(as.matrix(df_state1[,c(7,16)]))
# mat_county1 <- t(as.matrix(df_state1[,c(8,17)]))
# mat_local1 <- t(as.matrix(df_state1[,c(9,18)]))
# 
# par(mar = c(7.1, 4.1, 4.1, 2.1))
# barplot(mat_state1, col=c("darkgreen", "darkred"), ylab="% appearances in organic", main="State Sources", las=2)
# barplot(mat_county1, col=c("darkgreen","darkred"), ylab="% appearances in organic", main="County Sources",las=2)
# barplot(mat_local1, legend.text=c("Correct", "Mistargeted"), col=c("darkgreen", "darkred"), ylab="% appearances in organic", main="Local Sources", las=2)

# ######## BARPLOTS WITH CORRECT, INSTATE, MISTARGETED
# df_state <- as.data.frame(final_by_state_clean)
# df_state$state_mistarget <- df_state$State - df_state$`State Correct`
# # Add in state for county and local
# df_state$county_instate_pct <- df_state$County*(df_state$`County Mis. - In State`/100)
# df_state$local_instate_pct <- df_state$Local*(df_state$`Local Mis. - In State`/100)
# df_state$county_mistarget <- df_state$County - df_state$`County Correct`-df_state$county_instate_pct
# df_state$local_mistarget <- df_state$Local - df_state$`Local Correct`-df_state$local_instate_pct
# 
# 
# # Make matrices for barplots
# mat_state <- t(as.matrix(df_state[,c(7,16)]))
# mat_county <- t(as.matrix(df_state[,c(8,17,19)]))
# mat_local <- t(as.matrix(df_state[,c(9,18,20)]))
# 
# # Barplots [need to fix legend placement]
# par(mar = c(7.1, 4.1, 4.1, 2.1))
# barplot(mat_state, col=c("darkgreen", "darkred"), ylab="% appearances in organic", main="State Sources", las=2)
# barplot(mat_county, col=c("darkgreen", "darkgoldenrod1","darkred"), ylab="% appearances in organic", main="County Sources",las=2)
# barplot(mat_local, legend.text=c("Correct", "In State","Mistargeted"), col=c("darkgreen", "darkgoldenrod1","darkred"), ylab="% appearances in organic", main="Local Sources", las=2)


#################### OTHER ANALYSIS #################
# Proportion of SERPs with only incorrect results
nrow(data_clean[(data_clean$gov_count-data_clean$federal_count)>0 & data_clean$Overall_correct==0,])/nrow(data_clean)

ids_all_incorrect <- which((data_clean$gov_count-data_clean$federal_count)>0 & data_clean$Overall_correct==0)

# all incorrect by query category
table(data_clean$query_category[ids_all_incorrect])/as.vector(table(data_clean$query_category))

# Proportion of SERPs with all incorrect by state
test <- table(data_clean$state[ids_all_incorrect])/as.vector(table(data_clean$state))
barplot(test, las=2)

# Which are all incorrect, but have some instate ("close")
ids_some_instate <- which((data_clean$gov_count-data_clean$federal_count)>0 & data_clean$Overall_correct==0 & data_clean$Overall_instate>0)
length(ids_some_instate)/length(ids_all_incorrect) #12.5% of the 19.9%
length(ids_some_instate)/nrow(data_clean) # about 2.5% of pages overall have no correctly targeted results, but some "close" in state (county or local) results


# In which query categories are all incorrect results most common
table(data_clean$query_category[ids_all_incorrect])/as.vector(table(data_clean$query))
# All incorrect results are most common in candidates and issues theme - from manual inspection, these pages seem to be more stagnant as well