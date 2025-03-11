# Load required library
library(dplyr)

# Question 1: Understanding Code Snippets
df1 = data.frame(
  Name=c('James','Paul','Richards','Marico','Samantha','Ravi','Raghu',
         'Richards','George','Ema','Samantha','Catherine'),
  State=c('Alaska','California','Texas','North Carolina','California','Texas',
          'Alaska','Texas','North Carolina','Alaska','California','Texas'),
  Sales=c(14,24,31,12,13,7,9,31,18,16,18,14)
)

# Aggregate sales by state
aggregate(df1$Sales, by=list(df1$State), FUN=sum)

# Using dplyr to group and summarize sales
df1 %>% group_by(State) %>% summarise(sum_sales = sum(Sales))

# Question 2: Analyzing WorldCupMatches.csv
df_wc = read.csv("WorldCupMatches.csv")

# (a) Find size of the data frame
dim(df_wc)  

# (b) Statistical summary of the data
summary(df_wc)

# (c) Number of unique locations where World Cup matches were held
length(unique(df_wc$City))  

# (d) Average attendance
mean(df_wc$Attendance, na.rm=TRUE)

# (e) Total goals scored by each home team
df_wc %>% group_by(Home.Team.Name) %>% summarise(total_goals = sum(Home.Team.Goals, na.rm=TRUE))

# (f) Average attendance per year
df_wc %>% group_by(Year) %>% summarise(avg_attendance = mean(Attendance, na.rm=TRUE))

# Question 3: Analyzing metabolite.csv
df_metabolites = read.csv("metabolite.csv")

# (a) Find number of Alzheimer's patients
df_metabolites %>% filter(Diagnosis == "Alzheimer’s") %>% summarise(count = n())

# (b) Determine the number of missing values for each column
colSums(is.na(df_metabolites))

# (c) Remove rows with missing values in the Dopamine column
df_clean = df_metabolites[!is.na(df_metabolites$Dopamine), ]

# (d) Replace missing values in c4-OH-Pro with median
df_clean$c4.OH.Pro[is.na(df_clean$c4.OH.Pro)] = median(df_clean$c4.OH.Pro, na.rm=TRUE)

# (e) Drop columns with more than 25% missing values (Optional)
cols_to_drop <- names(df_clean)[colSums(is.na(df_clean)) / nrow(df_clean) > 0.25]
print(cols_to_drop)
df_clean <- df_clean[, colSums(is.na(df_clean)) / nrow(df_clean) <= 0.25]

