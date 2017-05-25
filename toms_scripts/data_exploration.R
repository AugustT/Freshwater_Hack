# Lets have a look at the data

raw_data <- read.csv('data/FWW_EngagementData_FinalSum.csv')

summary(raw_data)

# What do the last dates of activity look like
raw_data$Latest <- as.Date(as.character(raw_data$Latest), format = '%d-%b-%y')

hist(raw_data$Latest[raw_data$Latest > as.Date('2011-01-01')], breaks = 'weeks')

table(raw_data$Latest)
