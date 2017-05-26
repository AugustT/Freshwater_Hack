# Lets have a look at the data
raw_data <- read.csv('data/FWW_EngagementData_FinalSum.csv',
                     stringsAsFactors = FALSE)

summary(raw_data)

# What do the last dates of activity look like
raw_data$Latest <- as.Date(as.character(raw_data$Latest), format = '%d-%b-%y')

hist(raw_data$Latest[raw_data$Latest > as.Date('2011-01-01')], breaks = 'weeks')

table(raw_data$Latest)

# Okay, so which columns do we need to fix?
str(raw_data)

# There are a bunch of columns that are factors that need to be numeric
# they have strange NA values that need to be fixed
new_data <- raw_data

for(i in c('HWP', 'HWP.1', 'Time', 'Bulk', 'Complex', 'Task', 'Online')){
  
  new_data[,i] <- as.numeric(gsub('#N/A', 'NA', raw_data[,i]))
  
}

str(new_data)

# Now fix the date columns
for(i in c('created', 'access', 'login')){
  new_data[,i] <- as.Date(as.character(raw_data[,i]), format = '%d-%b-%y')
}  

str(new_data)
save(new_data, file = 'data/new_data.rdata')
