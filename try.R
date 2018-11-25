raw_data <- file.path('data', paste('NIH','.csv', sep='')) %>%
  read.csv(., header = TRUE, sep = '\t')
write.csv(raw_data, file = 'nih.csv', sep = ',')