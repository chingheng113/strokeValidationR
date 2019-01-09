require(dplyr)

load_csv_data <- function(fName){
  raw_data <- file.path('data', paste(fName,'.csv', sep='')) %>%
                read.csv(., header = TRUE, sep = ',')
  return (raw_data)
}

# load_nih_data <- function(mrs){
#   data <- load_csv_data('NIH')
#   m_data <- dplyr::select(data, discharged_mrs)
#   id_data <- dplyr::select(data, IDCASE_ID)
#   b_data <- dplyr::select(data, c(4:13))
#   n_data <- dplyr::select(data, c(16:30))
#   if (mrs != 'all'){
#     id_data <- id_data[which(m_data==mrs), ]
#     b_data <- b_data[which(m_data==mrs), ]
#     n_data <- n_data[which(m_data==mrs), ]
#     m_data <- filter(m_data, discharged_mrs == mrs)
#   }
#   m_data <- data.frame(lapply(m_data, as.character), stringsAsFactors=FALSE)
#   return (list(id_data=id_data, b_data=b_data, n_data=n_data, m_data=m_data))
# }

load_tsr_data <- function(mrs, stroke_type){
  data <- load_csv_data('TSR_2017_cleaned')
  if(stroke_type == 'is'){
    data <- filter(data, (data$ICD_ID_1.0=='1' | data$ICD_ID_2.0=='1'))
  }else if (stroke_type == 'he'){
    data <- filter(data, (data$ICD_ID_3.0=='1' | data$ICD_ID_4.0=='1'))
  }else{
    # do nothing
  }
  id_data <- dplyr::select(data, CASE_ID)
  m_data <- dplyr::select(data, discharged_mrs)
  b_data <- dplyr::select(data, c(4:13))
  n_data <- dplyr::select(data, c(16:30))
  if (mrs != 'all'){
    id_data <- id_data[which(m_data==mrs), ]
    b_data <- b_data[which(m_data==mrs), ]
    n_data <- n_data[which(m_data==mrs), ]
    m_data <- subset(m_data, discharged_mrs == mrs)
  }
  m_data <- data.frame(lapply(m_data, as.character), stringsAsFactors=FALSE, row.names = rownames(m_data))
  return (list(id_data=id_data, b_data=b_data, n_data=n_data, m_data=m_data))
}


load_test <- function(dataset, mrs){
  fileName <- paste(dataset, '_testing_', mrs, '_org', sep='') 
  data <- load_csv_data(fileName)
  return (data)
}

data_clean <- function(data_name, filter.levels.lower, filter.levels.upper){
  data <- load_csv_data(data_name)
  
  keep.mrs.0 <- filter(data, data$discharged_mrs == '0')
  keep.mrs.0 <- filter(keep.mrs.0, (keep.mrs.0$Barthel_Total > filter.levels.lower[1] & keep.mrs.0$Barthel_Total < filter.levels.upper[1]))
  
  keep.mrs.1 <- filter(data, data$discharged_mrs == '1')
  keep.mrs.1 <- filter(keep.mrs.1, (keep.mrs.1$Barthel_Total > filter.levels.lower[2] & keep.mrs.1$Barthel_Total < filter.levels.upper[2]))
  
  keep.mrs.2 <- filter(data, data$discharged_mrs == '2')
  keep.mrs.2 <- filter(keep.mrs.2, (keep.mrs.2$Barthel_Total > filter.levels.lower[3] & keep.mrs.2$Barthel_Total < filter.levels.upper[3]))
  
  keep.mrs.3 <- filter(data, data$discharged_mrs == '3')
  keep.mrs.3 <- filter(keep.mrs.3, (keep.mrs.3$Barthel_Total > filter.levels.lower[4] & keep.mrs.3$Barthel_Total < filter.levels.upper[4]))
  
  keep.mrs.4 <- filter(data, data$discharged_mrs == '4')
  keep.mrs.4 <- filter(keep.mrs.4, (keep.mrs.4$Barthel_Total > filter.levels.lower[5] & keep.mrs.4$Barthel_Total < filter.levels.upper[5]))
  
  keep.mrs.5 <- filter(data, data$discharged_mrs == '5')
  keep.mrs.5 <- filter(keep.mrs.5, (keep.mrs.5$Barthel_Total > filter.levels.lower[6] & keep.mrs.5$Barthel_Total < filter.levels.upper[6]))
  
  final.keep.set <- rbind(keep.mrs.0, keep.mrs.1, keep.mrs.2, keep.mrs.3, keep.mrs.4, keep.mrs.5)
  return (final.keep.set)
}