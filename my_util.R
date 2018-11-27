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
  data <- load_csv_data('TSR')
  if(stroke_type == 'is'){
    data <- filter(data, (data$ICD_ID_1.0=='1' | data$ICD_ID_2.0=='1'))
  }else if (stroke_type == 'he'){
    data <- filter(data, (data$ICD_ID_3.0=='1' | data$ICD_ID_4.0=='1'))
  }else{
    # do nothing
  }
  id_data <- dplyr::select(data, IDCASE_ID)
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