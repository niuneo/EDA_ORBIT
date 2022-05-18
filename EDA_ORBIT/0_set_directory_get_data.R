# setting working directory
setwd('C:/Users/U058507/UCB/My Objectives 2022/Code Pool/EDA/EDA_ORBIT')

## ----import_data, warning=FALSE--------------------------------------------------------------
library(readxl)

df <- read_excel('./data/Events_iPVU_Germany.xlsx') #  tbl_df format

print("Dimension of data:")
dim(df)