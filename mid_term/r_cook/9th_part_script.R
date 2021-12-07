library(readr)  #verilerimizi çekmek için kullandığımız paket
dataset_ex <- read_delim("~/GitHub/Statistical-Analyze/datasets/dataset_ex.csv", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE)


head(dataset_ex, 3) #kitlemiz
set.seed(300) #kodu her çalıştırdığımızda örneklemimizin değişmemesi için kullandığımız bir fonksiyon
data_sample <- dataset_ex[sample(nrow(dataset_ex),300),replace=F]
head(data_sample, 3)