###Lascar Calibration analysis
#Jiawen Liao
#May 30th, 2018


library(readxl)
library(data.table)
#read in the data
H47_CRF_GT <- read_excel("~/Box Sync/Exposure Data--GUATEMALA/Excel CRFs/EXP_H47-Lascar Calibration_2017-10-9.xlsx", 
                         sheet = "Template", col_types = c("date", 
                                                           "text", "text", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric"), 
                         skip = 1)


H47_CRF_GT = as.data.table(H47_CRF_GT)
H47_CRF_GT = H47_CRF_GT[Name != "RP",c(1:29)]
##Change the lascar ID
unique(H47_CRF_GT$`Lascar ID`)
H47_CRF_GT[nchar(`Lascar ID`) == 3, `Lascar ID`:=paste("USB-CO-",`Lascar ID`, sep = "")]
H47_1 = H47_CRF_GT[nchar(`Lascar ID`) == 10]

#analysis of calibration slope and intercept
H47_1[,slope:=`Calibration Slope [ppm Lascar/ppm actual]`]
H47_1[,intercept:=`Calibration Intercept [ppm]`]
#secular trend
H47_1[,slope_daily := mean(slope), by = `Date (dd-mm-yyyy)`]
H47_1[,intercept_daily := mean(intercept), by = `Date (dd-mm-yyyy)`]

secular = unique(H47_1[,c("Date (dd-mm-yyyy)","slope_daily","intercept_daily")])

#analysis of each lascar QC data
H47_1[,slope_lascar := mean(slope), by = `Lascar ID`]
H47_1[,intercept_lascar := mean(intercept), by = `Lascar ID`]
H47_1[,number_measure := length(intercept), by = `Lascar ID`]


lascar_data = unique(H47_1[,c("Lascar ID","slope_lascar","intercept_lascar","number_measure")])
options(scipen=999)
#export
write.csv(lascar_data, file = "~/Box Sync/Exposure Data--GUATEMALA/Excel CRFs/H47_results.csv")





