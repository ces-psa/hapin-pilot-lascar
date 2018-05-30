##Analysis of PEC data CO data and compare direct and indirect Lascar concentration
#Feb 21st, 2018


message("Processing Children Lascar in household ",hhid_selected," in phase ",phase_selected)
#select ECM by hhid and phase (BL1/BL2FU1FU2)
Lascar_selected = Lascar_data2[phase == phase_selected&hhid == hhid_selected,]
Lascar_selected[Lascar_location == "KAP",Lascar_location:= "KAP1"]
Lascar_selected[,datetime2 := floor_date(datetime, unit = "minute")]
#select Beacon by hhid and phase (BL1/BL2FU1/FU2)
Beacon_data_selected = Beacon_data2[replicate == phase_selected & hhid == hhid_selected,]

#change to minute level data, since lascar is 1 minut interval
Beacon_data_selected[,datetime4:= floor_date(datetime3, unit = "minute")]
Beacon_data_selected[,RSSI_minute := mean(RSSI_30s, na.rm = T), by = c("datetime4","monitor_env")]
Beacon_data_selected[,RSSI_minute := round(RSSI_minute, digits = 1)]
Beacon_data_selected = unique(Beacon_data_selected[,c("datetime4","hhid","MAC","monitor_env",
                                                      "replicate","Beacon_logger_ID","start_date",
                                                      "RSSI_minute")])
#change letter size of Beacon data
Beacon_data_selected[, MAC := toupper(MAC)]
#merge Beacon location and Beacon ID
Beacon_data_selected = merge(Beacon_data_selected,r_Beacon_log[record_id ==hhid_selected&h41_visit==as.integer(phase_selected),
                                                               c("Beacon_MAC","Beacon_ID","Beacon_location","Notes")],
                             by.x = "MAC", by.y = "Beacon_MAC", all.x = T, all.y = F )
#firstly, focus on beacon worn by children
Children_Beacon = Beacon_data_selected[Beacon_location == "PEC",]
#check whether matched
if(nrow(Children_Beacon)==0){
      message("No Children Beacon information")
      return()
}
#closest Beacon logger
Children_Beacon[,RSSI_max := max(RSSI_minute), by = c("datetime4","MAC")]
Children_Beacon[RSSI_max != RSSI_minute,RSSI_max := NA]
Children_Beacon[!is.na(RSSI_max) ,closest_logger_ID := Beacon_logger_ID]

Children_Beacon = Children_Beacon[!is.na(RSSI_max),]
#merge with Lascar for indirect CO (there will be some duplicate?)
Children_Beacon = merge(Children_Beacon, Lascar_selected[,c("datetime2","Lascar_id","Lascar_location","CO_ppm")], 
                      by.x = c("datetime4","monitor_env"),
                      by.y = c("datetime2","Lascar_location"), all.x = T, all.y = F)
#creat new variable for display
Children_Beacon[,BeaconID_location := paste(Beacon_ID,Beacon_location)] #direct one
Children_Beacon[,Logger_ID_location := paste(Beacon_logger_ID,monitor_env)] #indirect one

#direct ECM (also may be duplicate ECM)
PEC_CO = Lascar_selected[Lascar_location == "PEC",c("datetime2","Lascar_id","Lascar_location","CO_ppm")]

#merge indirect with direct

Children_exposure = merge(Children_Beacon,PEC_CO,by.x = "datetime4", by.y = "datetime2", all.x = T, all.y = F)  #x is indirect, y is direct
Children_exposure = Children_exposure[!is.na(closest_logger_ID),]

#KAP CO
KAP_CO = Lascar_selected[Lascar_location == "KAP1",c("datetime2","Lascar_id","Lascar_location","CO_ppm")]
colnames(KAP_CO) = c("datetime2","Lascar_id.KAP","Lascar_location.KAP","CO_ppm.KAP")
#duplicate KAP lascar
if(length(unique(KAP_CO$Lascar_id))>1){
      KAP_CO = KAP_CO[Lascar_id.KAP == unique(KAP_CO$Lascar_id.KAP)[KAP_duplicate_Lascar],]
      
}else {
      KAP_CO = KAP_CO
}


#PEO CO
PEO_CO = Lascar_selected[Lascar_location == "PEO",c("datetime2","Lascar_id","Lascar_location","CO_ppm")]
colnames(PEO_CO) = c("datetime2","Lascar_id.PEO","Lascar_location.PEO","CO_ppm.PEO")
#duplicate mother lascar
if(length(unique(PEO_CO$Lascar_id.PEO))>1){
      PEO_CO = PEO_CO[Lascar_id.PEO == unique(PEO_CO$Lascar_id.PEO)[mother_duplicate_Lascar],]
      
}else {
      PEO_CO = PEO_CO
}

Children_exposure2 = merge(Children_exposure, KAP_CO, by.x = "datetime4",by.y = "datetime2", all.x = T, all.y = F)
Children_exposure2 = merge(Children_exposure2, PEO_CO, by.x = "datetime4",by.y = "datetime2", all.x = T, all.y = F)

#select which beacon to analyze (there are two duplicate in the person)
PEC_Beacon_list = unique(Children_Beacon$MAC)
Children_exposure2 = Children_exposure2[MAC == PEC_Beacon_list[duplicate_Beacon],]
#hourly data
Children_exposure2[,hourtime := floor_date(datetime4, unit = "hours") ]
Children_exposure2[,c_h_indirect_CO := mean(CO_ppm.x, na.rm = T), by = hourtime]
Children_exposure2[,c_h_direct_CO := mean(CO_ppm.y, na.rm = T), by = hourtime]
Children_exposure2[,h_KAP_CO := mean(CO_ppm.KAP,na.rm = T), by = hourtime]
Children_exposure2[,h_PEO_CO := mean(CO_ppm.PEO,na.rm = T), by = hourtime]

c_hour_data_CO = unique(Children_exposure2[,c("hhid","replicate","hourtime","c_h_indirect_CO","c_h_direct_CO", "h_KAP_CO","h_PEO_CO")])
#correlation of CO between mother, 
c_correlation_d_i = cor(c_hour_data_CO$c_h_direct_CO,c_hour_data_CO$c_h_indirect_CO, use = "na.or.complete", method = "spearman")
c_correlation_d_k = cor(c_hour_data_CO$c_h_direct_CO,c_hour_data_CO$h_KAP_CO, use = "na.or.complete", method = "spearman")
c_correlation_d_m = cor(c_hour_data_CO$c_h_direct_CO,c_hour_data_CO$h_PEO_CO, use = "na.or.complete", method = "spearman")

c_hour_data_CO[,c_correlation_d_i :=c_correlation_d_i]
c_hour_data_CO[,c_correlation_d_k :=c_correlation_d_k]
c_hour_data_CO[,c_correlation_d_m :=c_correlation_d_m]
c_hour_data_CO[,phase := phase_selected];c_hour_data_CO[,hhid := hhid_selected]

#row bind
c_hour_data_CO_2 = rbind(c_hour_data_CO_2, c_hour_data_CO)








