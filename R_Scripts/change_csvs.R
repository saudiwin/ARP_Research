# need to change encoding of text to be UTF-8 for the web

write.csv(model_data[[1]],file="oned_test.csv",fileEncoding = "UTF-8",row.names = FALSE)
write.csv(model_data[[2]],file="twod_test.csv",fileEncoding = "UTF-8",row.names = FALSE)

oned <- read.csv("oned_test.csv")
twod <- read.csv("twod_test.csv")

oned$Party <- as.character(oned$Party)
twod$Party <- as.character(twod$Party)
oned$Party[grepl("Social",x = oned$Party)] <- "Social-Dem"
twod$Party[grepl("Social",x = twod$Party)] <- "Social-Dem"
twod$Party[is.na(twod$Party)] <- "Independent"
Encoding(oned$Party) <- "UTF-8"
Encoding(twod$Party) <- "UTF-8"
oned$Nawab <- as.character(oned$Nawab)
twod$Nawab <- as.character(twod$Nawab)
Encoding(oned$Nawab) <- "UTF-8"
Encoding(twod$Nawab) <- "UTF-8"
names(oned) <- c("Nawab","Party","5%","50%","95%","Mean")
names(twod) <- c("Nawab","Party","One_5%","One_50%","One_95%","One_Mean",
                 "Two_5%","Two_50%","Two_95%","Two_Mean")

saveRDS(oned,file="data/oned_revised.rds")
saveRDS(twod,file="data/twod_revised.rds")
