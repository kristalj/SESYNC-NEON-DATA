acs = read.csv("/research-home/aelmore/NEON/NEON-AOP-ACS.csv")

data_dir <- "/nfs/public-data/NEON_workshop_data/NEON"
ACScodes <-  readr::read_csv(file.path(data_dir, "NEON-AOP-ACSdatasets.csv"), col_types = "cc")
ACScodes$acsidx = seq(44)+9

# Simple Pie Chart
slices <- c(10, 12,4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")
pie(slices, labels = lbls, main="Pie Chart of Countries")

#par(mfrow=(c(1,1)))



par(mar = c(0,0,0,0))
lbls <- c("White","African American","Asian","Other","Native American")
pie(c(sum(acs[,11]),sum(acs[,12]),sum(acs[,14]),sum(acs[,c(15,16,17,18,19)]),sum(acs[,13])),labels = lbls, cex = 2)

pie(c(sum(acs[,11]),sum(acs[,12]),sum(acs[,14]),sum(acs[,c(15,16,17,18,19)]),sum(acs[,13])),labels = "",col =c("beige","cadetblue1","chartreuse1","chocolate1","deeppink"))

lbls <- c("48","49","51","52","53","50")
pie(c(sum(acs[,48]),sum(acs[,49]),sum(acs[,51]),sum(acs[,52]),sum(acs[,53]),sum(acs[,50])),labels = lbls,col =c("beige","cadetblue1","chartreuse1","chocolate1","deeppink","darkseagreen"))
pie(c(sum(acs[,48]),sum(acs[,49]),sum(acs[,51]),sum(acs[,52]),sum(acs[,53]),sum(acs[,50])),labels = "",col =c("beige","cadetblue1","chartreuse1","chocolate1","deeppink","darkseagreen"))

walked = sum(acs[,51])/sum(sum(acs[,48]),sum(acs[,49]),sum(acs[,51]),sum(acs[,52]),sum(acs[,53]),sum(acs[,50]))
carpooled = sum(acs[,49])/sum(sum(acs[,48]),sum(acs[,49]),sum(acs[,51]),sum(acs[,52]),sum(acs[,53]),sum(acs[,50]))
drove = sum(acs[,48])/sum(sum(acs[,48]),sum(acs[,49]),sum(acs[,51]),sum(acs[,52]),sum(acs[,53]),sum(acs[,50]))
