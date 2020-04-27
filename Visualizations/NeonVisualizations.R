###########################################################################
# Visualize some of the census and land-use data across AOP sites
###########################################################################


acs = read.csv("/research-home/aelmore/NEON/NEON-AOP-ACS.csv")

ACScodes <-  readr::read_csv(file.path(data_dir, "NEON-AOP-ACSdatasets.csv"), col_types = "cc")
ACScodes$acsidx = seq(44)+9 #helps remember the column number for each dataset

#total number of people
paste(sum(acs[,10]), "people live in census tracts that overlap AOP footprints.")
paste("That is ",round((sum(acs[,10])/320000000)*100,2), "percent of the US population")

hist(acs[,10], xlab = ACScodes$description[10-9], main = "")

paste(sum(acs[,12]), "African Americans live in census tracts that overlap AOP footprints.")
hist(acs[,12]/acs[,10], xlab = ACScodes$description[12-9], main = "")
table(acs[,12]/acs[,10] == 0)
table(acs[,12]/acs[,10] <.01 ) #91 out of 221 tracts have less than 1% black

paste(sum(acs[,13]), "Native Americans live in census tracts that overlap AOP footprints.")
hist(acs[,13]/acs[,10], xlab = ACScodes$description[13-9], main = "")
table(acs[,13]/acs[,10] == 0)
table(acs[,13]/acs[,10] <.01 ) #

hist(acs[,27]/acs[,23], xlab = ACScodes$description[27-9], main = "")

cs = colSums(acs[,30:43])
cs = cs/cs[1]

par(mfrow = c(1,1),mar = c(3,35,1,1))
barplot(cs[2:14]*100,names.arg = ACScodes$description[(31:43)-9], horiz = TRUE, las = 2)#ACScodes$description[(31:43)-9]
