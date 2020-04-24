install.packages("devtools")
install.packages("Rtools")
install.packages("curl")
install.packages("Rcpp")
install.packages("backports")
install.packages("digest")
devtools::install_github("rdinter/usdarnass", force=T)
nass_set_key("9E58000D-72D6-33DE-8040-1C7AD76030FA")
Sys.getenv("NASS_KEY")
#list of counties that overlap with AOP sites
counties<-read.csv("C:\\Users\\kjone\\Dropbox\\Backup\\Other jobs\\SESYNC\\NEON\\NASS\\countylist.csv")
counties$County<-as.character(counties$County)
counties$State<-as.character(counties$State)
###gather and clean data
##animal products
#get NASS data
animals<-nass_data(source_desc = "CENSUS", 
             sector_desc = "ANIMALS & PRODUCTS",
             group_desc = "ANIMAL TOTALS",
             commodity_desc = "ANIMAL TOTALS",
             short_desc = "ANIMAL TOTALS, INCL PRODUCTS - OPERATIONS WITH SALES",
             domain_desc = "TOTAL",
             agg_level_desc = "COUNTY",
             statisticcat_desc = "SALES",
             state_name = c("Alaska", "Alabama", "Arizona", "Arizona","California","Colorado",
                            "Florida","Georgia","Hawaii","Kansas","Massachusetts","Maryland",
                            "Michigan","North Carolina","North Dakota","New Hampshire",
                            "New Mexico","Oklahoma","Oregon","Puerto Rico","Tennessee",
                            "Texas","Utah","Virginia","Washington","Wisconsin","West Virginia",
                            "Wyoming"),
             county_name = c("Denali","Dillingham","North Slope","Southeast Fairbanks","Yukon-Koyukuk",
                             "Blount","Choctaw","Clarke","Greene","Hale","Marengo","Perry","Sumter",
                             "Pima","Santa Cruz","Fresno","Madera","Boulder","Grand","Larimer","Logan",
                             "Washington","Weld","Yuma","Alachua","Bradford","Clay","Osceola",
                             "Polk","Putnam","Baker","Miller","Mitchell","Hawaii","Douglas","Geary",
                             "Jefferson","Leavenworth","Pottawatomie","Riley","Wabaunsee","Franklin",
                             "Worcester","Anne Arundel","Gogebic","Haywood","Swain","Morton","Stutsman",
                             "Carroll","Dona Ana","Johnston","Washita","Linn","Cabo Rojo","Guanica",
                             "Guayanilla","Hormigueros","Lajas","Sabana Grande","San German","Yauco",
                             "Anderson","Cocke","Knox","Loudon","Roane","Sevier","Wise","Salt Lake",
                             "San Juan","Tooele","Clarke","Craig","Fauquier","Frederick","Giles",
                             "Rappahannock","Warren","Clark","Skamania","Landglade","Lincoln","Oneida",
                             "Price","Vilas","Monroe","Park"),
             year=2017)
#merge to remove extra counties
animals<-merge(animals, counties, by.x = c("county_name","state_name"), by.y = c("County","State"))
#remove extra variables
animals<-select(animals, County=county_name, State=state_name, County.fips=county_code, Unit=unit_desc, Description=short_desc, State.fips=state_fips_code, Operations.animal.sales=Value)
#rename variables
animals<-rename(animals, Animal_Sales_Num_Ops=Operations.animal.sales)
#write metadata file
write.csv(animals,"animalsmedata.csv")
#create merge file
animalsmerge<-select(animals, County, State, Animal_Sales_Num_Ops)

##field crops - corn
#get NASS data
corn<-nass_data(source_desc = "CENSUS", 
                   sector_desc = "Crops",
                   group_desc = "Field Crops",
                   commodity_desc = "Corn",
                   short_desc = "CORN, GRAIN - ACRES HARVESTED",
                   domain_desc = "TOTAL",
                   agg_level_desc = "COUNTY",
                   statisticcat_desc = "Area Harvested",
                   state_name = c("Alaska", "Alabama", "Arizona", "Arizona","California","Colorado",
                                  "Florida","Georgia","Hawaii","Kansas","Massachusetts","Maryland",
                                  "Michigan","North Carolina","North Dakota","New Hampshire",
                                  "New Mexico","Oklahoma","Oregon","Puerto Rico","Tennessee",
                                  "Texas","Utah","Virginia","Washington","Wisconsin","West Virginia",
                                  "Wyoming"),
                   county_name = c("Denali","Dillingham","North Slope","Southeast Fairbanks","Yukon-Koyukuk",
                                   "Blount","Choctaw","Clarke","Greene","Hale","Marengo","Perry","Sumter",
                                   "Pima","Santa Cruz","Fresno","Madera","Boulder","Grand","Larimer","Logan",
                                   "Washington","Weld","Yuma","Alachua","Bradford","Clay","Osceola",
                                   "Polk","Putnam","Baker","Miller","Mitchell","Hawaii","Douglas","Geary",
                                   "Jefferson","Leavenworth","Pottawatomie","Riley","Wabaunsee","Franklin",
                                   "Worcester","Anne Arundel","Gogebic","Haywood","Swain","Morton","Stutsman",
                                   "Carroll","Dona Ana","Johnston","Washita","Linn","Cabo Rojo","Guanica",
                                   "Guayanilla","Hormigueros","Lajas","Sabana Grande","San German","Yauco",
                                   "Anderson","Cocke","Knox","Loudon","Roane","Sevier","Wise","Salt Lake",
                                   "San Juan","Tooele","Clarke","Craig","Fauquier","Frederick","Giles",
                                   "Rappahannock","Warren","Clark","Skamania","Landglade","Lincoln","Oneida",
                                   "Price","Vilas","Monroe","Park"),
                   year=2017)
#merge to remove extra counties
corn<-merge(corn, counties, by.x = c("county_name","state_name"), by.y = c("County","State"))
#remove extra variables
corn<-select(corn, County=county_name, State=state_name, County.fips=county_code, Unit=unit_desc, Description=short_desc, State.fips=state_fips_code, Corn.acres.harvested=Value)
#rename variables
corn<-rename(corn, Corn_Acres_Harvested=Corn.acres.harvested)
#write metadata
write.csv(corn, "cornmetadata.csv")
#create merge file
cornmerge<-select(corn, County, State, Corn_Acres_Harvested)

##tomatoes
#get NASS data
tomatoes<-nass_data(source_desc = "CENSUS", 
                      sector_desc = "Crops",
                      group_desc = "Vegetables",
                      commodity_desc = "Tomatoes",
                      short_desc = "TOMATOES, IN THE OPEN - ACRES HARVESTED",
                      domain_desc = "TOTAL",
                      agg_level_desc = "COUNTY",
                      statisticcat_desc = "Area Harvested",
                      state_name = c("Alaska", "Alabama", "Arizona", "Arizona","California","Colorado",
                                     "Florida","Georgia","Hawaii","Kansas","Massachusetts","Maryland",
                                     "Michigan","North Carolina","North Dakota","New Hampshire",
                                     "New Mexico","Oklahoma","Oregon","Puerto Rico","Tennessee",
                                     "Texas","Utah","Virginia","Washington","Wisconsin","West Virginia",
                                     "Wyoming"),
                      county_name = c("Denali","Dillingham","North Slope","Southeast Fairbanks","Yukon-Koyukuk",
                                      "Blount","Choctaw","Clarke","Greene","Hale","Marengo","Perry","Sumter",
                                      "Pima","Santa Cruz","Fresno","Madera","Boulder","Grand","Larimer","Logan",
                                      "Washington","Weld","Yuma","Alachua","Bradford","Clay","Osceola",
                                      "Polk","Putnam","Baker","Miller","Mitchell","Hawaii","Douglas","Geary",
                                      "Jefferson","Leavenworth","Pottawatomie","Riley","Wabaunsee","Franklin",
                                      "Worcester","Anne Arundel","Gogebic","Haywood","Swain","Morton","Stutsman",
                                      "Carroll","Dona Ana","Johnston","Washita","Linn","Cabo Rojo","Guanica",
                                      "Guayanilla","Hormigueros","Lajas","Sabana Grande","San German","Yauco",
                                      "Anderson","Cocke","Knox","Loudon","Roane","Sevier","Wise","Salt Lake",
                                      "San Juan","Tooele","Clarke","Craig","Fauquier","Frederick","Giles",
                                      "Rappahannock","Warren","Clark","Skamania","Landglade","Lincoln","Oneida",
                                      "Price","Vilas","Monroe","Park"),
                      year=2017)
#merge to remove extra counties
tomatoes<-merge(tomatoes, counties, by.x = c("county_name","state_name"), by.y = c("County","State"))
#remove extra variables
tomatoes<-select(tomatoes, County=county_name, State=state_name, County.fips=county_code, Unit=unit_desc, Description=short_desc, State.fips=state_fips_code, Tomato.acres.harvested=Value)
#rename variables
tomatoes<-rename(tomatoes, Tomato_Acres_Harvested=Tomato.acres.harvested)
#write metadata
write.csv(tomatoes, "tomatoesmetadata.csv")
#create merge file
tomatoesmerge<-select(tomatoes, County, State, Tomato_Acres_Harvested)

##fertilizer
#get NASS data
fertilizer<-nass_data(source_desc = "CENSUS",
                    sector_desc = "ENVIRONMENTAL",
                    group_desc = "FARMS & LAND & ASSETS",
                    commodity_desc = "AG LAND",
                    short_desc = "AG LAND - TREATED, MEASURED IN ACRES",
                    domain_desc = "Fertilizer",
                    agg_level_desc = "COUNTY",
                    statisticcat_desc = "Treated",
                    state_name = c("Alaska", "Alabama", "Arizona", "Arizona","California","Colorado",
                                   "Florida","Georgia","Hawaii","Kansas","Massachusetts","Maryland",
                                   "Michigan","North Carolina","North Dakota","New Hampshire",
                                   "New Mexico","Oklahoma","Oregon","Puerto Rico","Tennessee",
                                   "Texas","Utah","Virginia","Washington","Wisconsin","West Virginia",
                                   "Wyoming"),
                    county_name = c("Denali","Dillingham","North Slope","Southeast Fairbanks","Yukon-Koyukuk",
                                    "Blount","Choctaw","Clarke","Greene","Hale","Marengo","Perry","Sumter",
                                    "Pima","Santa Cruz","Fresno","Madera","Boulder","Grand","Larimer","Logan",
                                    "Washington","Weld","Yuma","Alachua","Bradford","Clay","Osceola",
                                    "Polk","Putnam","Baker","Miller","Mitchell","Hawaii","Douglas","Geary",
                                    "Jefferson","Leavenworth","Pottawatomie","Riley","Wabaunsee","Franklin",
                                    "Worcester","Anne Arundel","Gogebic","Haywood","Swain","Morton","Stutsman",
                                    "Carroll","Dona Ana","Johnston","Washita","Linn","Cabo Rojo","Guanica",
                                    "Guayanilla","Hormigueros","Lajas","Sabana Grande","San German","Yauco",
                                    "Anderson","Cocke","Knox","Loudon","Roane","Sevier","Wise","Salt Lake",
                                    "San Juan","Tooele","Clarke","Craig","Fauquier","Frederick","Giles",
                                    "Rappahannock","Warren","Clark","Skamania","Landglade","Lincoln","Oneida",
                                    "Price","Vilas","Monroe","Park"),
                    year=2017)
#merge to remove extra counties
fertilizer<-merge(fertilizer, counties, by.x = c("county_name","state_name"), by.y = c("County","State"))
#remove extra variables and make long data wide
fertilizer<-select(fertilizer, County=county_name, State=state_name, County.fips=county_code, Unit=unit_desc, Description=short_desc, State.fips=state_fips_code, Acres.Treated=Value, Fert.type=domaincat_desc)
fertilizer<-fertilizer %>% spread (key = Fert.type, value= Acres.Treated) 
#rename variables
fertilizer<-rename(fertilizer, Acres_Treated_Manure=`FERTILIZER: (MANURE)`, Acres_Treated_Organic_Fert=`FERTILIZER: (ORGANIC)`, Acres_Treated_Fert_Total=`FERTILIZER: (TOTAL)`)
#write metadata
write.csv(fertilizer, "fertilizermetadata.csv")
#create merge file
fertilizermerge<-select(fertilizer, County, State, Acres_Treated_Manure, Acres_Treated_Organic_Fert, Acres_Treated_Fert_Total)

##cropland
#get NASS data
cropland<-nass_data(source_desc = "CENSUS",
                      sector_desc = "Demographics",
                      group_desc = "FARMS & LAND & ASSETS",
                      commodity_desc = "AG LAND",
                      short_desc = "AG LAND, CROPLAND - AREA, MEASURED IN PCT OF AG LAND",
                      domain_desc = "Producers",
                      agg_level_desc = "COUNTY",
                      statisticcat_desc = "Area",
                      state_name = c("Alaska", "Alabama", "Arizona", "Arizona","California","Colorado",
                                     "Florida","Georgia","Hawaii","Kansas","Massachusetts","Maryland",
                                     "Michigan","North Carolina","North Dakota","New Hampshire",
                                     "New Mexico","Oklahoma","Oregon","Puerto Rico","Tennessee",
                                     "Texas","Utah","Virginia","Washington","Wisconsin","West Virginia",
                                     "Wyoming"),
                      county_name = c("Denali","Dillingham","North Slope","Southeast Fairbanks","Yukon-Koyukuk",
                                      "Blount","Choctaw","Clarke","Greene","Hale","Marengo","Perry","Sumter",
                                      "Pima","Santa Cruz","Fresno","Madera","Boulder","Grand","Larimer","Logan",
                                      "Washington","Weld","Yuma","Alachua","Bradford","Clay","Osceola",
                                      "Polk","Putnam","Baker","Miller","Mitchell","Hawaii","Douglas","Geary",
                                      "Jefferson","Leavenworth","Pottawatomie","Riley","Wabaunsee","Franklin",
                                      "Worcester","Anne Arundel","Gogebic","Haywood","Swain","Morton","Stutsman",
                                      "Carroll","Dona Ana","Johnston","Washita","Linn","Cabo Rojo","Guanica",
                                      "Guayanilla","Hormigueros","Lajas","Sabana Grande","San German","Yauco",
                                      "Anderson","Cocke","Knox","Loudon","Roane","Sevier","Wise","Salt Lake",
                                      "San Juan","Tooele","Clarke","Craig","Fauquier","Frederick","Giles",
                                      "Rappahannock","Warren","Clark","Skamania","Landglade","Lincoln","Oneida",
                                      "Price","Vilas","Monroe","Park"),
                      year=2017)
#merge to remove extra counties
cropland<-merge(cropland, counties, by.x = c("county_name","state_name"), by.y = c("County","State"))
#remove extra variables and make long data wide
cropland<-select(cropland, County=county_name, State=state_name, County.fips=county_code, Unit=unit_desc, Description=short_desc, State.fips=state_fips_code,Cropland.Percent=Value, Demographic=domaincat_desc)
cropland<-cropland %>% spread (key = Demographic, value= Cropland.Percent) 
#rename variables
cropland$`PRODUCERS: (GENDER = FEMALE)`
cropland<-rename(cropland, Percent_Ag_Land_In_Crop_Woman_Owned=`PRODUCERS: (GENDER = FEMALE)`)
#write metadata
write.csv(cropland, "croplandmetadata.csv")
#create merge file
croplandmerge<-select(cropland, County, State, Percent_Ag_Land_In_Crop_Woman_Owned)

##govt programs
#get NASS data
govtprograms<-nass_data(source_desc = "CENSUS",
                    sector_desc = "Economics",
                    group_desc = "Income",
                    commodity_desc = "Govt Programs",
                    short_desc = "GOVT PROGRAMS, FEDERAL - RECEIPTS, MEASURED IN $ / OPERATION",
                    agg_level_desc = "COUNTY",
                    domain_desc = "Total",
                    statisticcat_desc = "Receipts",
                    state_name = c("Alaska", "Alabama", "Arizona", "Arizona","California","Colorado",
                                   "Florida","Georgia","Hawaii","Kansas","Massachusetts","Maryland",
                                   "Michigan","North Carolina","North Dakota","New Hampshire",
                                   "New Mexico","Oklahoma","Oregon","Puerto Rico","Tennessee",
                                   "Texas","Utah","Virginia","Washington","Wisconsin","West Virginia",
                                   "Wyoming"),
                    county_name = c("Denali","Dillingham","North Slope","Southeast Fairbanks","Yukon-Koyukuk",
                                    "Blount","Choctaw","Clarke","Greene","Hale","Marengo","Perry","Sumter",
                                    "Pima","Santa Cruz","Fresno","Madera","Boulder","Grand","Larimer","Logan",
                                    "Washington","Weld","Yuma","Alachua","Bradford","Clay","Osceola",
                                    "Polk","Putnam","Baker","Miller","Mitchell","Hawaii","Douglas","Geary",
                                    "Jefferson","Leavenworth","Pottawatomie","Riley","Wabaunsee","Franklin",
                                    "Worcester","Anne Arundel","Gogebic","Haywood","Swain","Morton","Stutsman",
                                    "Carroll","Dona Ana","Johnston","Washita","Linn","Cabo Rojo","Guanica",
                                    "Guayanilla","Hormigueros","Lajas","Sabana Grande","San German","Yauco",
                                    "Anderson","Cocke","Knox","Loudon","Roane","Sevier","Wise","Salt Lake",
                                    "San Juan","Tooele","Clarke","Craig","Fauquier","Frederick","Giles",
                                    "Rappahannock","Warren","Clark","Skamania","Landglade","Lincoln","Oneida",
                                    "Price","Vilas","Monroe","Park"),
                    year=2017)
#merge to remove extra counties
govtprograms<-merge(govtprograms, counties, by.x = c("county_name","state_name"), by.y = c("County","State"))
#remove extra variables and make long data wide
govtprograms<-select(govtprograms, County=county_name, State=state_name, County.fips=county_code, Unit=unit_desc, Description=short_desc, State.fips=state_fips_code, Govt.dollars.per.acre=Value)
#rename variables
govtprograms<-rename(govtprograms, Govt_Dollars_Per_Operation=Govt.dollars.per.acre)
#write metadata
write.csv(govtprograms, "govtprogramsmetadata.csv")
#create merge file
govtprogramsmerge<-select(govtprograms, County, State, Govt_Dollars_Per_Operation)

##income
#get NASS data
income<-nass_data(source_desc = "CENSUS",
                        sector_desc = "Economics",
                        group_desc = "Income",
                        commodity_desc = "INCOME, NET CASH FARM",
                        short_desc = "INCOME, NET CASH FARM, OF OPERATIONS - NET INCOME, MEASURED IN $ / OPERATION",
                        domain_desc = "Total",
                        agg_level_desc = "COUNTY",
                        statisticcat_desc = "Net income",
                        state_name = c("Alaska", "Alabama", "Arizona", "Arizona","California","Colorado",
                                       "Florida","Georgia","Hawaii","Kansas","Massachusetts","Maryland",
                                       "Michigan","North Carolina","North Dakota","New Hampshire",
                                       "New Mexico","Oklahoma","Oregon","Puerto Rico","Tennessee",
                                       "Texas","Utah","Virginia","Washington","Wisconsin","West Virginia",
                                       "Wyoming"),
                        county_name = c("Denali","Dillingham","North Slope","Southeast Fairbanks","Yukon-Koyukuk",
                                        "Blount","Choctaw","Clarke","Greene","Hale","Marengo","Perry","Sumter",
                                        "Pima","Santa Cruz","Fresno","Madera","Boulder","Grand","Larimer","Logan",
                                        "Washington","Weld","Yuma","Alachua","Bradford","Clay","Osceola",
                                        "Polk","Putnam","Baker","Miller","Mitchell","Hawaii","Douglas","Geary",
                                        "Jefferson","Leavenworth","Pottawatomie","Riley","Wabaunsee","Franklin",
                                        "Worcester","Anne Arundel","Gogebic","Haywood","Swain","Morton","Stutsman",
                                        "Carroll","Dona Ana","Johnston","Washita","Linn","Cabo Rojo","Guanica",
                                        "Guayanilla","Hormigueros","Lajas","Sabana Grande","San German","Yauco",
                                        "Anderson","Cocke","Knox","Loudon","Roane","Sevier","Wise","Salt Lake",
                                        "San Juan","Tooele","Clarke","Craig","Fauquier","Frederick","Giles",
                                        "Rappahannock","Warren","Clark","Skamania","Landglade","Lincoln","Oneida",
                                        "Price","Vilas","Monroe","Park"),
                        year=2017)
#merge to remove extra counties
income<-merge(income, counties, by.x = c("county_name","state_name"), by.y = c("County","State"))
#remove extra variables and make long data wide
income<-select(income, County=county_name, State=state_name, County.fips=county_code, Unit=unit_desc, Description=short_desc, State.fips=state_fips_code, Net.income.per.operation=Value)
#rename variables
income<-rename(income, Net_Income_Per_Operation=Net.income.per.operation)
#write metadata
write.csv(income, "incomemetadata.csv")
#create merge file
incomemerge<-select(income, County, State, Net_Income_Per_Operation)
##age
#get NASS data
age<-nass_data(source_desc = "CENSUS",
                  sector_desc = "Demographics",
                  group_desc = "Producers",
                  commodity_desc = "Producers",
                  short_desc = "PRODUCERS - AGE, AVG, MEASURED IN YEARS",
                  domain_desc = "Total",
                  agg_level_desc = "COUNTY",
                  statisticcat_desc = "Age, Avg",
                  state_name = c("Alaska", "Alabama", "Arizona", "Arizona","California","Colorado",
                                 "Florida","Georgia","Hawaii","Kansas","Massachusetts","Maryland",
                                 "Michigan","North Carolina","North Dakota","New Hampshire",
                                 "New Mexico","Oklahoma","Oregon","Puerto Rico","Tennessee",
                                 "Texas","Utah","Virginia","Washington","Wisconsin","West Virginia",
                                 "Wyoming"),
                  county_name = c("Denali","Dillingham","North Slope","Southeast Fairbanks","Yukon-Koyukuk",
                                  "Blount","Choctaw","Clarke","Greene","Hale","Marengo","Perry","Sumter",
                                  "Pima","Santa Cruz","Fresno","Madera","Boulder","Grand","Larimer","Logan",
                                  "Washington","Weld","Yuma","Alachua","Bradford","Clay","Osceola",
                                  "Polk","Putnam","Baker","Miller","Mitchell","Hawaii","Douglas","Geary",
                                  "Jefferson","Leavenworth","Pottawatomie","Riley","Wabaunsee","Franklin",
                                  "Worcester","Anne Arundel","Gogebic","Haywood","Swain","Morton","Stutsman",
                                  "Carroll","Dona Ana","Johnston","Washita","Linn","Cabo Rojo","Guanica",
                                  "Guayanilla","Hormigueros","Lajas","Sabana Grande","San German","Yauco",
                                  "Anderson","Cocke","Knox","Loudon","Roane","Sevier","Wise","Salt Lake",
                                  "San Juan","Tooele","Clarke","Craig","Fauquier","Frederick","Giles",
                                  "Rappahannock","Warren","Clark","Skamania","Landglade","Lincoln","Oneida",
                                  "Price","Vilas","Monroe","Park"),
                  year=2017)
#merge to remove extra counties
age<-merge(age, counties, by.x = c("county_name","state_name"), by.y = c("County","State"))
#remove extra variables
age<-select(age, County=county_name, State=state_name, County.fips=county_code, Unit=unit_desc, Description=short_desc, State.fips=state_fips_code, Avg.age=Value)
#rename variables
age<-rename(age, Avg_Age_Operator=Avg.age)
write.csv(age, "agemetadata.csv")
#create merge file
agemerge<-select(age, County, State, Avg_Age_Operator)

##gender
#get NASS data
gender<-nass_data(source_desc = "CENSUS",
                  sector_desc = "Demographics",
                  group_desc = "Producers",
                  commodity_desc = "Producers",
                  short_desc = "PRODUCERS, FEMALE - AREA OPERATED, MEASURED IN ACRES / OPERATION",
                  domain_desc = "Total",
                  agg_level_desc = "COUNTY",
                  statisticcat_desc = "Area operated",
                  state_name = c("Alaska", "Alabama", "Arizona", "Arizona","California","Colorado",
                                 "Florida","Georgia","Hawaii","Kansas","Massachusetts","Maryland",
                                 "Michigan","North Carolina","North Dakota","New Hampshire",
                                 "New Mexico","Oklahoma","Oregon","Puerto Rico","Tennessee",
                                 "Texas","Utah","Virginia","Washington","Wisconsin","West Virginia",
                                 "Wyoming"),
                  county_name = c("Denali","Dillingham","North Slope","Southeast Fairbanks","Yukon-Koyukuk",
                                  "Blount","Choctaw","Clarke","Greene","Hale","Marengo","Perry","Sumter",
                                  "Pima","Santa Cruz","Fresno","Madera","Boulder","Grand","Larimer","Logan",
                                  "Washington","Weld","Yuma","Alachua","Bradford","Clay","Osceola",
                                  "Polk","Putnam","Baker","Miller","Mitchell","Hawaii","Douglas","Geary",
                                  "Jefferson","Leavenworth","Pottawatomie","Riley","Wabaunsee","Franklin",
                                  "Worcester","Anne Arundel","Gogebic","Haywood","Swain","Morton","Stutsman",
                                  "Carroll","Dona Ana","Johnston","Washita","Linn","Cabo Rojo","Guanica",
                                  "Guayanilla","Hormigueros","Lajas","Sabana Grande","San German","Yauco",
                                  "Anderson","Cocke","Knox","Loudon","Roane","Sevier","Wise","Salt Lake",
                                  "San Juan","Tooele","Clarke","Craig","Fauquier","Frederick","Giles",
                                  "Rappahannock","Warren","Clark","Skamania","Landglade","Lincoln","Oneida",
                                  "Price","Vilas","Monroe","Park"),
                  year=2017)
#merge to remove extra counties
gender<-merge(gender, counties, by.x = c("county_name","state_name"), by.y = c("County","State"))
#remove extra variables
gender<-select(gender, County=county_name, State=state_name, County.fips=county_code, Unit=unit_desc, Description=short_desc, State.fips=state_fips_code, Female.acres.per.operation=Value)
#rename variables
gender<-rename(gender, Acres_Per_WomanOwned_Operation=Female.acres.per.operation)
#write metadata
write.csv(gender,"gendermetadata.csv")
#create merge file
gendermerge<-select(gender, County, State, Acres_Per_WomanOwned_Operation)

##expenses
#get NASS data
expenses<-nass_data(source_desc = "CENSUS",
                    sector_desc = "Economics",
                    group_desc = "Expenses",
                    commodity_desc = "Expense totals",
                    short_desc = "EXPENSE TOTALS, OPERATING - EXPENSE, MEASURED IN $ / OPERATION",
                    domain_desc = "Total",
                    agg_level_desc = "COUNTY",
                    statisticcat_desc = "Expense",
                    state_name = c("Alaska", "Alabama", "Arizona", "Arizona","California","Colorado",
                                   "Florida","Georgia","Hawaii","Kansas","Massachusetts","Maryland",
                                   "Michigan","North Carolina","North Dakota","New Hampshire",
                                   "New Mexico","Oklahoma","Oregon","Puerto Rico","Tennessee",
                                   "Texas","Utah","Virginia","Washington","Wisconsin","West Virginia",
                                   "Wyoming"),
                    county_name = c("Denali","Dillingham","North Slope","Southeast Fairbanks","Yukon-Koyukuk",
                                    "Blount","Choctaw","Clarke","Greene","Hale","Marengo","Perry","Sumter",
                                    "Pima","Santa Cruz","Fresno","Madera","Boulder","Grand","Larimer","Logan",
                                    "Washington","Weld","Yuma","Alachua","Bradford","Clay","Osceola",
                                    "Polk","Putnam","Baker","Miller","Mitchell","Hawaii","Douglas","Geary",
                                    "Jefferson","Leavenworth","Pottawatomie","Riley","Wabaunsee","Franklin",
                                    "Worcester","Anne Arundel","Gogebic","Haywood","Swain","Morton","Stutsman",
                                    "Carroll","Dona Ana","Johnston","Washita","Linn","Cabo Rojo","Guanica",
                                    "Guayanilla","Hormigueros","Lajas","Sabana Grande","San German","Yauco",
                                    "Anderson","Cocke","Knox","Loudon","Roane","Sevier","Wise","Salt Lake",
                                    "San Juan","Tooele","Clarke","Craig","Fauquier","Frederick","Giles",
                                    "Rappahannock","Warren","Clark","Skamania","Landglade","Lincoln","Oneida",
                                    "Price","Vilas","Monroe","Park"),
                    year=2017)
#merge to remove extra counties
expenses<-merge(expenses, counties, by.x = c("county_name","state_name"), by.y = c("County","State"))
#remove extra variables
expenses<-select(expenses, County=county_name, State=state_name, County.fips=county_code, Unit=unit_desc, Description=short_desc, State.fips=state_fips_code, Expenses.per.operation=Value)
#rename variables
expenses<-rename(expenses, Expenses_Per_Operation=Expenses.per.operation)
#write metadata
write.csv(expenses, "expensesmetadata.csv")
#create merge file
expensesmerge<-select(expenses, County, State, Expenses_Per_Operation)

###create merged dataset
nassdata<-merge(animalsmerge, cornmerge, by=c("County","State"), all=T)
nassdata<-merge(nassdata, croplandmerge, by=c("County","State"), all=T)
nassdata<-merge(nassdata, expensesmerge, by=c("County","State"), all=T)
nassdata<-merge(nassdata, fertilizermerge, by=c("County","State"), all=T)
nassdata<-merge(nassdata, gendermerge, by=c("County","State"), all=T)
nassdata<-merge(nassdata, govtprogramsmerge, by=c("County","State"), all=T)
nassdata<-merge(nassdata, incomemerge, by=c("County","State"), all=T)
nassdata<-merge(nassdata, tomatoesmerge, by=c("County","State"), all=T)
nassdata<-merge(nassdata, agemerge, by=c("County","State"), all=T)
#add site IDs
nassdata<-merge(nassdata, counties, by=c("County","State"), all=T)
nassdata$Site<-as.character(nassdata$Site)
#change data to numeric
nassdata$Animal_Sales_Num_Ops<-as.numeric(gsub(",", "", nassdata$Animal_Sales_Num_Ops))
nassdata$Corn_Acres_Harvested<-as.numeric(gsub(",", "", nassdata$Corn_Acres_Harvested))
nassdata$Percent_Ag_Land_In_Crop_Woman_Owned<-as.numeric(gsub(",", "", nassdata$Percent_Ag_Land_In_Crop_Woman_Owned))
nassdata$Expenses_Per_Operation<-as.numeric(gsub(",", "", nassdata$Expenses_Per_Operation))
nassdata$Acres_Treated_Manure<-as.numeric(gsub(",", "", nassdata$Acres_Treated_Manure))
nassdata$Acres_Treated_Organic_Fert<-as.numeric(gsub(",", "", nassdata$Acres_Treated_Organic_Fert))
nassdata$Acres_Treated_Fert_Total<-as.numeric(gsub(",", "", nassdata$Acres_Treated_Fert_Total))
nassdata$Acres_Per_WomanOwned_Operation<-as.numeric(gsub(",", "", nassdata$Acres_Per_WomanOwned_Operation))
nassdata$Govt_Dollars_Per_Operation<-as.numeric(gsub(",", "", nassdata$Govt_Dollars_Per_Operation))
nassdata$Net_Income_Per_Operation<-as.numeric(gsub(",", "", nassdata$Net_Income_Per_Operation))
nassdata$Tomato_Acres_Harvested<-as.numeric(gsub(",", "", nassdata$Tomato_Acres_Harvested))
nassdata$Avg_Age_Operator<-as.numeric(gsub(",", "", nassdata$Avg_Age_Operator))
#save
write.csv(nassdata,"nassdataclean.csv")


