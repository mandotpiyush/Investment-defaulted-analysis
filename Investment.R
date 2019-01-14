library(tidyr)
library(dplyr)
library(sqldf)
library(stringr)
library(countrycode)

#Importing companies.csv file
companies <- read.table("companies.txt", header = TRUE, sep = "\t", quote = "\"", fill = T, comment.char = "")

#Importing rounds2.csv
rounds2 <- read.csv("rounds2.csv", header = T, stringsAsFactors = F)

#Importing mapping.csv file
mapping <- read.csv("mapping.csv", check.names = F, stringsAsFactors = FALSE)


#Checkpoint 1
#Converting permalinks to lower case for companies and rounds2
rounds2$company_permalink <- tolower(rounds2$company_permalink)
companies$permalink <- tolower(companies$permalink)

#Number of unique companies from companies and rounds2 table
sqldf("SELECT count(distinct permalink) FROM companies")
sqldf("SELECT count(distinct company_permalink) FROM rounds2")

#merging both the tables to get master frame
master_frame <- merge(rounds2, companies, by.x = "company_permalink", by.y = "permalink")

#Checkpoint 2
#Creating df consisting average investment for each funding type
master_frame1 <- aggregate(raised_amount_usd~funding_round_type, 
                           master_frame, mean, na.rm = T)

#Identifying funding type which funds between $5 million and $15 million
sqldf("SELECT funding_round_type FROM master_frame1 
      WHERE raised_amount_usd >= 5000000 AND raised_amount_usd <= 15000000")



#Checkpoint 3
#Analyzing countries on the basis of venture funding type and sorting in decreasing order
country_analysis <- filter(master_frame, funding_round_type == "venture")
country_analysis <- aggregate(raised_amount_usd~country_code, country_analysis, sum)
country_analysis <- arrange(country_analysis, desc(raised_amount_usd))
country_analysis <- filter(country_analysis, country_code != "")

#Selecting top 9 countries 
top9 <- head(country_analysis, n = 9)
top9

#Getting country name countries, having english as official language
countrycode(c('USA', 'CHN', 'GBR'), 'iso3c', 'country.name')
countrycode(c('IND'),'iso3c','country.name')


#Checkpoint 4
#Splitting category_list to get primary sector
primary_split <- str_split(master_frame$category_list, pattern = "\\|")
primary_sector <- sapply(primary_split, function(a) a[1])
master_frame <- cbind(master_frame, primary_sector)

#Replacing 0 with na AND Na
mapping$category_list <- str_replace(mapping$category_list, '^0', 'Na')
mapping$category_list <- str_replace_all(mapping$category_list, '([^\\.])(0)', '\\1na')

#Converting mapping df into long data
mapping_sectors <- gather(mapping, main_sector, my_val, -category_list)
mapping_sectors <- mapping_sectors[!(mapping_sectors$my_val ==0),]
mapping_sectors <- mapping_sectors[,-3]
master_frame_sector <- left_join(master_frame, mapping_sectors, 
                                 by=c("primary_sector"="category_list"))

#Checkpoint 5
#D1 - USA
#Total number of Investments and total sum for venture type and fundings between $5 and $15 million
master_frame_USA_D1 <- filter(master_frame_sector, country_code == "USA", country_code != "",
                              funding_round_type == "venture", raised_amount_usd >= 5000000, 
                              raised_amount_usd <= 15000000,
                              main_sector != 'Blanks')
master_frame_USA_D1 <- group_by(master_frame_USA_D1, main_sector)
D1_main_sector <- summarise(master_frame_USA_D1, "count" = n(), "total_amount" = sum(raised_amount_usd))
sum(D1_main_sector$total_amount)
sum(D1_main_sector$count)

master_frame_USA <- inner_join(master_frame_USA_D1, D1_main_sector, 
                               by = c("main_sector" = "main_sector"))

#Finding company name which recieved highest investment in top sector
USA_company1 <- filter(master_frame_USA_D1, main_sector == 'Others')
USA_company1 <- group_by(USA_company1, company_permalink)
USA_company1 <- aggregate(raised_amount_usd~company_permalink, USA_company1, sum)
USA_company1 <- arrange(USA_company1, desc(raised_amount_usd))
USA_company1$company_permalink[1]

#Finding company name which recieved highest investment in top 2nd sector
USA_company2 <- filter(master_frame_USA_D1, 
                       main_sector == 'Social, Finance, Analytics, Advertising')
USA_company2 <- group_by(USA_company2, company_permalink)
USA_company2 <- aggregate(raised_amount_usd~company_permalink, USA_company2, sum)
USA_company2 <- arrange(USA_company2, desc(raised_amount_usd))
USA_company2$company_permalink[1]

#D2 - GBR
#Total number of Investments and total sum for venture type and fundings between $5 and $15 million
master_frame_GBR_D2 <- filter(master_frame_sector, country_code == "GBR", country_code != "",
                              funding_round_type == "venture", raised_amount_usd >= 5000000, 
                              raised_amount_usd <= 15000000,
                              main_sector != 'Blanks')
master_frame_GBR_D2 <- group_by(master_frame_GBR_D2, main_sector)
D2_main_sector <- summarise(master_frame_GBR_D2, "count" = n(), "total_amount" = sum(raised_amount_usd))
sum(D2_main_sector$total_amount)
sum(D2_main_sector$count)
master_frame_GBR <- inner_join(master_frame_GBR_D2, D2_main_sector, 
                               by = c("main_sector" = "main_sector"))

#Finding company name which recieved highest investment in top sector
GBR_company1 <- filter(master_frame_GBR_D2, 
                       main_sector == 'Others')
GBR_company1 <- group_by(GBR_company1, company_permalink)
GBR_company1 <- aggregate(raised_amount_usd~company_permalink, GBR_company1, sum)
GBR_company1 <- arrange(GBR_company1, desc(raised_amount_usd))
GBR_company1$company_permalink[1]

#Finding company name which recieved highest investment in top 2nd sector
GBR_company2 <- filter(master_frame_GBR_D2, 
                       main_sector == 'Social, Finance, Analytics, Advertising')
GBR_company2 <- group_by(GBR_company2, company_permalink)
GBR_company2 <- aggregate(raised_amount_usd~company_permalink, GBR_company2, sum)
GBR_company2 <- arrange(GBR_company2, desc(raised_amount_usd))
GBR_company2$company_permalink[1]


#D3 - IND
#Total number of Investments and total sum for venture type and fundings between $5 and $15 million
master_frame_IND_D3 <- filter(master_frame_sector, country_code == "IND", country_code != "",
                              funding_round_type == "venture", raised_amount_usd >= 5000000, 
                              raised_amount_usd <= 15000000,
                              main_sector != 'Blanks')
master_frame_IND_D3 <- group_by(master_frame_IND_D3, main_sector)
D3_main_sector <- summarise(master_frame_IND_D3, "count" = n(), "total_amount" = sum(raised_amount_usd))
sum(D3_main_sector$total_amount)
sum(D3_main_sector$count)
master_frame_IND <- inner_join(master_frame_IND_D3, D3_main_sector, 
                               by = c("main_sector" = "main_sector"))
IND_company1 <- filter(master_frame_IND_D3, 
                       main_sector == 'Others')

#Finding company name which recieved highest investment in top sector
IND_company1 <- group_by(IND_company1, company_permalink)
IND_company1 <- aggregate(raised_amount_usd~company_permalink, IND_company1, sum)
IND_company1 <- arrange(IND_company1, desc(raised_amount_usd))
IND_company1$company_permalink[1]

#Finding company name which recieved highest investment in top 2nd sector
IND_company2 <- filter(master_frame_IND_D3, 
                       main_sector == 'Social, Finance, Analytics, Advertising')
IND_company2 <- group_by(IND_company2, company_permalink)
IND_company2 <- aggregate(raised_amount_usd~company_permalink, IND_company2, sum)
IND_company2 <- arrange(IND_company2, desc(raised_amount_usd))
IND_company2$company_permalink[1]
