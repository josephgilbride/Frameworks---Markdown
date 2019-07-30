setwd("C:/Users/RD45077/Documents/")
gov_data <- read.csv(file = "AssistanceListings_DataGov_PUBLIC_CURRENT_20190706 - AssistanceListings_DataGov_PUBLIC_CURRENT_20190706.csv")
head(gov_data)
dim(gov_data)
library(dplyr)
library(stringr)
library(tidyr)
#gov_data$obligations_new <- replace(starts_with(gov_data$Obligations..122.,
 #                          c("(Project Grants", "(Salaries and Expenses"),
  #                         c(" "," "))

#Pre-Processing                           
#creating duplicate col of obligations for referral later on
gov_data$new_obligations <- gov_data$Obligations..122.
#stripping the noise data
gov_data$new_obligations <- gsub("(Project Grants)", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("(Salaries and Expenses)", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("(Direct Payments with Unrestricted Use)", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("(Direct Loans)", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("(Cooperative Agreements)", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("( ( or Contracts))", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("(Direct Payments for Specified Use)", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("( (for specified projects))", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("(Information)", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("(Advisory Services and Counseling)", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("(Investigation of Complaints)", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("(Formula Grants (Apportionments))", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("(Formula Grants)", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("( (Apportionments))", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("(Guaranteed/Insured Loans)", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("( (Discretionary Grants))", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("( (Discretionary))", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("(Advisory Services on Compliance)", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("( (for specified projects))", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("( (or Contracts))", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("( (Apportionments))", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("( (Discretionary Grants))", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("( (Discretionary))", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("(Insurance)", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("( (Special)) ", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("(Training)", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("(Fellowships)", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("( (for specified projects))", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("( (Apportionments))", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("( (Discretionary))", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("(Sale, Exchange, or Donation of Property and Goods)", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("(Loan Guarantees/Grants)", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("(Dissemination of Technical )", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("(Provision of Specialized Services)", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("(Not Applicable)", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("()", "", 
                                 ignore.case = T, gov_data$new_obligations)
gov_data$new_obligations <- gsub("( ())", "", 
                                 ignore.case = T, gov_data$new_obligations)
gov_data$new_obligations <- gsub("( (for specified projects))", "", 
                                 ignore.case = T, gov_data$new_obligations)
gov_data$new_obligations <- gsub("((forspecifiedprojects))", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("((Apportionments))", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("((DiscretionaryGrants))", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("((Discretionary))", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("((Contracts))", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("((withFormulaDistribution))", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("((Special))", "", 
                                 ignore.case = F, gov_data$new_obligations)
#checking to see if there is still any noise text prior to the FY values
write.csv(gov_data, file = "gov_assist_obligation_check.csv")
#removing remaining strings
gov_data$new_obligations <- gsub("((includingindividualawards))", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("(UseofProperty,Facilities,andEquipment)", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("(FederalEmployment)", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("((HealthIncentiveGrants))", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("((GuaranteedSuretyBonds))", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("((CapacityBuildingandComplaintProcessing,))", "", 
                                 ignore.case = F, gov_data$new_obligations)
gov_data$new_obligations <- gsub("((tocapitalizeloanfunds))", "", 
                                 ignore.case = F, gov_data$new_obligations)
#Extracting the FY values to individual columns
#may have to group sections of the data based on whether they have a value for FY or not
#to properly extract the info to the correct columns
install.packages('reshape')
library(reshape)
require(reshape)
df_ob <- data.frame(gov_data$new_obligations)
vec_ob <- as.vector(df_ob)
gov_data$new_obligations <- transform(gov_data,  )
df_ob$FY17 <- NA
df_ob$FY18 <- NA
df_ob$FY19 <- NA
df_ob$FY16 <- NA
#separate(vec_ob, vec_ob$FY17, sep = ";")

if(starts_with("()")){
  str_remove(vec_ob$gov_data.new_obligations,"()")
}


























if(gov_data$Obligations..122. == '(Project Grants)'){
  gov_data$Obligations..122. <- gsub("(Project Grants)", "", gov_data$Obligations..122.)
} else{
  if(gov_data$Obligations..122. == '(Salaries and Expenses)'){
    gov_data$Obligations..122. <- gsub("(Salaries and Expenses)", "", gov_data$Obligations..122.)
  }
}


mutate_at(vars(starts_with("Obligations..122.")), ~ str_sub(. start = 16))

gov_data %>%
  group_by(newColumn = str_sub(`Obligations..122.`, start = 16)) %>%
  mutate(ind = row_number(), i1 = 1) %>%
  spread(newColumn, i1, fill = 0)
?replace