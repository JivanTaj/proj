#######################################################################
# Project: Putting it together 
# Author: Jivan taj 
# Date: 4/22/24
#####################################################################

#packages 

#read in data
data <- read.csv("bank_loan_approval_dataset.csv")

#init print the data 
print("Raw data peak:")
print(head(data))

# pause execution until any key is pressed
cat("\nPress enter get missing report...\n")
invisible(readLines("stdin", n = 1))

########################### Cleaning the data ################################
# Col analysiss
row_count <- nrow(data) # total row count for comparison 

# missing counts 
Ugender <- sum(data$Gender == "Other") #gender unknown is 'other'
Umarried <- sum(data$Married == "Unknown") #married unknown is 'unknown'
Udependents <- sum(data$Dependents == "Unknown") #dependents unknown is 'unknown'
Ueducation <- sum(data$Education == "Unknown") #education unknown is 'unknown'
Uself_employed <- sum(data$Self_Employed == "Unknown") #self employed unknown is 'unknown'
Uapplicant_income <- sum(data$ApplicantIncome == "Unknown") #applicant income unknown is 'unknown'
Ucoapplicant_income <- sum(data$CoapplicantIncome == "Unknown") #coapplicant income unknown is 'unknown'
Uhas_credit_card <- sum(data$Has_CreditCard == "Unknown") #has credit card unknown is 'unknown'
Uloan_amount <- sum(data$LoanAmount == "Unknown") #loan amount unknown is 'unknown'
Uloan_amount_term <- sum(data$Loan_Amount_Term == "Unknown") #loan amount term unknown is 'unknown'
Uowns_car <- sum(data$Owns_Car == "Unknown") #owns car unknown is 'unknown'
Uowns_house <- sum(data$Owns_House == "Unknown") #owns house unknown is 'unknown'
Uproperty_area <- sum(data$Property_Area == "Unknown") #property area unknown is 'unknown'
Uloan_status <- sum(data$Loan_Status == "Unknown") #loan status unknown is 'unknown'

# Print missing counts and percentages for each column
cat("Gender missing:", Ugender, "(", round(Ugender/row_count*100, 2), "%)\n")
cat("Married missing:", Umarried, "(", round(Umarried/row_count*100, 2), "%)\n")
cat("Dependents missing:", Udependents, "(", round(Udependents/row_count*100, 2), "%)\n")
cat("Education missing:", Ueducation, "(", round(Ueducation/row_count*100, 2), "%)\n")
cat("Self_Employed missing:", Uself_employed, "(", round(Uself_employed/row_count*100, 2), "%)\n")
cat("ApplicantIncome missing:", Uapplicant_income, "(", round(Uapplicant_income/row_count*100, 2), "%)\n")
cat("CoapplicantIncome missing:", Ucoapplicant_income, "(", round(Ucoapplicant_income/row_count*100, 2), "%)\n")
cat("Has_CreditCard missing:", Uhas_credit_card, "(", round(Uhas_credit_card/row_count*100, 2), "%)\n")
cat("LoanAmount missing:", Uloan_amount, "(", round(Uloan_amount/row_count*100, 2), "%)\n")
cat("Loan_Amount_Term missing:", Uloan_amount_term, "(", round(Uloan_amount_term/row_count*100, 2), "%)\n")
cat("Owns_Car missing:", Uowns_car, "(", round(Uowns_car/row_count*100, 2), "%)\n")
cat("Owns_House missing:", Uowns_house, "(", round(Uowns_house/row_count*100, 2), "%)\n")
cat("Property_Area missing:", Uproperty_area, "(", round(Uproperty_area/row_count*100, 2), "%)\n")
cat("Loan_Status missing:", Uloan_status, "(", round(Uloan_status/row_count*100, 2), "%)\n")

# pause execution until any key is pressed
cat("\nPress enter delete bad columns and normalize...\n")
invisible(readLines("stdin", n = 1))

# delete cols we don't need 
cleanData <- subset(data, 
                    select = -c(Loan_ID, #id
                                Applicant_ID, #id
                                Gender, #missing values
                                Has_CreditCard, #low variance 
                                Owns_Car)) #missing values  

# normalize binary cols. note: Owns_House is already normalized in provided data 
cleanData$Married <- ifelse(cleanData$Married == "Yes", 1, 0) #married y/n
cleanData$Education <- ifelse(cleanData$Education == "Graduate", 1, 0) #education graduate y/n
cleanData$Self_Employed <- ifelse(cleanData$Self_Employed == "Yes", 1, 0) #self employeed y/n
cleanData$Loan_Status <- ifelse(cleanData$Loan_Status == "Y", 1, 0) #approved y/n

# clean outliers from (co)applicant income 
cleanData$ApplicantIncome[cleanData$ApplicantIncome == "Unknown"] <- NA #sets unknown to na
cleanData$CoapplicantIncome[cleanData$CoapplicantIncome == "Unknown"] <- NA #sets unknown to na
#make income numeric 
# Convert income columns to numeric
cleanData$ApplicantIncome <- as.numeric(cleanData$ApplicantIncome)
cleanData$CoapplicantIncome <- as.numeric(cleanData$CoapplicantIncome)
# Generate box plot for ApplicantIncome
boxplot(cleanData$ApplicantIncome)
#save data to a new csv called cleanData
cat("\nSaving cleaned data to new file....\n")
write.csv(cleanData, file = "cleanData.csv", row.names = FALSE)
