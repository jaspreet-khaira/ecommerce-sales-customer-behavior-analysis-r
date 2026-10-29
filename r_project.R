#Install package
install.packages("dplyr")

#Load the package
library(dplyr)


data <- read.csv("C:/Users/khair/Downloads/ecommerce_customer_behavior_dataset.csv")
#Display the data:
print(data)

#Display the first six rows:
head(data)

#Display the last six rows:
tail(data)

total_spending <- sum(data$Total_Amount, na.rm = TRUE) 
average_spending <- mean(data$Total_Amount, na.rm = TRUE) 
median_spending <- median(data$Total_Amount, na.rm = TRUE) 
minimum_spending <- min(data$Total_Amount, na.rm = TRUE) 
maximum_spending <- max(data$Total_Amount, na.rm = TRUE) 
total_quantity <- sum(data$Quantity, na.rm = TRUE) 
average_rating <- mean(data$Customer_Rating, na.rm = TRUE) 

cat("Total Spending:", total_spending, "\n")
cat("Average Spending:", average_spending, "\n")
cat("Median Spending:", median_spending, "\n")
cat("Minimum Spending:", minimum_spending, "\n")
cat("Maximum Spending:", maximum_spending, "\n")
cat("Total Quantity:", total_quantity, "\n")
cat("Average Rating:", average_rating, "\n")

#Number of Rows and Columns
dim(data)
#Number of Rows
nrow(data)
#Number of Columns
ncol(data)
#Column Names
names(data)
glimpse(data)
str(data)

#Summary
summary(data)
#Missing Values
print(colSums(is.na(data)))
#Duplicate Rows
print(sum(duplicated(data)))

data$Average_Order_Value <- data$Total_Amount / data$Quantity 
data$Discount_Percentage <- (data$Discount_Amount / (data$Unit_Price * data$Quantity)) * 100

data$Spending_Category <- "" 
for(i in 1:nrow(data)) { 
  if(data$Total_Amount[i] >= 5000) { 
    data$Spending_Category[i] <- "High" 
    } else if(data$Total_Amount[i] >= 2000) { 
      data$Spending_Category[i] <- "Medium" 
      } else { 
        data$Spending_Category[i] <- "Low" 
        } 
  } 
# Display classification 
print(head(data[, c("Customer_ID", 
                    "Total_Amount", 
                    "Spending_Category")]))

category_analysis <- data %>% 
  group_by(Product_Category) %>% 
  summarise( 
    Total_Spending = sum(Total_Amount, na.rm = TRUE), 
    Average_Spending = mean(Total_Amount, na.rm = TRUE), 
    Total_Quantity = sum(Quantity, na.rm = TRUE), 
    Number_of_Orders = n() 
    ) 
print(category_analysis)

print(table(data$Payment_Method))

print(table(data$Gender)) 
gender_analysis <- data %>% 
  group_by(Gender) %>% 
  summarise( 
    Total_Spending = sum(Total_Amount, na.rm = TRUE), 
    Average_Spending = mean(Total_Amount, na.rm = TRUE) 
    ) 
print(gender_analysis)

print(table(data$Is_Returning_Customer)) 
returning_analysis <- data %>% 
  group_by(Is_Returning_Customer) %>% 
  summarise( 
    Total_Spending = sum(Total_Amount, na.rm = TRUE), 
    Average_Spending = mean(Total_Amount, na.rm = TRUE) 
    ) 
print(returning_analysis)

print(table(data$Device_Type)) 
device_analysis <- data %>% 
  group_by(Device_Type) %>% 
  summarise( 
    Total_Spending = sum(Total_Amount, na.rm = TRUE), 
    Average_Spending = mean(Total_Amount, na.rm = TRUE) 
    ) 
print(device_analysis)

hist(data$Total_Amount, 
     main = "Distribution of Customer Spending", 
     col = "skyblue", 
     xlab = "Total Amount", 
     ylab = "Frequency", 
     border = "black", 
     breaks = 10)


hist(data$Age, 
     main = "Distribution of Customer Age", 
     col = "lightgreen", 
     xlab = "Age", 
     ylab = "Frequency", 
     border = "black", 
     breaks = 10)

boxplot(data$Total_Amount, 
        main = "Boxplot of Customer Spending", 
        ylab = "Total Amount", 
        col = "lightblue")

boxplot(Total_Amount ~ Product_Category, 
        data = data, 
        main = "Customer Spending by Product Category", 
        xlab = "Product Category", 
        ylab = "Total Amount", 
        col = "lightgreen", 
        las = 2)

