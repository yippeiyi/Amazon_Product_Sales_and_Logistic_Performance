# Loading all dataset
Amazon_report <- read.csv("C:/Users/yippe/OneDrive/Documents/Y3S2/ETW2001/Group_Assignment/Datasets/Amazon Sale Report.csv", header = TRUE)

# Check head of dataset
head(Amazon_report, 3)
View(Amazon_report)

# Importing libraries
library(tidyverse)
library(dplyr)
library(ggplot2)

################################################################################
#------------------------------DATA WRANGLING-----------------------------------
# Data Cleaning
# Cleaning Amazon dataset
amazon_report_2 <- read.csv("C:/Users/yippe/OneDrive/Documents/Y3S2/ETW2001/Group_Assignment/Datasets/Amazon Sale Report.csv", na.strings = c("", "NA"))                            # reading and substituting empty data with NAs

# Checking structure of dataset
str(amazon_report_2)

# Check columns with most NAs
colSums(is.na(amazon_report_2))                                                                            # check for NAs

# Clean dataset
amazon_report_2 <- select(amazon_report_2, -c(index, currency, promotion.ids, fulfilled.by, Unnamed..22))  # remove columns
amazon_report_2 <- na.omit(amazon_report_2)                                                                # remove NAs
amazon_report_2 <- rename(amazon_report_2,                                                                 # rename columns
                          "Order_ID" = "Order.ID",
                          "Order_Date" = "Date",
                          "Order_Status"= "Status",
                          "Sales_Channel" = "Sales.Channel",
                          "Service_Level" = "ship.service.level",
                          "Courier_Status" = "Courier.Status",
                          "Quantity" = "Qty",
                          "City" = "ship.city",
                          "State" = "ship.state",
                          "Postal_Code" = "ship.postal.code",
                          "Country" = "ship.country")

# View the data
# Sales channel is Amazon for all
View(amazon_report_2)

# Descriptive statistics
summary(amazon_report_2)
str(amazon_report_2)

################################################################################
#------------------------------VISUALISATIONS-----------------------------------

#-----------------1st visualisation (Revenue by Product Category)

# Extracting data
relevant_data_statement1 <- amazon_report_2[, c('Category', 'Amount')]

summary_data <- relevant_data_statement1 %>%
  group_by(Category) %>%
  summarise(TotalSales = sum(Amount, na.rm = TRUE))

# visualisation
ggplot(summary_data, aes(x = reorder(Category, TotalSales), y = TotalSales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  geom_text(data = summary_data, aes(label = scales::comma(TotalSales), hjust = 0.5), size = 4)+# For horizontal bars
  labs(title = "Revenue by Product Category in India",
       x = "Product Category",
       y = "Revenue (INR)") +
  theme_grey() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
        axis.title = element_text(face = "bold", size = 10),
        axis.text = element_text(face = "bold", size = 9))+
  scale_y_continuous(labels = scales::comma)

#----------------- 2nd visualisation (Percentage of Revenue by Product Category of Amazon)

# Calculate the percentage of revenue for each category
summary_data2 <- relevant_data_statement1 %>%
  group_by(Category) %>%
  summarise(TotalSales = sum(Amount, na.rm = TRUE)) %>%
  mutate(Percentage = TotalSales / sum(TotalSales) * 100)

# visualisation
ggplot(summary_data2, aes(x = reorder(Category, -TotalSales), y = Percentage, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(Percentage / 100, accuracy = 0.1), group = Category),
            position = position_stack(vjust = 0.5), size = 4) +
  labs(title = "Percentage of Revenue by Product Category of Amazon",
       x = "Product Category",
       y = "Percentage of Revenue") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
        axis.title = element_text(face = "bold", size = 10),
        axis.text = element_text(face = "bold", size = 9),
        legend.position = "right") +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

#----------------- 3rd visualisation (Amazon monthly sales revenue)

# Creating a new table (months) to preserve cleaned original one
months <- amazon_report_2[, c("Order_Date", "Amount")]
months$Order_Date <- as.character(months$Order_Date)
months$Order_Date <- as.Date(lubridate::mdy(months$Order_Date))

# Calculating monthly sales revenue
monthly_rev <- months %>%
  group_by(Month = format(Order_Date, "%Y-%m-%d")) %>%
  summarise(Revenue = sum(Amount, na.rm = TRUE))

# Converting Month variable to date
monthly_rev$Month <- as.Date(monthly_rev$Month, format = "%Y-%m-%d")

# visualisation 
ggplot(monthly_rev, aes(x = Month, y = Revenue, group = 1)) +
  geom_line(color = "blue", size = 1.5) +
  theme_grey() +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  labs(title = "Growth in Amazon Sales From March to July 2022",
       y = "Total Revenue (INR)",
       x = "Date (Month)") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
        axis.title = element_text(face = "bold", size = 13),
        axis.text = element_text(size = 12))

#----------------- 4th visualisation (order outcomes by fulfilment entities)

# create a new table (df_order) so that preserve the cleaned original one
df_order <- amazon_report_2 %>%
  mutate(Positive_Negative = ifelse(amazon_report_2$Order_Status %in% c("Shipped - Returned to Seller",
                                                                        "Shipped - Rejected by Buyer",
                                                                        "Shipped - Lost in Transit",
                                                                        "Shipped - Damaged",
                                                                        "Shipped - Returning to seller",
                                                                        "Cancelled"),
                                    "Negative", "Positive")) %>%
  select(c(Fulfilment, Positive_Negative)) %>%
  group_by(Fulfilment, Positive_Negative) %>%
  summarise(Freq = n()) %>%
  unite(col = "Outcome_by_Fulfilment_Entities",
        c("Fulfilment", "Positive_Negative"),
        sep = " ")

# visualisation
ggplot(df_order, aes(x = "", y = Freq, fill = Outcome_by_Fulfilment_Entities)) +
  geom_col() +
  geom_text(aes(label = scales::percent(Freq/sum(Freq), 1)),
            position = position_stack(vjust = 0.5), size = 10) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette="Set2") +
  labs(title = "Distribution of Order Outcomes by Fulfilment Entities") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15))

#----------------- 5th visualisation (Negative customer delivery experience by state in India)

# Subsetting dataset based on customer experience
trial <- amazon_report_2 %>%
  mutate(Positive_Negative = ifelse(amazon_report_2$Order_Status %in% c("Shipped - Returned to Seller",
                                                                        "Shipped - Rejected by Buyer",
                                                                        "Shipped - Lost in Transit",
                                                                        "Shipped - Damaged",
                                                                        "Shipped - Returning to seller",
                                                                        "Cancelled"),
                                    "Negative", "Positive")) %>%
  group_by(State, Positive_Negative) %>% 
  select(c(State, Positive_Negative))
  
trial$State <- tolower(trial$State)
trial$State <- str_to_title(trial$State)
trial <- trial %>% 
  summarise(Count = n()) %>%
  group_by(State) %>%
  mutate(Total = sum(Count)) %>%
  mutate(Percentage = (Count / Total) * 100) 

trial$State[trial$State == "Ar"] <- "Arunachal Pradesh"
trial$State[trial$State =="Chhattisgarh"] <- "Chandigarh"
trial$State[trial$State == "Dadra And Nagar"] <- "Dadra & Nagar"
trial$State[trial$State == "Nl"] <- "New Delhi"
trial$State[trial$State == "Delhi"] <- "New Delhi"
trial$State[trial$State == "Pb"] <- "Punjab"
trial$State[trial$State == "Punjab/Mohali/Zirakpur"] <- "Punjab"
trial$State[trial$State == "Rajshthan"] <- "Rajasthan"
trial$State[trial$State == "Rajsthan"] <- "Rajasthan"

n_exp <- trial %>% 
  filter(Positive_Negative == "Negative")

# visualisation (negative experience)
ggplot(n_exp, aes(fill = Positive_Negative, y = Percentage, x = State)) +
  geom_bar(stat = "identity") +
  labs(title = "Negative Customer Delivery Experience (%) by State in India",
       x = "States in India",
       y = "Percentage (%)",
       fill = "Delivery Experience") +
  theme_grey() +
  theme(axis.text.x = element_text(face = "bold", angle = 70, hjust = 1),
        legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
        axis.title = element_text(face = "bold", size = 10),
        axis.text = element_text(face = "bold", size = 9))

#----------------- 6th visualisation (visualize the distribution of order statuses)

# Calculate the distribution of order statuses
order_status_counts <- amazon_report_2 %>%
  group_by(Order_Status) %>%
  summarise(Count = n())

# Filter the data frame to include only the six specified variables
filtered_order_status_counts <- order_status_counts %>%
  filter(Order_Status %in% c("Shipped - Returned to Seller",
                             "Shipped - Rejected by Buyer",
                             "Shipped - Lost in Transit",
                             "Shipped - Damaged",
                             "Shipped - Returning to seller",
                             "Cancelled"))

ggplot(filtered_order_status_counts, aes(x = "", y = Count, fill = Order_Status)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette="Set2") +
  geom_text(aes(label = scales::percent(Count/sum(Count), 1)),
            position = position_stack(vjust = 0.5), size = 10) +
  labs(title = "Distribution of Order Statuses for Amazon Sales") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15))
