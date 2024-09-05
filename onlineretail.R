library(dplyr)
library(mclust)
library(plotly)
library(ggplot2)

file_path <- "~/Downloads/OnlineRetail.csv"
retail_df <- read.csv(file_path)
df_original <- retail_df
retail_df <- subset(retail_df, select = -Description)
retail_df <- subset(retail_df, select = -Country)

retail_df <- retail_df[retail_df$Quantity > 0, ] # delete quantity<0
retail_df <- retail_df[retail_df$UnitPrice > 0, ] # delete uniteprice<0
retail_df <- retail_df[!is.na(retail_df$CustomerID), ] # delete null value
colSums(is.na(retail_df)) # check null value

df_original$SaleAmount <- df_original$Quantity * df_original$UnitPrice # calculate the amount of each invoice
retail_df$CustomerID <- as.integer(retail_df$CustomerID) 

head(retail_df, 3)

print(head(sort(table(retail_df$CustomerID), decreasing = TRUE)))

mean(aggregate(InvoiceNo ~ InvoiceNo + StockCode, retail_df, length)$InvoiceNo)
retail_df$InvoiceDate <- as.POSIXct(retail_df$InvoiceDate, format = "%d-%m-%Y %H:%M") # redefine the data format

df_original$SaleAmount <- as.numeric(df_original$SaleAmount)

cust_df <- retail_df %>% # combine the customer record
  group_by(CustomerID) %>%
  summarise(
    Recency = max(InvoiceDate), # find the most recent date
    Frequency = n(), # count
  )
monetary_tmp <- df_original %>%
  group_by(CustomerID) %>%
  summarise(
    Monetary = sum(SaleAmount)
  )
cust_df <- cust_df %>%
  left_join(monetary_tmp, by = "CustomerID")

head(cust_df)

names(cust_df)[names(cust_df) == "InvoiceDate"] <- "Recency" # change name
names(cust_df)[names(cust_df) == "InvoiceNo"] <- "Frequency"
names(cust_df)[names(cust_df) == "SaleAmount"] <- "Monetary"

cust_df <- as.data.frame(cust_df)
cust_df <- cust_df[, c("CustomerID", "Recency", "Frequency", "Monetary")]

head(cust_df)

max(cust_df$Recency) # find the most recent date and used in the code below

reference_date <- as.POSIXct("2011-12-10 12:50:00", format = "%Y-%m-%d %H:%M") 

cust_df <- cust_df %>%
  mutate(Recency = as.numeric(difftime(reference_date, Recency, units = "days")) + 1) # to get a numerical value to put into the model

print(dim(cust_df))

head(cust_df, 10)

gmm <- Mclust(cust_df)
summary(gmm)
cluster_label <- gmm$classification
cust_result <- cust_df
cust_result$cluster <- as.factor(cluster_label)

fig <- plot_ly(cust_result, 
               x = ~Recency, 
               y = ~Frequency, 
               z = ~Monetary, 
               color = ~as.factor(cluster), 
               type = 'scatter3d', 
               mode = 'markers') %>%
  layout(title = 'GMM clustering of online retail dataset',
         scene = list(xaxis = list(title = 'Recency'),
                      yaxis = list(title = 'Frequency'),
                      zaxis = list(title = 'Monetary')))

fig

cluster_means <- cust_result %>%
  group_by(cluster) %>%
  summarise(
    Mean_Recency = mean(Recency, na.rm = TRUE),
    Mean_Frequency = mean(Frequency, na.rm = TRUE),
    Mean_Monetary = mean(Monetary, na.rm = TRUE)
  )

print(cluster_means)
