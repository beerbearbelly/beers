ggplot(cust_df, aes(x = Recency)) + 
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Recency",
       subtitle = "Data from the Online Retail Dataset",
       x = "Recency",
       y = "Count") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 26),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(face = "bold", color = "black", size = 26),
    axis.title.y = element_text(face = "bold", color = "black", size = 26),
    panel.grid.major = element_line(color = "gray", size = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 26),      
    axis.text.y = element_text(size = 26)
  ) +
  geom_vline(aes(xintercept = mean(Recency)), color = "red", linetype = "dashed", size = 1)


##################################################################################################################
ggplot(cust_df, aes(x = Frequency)) + 
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Frequency",
       subtitle = "Data from the Online Retail Dataset",
       x = "Frequency",
       y = "Count") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 26),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(face = "bold", color = "black", size = 26),
    axis.title.y = element_text(face = "bold", color = "black", size = 26),
    panel.grid.major = element_line(color = "gray", size = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 26),      
    axis.text.y = element_text(size = 26)
  ) +
  geom_vline(aes(xintercept = mean(Frequency)), color = "red", linetype = "dashed", size = 1)

n <- length(cust_df$Frequency)
outlier <- floor(n*0.05)
cust_frequency <- sort(cust_df$Frequency)[outlier:(n-outlier)]
cust_frequency_df <- data.frame(Frequency = cust_frequency)

ggplot(cust_frequency_df, aes(x = Frequency)) + 
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Frequency (Delete Outlier)",
       subtitle = "Data from the Online Retail Dataset",
       x = "Frequency",
       y = "Count") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 26),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(face = "bold", color = "black", size = 26),
    axis.title.y = element_text(face = "bold", color = "black", size = 26),
    panel.grid.major = element_line(color = "gray", size = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 26),      
    axis.text.y = element_text(size = 26)
  ) +
  geom_vline(aes(xintercept = mean(Frequency)), color = "red", linetype = "dashed", size = 1)



##################################################################################################################
ggplot(cust_df, aes(x = Monetary)) + 
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Monetary",
       subtitle = "Data from the Online Retail Dataset",
       x = "Monetary",
       y = "Count") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 26),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(face = "bold", color = "black", size = 26),
    axis.title.y = element_text(face = "bold", color = "black", size = 26),
    panel.grid.major = element_line(color = "gray", size = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 26),      
    axis.text.y = element_text(size = 26)
  ) +
  geom_vline(aes(xintercept = mean(Monetary)), color = "red", linetype = "dashed", size = 1)

n <- length(cust_df$Monetary)
outlier <- floor(n*0.05)
cust_monetary <- sort(cust_df$Monetary)[outlier:(n-outlier)]
cust_monetary_df <- data.frame(Monetary = cust_monetary)

ggplot(cust_monetary_df, aes(x = Monetary)) + 
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Monetary (Delete Outlier)",
       subtitle = "Data from the Online Retail Dataset",
       x = "Monetary",
       y = "Count") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 26),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(face = "bold", color = "black", size = 26),
    axis.title.y = element_text(face = "bold", color = "black", size = 26),
    panel.grid.major = element_line(color = "gray", size = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 26),      
    axis.text.y = element_text(size = 26)
  ) +
  geom_vline(aes(xintercept = mean(Monetary)), color = "red", linetype = "dashed", size = 1)

