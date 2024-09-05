mapping <- c(`0` = 1, `2` = 2, `1` = 3, `3` = 4)
# 应用映射到 cluster 列
df_original$color <- mapping[as.character(df_original$color)]

df_original$pred_cluster <- gmm$classification
df_original$pred_cluster <- ifelse(df_original$color == df_original$pred_cluster, "Correct", "Incorrect")

ggplot(df_original, aes(x = x, y = y, color = pred_cluster)) +
  geom_point() +
  scale_color_manual(values = c("Correct" = "black", "Incorrect" = "red")) +
  labs(title = "GMM Clustering of Blob Dataset",
       x = "",
       y = "") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 26),       # 标题字体大小
    axis.title.x = element_text(size = 26),     # x轴标签字体大小
    axis.title.y = element_text(size = 26),     # y轴标签字体大小
    axis.text.x = element_text(size = 26),      # x轴刻度字体大小
    axis.text.y = element_text(size = 26),
    legend.position = 'none' 
  )

accuracy <- mean(df_original$pred_cluster == "Correct")
cat("分类准确率:", accuracy, "\n")

0 > 1
2 > 2
1 > 3
3 > 4

