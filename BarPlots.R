library(dplyr) 
library(ggplot2)
library(ggpubr)
library(viridis)

data <- read.csv("~/Desktop/extraxylary_g_fibers.csv", stringsAsFactors = T)
data$Mechanism <- factor(data$Mechanism,levels = c( "root-climber", "tendril-climber", "stem-twiner"))

data$extraxylary_g_fibers
df <- data %>%
  filter(extraxylary_g_fibers %in% c("absent", "present")) %>%
  group_by(Mechanism, extraxylary_g_fibers, .drop = F) %>%
  summarise(counts = n()) 


#two bar charts
p <- ggplot(df, aes(x = Mechanism, y = counts)) +
  geom_bar(
    aes(fill = extraxylary_g_fibers),
    colour="black", stat = "identity", position = position_dodge(.95),
    width = 0.7 + geom_text(
      aes(color= extraxylary_g_fiberss, label = counts, group = extraxylary_g_fibers), 
      position = position_dodge(0.8),
      vjust = -0.3, size = 4))
#+geom_text(aes(label=counts), position=position_dodge(width=.09), vjust=.01)


p + scale_fill_brewer(palette = "Dark2", direction = -1) +
  xlab("Climbing Mechanism") +
  ylab("Count") +
  ggtitle("Extraxylary Gelatinous Fibers")  +
  theme_minimal() +
  theme(legend.position = "top",
        title = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "bold"),  
        axis.title.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 17, angle = 90, hjust = .5, vjust = .5, face = "bold")) +
  scale_y_continuous(breaks=seq(0,35,5))


