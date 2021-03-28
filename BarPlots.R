library(dplyr) 
library(ggplot2)
library(ggpubr)
library(viridis)

data <- read.csv("~/xylary_gfibers.csv", stringsAsFactors = T)
data$Category <- factor(data$Category,levels = c( "root climber", "tendrilate climber", "stem-twiner"))

data$g.fiber
df <- data %>%
  filter(g.fiber %in% c("absent", "present")) %>%
  group_by(Category, g.fiber, .drop = F) %>%
  summarise(counts = n()) 


#two bar charts
p <- ggplot(df, aes(x = Category, y = counts)) +
  geom_bar(
    aes(fill = g.fiber),
    colour="black", stat = "identity", position = position_dodge(.95),
    width = 0.7 + geom_text(
    aes(color= g.fibers, label = counts, group = g.fiber), 
    position = position_dodge(0.8),
    vjust = -0.3, size = 4))

p + scale_fill_brewer(palette = colorBlindGrey8[1,2], direction = -1) +
  xlab("Climbing Mechanism") +
  ylab("Count") +
  ggtitle("Xylary Gelatinous Fibers")  +
  theme_minimal() +
  theme(legend.position = "top",
        title = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = .5, face = "bold"),
        axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "bold"),  
        axis.title.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = 0, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 17, angle = 90, hjust = .5, vjust = .5, face = "bold")) +
  scale_y_continuous(breaks=seq(0,35,5))

