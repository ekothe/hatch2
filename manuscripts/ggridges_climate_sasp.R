library(ggridges)
library(ggplot2)
library(dplyr)
library(papaja)
library(cowplot)

ggplot(climate, aes(y = condition, x = cc_int, colour = condition, fill = condition)) + 
  geom_density_ridges(
    alpha = 1, scale = 3
  ) + 
  scale_colour_brewer(type = "qual", palette = 2, direction = 1,
                      aesthetics = c("colour", "fill"))  +
  labs(x = "Intention", y = "Message") + 
  theme(legend.position = "none")


ggplot(climate, aes(condition, cc_int, colour = condition, fill = condition)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), adjust = 2)+
  geom_point(position = position_jitter(width = .10), size = .25) + 
  geom_boxplot(outlier.shape = NA, alpha = 0.5, width = .1, colour = "BLACK") +
  theme_classic() + coord_flip() + labs(x = "", y = "Intention") + theme(legend.position = "none")

sevplot <- ggplot(filter(climate, condition =="Control" | condition == "Severity"), aes(y = condition, x = cc_sev, fill = condition)) +  
  geom_density_ridges(scale = 4) + theme_apa() + labs(y = "Message", x = "Severity Score") + theme(legend.position = "none")

ggplot(climate, aes(condition, cc_int, colour = condition, fill = condition)) + 
  scale_fill_manual(values = c("grey", "white", "white", "cornflowerblue", "darkblue", "white", "white")) +
  scale_colour_manual(values = c("grey", "yellow", "green", "cornflowerblue", "darkblue", "purple", "pink")) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), adjust = 2)+
  geom_point(position = position_jitter(width = .10), size = .25) + 
  geom_boxplot(outlier.shape = NA, alpha = 0.5, width = .1, colour = "BLACK") +
  theme_classic() + coord_flip() + labs(x = "", y = "Intention") + theme(legend.position = "none")


sevplot <- ggplot(filter(climate, condition =="Control" | condition == "Severity"), aes(condition, cc_int)) + geom_flat_violin(position = position_nudge(x = .2, y = 0), adjust = 2)+
  geom_point(position = position_jitter(width = .10), size = .25) + 
  geom_boxplot(outlier.shape = NA, alpha = 0.5, width = .1, colour = "BLACK") +
  theme_classic() +  labs(x = "", y = "Intention") + theme(legend.position = "none") +
  labs(x = "", y = "Severity Score")

susplot <- ggplot(filter(climate, condition =="Control" | condition == "Susceptibility"), aes(condition, cc_int)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), adjust = 2)+
  geom_point(position = position_jitter(width = .10), size = .25) + 
  geom_boxplot(outlier.shape = NA, alpha = 0.5, width = .1, colour = "BLACK") +
  theme_classic() +  labs(x = "", y = "Intention") + theme(legend.position = "none") + 
  labs(x = "", y = "Susceptibility Score")

mrrplot <- ggplot(filter(climate, condition =="Control" | condition == "MRR"), aes(condition, cc_int)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), adjust = 2)+
  geom_point(position = position_jitter(width = .10), size = .25) + 
  geom_boxplot(outlier.shape = NA, alpha = 0.5, width = .1, colour = "BLACK") +
  theme_classic() + labs(x = "", y = "Intention") + theme(legend.position = "none") + 
  labs(x = "", y = "MRR Score")


seplot <- ggplot(filter(climate, condition =="Control" | condition == "Self-Efficacy"), aes(condition, cc_int, colour = condition, fill = condition)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), adjust = 2)+
  geom_point(position = position_jitter(width = .10), size = .25) + 
  geom_boxplot(outlier.shape = NA, alpha = 0.5, width = .1, colour = "BLACK") +
  theme_classic() + labs(x = "", y = "Intention") + theme(legend.position = "none") + 
  labs(x = "", y = "Self-Efficacy Score")
  

replot <- ggplot(filter(climate, condition =="Control" | condition == "Response Efficacy"), aes(condition, cc_int)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), adjust = 2)+
  geom_point(position = position_jitter(width = .10), size = .25) + 
  geom_boxplot(outlier.shape = NA, alpha = 0.5, width = .1, colour = "BLACK") +
  theme_classic() +   labs(x = "", y = "RE Score") + theme(legend.position = "none") 



rcplot <- ggplot(filter(climate, condition =="Control" | condition == "Response Costs"), aes(condition, cc_int)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), adjust = 2)+
  geom_point(position = position_jitter(width = .10), size = .25) + 
  geom_boxplot(outlier.shape = NA, alpha = 0.5, width = .1, colour = "BLACK") +
  theme_classic() +   labs(x = "", y  = "Response Costs Score") + theme(legend.position = "none")


plot_grid(sevplot, susplot, mrrplot, ncol=3, labels = c("A", "B", "C"))

plot_grid(seplot, replot, rcplot, ncol=3, labels = c("D", "E", "F"))
