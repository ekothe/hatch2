library(ggridges)
library(ggplot2)
library(dplyr)
library(papaja)
library(cowplot)

ggplot(flu, aes(y = condition, x = flu_int, colour = condition, fill = condition)) + 
  geom_density_ridges(
    alpha = 1, scale = 3
  ) + 
  scale_colour_brewer(type = "qual", palette = 2, direction = 1,
                      aesthetics = c("colour", "fill"))  +
  labs(x = "Intention", y = "Message") + 
  theme(legend.position = "none")


ggplot(flu, aes(condition, flu_int, colour = condition, fill = condition)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), adjust = 2)+
  geom_point(position = position_jitter(width = .10), size = .25) + 
  geom_boxplot(outlier.shape = NA, alpha = 0.5, width = .1, colour = "BLACK") +
  theme_classic() + coord_flip() + labs(x = "", y = "Intention") + theme(legend.position = "none")

sevplot <- ggplot(filter(flu, condition =="Control" | condition == "Severity"), aes(y = condition, x = flu_sev, fill = condition)) +  
  geom_density_ridges(scale = 4) + theme_apa() + labs(y = "Message", x = "Severity Score") + theme(legend.position = "none")

ggplot(flu, aes(condition, flu_int, colour = condition, fill = condition)) + 
  scale_fill_manual(values = c("grey", "tan2", "darkgreen", "cornflowerblue", "darkblue", "purple", "pink")) +
  scale_colour_manual(values = c("grey", "tan2", "darkgreen", "cornflowerblue", "darkblue", "purple", "pink")) +
  #geom_flat_violin(position = position_nudge(x = .2, y = 0), adjust = 2)+
  geom_point(position = position_jitter(width = .10), size = .25) + 
  geom_boxplot(position = position_nudge(x = .2, y = 0), outlier.shape = NA, alpha = 0.5, width = .1, colour = "BLACK") +
  theme_classic() + coord_flip() + labs(x = "", y = "Intention") + theme(legend.position = "none")


sevplot <- ggplot(filter(flu, condition =="Control" | condition == "Severity"), aes(condition, flu_int, colour = condition, fill = condition)) + 
  scale_fill_manual(values = c("grey", "pink")) +
  scale_colour_manual(values = c("grey", "pink")) +
  #geom_flat_violin(position = position_nudge(x = .2, y = 0), adjust = 2)+
  geom_point(position = position_jitter(width = .10), size = .25) + 
  geom_boxplot(position = position_nudge(x = -0.2, y = 0), outlier.shape = NA, alpha = 0.5, width = .1, colour = "BLACK") +
  theme_classic() +  labs(x = "", y = "Intention") + theme(legend.position = "none") +
  labs(x = "", y = "")

susplot <- ggplot(filter(flu, condition =="Control" | condition == "Susceptibility"), aes(condition, flu_int, colour = condition, fill = condition)) + 
  #geom_flat_violin(position = position_nudge(x = .2, y = 0), adjust = 2)+
  scale_fill_manual(values = c("grey", "purple")) +
  scale_colour_manual(values = c("grey", "purple")) +
  geom_point(position = position_jitter(width = .10), size = .25) + 
  geom_boxplot(position = position_nudge(x = -0.2, y = 0), outlier.shape = NA, alpha = 0.5, width = .1, colour = "BLACK") +
  theme_classic() +  labs(x = "", y = "Intention") + theme(legend.position = "none") + 
  labs(x = "", y = "")

mrrplot <- ggplot(filter(flu, condition =="Control" | condition == "MRR"), aes(condition, flu_int, colour = condition, fill = condition)) + 
  #geom_flat_violin(position = position_nudge(x = .2, y = 0), adjust = 2)+
  scale_fill_manual(values = c("grey", "darkblue")) +
  scale_colour_manual(values = c("grey", "darkblue")) +
  geom_point(position = position_jitter(width = .10), size = .25) + 
  geom_boxplot(position = position_nudge(x = -0.2, y = 0), outlier.shape = NA, alpha = 0.5, width = .1, colour = "BLACK") +
  theme_classic() + labs(x = "", y = "Intention") + theme(legend.position = "none") + 
  labs(x = "", y = "")


seplot <- ggplot(filter(flu, condition =="Control" | condition == "Self-Efficacy"), aes(condition, flu_int, colour = condition, fill = condition)) + 
  #geom_flat_violin(position = position_nudge(x = .2, y = 0), adjust = 2)+
  scale_fill_manual(values = c("grey", "cornflowerblue")) +
  scale_colour_manual(values = c("grey", "cornflowerblue")) +
  geom_point(position = position_jitter(width = .10), size = .25) + 
  geom_boxplot(position = position_nudge(x = -0.2, y = 0), outlier.shape = NA, alpha = 0.5, width = .1, colour = "BLACK") +
  theme_classic() + labs(x = "", y = "Intention") + theme(legend.position = "none") + 
  labs(x = "", y = "")


replot <- ggplot(filter(flu, condition =="Control" | condition == "Response Efficacy"), aes(condition, flu_int, colour = condition, fill = condition)) + 
  #geom_flat_violin(position = position_nudge(x = .2, y = 0), adjust = 2)+
  scale_fill_manual(values = c("grey", "darkgreen")) +
  scale_colour_manual(values = c("grey", "darkgreen")) +
  geom_point(position = position_jitter(width = .10), size = .25) + 
  geom_boxplot(position = position_nudge(x = -0.2, y = 0), outlier.shape = NA, alpha = 0.5, width = .1, colour = "BLACK") +
  theme_classic() +   labs(x = "", y = "") + theme(legend.position = "none") 



rcplot <- ggplot(filter(flu, condition =="Control" | condition == "Response Costs"), aes(condition, flu_int, colour = condition, fill = condition)) + 
  #geom_flat_violin(position = position_nudge(x = .2, y = 0), adjust = 2)+
  scale_fill_manual(values = c("grey", "tan2")) +
  scale_colour_manual(values = c("grey", "tan2")) +
  geom_point(position = position_jitter(width = .10), size = .25) + 
  geom_boxplot(position = position_nudge(x = -0.2, y = 0), outlier.shape = NA, alpha = 0.5, width = .1, colour = "BLACK") +
  theme_classic() +   labs(x = "", y  = "") + theme(legend.position = "none")


plot_grid(sevplot, susplot, mrrplot, seplot, replot, rcplot, ncol=3, nrow = 2, labels = c("A", "B", "C","D", "E", "F"))

