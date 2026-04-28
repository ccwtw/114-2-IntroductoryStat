

gender_data <- data %>%
  distinct(name, gender) %>% 
  count(gender) %>%
  mutate(percent = n / sum(n) * 100,
         label = paste0(ifelse(gender == "M", "Male", "Female"),
                        " - ", n, " (", round(percent, 0), "%)"),
         percent_plot = ifelse(gender == "F", -percent, percent))


data %>%
  distinct(name, gender) %>%
  count(gender)



# 畫橫向長條圖
ggplot(gender_data, aes(x = "Banknote Figures", y = percent_plot, fill = gender)) +
  geom_col(width = 0.3, color = "white", size = 2) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 3.5, fontface = "bold", vjust = 8) +
  scale_fill_manual(values = c("M" = "#800020", "F" = "#FFD700")) +
  coord_flip() +
  theme_minimal() +
  labs(title = NULL, x = NULL, y = NULL) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA)
  )




data %>%
  distinct(name, gender) %>% 
  count(gender) %>%
  mutate(percent = n / sum(n) * 100,
         label = paste0(ifelse(gender == "M", "Male", "Female"),
                        " - ", n, " (", round(percent, 0), "%)"),
         percent_plot = ifelse(gender == "F", -percent, percent)) %>%
  ggplot(aes(x = "Banknote Figures", y = percent_plot, fill = gender)) +
  geom_col(width = 0.3, color = "white", size = 0.5) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 3.5, fontface = "bold", vjust = 8.5) +
  scale_fill_manual(values = c("M" = "#800020", "F" = "#FFD700")) +
  coord_flip() +
  theme_minimal() +
  labs(title = NULL, x = NULL, y = NULL) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA)
  )

ggsave("gender_distribution.png", plot = last_plot(), bg = "transparent", width = 6, height = 2.5)

ggplot(notes, aes(x = gender)) +
  geom_bar(stat = "identity", fill = c("#FFD700","#800020"), width = 0.5) +
  coord_polar("y", start=0) +
  theme_minimal()
ggplot(notes, aes(x = gender, fill = gender)) +
  geom_bar(width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("F" = "#FFD700", "M" = "#800020")) +
  theme_minimal()

library(forcats)
notes %>%
  distinct(name, profession) %>% 
  ggplot(aes(x = fct_rev(fct_infreq(profession)))) +
  geom_bar(fill = "orange", width = 0.5) +
  labs(title = "", x = "", y = "") +
  coord_flip() +
  theme_minimal() 

sum(notes$appearanceDeathDiff < 0, na.rm = T) * 100 /  length(notes$appearanceDeathDiff)
length(notes$appearanceDeathDiff < 0) * 100 /  length(notes$appearanceDeathDiff)
ggplot(notes, aes(x = appearanceDeathDiff)) + 
  geom_histogram(fill="#69b3a2", binwidth = 20)
library(tidyverse)
notes %>%
  distinct(name, gender, firstAppearanceDate) %>%
  group_by(firstAppearanceDate, gender) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(gender, firstAppearanceDate) %>%
  group_by(gender) %>%
  mutate(cumulative = cumsum(count)) %>%
  ggplot(aes(x = firstAppearanceDate,
             y = cumulative, color = gender)) +
  geom_line(size = 1.5) + 
  scale_color_manual(values = c("F" = "#FFD700", "M" = "#800020"),
                     labels = c("Female", "Male")) + 
  theme_minimal()

ggplot(notes, aes(x = gender, y = currentBillValue, fill = gender)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("F" = "#FFD700", "M" = "#800020"),
                    labels = c("Female", "Male")) +
  theme_minimal()


ggplot(notes, aes(x = gender, y = currentBillValue, fill = gender)) + 
  geom_boxplot(outlier.shape = NA) + 
  scale_fill_manual(values = c("F" = "#FFD700", "M" = "#800020"),
                    labels = c("Female", "Male")) +
  coord_cartesian(ylim = c(0, 2500)) +
  theme_minimal()

ggplot(notes, aes(x = gender, y = currentBillValue, fill = gender)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(color="black", size = 0.7, alpha = 0.9) +
  scale_fill_manual(values = c("F" = "#FFD700", "M" = "#800020"),
                    labels = c("Female", "Male")) +
  coord_cartesian(ylim = c(0, 2500)) +
  theme_minimal()
