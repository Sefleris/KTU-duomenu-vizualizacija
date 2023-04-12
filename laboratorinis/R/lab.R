library(readr)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)
lab_sodra = read_csv("laboratorinis/data/lab_sodra.csv")

data = lab_sodra %>%
  filter(lab_sodra$ecoActCode == '451100')

#Format month as date and create new value formatted_date for graphs
data = data %>%
  mutate(
    month = as.Date(paste0(data$month, "01"), format = "%Y%m%d"),
    formatted_date = format(month, "%y\'%m")
  )

summary(data)
unique = data %>% filter(avgWage<100)

# First plot
p = ggplot(data, aes(x = avgWage)) +
  geom_histogram(binwidth = 100, fill = "#0072B2", color = "black") +
  labs(title = "Vidutinių atlyginimų histograma", x = "Vidutinis atlyginimas", y = "Dažnumas") +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = 'white', color = 'white'),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dashed")
  )+
  scale_x_continuous(breaks = seq(0, max(data$avgWage[!is.na(data$avgWage) & is.finite(data$avgWage)]), by = 1000))

print(p)
ggsave("laboratorinis/img/Rplot1.png", p, width = 10, height = 4, dpi = 200)

#top 5 companies
top_5_companies = data %>%
  group_by(name) %>%
  summarise(avgWage = mean(avgWage, na.rm = TRUE)) %>%
  arrange(desc(avgWage)) %>%
  head(5)

# Filter data for the top 5 companies
top_5_data = data %>%
  filter(name %in% top_5_companies$name)

# Second plot
p2 = ggplot(top_5_data, aes(x = formatted_date, y = avgWage, group = name, color = name)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Top 5 dinamika",
       x = "Mėnesis",
       y = "Vidutinis atlyginimas") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, ),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    legend.title = element_blank()
    )


print(p2)
ggsave("laboratorinis/img/Rplot2.png", p2, width = 10, height = 4, dpi = 200)


#Filter max number for the 5 companies
top_5_maxInsured = top_5_data %>%  
  group_by(name) %>% 
  top_n(numInsured,n=1) %>% 
  distinct(name,numInsured)

# Third plot 
p3= ggplot(top_5_maxInsured, aes(x = (reorder(name,-numInsured)), y = numInsured, fill=name))+
  geom_col()+
  labs(title= "Apdraustųjų skaičius",
       x="Įmonė", y="Darbuotojų skaičius") +
  theme_classic()+
  theme(
    panel.background = element_rect(fill = 'white', color = 'white'),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 8, face = "bold.italic"),
    legend.title = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dashed")
  )+
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))+
  scale_y_continuous(breaks = seq(0, max(data$numInsured[!is.na(data$numInsured) & is.finite(data$numInsured)]), by = 5))
    
print(p3)
ggsave("laboratorinis/img/Rplot3.png", p3, width = 10, height = 6, dpi = 200)

