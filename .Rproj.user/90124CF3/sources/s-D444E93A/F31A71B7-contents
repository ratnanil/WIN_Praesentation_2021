
library(tidyverse)
library(lubridate)

jpgs_nkm02 <- read_csv("Data/Nistkammer-Monitoring/nkm02_datetimeoriginal.csv")
jpgs_nkm02 <- jpgs_nkm02%>%
  mutate(
    DateTimeOriginal = as.POSIXct(DateTimeOriginal,format = "%Y:%m:%d %H:%M:%S")
  ) %>%
  arrange(DateTimeOriginal) %>%
  mutate(
    timediff = as.integer(difftime(lead(DateTimeOriginal),DateTimeOriginal,units = "secs")),
    timediff_small = timediff<30,
    timediff_small_inverse = as.integer(!timediff_small),
    id = cumsum(!timediff_small),
    # id = ifelse(timediff_small,id,NA)
  )

jpgs_smry <- jpgs_nkm02 %>%
  group_by(timediff_small,id) %>%
  summarise(
    start = min(DateTimeOriginal),
    end = max(DateTimeOriginal)
  ) %>%
  mutate(
    diff = as.integer(difftime(end,start,units = "secs"))
  )


breaks <- unlist(map(c(2017,2018),~paste(.x,str_pad(seq(2,12,2),2,pad = 0),sep = "-")))
labels <- month.abb[as.integer(str_match(breaks, "\\d{4}-(\\d{2})")[,2])]

jpgs_smry %>%
  filter(start < "2018-08-01") %>%
  group_by(
    yearmonth = paste(year(start),str_pad(month(start),2,pad = 0),sep = "-")
  ) %>%
  count() %>% 
  ggplot(aes(yearmonth,n)) +
  geom_col(fill = "grey") +
  scale_x_discrete(breaks = breaks, labels = labels) +
  # scale_fill_discrete() +
  labs(y = "Anzahl registrierte Besuche",x = "") +
  theme_classic() +
  theme(legend.position = "none", 
        text = element_text(colour = "grey"),
        axis.text = element_text(colour = "grey"),
        axis.line = element_line(colour = "grey"),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA))

ggsave("images/nistkastenmonitoring.png",bg = "transparent")


