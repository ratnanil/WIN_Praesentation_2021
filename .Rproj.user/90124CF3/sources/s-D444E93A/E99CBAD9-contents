library(tidyverse)


nachweiserfolg_ref <- read_delim("Data/Nachweiserfolge_zusammenstellung/nachweiserfolge_zusammenstellung.csv", ";") %>%
  mutate(
    Studie = paste(Bezeichnung,Jahr),
    nachweiserfolg = tunnel_mit_nachweisen / tunnel_total,
    erfolgskontrolle_name = ifelse(erfolgskontrolle,"Erfolgskontrollen","andere Untersuchungen"),
  )

vorliegende_studie <- c("Horgen 2019", "Horgen 2020", "Horgen 2014 - 2019")


nachweiserfolg_smry <- nachweiserfolg_ref %>%
  group_by(erfolgskontrolle_name) %>%
  summarise(
    tunnel = median(tunnel_total),
    nachweiserfolg_median = median(nachweiserfolg),
    nachweiserfolg_q75 = quantile(nachweiserfolg, 0.75),
    nachweiserfolg_q25 = quantile(nachweiserfolg, 0.25),
  ) %>%
  mutate(
    Studie = "Verteilung"
  )

nachweiserfolg_ref2 <- bind_rows(nachweiserfolg_ref, nachweiserfolg_smry) %>%
  mutate(
    Studie = fct_reorder(Studie,nachweiserfolg),
    erfolgskontrolle_name = fct_rev(erfolgskontrolle_name),
    Studie = fct_relevel(Studie,"Verteilung",after = 0)
  )



nachweiserfolg_ref2%>%
  ggplot(aes(nachweiserfolg,Studie, size = tunnel_total)) +
  geom_point(colour = "grey") +
  geom_pointrange(aes(x = nachweiserfolg_median, xmin = nachweiserfolg_q25, xmax = nachweiserfolg_q75, y = Studie),
                  inherit.aes = FALSE, colour = "grey") +
  labs(size = "Anzahl Spurentunnel", y = "", x = "Nachweiserfolg") +
  scale_x_continuous(labels = scales::percent_format(),limits = c(0,1)) +
  # scale_size(limits = c(20,100)) +
  # lims(x = c(0,100)) +
  theme_light() +
  theme(legend.position = "bottom") +
  facet_grid(erfolgskontrolle_name~., scales = "free_y",space = "free") +
  theme(legend.position = "none", 
        text = element_text(colour = "grey"),
        axis.text = element_text(colour = "grey"),
        axis.line = element_line(colour = "grey"),
        panel.grid.major.y = element_line(linetype = 3),
        panel.grid.major.x = element_line(linetype = 3),
        panel.grid.minor.x = element_line(linetype = 3),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA))


ggsave("images/nachweiserfolge_zusammenstellung.png",bg = "transparent")
