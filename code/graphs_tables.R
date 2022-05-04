# Setting the ggplot theme
ester_theme <- function() {
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # text = element_text(family = "PT-Regular"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )
}


# Set colours -------------------------------------------------------------

# https://coolors.co/00759c

# Create colour palette for ggplot
mypalette = 'Set1'
colourquarantine = "#00759c"
colourisolation = "#9E2800" 
  



create_figure_incidence <- function(df, demographiedaten){
  # Quarantines and isolation by population per age group
  incidence <- df %>%
    group_by(AgeGroup, DatensatzKategorie) %>%
    summarise(n = n(), .groups = 'drop') %>%
    left_join(demographiedaten, by= c("AgeGroup" = "name") ) %>%
    rename(N = value) %>%
    mutate(incidence = round(100*n/N)) %>%
    mutate(DatensatzKategorie = recode_factor(DatensatzKategorie, "COVID-19" = "isolation", "Kontakt-COVID-19" = "quarantine"))
p1 <- incidence %>%
  mutate(AgeGroup = recode_factor(AgeGroup, "0 to 6" = "0 to 6", "7 to 17" = "7 to 17", "18 to 64" = "18 to 64", "65 to 110" = "65 to 110", .ordered = TRUE)) %>%
  filter(DatensatzKategorie == "isolation") %>%
  mutate(labelvalue = paste(incidence, "%")) %>%
  ggplot(aes(x=AgeGroup, y=incidence, fill=DatensatzKategorie)) +
  ester_theme() +
  geom_col() +
  scale_fill_brewer(palette = mypalette) +
  geom_text(aes(label = labelvalue), nudge_y = 3) +
  xlab("") +
  ylab("isolations per 100 inhabitants") +
  ggtitle("Isolations by age group") +
  theme(legend.position = "none")
p2 <- incidence %>%
  mutate(AgeGroup = recode_factor(AgeGroup, "0 to 6" = "0 to 6", "7 to 17" = "7 to 17", "18 to 64" = "18 to 64", "65 to 110" = "65 to 110", .ordered = TRUE)) %>%
  filter(DatensatzKategorie == "quarantine") %>%
  mutate(labelvalue = paste(incidence, "%")) %>%
  ggplot(aes(x=AgeGroup, y=incidence, fill=DatensatzKategorie)) +
  ester_theme() +
  geom_col(fill = brewer.pal(3, mypalette)[2]) +
  # scale_fill_brewer(palette = mypalette[2]) +
  geom_text(aes(label = labelvalue), nudge_y = 3) +
  xlab("") +
  ylab("quarantines per 100 inhabitants") +
  ggtitle("Quarantine by age group") +
  theme(legend.position = "none")

p_gesamt <- grid.arrange(p1,p2)
ggsave("graph/incidence.eps", p_gesamt, width = 7, height = 7)
"graph/incidence.eps"

}




create_figure_duration <- function(df, demographiedaten){
  colourquarantine = "#00759c"
  colourisolation = "#9E2800" 
p1 <- df %>%
  filter(DatensatzKategorie == "Kontakt-COVID-19") %>% 
  filter(!is.na(Q_Duration)) %>% 
  group_by(AgeGroup) %>% 
  ggplot(aes(factor(AgeGroup), as.numeric(dauer))) +
  ester_theme() +
  geom_boxplot(outlier.shape = NA, fill = colourquarantine) +
  facet_grid(~Q_Duration) +
  coord_cartesian(ylim = c(0, 23)) +
  ylab("duration in days") +
  xlab("") +
  ggtitle("Duration of quarantine in Reinickendorf Berlin")  +
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1))
p2 <- df %>% 
  filter(DatensatzKategorie == "COVID-19") %>% 
  filter(!is.na(Q_Duration)) %>% 
  group_by(AgeGroup) %>% 
  ggplot(aes(factor(AgeGroup), as.numeric(dauer))) +
  ester_theme() +
  geom_boxplot(outlier.shape = NA) +
  # geom_violin(scale = "count") +
  facet_grid(~I_Duration) +
  coord_cartesian(ylim = c(0,28)) +
  geom_boxplot(outlier.shape = NA, fill = colourisolation) +
  ylab("duration in days") +
  xlab("") +
  ggtitle("Duration of isolation in Reinickendorf Berlin") +
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1))

p_gesamt <- grid.arrange(p1,p2)
ggsave("graph/duration.eps", p_gesamt)
"graph/duration.eps"
}






create_figure_adjoining <- function(df, demographiedaten, resultslist, externalinput){
  
  ueberlappendeQundI <- df %>%
    filter(AbsonderungVon > externalinput$StartDateKP & AbsonderungVon < externalinput$EndDateKP) %>% 
    group_by(DatensatzKategorie, adjoiningQandI, AgeGroup, Q_Def) %>%
    count() %>% 
    mutate(result = NA) %>% 
    mutate(result = ifelse(adjoiningQandI == 0, "I_correct_after_Q", result)) %>% 
    mutate(result = ifelse(adjoiningQandI > 0, "I_too_long_after_Q", result)) %>% 
    mutate(result = ifelse(is.na(adjoiningQandI), "No_I_after_Q", result)) 
  
  
p1 <- ueberlappendeQundI %>%
  filter(DatensatzKategorie == "Kontakt-COVID-19") %>%
  group_by(Q_Def, result) %>%
  summarise(n = sum(n)) %>%
  mutate(percentage = prop.table(n)*100) %>%
  ungroup() %>%
  filter(result == "I_correct_after_Q" | result == "I_too_long_after_Q") %>%
  mutate(Q_Def = recode_factor(Q_Def, "Q_Def_1" = "Def_1", "Q_Def_2" = "Def_2", "Q_Def_3" = "Def_3", .ordered = TRUE)) %>%
  mutate(result = recode_factor(result, "I_too_long_after_Q" = "Isolation missed", "I_correct_after_Q" = "Isolation starts after quarantine", .ordered = TRUE)) %>%
  ggplot(aes(x=Q_Def, y = percentage, fill = result)) +
  geom_col() +
  ester_theme() +
  scale_fill_brewer(palette=mypalette) +
  scale_y_continuous(limits = c(0,21)) +
  ylab("%") +
  xlab("Contact person definition period") +
  theme(legend.position = "none")

p2 <- ueberlappendeQundI %>%
  filter(DatensatzKategorie == "Kontakt-COVID-19") %>%
  group_by(AgeGroup, result) %>%
  summarise(n = sum(n)) %>%
  mutate(percentage = prop.table(n)*100) %>%
  ungroup() %>%
  filter(result == "I_correct_after_Q" | result == "I_too_long_after_Q") %>%
  mutate(result = recode_factor(result, "I_too_long_after_Q" = "Isolation missed", "I_correct_after_Q" = "Isolation starts after quarantine", .ordered = TRUE)) %>%
  ggplot(aes(x=AgeGroup, y = percentage, fill = result)) +
  geom_col() +
  ester_theme() +
  scale_fill_brewer(palette=mypalette) +
  scale_y_continuous(limits = c(0,21)) +
  ylab("%") +
  xlab("Age group") +
  theme(legend.position = "none")

p3 <- ueberlappendeQundI %>%
  filter(DatensatzKategorie == "COVID-19") %>%
  group_by(Q_Def, result) %>%
  summarise(n = sum(n)) %>%
  mutate(percentage = prop.table(n)*100) %>%
  ungroup() %>%
  filter(result == "I_correct_after_Q" | result == "I_too_long_after_Q") %>%
  mutate(Q_Def = recode_factor(Q_Def, "Q_Def_1" = "Def_1", "Q_Def_2" = "Def_2", "Q_Def_3" = "Def_3", .ordered = TRUE)) %>%
  mutate(result = recode_factor(result, "I_too_long_after_Q" = "Isolation missed", "I_correct_after_Q" = "Isolation starts after quarantine", .ordered = TRUE)) %>%
  ggplot(aes(x=Q_Def, y = percentage)) +
  geom_col() +
  ester_theme() +
  scale_y_continuous(limits = c(0,40)) +
  scale_fill_brewer(palette=mypalette) +
  ylab("%") +
  xlab("Contact person definition period") +
  theme(legend.position = "none")

p4 <- ueberlappendeQundI %>%
  filter(DatensatzKategorie == "COVID-19") %>%
  group_by(AgeGroup, result) %>%
  summarise(n = sum(n)) %>%
  mutate(percentage = prop.table(n)*100) %>%
  ungroup() %>%
  filter(result == "I_correct_after_Q" | result == "I_too_long_after_Q") %>%
  mutate(result = recode_factor(result, "I_too_long_after_Q" = "Isolation missed", "I_correct_after_Q" = "Isolation starts after quarantine", .ordered = TRUE)) %>%
  ggplot(aes(x=AgeGroup, y = percentage)) +
  geom_col() +
  ester_theme() +
  scale_fill_brewer(palette=mypalette) +
  scale_y_continuous(limits = c(0,40)) +
  ylab("%") +
  xlab("Age group") +
  theme(legend.position = "none")

p_gesamt_1 <- grid.arrange(p1,p2,
                           top = grid::textGrob("Percentage of quarantines that were followed by an isolation period", gp = grid::gpar(fontsize=14)),
                           ncol = 2)
p_gesamt_2 <- grid.arrange(p3,p4,
                           top = grid::textGrob("Percentage of isolations that were preceeded by a quarantine period", gp = grid::gpar(fontsize=14)),
                           ncol = 2)
p_gesamt <- grid.arrange(p_gesamt_1, p_gesamt_2)


ggsave("graph/adjoining.eps", p_gesamt, width = 7, height = 7)

"graph/adjoining.eps"
}



create_figure_epicurve <- function(df, demographiedaten, resultslist, externalinput){

  # Dates for the Q_Def subgraph
  start <- externalinput$StartDate  
  end <- externalinput$EndDate  
  date_q1_q2 <- externalinput$zeiten %>% filter(Q_Def == "Q_Def_1") %>% slice_max(dates) %>% pull(dates)
  date_q2_q3 <- externalinput$zeiten %>% filter(Q_Def == "Q_Def_2") %>% slice_max(dates) %>% pull(dates)
  date_q1 <- start + (as.numeric(date_q1_q2 - start) / 2) 
  date_q2 <- date_q1_q2 + (as.numeric(date_q2_q3 - date_q1_q2) / 2) 
  date_q3 <- date_q2_q3 + (as.numeric(end - date_q2_q3) / 2) 
  qdefheight = -150
  xadjustment = 30
  
  p <- df %>% 
    mutate(meldezeit = floor_date(AbsonderungVon, "week")) %>% 
    ggplot(aes(meldezeit, fill = DatensatzKategorie)) + 
    ester_theme() +
    geom_bar() +
    scale_fill_manual(values = c(colourisolation, colourquarantine)) +
    ylab("n") +
    xlab("") +
    ggtitle(paste("COVID-19 isolations and quarantines in a district of Berlin, Germany \n and contact person definition by the Robert Koch Institute")) +
    theme(legend.position = "none") + 
    # Add contact person defintion period
    annotate(geom = "text", x = dmy("15052020"), y = 950, label = "Start recording \n of quarantines", hjust = "left") +  
    annotate(geom = "curve", x = dmy("05062020"), y = 700, xend = dmy("25052020"), yend = 100,   curvature = .2, arrow = arrow(length = unit(2, "mm"))) +
    annotate(geom = "text", x = date_q1, y = qdefheight, label = "Q_1", hjust = "center") +
    geom_segment(aes(x = date_q1 - xadjustment, y = qdefheight, xend = start, yend = qdefheight), arrow = arrow(length = unit(2, "mm"))) +
    geom_segment(aes(x = date_q1 + xadjustment, y = qdefheight, xend = date_q1_q2 -10, yend = qdefheight), arrow = arrow(length = unit(2, "mm"))) +
    annotate(geom = "text", x = date_q2, y = qdefheight, label = "Q_2", hjust = "center") +
    annotate(geom = "text", x = date_q3, y = qdefheight, label = "Q_3", hjust = "center") +
    geom_segment(aes(x = date_q3 - xadjustment, y = qdefheight, xend = date_q2_q3 + 10, yend = qdefheight), arrow = arrow(length = unit(2, "mm"))) +
    geom_segment(aes(x = date_q3 + xadjustment, y = qdefheight, xend = end, yend = qdefheight), arrow = arrow(length = unit(2, "mm"))) +
    geom_segment(aes(x = date_q1_q2, y = -250, xend = date_q1_q2, yend = -50)) +
    geom_segment(aes(x = date_q2_q3, y = -250, xend = date_q2_q3, yend = -50)) 
  
  
  ggsave("graph/epicurve.eps", p, width = 9, height = 4.5)
  ggsave("graph/epicurve.png", p, width = 9, height = 4.5)
  
  "graph/epicurve.eps"
}



