# Setting the ggplot theme
ester_theme <- function() {
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # text = element_text(family = "arial"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )
}


# Set colours -------------------------------------------------------------

# https://coolors.co/00759c

# Create colour palette for ggplot
colourquarantine = "#00759c"
colourisolation = "#9E2800" 
triplecolours <- c("#009E5C", "#00759C", "#9E2800")

  


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



create_figure_adjoining <- function(df) {

  triplecolours <- c("#009E5C", "#00759C", "#9E2800")
  
  p1 <-   df %>% 
  filter(DatensatzKategorie == "COVID-19") %>% 
  filter(result != "outside_of_kp_time") %>%
  mutate(Q_Def = recode_factor(Q_Def, "Q_Def_1" = "Def_1", "Q_Def_2" = "Def_2", "Q_Def_3" = "Def_3", .ordered = TRUE)) %>%
  mutate(result = recode_factor(result, "I_too_long_after_Q" = "Isolation missed", "I_correct_after_Q" = "Isolation starts after quarantine", .ordered = TRUE)) %>% 
  group_by(Q_Def, AgeGroup, result) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Q_Def, AgeGroup) %>%
  mutate(p = round(100*count/sum(count))) %>%
  filter(result != "No_I_after_Q") %>%
  ggplot(aes(x = AgeGroup, y = p)) +
  geom_col(fill = triplecolours[2]) +
  facet_wrap(~Q_Def) +
  ester_theme() +
  ylab("%") +
  xlab("Age group") +
  theme(legend.position = "none")  
                                   
                                   
p2 <- df %>% 
  filter(DatensatzKategorie == "Kontakt-COVID-19") %>% 
  filter(result != "outside_of_kp_time") %>%
  mutate(Q_Def = recode_factor(Q_Def, "Q_Def_1" = "Def_1", "Q_Def_2" = "Def_2", "Q_Def_3" = "Def_3", .ordered = TRUE)) %>%
  mutate(result = recode_factor(result, "I_too_long_after_Q" = "Isolation 1 to 7 days after quarantine", "I_correct_after_Q" = "Isolation directly after quarantine", .ordered = TRUE)) %>% 
  group_by(Q_Def, AgeGroup, result) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Q_Def, AgeGroup) %>%
  mutate(p = round(100*count/sum(count))) %>%
  filter(result != "No_I_after_Q") %>%
  ggplot(aes(x = AgeGroup, y = p, fill = result)) +
  geom_col() +
  facet_wrap(~Q_Def) +
  ester_theme() +
  ylab("%") +
  xlab("Age group") +
  scale_fill_manual('', values = c(triplecolours[3], triplecolours[1])) 

p_gesamt_1 <- grid.arrange(p1,
                           top = grid::textGrob("Percentage of isolations that were preceded by a quarantine period", gp = grid::gpar(fontsize=14)),
                           ncol = 1)
p_gesamt_2 <- grid.arrange(p2,
                           top = grid::textGrob("Percentage of quarantines that were followed by an isolation period", gp = grid::gpar(fontsize=14)),
                           ncol = 1)
p_gesamt <- grid.arrange(p_gesamt_1, p_gesamt_2)

ggsave("graph/adjoining.eps", p_gesamt, width = 7, height = 5)

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
  
  
  ggsave("graph/epicurve.png", p, width = 9, height = 4.5)
  ggsave("graph/epicurve.eps", p, width = 9, height = 4.5)
  
  "graph/epicurve.eps"
}



create_measures_table <- function(resultslist) {
  resultslist <- resultslist
  measures_table <- resultslist$total_table %>% rename(myvar = total) %>% 
    bind_rows(resultslist$agegroup_table %>% rename(myvar = AgeGroup)) %>% 
    bind_rows(resultslist$qdef_table %>% rename(myvar = Q_Def)) %>% 
    select(-q_sum_in_y, -i_sum_in_y) %>% 
    mutate_all(.funs = as.character)
  
  
  print(xtable(measures_table, type = "latex"), file = "graph/measures_table.tex", include.rownames=FALSE)
  "graph/measures_table.tex"
}


create_figure_inclusionexclusion <- function(resultslist){
  # Serves the scaling of the graph
  commondivider <- 100000 / 7
  
  grViz("
digraph boxes_and_circles {

  graph [layout = dot,
       rankdir = LR]

subgraph {
rank = same; 
  Queried[label = 'Queried \n N = @@1-2',
        shape = box,
        fontname = Helvetica,
        width = @@1-1]

  Available_Dates[label = 'No missing dates \n N = @@2-3',
        shape = box,
        fontname = Helvetica,
        width = @@2-1]

  Correct_ID[label = 'Valid person ID \n N = @@3-3',
        shape = box,
        fontname = Helvetica,
        width = @@3-1]

  Inside_Studyperiod[label = 'Beginning in study period \n N = @@4-3',
        shape = box,
        fontname = Helvetica,
        width = @@4-1]
        
  Typing_Correct[label = 'Dates without typing error \n N = @@5-3',
        shape = box,
        fontname = Helvetica,
        width = @@5-1]
        
  Unique[label = 'Not duplicated \n N = @@6-3',
        shape = box,
        fontname = Helvetica,
        width = @@6-1]
}

  Queried             ->  Available_Dates
  Available_Dates     ->  Correct_ID
  Correct_ID          ->  Inside_Studyperiod
  Inside_Studyperiod  ->  Typing_Correct
  Typing_Correct      ->  Unique

 }

[1]: c(resultslist$queried / commondivider, resultslist$queried)
[2]: c( (resultslist$queried - resultslist$emptydates)  / commondivider,  resultslist$emptydates / commondivider,  (resultslist$queried - resultslist$emptydates),  resultslist$emptydates)
[3]: c( (resultslist$queried - resultslist$emptydates - resultslist$wrongid)  / commondivider, resultslist$wrongid / commondivider, (resultslist$queried - resultslist$emptydates - resultslist$wrongid), resultslist$wrongid)
[4]: c( (resultslist$queried - resultslist$emptydates - resultslist$wrongid - resultslist$outofrange)  / commondivider, resultslist$outofrange / commondivider, (resultslist$queried - resultslist$emptydates - resultslist$wrongid - resultslist$outofrange), resultslist$outofrange)
[5]: c( (resultslist$queried - resultslist$emptydates - resultslist$wrongid - resultslist$outofrange - resultslist$typingerror)  / commondivider, resultslist$typingerror / commondivider, (resultslist$queried - resultslist$emptydates - resultslist$wrongid - resultslist$outofrange - resultslist$typingerror), resultslist$typingerror)
[6]: c( (resultslist$queried - resultslist$emptydates - resultslist$wrongid - resultslist$outofrange - resultslist$typingerror - resultslist$deleted_duplicates_quarantines - resultslist$deleted_duplicates_isolations)  / commondivider, (resultslist$deleted_duplicates_quarantines + resultslist$deleted_duplicates_isolations) / commondivider, (resultslist$queried - resultslist$emptydates - resultslist$wrongid - resultslist$outofrange - resultslist$typingerror - resultslist$deleted_duplicates_quarantines - resultslist$deleted_duplicates_isolations), (resultslist$deleted_duplicates_quarantines + resultslist$deleted_duplicates_isolations))
")  %>% 
    export_svg() %>%
    charToRaw %>% 
    rsvg_eps("graph/inclusionexclusion.eps")
  "graph/inclusionexclusion.eps"
}
