avg_score_ship_prod <- child_bothtasks_first %>% 
  filter(criteria == "Production (Same Language)" & language == "Spanish") %>%
  group_by(subj) %>%
  summarise(age = first(age), 
            mean_score = mean(score, na.rm = T))

cor.test(avg_score_ship_prod$mean_score, avg_score_ship_prod$age)


child_data %>%
  filter(task == "Production" & language == "Shipibo") %>%
  mutate(spanish_use = ifelse(response %in% spanish_terms, 1, 0)) %>%
  summarise(spanish_use = mean(spanish_use))

child_data %>%
  filter(task == "Production" & language == "Shipibo") %>%
  mutate(spanish_use = ifelse(response %in% spanish_terms, 1, 0)) %>%
  group_by(subj) %>%
  summarise(spanish_use = mean(spanish_use)) %>%
  mutate(over50 = ifelse(spanish_use >= 0.5, 1, 0),
         over25 = ifelse(spanish_use >= 0.25, 1, 0)) %>%
  ungroup() %>%
  summarise(over50 = mean(over50),
            over25 = mean(over25))

age_spanish <- child_data %>%
  filter(task == "Production" & language == "Shipibo") %>%
  mutate(spanish_use = ifelse(response %in% spanish_terms, 1, 0)) %>%
  group_by(subj) %>%
  summarise(age = first(age),spanish_use = mean(spanish_use))

cor.test(age_spanish$age, age_spanish$spanish_use)


production_entropy <- curr_naming_consensus %>%
  filter(`Chip ID` %in% all_chip_sets$chip_id) %>%
  mutate(`% of Subjects` = `% of Subjects`/100) %>%
  spread(key = 'Color Term', value = '% of Subjects', fill = 0) %>%
  mutate_if(is.double, funs( . * log(., base = n_distinct(curr_naming_consensus$`Color Term`)))) %>%
  mutate_if(is.double, funs(replace(., is.nan(.), 0))) %>%
  ungroup() %>%
  mutate(Entropy = -rowSums(.[-1])) %>%
  select(`Chip ID`, Entropy)

age_spanish <- child_data %>%
  filter(task == "Production" & language == "Shipibo") %>%
  mutate(correct_switch = ifelse(is.na(correct_switch), 0, correct_switch),
         correct_either = ifelse(correct + correct_switch > 0, 1, 0),
         spanish_use = ifelse(response %in% spanish_terms, 1, 0)) %>%
  left_join(production_entropy %>% mutate(`Chip ID` = as.character(`Chip ID`)),
            by = c("prompt" = "Chip ID"))

age_spanish$age.years.c <- scale(age_spanish$age, scale = FALSE)[,1]


switch_entropy <- lme4::glmer(correct_either ~ age.years.c * Entropy + (1|subj), data=age_spanish,
                    family = "binomial")

same_entropy <- lme4::glmer(correct ~ age.years.c * Entropy + (1|subj), data=age_spanish,
                    family = "binomial")

summary(lme4::glmer(spanish_use ~ age.years.c * Entropy + (1|subj), data=age_spanish,
                            family = "binomial"))

AIC(switch_entropy, same_entropy)


age_shipibo <- child_data %>%
  filter(task == "Production" & language == "Spanish") %>%
  mutate(correct_switch = ifelse(is.na(correct_switch), 0, correct_switch),
         correct_either = ifelse(correct + correct_switch > 0, 1, 0),
         shipibo_use = ifelse(response %in% shipibo_terms, 1, 0)) %>%
  left_join(production_entropy %>% mutate(`Chip ID` = as.character(`Chip ID`)),
            by = c("prompt" = "Chip ID"))
age_shipibo$age.years.c <- scale(age_shipibo$age, scale = FALSE)[,1]

summarise(age_shipibo, shipibo_use = mean(shipibo_use))

summary(lme4::glmer(shipibo_use ~ age.years.c * Entropy + (1|subj), data=age_shipibo,
                    family = "binomial"))

summary(lme4::glmer(correct_either ~ age.years.c * Entropy + (1|subj), data=age_shipibo,
                    family = "binomial"))
summary(lme4::glmer(correct ~ age.years.c * Entropy + (1|subj), data=age_shipibo,
                    family = "binomial"))

child_figure <- ggplot(first_production %>%
                                  filter(prompt %in% between_chip_sets$chip_id) %>%
                                  rename(`Correct (Same Language)` = correct, 
                                         `Correct (Either Language)` = correct_either) %>%
                                  gather(key = "criteria", value = "score", 
                                         `Correct (Same Language)`, `Correct (Either Language)`) %>%
                                  group_by(language, prompt, age_interval, criteria) %>%
                                  left_join(production_entropy %>% 
                                              mutate(`Chip ID` = as.character(`Chip ID`),
                                                     Entropy = sprintf("%.2f", round(Entropy,2))), 
                                            by = c("prompt" = "Chip ID")),
                                aes(x = age_interval, y = score, colour = prompt, 
                                    group = criteria, linetype = criteria, label = Entropy)) +
  geom_smooth(se = FALSE, method = "loess") +
  facet_grid(as.numeric(as.character(prompt)) ~ language) + 
  scale_y_continuous(limits = c(-0.1,1.1), breaks = seq(0,1,0.5), labels = scales::percent) + 
  xlim(c(5,11.5)) +
  scale_color_manual(values = chip_id_colors) +
  scale_linetype_manual(values = c("Correct (Same Language)" = "solid", 
                                   "Correct (Either Language)" = "dotted")) +
  labs(x = "Age", y = "% of Correct Responses", linetype = "Measure", colour = "Chip ID") +
  guides(label = "none") +
  theme_few(10) +
  theme(strip.background = element_blank(), plot.margin = unit(c(1,0.5,1,0.5), "in")) +
  geom_label(aes(x = Inf, y = -Inf, hjust = 1.1, vjust = -0.3), show.legend = FALSE, colour = "black")


curr_consensus_plot <- ggplot(curr_highest_consensus %>%
                                left_join(color_chip_data, by = c("Chip ID" = "#cnum")), 
                             aes(x = H, y = V, colour = `Color Term`, 
                                 size = `% of Subjects`)) + 
  geom_point() + 
  scale_size(range = c(0, 2.0)) + 
  scale_colour_manual(name = "Color Term",values = graph_colors) +
  ggtitle("Study 1") +
  theme_few(10) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

kay_consensus_plot <- ggplot(kay_highest_consensus %>%
                               left_join(color_chip_data, by = c("Chip ID" = "#cnum")), 
                             aes(x = H, y = V, colour = `Color Term`, 
                                 size = `% of Subjects`)) + 
  geom_point() + 
  scale_size(range = c(0, 2.0)) + 
  scale_colour_manual(name = "Color Term",values = graph_colors) +
  ggtitle("WCS Data") +
  theme_few(10) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

spanish_use_plot <- ggplot(spanish_use, 
                           aes(x = H, y = V)) + 
  geom_tile(aes(fill = `Spanish Term`)) + 
  scale_fill_viridis(option="magma") +
  labs(fill = "Spanish\nUse (%)") +
  theme_few(10) +
  theme(legend.position = "none")

term_agreement_plot <- ggplot(full_join(median_agreement, term_usage) %>% filter(!is.na(`Median Euc Dist`)), 
                              aes(x = `Median Euc Dist`, y = `Term Usage`, colour = `Color Term`)) +
  geom_point(size = 2) +
  scale_colour_manual(name = "Color Term",values = graph_colors) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.25), labels = scales::percent) +
  ggrepel::geom_text_repel(aes(label = `Color Term`), point.padding = 0.25, size = 3) +
  labs(x = "Median Euclidean Distance from\nMedian Point (Study 2)", 
       y = "% of Subjects\nWho Used Term (Study 1)") +
  ggthemes::theme_few(10) +
  theme(legend.position = "none", panel.grid = element_blank())

adult_figure <- cowplot::plot_grid(
  curr_consensus_plot, kay_consensus_plot, 
  spanish_use_plot, NULL, 
  labels = c("A", "B", "C"), ncol = 2
  ) + 
  theme(plot.margin = unit(c(1,0.5,1.5,0.5), "in")) 


# cowplot::save_plot(filename = "working/figures/adult_figure.png", adult_figure,
#                    base_height = 8, base_width = 8, base_aspect_ratio = 1:1)
# 
# cowplot::save_plot(filename = "working/figures/child_figure.png", child_figure,
#                    base_height = 8, base_width = 8, base_aspect_ratio = 1:1)
# 
