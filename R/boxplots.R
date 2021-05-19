source("R/general-stuff.R")

community_data <- read_rds("data/community_data.rds")

df_plot <- community_data %>% 
  select(community, TSCHQ_q02_sex, THI_score, TQ_score, TFI_score, MDI_score, TBF12_score) %>%
  mutate(community = as.factor(paste0("C", community)))

make_boxplot <- function(col_name, index) {
  
  df <- df_plot %>% select(community, y = all_of(col_name)) %>% mutate(dummy = col_name)
  
  ggplot(df, aes(x = community, y = y, fill = community)) +
    scale_x_discrete(expand = c(0,0.5)) +
    scale_y_continuous(expand = c(0.015,0)) +
    geom_boxplot(size = 0.35, outlier.size = 0.35) +
    facet_wrap(~ dummy) +
    guides(fill = FALSE) +
    labs(x = NULL, y = NULL, tag = paste0("(", letters[index], ")")) +
    colorblindr::scale_fill_OkabeIto() +
    theme_minimal(base_size = 8, base_family = main_font, base_line_size = 8/40, base_rect_size = 8/40) +
    # theme(axis.text = element_text(color = "black")) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(strip.text = element_text(size = rel(1), color = "black")) +
    theme(strip.background = element_rect(color = NA, fill = "gray90"))
}

boxplots <- c("THI_score", "TQ_score", "TFI_score", "MDI_score", "TBF12_score") %>%
  imap(~ make_boxplot(.x, .y + 1))
boxplots



df_gender <- df_plot %>% 
  count(community, TSCHQ_q02_sex) %>%
  group_by(community) %>%
  mutate(community_label = paste0(community, " (N=", sum(n), ")")) %>%
  mutate(sex_rel = n / sum(n)) %>%
  mutate(sex_rel_lab = format(round(100 * sex_rel, 1), nsmall = 1)) %>%
  ungroup() 
df_gender

p_gender <- ggplot(df_gender, aes(y = fct_rev(community_label), x = sex_rel*100, fill = fct_rev(TSCHQ_q02_sex))) +
  scale_x_continuous(expand = c(0,0,0.01,0)) +
  scale_y_discrete(expand = c(0,0,0,1)) +
  geom_segment(
    data = tibble(
      x = seq(0,100,25), xend = x,
      y = 0.45, yend = 0.5 + nlevels(df_gender$community)
    ),
    aes(x = x, xend = xend, y = y, yend = yend),
    color = "grey92", size = 0.4,
    
    inherit.aes = FALSE
  ) +
  geom_col() +
  scale_fill_manual(values = rev(unname(colors))) +
  guides(fill = FALSE) +
  annotate("text", 
           x = df_gender %>% 
             filter(community == levels(community)[[1]], TSCHQ_q02_sex == "f") %>% 
             pluck("sex_rel", 1) * 100/2,
           y = nlevels(df_gender$community) + 0.75, 
           label = "female", family = main_font, size = 8/.pt,
           color = colors["female"]) +
  annotate("text", 
           x = df_gender %>% 
             filter(community == levels(community)[[1]]) %>% 
             summarize(100*(sex_rel[1] + 0.5 * sex_rel[2])) %>%
             pluck(1, 1),
           y = nlevels(df_gender$community) + 0.75, 
           label = "male", family = main_font, size = 8/.pt,
           color = colors["male"]) +
  annotate("segment", x = 0, xend = 0, y = 0.5, yend = 0.5 + nlevels(df_gender$community)) +
  annotate("segment", 
           x = 100*mean(df_plot$TSCHQ_q02_sex == "f"), 
           xend = 100*mean(df_plot$TSCHQ_q02_sex == "f"), 
           y = 0.45, yend = 0.5 + nlevels(df_gender$community),
           color = "gray30", linetype = "dashed", size = 0.4) +
  labs(x = "Proportion (%)", y = NULL, tag = "(a)") +
  theme_minimal(base_size = 8, base_family = main_font, base_line_size = 8/40, base_rect_size = 8/40) +
  theme(panel.grid = element_blank()) +
  theme(plot.margin = margin(l = 0, unit = "mm"))
p_gender


wrap_plots(c(list(p_gender), boxplots), nrow = 1) &
  theme(plot.tag.position = "bottom") &
  theme(plot.margin = margin(0.5, 1, 0.5, 1, unit = "mm"))

path_out <- "figures/community-summary"
ggsave(paste0(path_out, ".png"), width = 18, height = 5, unit = "cm", dpi = 600)
ggsave(paste0(path_out, ".pdf"), width = 18, height = 5, unit = "cm", device = cairo_pdf)





# female = rgb(183, 51, 119, maxColorValue = 255),
# male = rgb(45, 169, 217, maxColorValue = 255)

# data <- data.frame(
#   group=LETTERS[1:5],
#   value=c(13,7,9,21,2)
# )
# 
# # Compute the position of labels
# data <- data %>% 
#   arrange(desc(group)) %>%
#   mutate(prop = value / sum(data$value) *100) %>%
#   mutate(ypos = cumsum(prop)- 0.5*prop )
# 
# # Basic piechart
# ggplot(data, aes(x="", y=prop, fill=group)) +
#   geom_bar(stat="identity", width=1, color="white") +
#   coord_polar("y", start=0) +
#   theme_void() + 
#   theme(legend.position="none") +
#   
#   geom_text(aes(y = ypos, label = group), color = "white", size=6) +
#   scale_fill_brewer(palette="Set1")
