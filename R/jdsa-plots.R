source("R/general-stuff.R")

df <- read_rds("data/jdsa.rds")

df_plot <- df %>% 
  select(community, TSCHQ_q02_sex, THI_score, TQ_score, TFI_score, MDI_score, TBF12_score) %>%
  rename_with(~ str_replace(.x, "_score$", "")) %>%
  mutate(community = as.factor(paste0("C", community)))

make_boxplot <- function(col_name, index) {
  
  df <- df_plot %>% select(community, y = all_of(col_name)) %>% mutate(dummy = col_name)
  
  ggplot(df, aes(x = community, y = y, fill = community)) +
    scale_x_discrete(expand = c(0,0.5)) +
    # scale_y_continuous(expand = c(0,0,0.02,0)) +
    geom_boxplot(size = 0.3, outlier.size = 0.3, alpha = 0.75) +
    facet_wrap(~ dummy) +
    guides(fill = FALSE) +
    labs(x = NULL, y = NULL, tag = paste0("(", letters[index], ") ", col_name)) +
    scale_fill_manual(values = colorblindr::palette_OkabeIto[c(1,3:7)]) +
    # colorblindr::scale_fill_OkabeIto() +
    theme_minimal(base_size = 8, base_family = main_font, base_line_size = 8/40, base_rect_size = 8/40) +
    theme(axis.text = element_text(color = "black")) +
    theme(axis.ticks.y = element_line()) +
    # theme(axis.text = element_text(color = "black")) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(strip.text = element_text(size = rel(1), color = "black")) +
    theme(strip.background = element_rect(color = NA, fill = "gray90")) +
    theme(strip.text = element_blank()) +
    theme(plot.margin = margin(0, 1, 0, 1, unit = "mm"))
}

boxplots <- c("THI", "TQ", "TFI", "MDI", "TBF12") %>%
  imap(~ make_boxplot(.x, .y + 1))
boxplots



# df_gender <- df_plot %>% 
#   count(community, TSCHQ_q02_sex) %>%
#   group_by(community) %>%
#   mutate(community_label = paste0(community, " (N=", sum(n), ")")) %>%
#   mutate(sex_rel = n / sum(n)) %>%
#   mutate(sex_rel_lab = format(round(100 * sex_rel, 1), nsmall = 1)) %>%
#   ungroup() 
# df_gender
# 
# p_gender <- ggplot(df_gender, aes(y = fct_rev(community_label), x = sex_rel*100, fill = fct_rev(TSCHQ_q02_sex))) +
#   scale_x_continuous(expand = c(0,0,0.01,0)) +
#   scale_y_discrete(expand = c(0,0,0,1)) +
#   geom_segment(
#     data = tibble(
#       x = seq(0,100,25), xend = x,
#       y = 0.45, yend = 0.5 + nlevels(df_gender$community)
#     ),
#     aes(x = x, xend = xend, y = y, yend = yend),
#     color = "grey92", size = 0.4,
#     
#     inherit.aes = FALSE
#   ) +
#   geom_col() +
#   scale_fill_manual(values = rev(unname(colors))) +
#   guides(fill = FALSE) +
#   annotate("text", 
#            x = df_gender %>% 
#              filter(community == levels(community)[[1]], TSCHQ_q02_sex == "f") %>% 
#              pluck("sex_rel", 1) * 100/2,
#            y = nlevels(df_gender$community) + 0.75, 
#            label = "female", family = main_font, size = 8/.pt,
#            color = colors["female"]) +
#   annotate("text", 
#            x = df_gender %>% 
#              filter(community == levels(community)[[1]]) %>% 
#              summarize(100*(sex_rel[1] + 0.5 * sex_rel[2])) %>%
#              pluck(1, 1),
#            y = nlevels(df_gender$community) + 0.75, 
#            label = "male", family = main_font, size = 8/.pt,
#            color = colors["male"]) +
#   annotate("segment", x = 0, xend = 0, y = 0.5, yend = 0.5 + nlevels(df_gender$community)) +
#   annotate("segment", 
#            x = 100*mean(df_plot$TSCHQ_q02_sex == "f"), 
#            xend = 100*mean(df_plot$TSCHQ_q02_sex == "f"), 
#            y = 0.45, yend = 0.5 + nlevels(df_gender$community),
#            color = "gray30", linetype = "dashed", size = 0.4) +
#   labs(x = "Proportion (%)", y = NULL, tag = "(a)") +
#   theme_minimal(base_size = 8, base_family = main_font, base_line_size = 8/40, base_rect_size = 8/40) +
#   theme(panel.grid = element_blank()) +
#   theme(plot.margin = margin(l = 0, unit = "mm"))
# p_gender

df_mosaic <-
  df_plot %>%
  count(community, TSCHQ_q02_sex, name = "count") %>%
  group_by(community) %>%
  mutate(group_count = sum(count)) %>%
  ungroup() %>%
  group_by(TSCHQ_q02_sex) %>%
  mutate(group_count2 = sum(count)) %>%
  ungroup()

mosaic_labels <- df_mosaic %>%
  group_by(community) %>%
  filter(count > 0) %>%
  mutate(y = (cumsum(count) - 0.5*count)/group_count) %>%
  ungroup()


p_gender <-
ggplot(df_mosaic) +
  aes(x = count, y = community) +
  coord_cartesian(clip = "off") +
  geom_col(aes(fill = fct_rev(TSCHQ_q02_sex), width = group_count), #, 
           position = "fill", color = "white", size = 0.35,
           alpha = 0.75) +
  geom_text(
    data = mosaic_labels,
    aes(x = y, label = count),
    color = "black", #grey30",
    family = main_font, 
    size = 0.75*base_size/.pt,
    show.legend = FALSE,
    na.rm = TRUE
    # bg.color = "white"
  ) +
  facet_grid(community~., scales = "free_y", space = "free_y") +
  geom_text(
    data = df_mosaic %>% group_by(community) %>% slice(1),
    aes(x = 1.01, label = paste0("= ", group_count)),
    hjust = 0, size = 0.75*base_size/.pt, color = "black", # "grey30", 
    family = main_font
  ) +
  # annotate("text", x = 1.01, y = factor(1), label = "test") +
  scale_x_continuous(
    expand = c(0,0,0,0.1),
    breaks = df_mosaic %>%
      filter(community == levels(community)[nlevels(community)]) %>%
      distinct(community, TSCHQ_q02_sex, .keep_all = TRUE) %>% 
      mutate(rel = (cumsum(count) - 0.5*count)/sum(count)) %>%
      pull(rel),
    labels = paste0("= ", distinct(df_mosaic, TSCHQ_q02_sex, group_count2) %>% pull(group_count2)),
    sec.axis = sec_axis(
      trans = ~ .,
      breaks = df_mosaic %>%
        filter(community == levels(community)[1]) %>%
        group_by(TSCHQ_q02_sex) %>%
        summarize(count = sum(count)) %>%
        ungroup() %>%
        mutate(rel = (cumsum(count) - 0.5*count)/sum(count)) %>%
        pull(rel),
      labels = str_replace(as.character(levels(df_mosaic$TSCHQ_q02_sex)), ":.*", "")
    )
  ) +
  scale_y_discrete(expand = c(0,0)) +
  # colorblindr::scale_fill_OkabeIto() +
  scale_fill_manual(values = unname(colors)) +
  guides(fill = "none") +
  labs(x = NULL, y = NULL, fill = NULL, tag = "(a) Sex") +
  theme_minimal(
    base_family = main_font,
    base_size = base_size,
    base_line_size = base_size/40,
    base_rect_size = base_size/40
  ) +
  theme(legend.position = "top") +
  theme(legend.text = element_text(size = 0.75 * base_size)) +
  theme(legend.title = element_text(size = 0.9 * base_size)) +
  theme(legend.margin = margin(t = -0.15, b = -0.25, l = -2.25, unit = "cm")) +
  theme(legend.key.size = unit(0.4, "cm")) +
  theme(axis.text = element_text(size = 0.75*base_size, color = "black")) +
  theme(panel.grid = element_blank()) +
  theme(axis.ticks.y = element_line()) +
  theme(axis.ticks.x.top = element_line()) +
  theme(strip.text.y = element_blank()) +
  theme(panel.spacing.y = unit(0, "pt"))  +
  theme(plot.margin = margin(r = 5, unit = "mm"))


wrap_plots(c(list(p_gender), boxplots), nrow = 1) &
  theme(plot.tag.position = "bottom") &
  theme(plot.tag = element_text(margin = margin(t = 1, unit = "mm")))

path_out <- "figures/jdsa-community-summary"
ggsave(paste0(path_out, ".png"), width = 18, height = 5, unit = "cm", dpi = 600)
ggsave(paste0(path_out, ".pdf"), width = 18, height = 5, unit = "cm", device = cairo_pdf)

