source("R/general-stuff.R")

df <- dir("data", pattern = "-count\\.rds", full.names = TRUE) %>%
  map_dfr(read_rds) 
df <-
  df %>%
  mutate(chr_opening = str_extract(weight_disc, "^.")) %>%
  mutate(chr_first = str_replace(weight_disc, "^.(.+),.*", "\\1")) %>%
  mutate(chr_second = str_replace(weight_disc, "^.+,(.+).$", "\\1")) %>%
  mutate(chr_closing = str_extract(weight_disc, ".$")) %>%
  mutate(across(c(chr_first, chr_second), ~ format(as.numeric(.x), nsmall = 3))) %>%
  mutate(weight_disc2 = glue::glue("{chr_opening}{chr_first}, {chr_second}{chr_closing}")) %>%
  mutate(weight_disc2 = fct_inorder(weight_disc2))
df

ggplot(df, aes(x = n, y = weight_disc2, fill = sig_disc)) +
  scale_x_continuous(expand = c(0.015,0), labels = function(x) x * 100) +
  scale_y_discrete(expand = c(0,0.5,0,0)) +
  geom_col(position = "fill", width = 0.775) +
  facet_wrap(~ q, scales = "free_y", nrow = 1) +
  geom_text(
    data = df %>% 
      filter(q == "MDI", weight_disc2 == "(1.400, 11.200]") %>% 
      mutate(perc =  1 - (0.5 * n / sum(n) + lag(n / sum(n), default = 0))) %>% 
      mutate(label = c("pruned", "kept")),
    aes(x = perc, y = weight_disc2, label = label),
    size = 8/.pt, fontface = "italic"
  ) +
  guides(fill = FALSE) +
  labs(x = "Proportion of edges (%)", y = "Weight") +
  scale_fill_manual(values = c("gray70", colorblindr::palette_OkabeIto[[2]])) +
  theme_minimal(base_size = 8, base_family = main_font, base_line_size = 8/40, base_rect_size = 8/40) +
  # theme(axis.text = element_text(color = "black")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(panel.grid.major.x = element_line(linetype = "dashed", color = "gray70")) +
  theme(strip.text = element_text(size = rel(1), color = "black")) +
  theme(strip.background = element_rect(color = NA, fill = "gray90")) +
  theme(panel.spacing.x = unit(0.25, "cm")) +
  theme(axis.text.x = element_text(margin = margin(t = -0.05, unit = "mm"))) +
  theme(axis.text.y = element_text(margin = margin(r = -0.25, unit = "mm"))) +
  theme(plot.margin = margin(1,2,1,1,"mm"))

ggsave("figures/weight-significance-bar-charts.png", width = 18, height = 4.5, units = "cm", dpi = 600)
ggsave("figures/weight-significance-bar-charts.pdf", width = 18, height = 4.5, units = "cm", device = cairo_pdf)
