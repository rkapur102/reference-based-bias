library(here)
library(tidyverse)
library(ggplot2)

df_third <- read_csv("./merged.csv")
df_third_sampled <- read_csv("./merged_one_combn.csv")
df_third[c(1, 2, 20)] <- NULL
df_third_sampled[c(1, 2, 20)] <- NULL

graph_trend <- function(df) {
  df_long <- df %>%
    pivot_longer(cols = c(Bleu_1, Bleu_2, Bleu_3, Bleu_4, METEOR, ROUGE_L), 
                 names_to = "metric", 
                 values_to = "score")
  
  trend <- ggplot(df_long, aes(x = reference_count, y = score, color = metric)) +
    geom_point() +
    stat_smooth(method = "loess") + # Adds trend lines
    labs(title = "Metric Score vs. Reference Count",
         x = "Reference Count",
         y = "Accuracy") +
    theme_minimal()
  
  trend
}

graph_trend(df_third_sampled)
graph_trend(df_third)


elisa_sanity_check_plot <- function(df) {
  df_long <- df %>%
    pivot_longer(cols = c(Bleu_1, Bleu_2, Bleu_3, Bleu_4, METEOR, ROUGE_L), 
                 names_to = "metric", 
                 values_to = "score")
  
  df_long %>%
    filter(metric == "METEOR") %>%
    ggplot(., aes(x = reference_count, y = score, color = hypothesis)) + 
    geom_point(alpha=0.2, position=position_jitter(height=0, width=0.3)) + 
    stat_summary(fun="mean", 
                 geom="point") +
    stat_summary(fun.data = "mean_cl_boot", 
                 geom="errorbar", 
                 width=0.2) +
    theme(legend.position = "none")
}

elisa_sanity_check_plot(df_third)
elisa_sanity_check_plot(df_third_sampled)

create_summarized <- function(df) {
  df_summarized <- df %>%
    group_by(hypothesis, reference_count) %>%
    summarise(across(starts_with("Bleu"), list(mean = ~mean(.x, na.rm = TRUE), sd = ~ifelse(n() > 1, sd(.x, na.rm = TRUE), NA)), .names = "{col}_{fn}"),
              METEOR_mean = mean(METEOR, na.rm = TRUE),
              METEOR_sd = ifelse(n() > 1, sd(METEOR, na.rm = TRUE), NA),
              ROUGE_L_mean = mean(ROUGE_L, na.rm = TRUE),
              ROUGE_L_sd = ifelse(n() > 1, sd(ROUGE_L, na.rm = TRUE), NA)) %>%
    ungroup()
  
  df_summarized <- df_summarized %>%
    mutate(truncated_hypothesis = str_trunc(hypothesis, width = 50, side = "right", ellipsis = "..."))
  return(df_summarized)
}

df_summarized <- create_summarized(df_third)

library(plotly)
library(RColorBrewer)
create_plot_mean <- function(metric_name, df_summarized) {
  colors <- rep(brewer.pal(n=8, "Dark2"), length.out = n_distinct(df_summarized$hypothesis))
  shapes <- rep(0:25, length.out = n_distinct(df_summarized$hypothesis)) # 0:25 are the default ggplot shapes
  
  p <- ggplot(df_summarized, aes(x = reference_count, y = get(paste0(metric_name, "_mean")), color = truncated_hypothesis, shape = truncated_hypothesis)) +
    geom_point(size = 5) +
    labs(title = paste("Mean", metric_name, "Score vs. Reference Count"),
         x = "Reference Count",
         y = paste("Mean", metric_name, "Score")) + scale_colour_manual(values = colors) +
    scale_shape_manual(values=shapes) +
    theme_minimal()
  ggplotly(p)
  # return(p)
}

create_plot_mean("Bleu_2", df_summarized)

create_plot_sd <- function(metric_name) {
  colors <- rep(brewer.pal(n=8, "Dark2"), length.out = n_distinct(df_summarized$hypothesis))
  shapes <- rep(0:25, length.out = n_distinct(df_summarized$hypothesis)) # 0:25 are the default ggplot shapes
  
  df_summarized_no_na <- df_summarized %>% drop_na()
  p <- ggplot(df_summarized_no_na, aes(x = reference_count, y = get(paste0(metric_name, "_sd")), color = truncated_hypothesis, shape = truncated_hypothesis)) +
    geom_point(size = 5) +
    labs(title = paste(metric_name, "Score Standard Deviation vs. Reference Count"),
         x = "Reference Count",
         y = paste(metric_name, "Score Standard Deviation")) + scale_colour_manual(values = colors) +
    scale_shape_manual(values=shapes) +
    theme_minimal()
  ggplotly(p)
  # return(p)
}

create_plot_sd("Bleu_2")

create_plot_combined <- function(metric_name) {
  colors <- rep(brewer.pal(n=8, "Dark2"), length.out = n_distinct(df_summarized$hypothesis))
  shapes <- rep(0:25, length.out = n_distinct(df_summarized$hypothesis)) # 0:25 are the default ggplot shapes
  
  df_summarized_no_na <- df_summarized %>% drop_na()
  p <- ggplot(df_summarized_no_na, aes(x = reference_count, y = get(paste0(metric_name, "_mean")), color = truncated_hypothesis, shape = truncated_hypothesis)) +
    geom_point(size = 5) +
    geom_errorbar(aes(ymin = get(paste0(metric_name, "_mean")) - get(paste0(metric_name, "_sd")),
                      ymax = get(paste0(metric_name, "_mean")) + get(paste0(metric_name, "_sd"))),
                  width = 0.2) +
    labs(title = paste(metric_name, "Score vs. Reference Count"),
         x = "Reference Count",
         y = paste("Mean", metric_name, "Score")) + scale_colour_manual(values = colors) +
    scale_shape_manual(values=shapes) +
    theme_minimal()
  ggplotly(p)
  # return(p)
}

create_plot_combined("METEOR")

# without Plotly?

# Robustness Plot - Correlations with Ratings

df_third_sam

ggplot(data = df_third_sampled, aes(x = hypothesis_avg_overall_blv_rating, y = METEOR)) +
  facet_wrap(~ reference_count) +
  geom_point() +
  coord_cartesian(ylim=c(0,1), xlim=c(1,5)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + 
  xlab("Avg. Overall BLV Rating") + theme(
    axis.title.y = element_blank())

meteor_hyplength <- ggplot(data = orig_sensitive, aes(x = hypothesis_length, y = METEOR)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + theme(
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_text()) + 
  ylab("METEOR") + 
  xlab("Hypothesis Length") + 
  ggtitle("METEOR Length Correlations: \nSensitive To Context")
meteor_hyplength

data_long <- orig_sensitive %>%
  select(hypothesis_length, Bleu_1, Bleu_2, Bleu_3, Bleu_4, METEOR, ROUGE_L) %>%
  pivot_longer(cols = c(Bleu_1, Bleu_2, Bleu_3, Bleu_4, METEOR, ROUGE_L), 
               names_to = "metric", 
               values_to = "value")

levels(data_long$metric) <- c("Bleu-1", "Bleu-2", "Bleu-3", "Bleu-4", "METEOR", "ROUGE-L")
metric_names <- list(
  "Bleu_1"="Bleu-1", "Bleu_2"="Bleu-2", "Bleu_3"="Bleu-3", "Bleu_4"="Bleu-4", "METEOR"="METEOR", "ROUGE_L"="ROUGE-L"
)
metric_labeller <- function(variable,value){
  return(metric_names[value])
}

correlation_plot <- ggplot(data = data_long, aes(x = hypothesis_length, y = value)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + 
  theme_minimal() +
  theme(
    axis.title.x = element_text(),
    axis.title.y = element_text(),
    strip.text = element_text(face="bold")
  ) + 
  ylab("Metric Value") + 
  xlab("Hypothesis Length") + 
  # ggtitle("Length Correlations: Context-Sensitive Condition") +
  facet_wrap(~metric, scales = "free_y", labeller = metric_labeller)

correlation_plot

correlation_plot <- function(df, cor_with, title_thing) {
  metrics <- c("Bleu-1", "Bleu-2", "Bleu-3", "Bleu-4", "METEOR", "ROUGE-L")
  result = df %>%
    group_by_at("reference_count") %>%
    summarise(across(all_of(metrics), ~ cor(.x, get(cor_with), use = "complete.obs")))
  
  result_long <- result %>%
    pivot_longer(cols = all_of(metrics), names_to = "metric", values_to = "correlation")
  
  ggplot(result_long, aes(x = reference_count, y = correlation, color = metric, group = metric)) +
    # stat_smooth(method = "loess") +
    geom_line() +
    geom_point() +
    labs(title = paste("Correlation of Metrics by Group", title_thing, sep = " "),
         x = "Group",
         y = "Correlation (r value)",
         color = "Metric") +
    theme_minimal()
}

corr <- function(x, y) {
  return(cor.test(x, y, use = "complete.obs")$estimate)
}

pval <- function(x, y) {
  return(cor.test(x, y, use = "complete.obs")$p.value)
}

metrics <- c("Bleu_1", "Bleu_2", "Bleu_3", "Bleu_4", "METEOR", "ROUGE_L")
result <- df_third_sampled %>%
  group_by(reference_count) %>%
  summarise(across(all_of(metrics), ~ cor_test(.x, get(cor_with)), .groups = "drop"))

result <- df_third_sampled %>%
  group_by_at("reference_count") %>%
  summarise(across(all_of(metrics), ~ corr(.x, get(cor_with))) %>%
  summarise(across(all_of(metrics), ~ pval(.x, get(cor_with)))

result_long <- result %>%
  pivot_longer(cols = all_of(metrics), names_to = "metric", values_to = "correlation")

hyp_count <- df_third_sampled %>%
  group_by_at("reference_count") %>%
  summarise(num_unique_hyp = n_distinct(hypothesis))

result_long <- result %>%
  pivot_longer(cols = all_of(metrics), names_to = "metric", values_to = "correlation")

plot_data <- result_long %>%
  left_join(hyp_count, by = "reference_count")

new_correlation_plot <- function(df, cor_with, title_thing) {
  metrics <- c("Bleu_1", "Bleu_2", "Bleu_3", "Bleu_4", "METEOR", "ROUGE_L")
  result = df %>%
    group_by_at("reference_count") %>%
    summarise(across(all_of(metrics), ~ cor(.x, get(cor_with), use = "complete.obs")))
  
  result_long <- result %>%
    pivot_longer(cols = all_of(metrics), names_to = "metric", values_to = "correlation")
  
  hyp_count <- df %>%
    group_by_at("reference_count") %>%
    summarise(num_unique_hyp = n_distinct(hypothesis))
  
  # max_hyp <- max(hyp_count$num_unique_hyp)
  # hyp_count <- hyp_count %>%
  #   mutate(scaled_hyp = num_unique_hyp / max_hyp)
  
  plot_data <- result_long %>%
    left_join(hyp_count, by = "reference_count")
  
  p <- ggplot(hyp_count, aes(x = reference_count, y = num_unique_hyp)) +
    geom_col(fill = "gray", alpha = 0.3) +
    scale_y_continuous(
      name = "Number of Unique Hypotheses",
      sec.axis = sec_axis(~ ., name = "Number of Unique Hypotheses")
    ) +
    labs(title = paste("Correlation of Metrics by Reference Count", title_thing, sep = " "),
         x = "Reference Count") +
    theme_minimal()
  
  p + geom_line(data = result_long, aes(x = reference_count, y = correlation * 68, color = metric, group = metric)) +
    geom_point(data = result_long, aes(x = reference_count, y = correlation * 68, color = metric)) +
    scale_y_continuous(
      name = "Number of Unique Hypotheses",
      sec.axis = sec_axis(~ ./68, name = "Correlation (r value)")
    ) +
    labs(color = "Metric")
}

library(dplyr)
library(tidyr)
library(ggplot2)

df <- df_third

metrics <- c("Bleu_1", "Bleu_2", "Bleu_3", "Bleu_4", "METEOR", "ROUGE_L")

# correlations
correlations <- df %>%
  group_by(reference_count) %>%
  summarise(across(all_of(metrics), ~ cor(.x, get(cor_with), use = "complete.obs"), .names = "cor_{col}"), .groups = "drop")

# p-values
p_values <- df %>%
  group_by(reference_count) %>%
  summarise(across(all_of(metrics), ~ cor.test(.x, get(cor_with), use = "complete.obs")$p.value, .names = "pval_{col}"), .groups = "drop")

# combine
combined_results <- correlations %>%
  left_join(p_values, by = "reference_count") %>%
  pivot_longer(cols = starts_with("cor_") | starts_with("pval_"),
               names_to = c(".value", "metric"),
               names_pattern = "(cor|pval)_(.*)") %>%
  mutate(significant = pval < 0.05)

new_correlation_plot <- function(df, cor_with, title_thing) {
  metrics <- c("Bleu_1", "Bleu_2", "Bleu_3", "Bleu_4", "METEOR", "ROUGE_L")
  
  correlations <- df %>%
    group_by(reference_count) %>%
    summarise(across(all_of(metrics), ~ cor(.x, get(cor_with), use = "complete.obs"), .names = "cor_{col}"), .groups = "drop")
  
  p_values <- df %>%
    group_by(reference_count) %>%
    summarise(across(all_of(metrics), ~ cor.test(.x, get(cor_with), use = "complete.obs")$p.value, .names = "pval_{col}"), .groups = "drop")
  
  combined_results <- correlations %>%
    left_join(p_values, by = "reference_count") %>%
    pivot_longer(cols = starts_with("cor_") | starts_with("pval_"),
                 names_to = c(".value", "metric"),
                 names_pattern = "(cor|pval)_(.*)") %>%
    mutate(significant = pval < 0.05)
  
  plot_data <- combined_results %>%
    arrange(metric, reference_count) %>%
    group_by(metric) %>%
    mutate(next_significant = lead(significant),
           segment_alpha = ifelse(significant & next_significant, 1, 0.2)) %>%
    ungroup()
  
  hyp_count <- df %>%
    group_by(reference_count) %>%
    summarise(num_unique_hyp = n_distinct(hypothesis), .groups = "drop")
  
  plot_data <- plot_data %>%
    left_join(hyp_count, by = "reference_count")
  
  p <- ggplot(hyp_count, aes(x = reference_count, y = num_unique_hyp)) +
    geom_col(fill = "gray", alpha = 0.3) +
    scale_y_continuous(
      name = "Number of Unique Hypotheses",
      sec.axis = sec_axis(~ ., name = "Number of Unique Hypotheses")
    ) +
    labs(title = title_thing,
         x = "Reference Count") +
    theme_minimal() +
    theme(plot.title=element_text(size=12), 
          ) +
    scale_color_manual(name="Metric", values = c("#b0c6e3", "#759bce", "#2f5a94", "#1d385c", "#d21240", "#3fb055"),
                         labels=c("Bleu 1", "Bleu 2", "Bleu 3", "Bleu 4", "METEOR", "ROUGE-L"))
  
  p + geom_line(data = plot_data, aes(x = reference_count, y = cor * 68, color = metric, 
                                      group = metric, alpha = segment_alpha), linewidth = 1) +
    geom_point(data = plot_data, aes(x = reference_count, y = cor * 68, color = metric), 
               size = ifelse(plot_data$significant, 1.5, 1), 
               alpha = ifelse(plot_data$significant, 1, 0.2)) +
    scale_alpha_identity() +
    scale_y_continuous(
      name = "Number of Unique Hypotheses",
      sec.axis = sec_axis(~ ./68, name = "Correlation (R value)")
    ) +
    labs(color = "Metric") +
    scale_color_manual(name="Metric", values = c("#b0c6e3", "#759bce", "#2f5a94", "#1d385c", "#d21240", "#3fb055"),
                         labels=c("BLEU-1", "BLEU-2", "BLEU-3", "BLEU-4", "METEOR", "ROUGE-L"))
}

library(ggpubr)

cor_with <- "hypothesis_avg_overall_blv_rating"
# new_correlation_plot(df_third_sampled, cor_with, "Sampled")
p1 <- new_correlation_plot(df_third, cor_with, "BLV")
cor_with <- "hypothesis_avg_overall_sighted_rating"
p2 <- new_correlation_plot(df_third, cor_with, "Sighted")


plot <- ggarrange(
  p1, NULL,  p2,
  nrow = 1, widths = c(1, 0.15, 1),common.legend = TRUE, legend="right")
plot
# annotate_figure(plot, top = text_grob("Reference Count and Correlations of Metrics with Participant Rating", face = "bold", size=13))


ggplot(plot_data, aes(x = reference_count)) +
  geom_line(aes(y = correlation, color = metric, group = metric)) +
  geom_point(aes(y = correlation, color = metric, group = metric)) +
  geom_bar(aes(y = scaled_hyp), stat = "identity", fill = "gray", alpha = 0.3) +
  scale_y_continuous(
    limits = c(-0.275, 1),
    name = "Correlation (r value)",
    sec.axis = sec_axis(~ . * max_hyp, name = "Number of Unique Hypotheses")
  ) +
  labs(title = paste("Correlation of Metrics by Group", title_thing, sep = " "),
       x = "Group",
       color = "Metric") +
  theme_minimal()
