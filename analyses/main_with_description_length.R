library(here)
library(tidyverse)
library(ggplot2)

library(openxlsx)
orig_sensitive = read.xlsx("new_sensitive_to_context.xlsx")
orig_sensitive[c(1, 13, 20)] <- NULL

# FIGURE 1

calculate_correlation <- function(metric) {
  cor_blv <- cor.test(orig_sensitive[[metric]], orig_sensitive$hypothesis_avg_overall_blv_rating)
  cor_sighted <- cor.test(orig_sensitive[[metric]], orig_sensitive$hypothesis_avg_overall_sighted_rating)
  
  data.frame(
    Metric = metric,
    Group = c("BLV", "Sighted"),
    Correlation = c(cor_blv$estimate, cor_sighted$estimate),
    P_Value = c(cor_blv$p.value, cor_sighted$p.value),
    Significance = c(
      ifelse(cor_blv$p.value < 0.001, "***",
             ifelse(cor_blv$p.value < 0.01, "**",
                    ifelse(cor_blv$p.value < 0.05, "*", ""))),
      ifelse(cor_sighted$p.value < 0.001, "***",
             ifelse(cor_sighted$p.value < 0.01, "**",
                    ifelse(cor_sighted$p.value < 0.05, "*", "")))
    )
  )
}

metrics <- c("Bleu_1", "Bleu_2", "Bleu_3", "Bleu_4", "METEOR", "ROUGE_L")
correlations <- bind_rows(lapply(metrics, calculate_correlation))

ggplot(correlations, aes(x = Metric, y = Correlation, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.4) +
  geom_text(aes(label = Significance), color = "black", position = position_dodge(width = 0.9), vjust = 0.35, size=8) +
  scale_fill_brewer(palette="Set2") +
  labs(
    y = "Correlation Coefficient",
    fill = ""
  ) +
  theme_minimal() +
  scale_x_discrete(labels=c("BLEU-1", "BLEU-2", "BLEU-3", "BLEU-4", "METEOR", "ROUGE-L")) +
  theme(
    axis.text = element_text(size = 20),
    legend.text=element_text(size=22),
    axis.title=element_text(size=22),
    axis.text.x = element_text(colour = "black"),
    axis.title.x=element_blank(),
    legend.position = c(.2, .94),
    legend.direction = "horizontal",
    # legend.position = "top"
    plot.margin = margin(0.6,0.2,0.2,0.2, "cm")
  )

# FIGURE 2

calculate_correlation_difference <- function(df, metric) {
  cor_blv <- cor(df[[metric]], df$hypothesis_avg_overall_blv_rating)
  cor_sighted <- cor(df[[metric]], df$hypothesis_avg_overall_sighted_rating)
  
  data.frame(
    Metric = metric,
    Group = c("BLV", "Sighted"),
    Correlation = c(cor_blv, cor_sighted)
  )
}

cor_sensitive <- bind_rows(lapply(metrics, calculate_correlation_difference, df = orig_sensitive))
cor_different <- bind_rows(lapply(metrics, calculate_correlation_difference, df = different_context))
cor_difference <- cor_sensitive %>%
  inner_join(cor_different, by = c("Metric", "Group"), suffix = c("_sensitive", "_different")) %>%
  mutate(Correlation_Difference = Correlation_different - Correlation_sensitive)

ggplot(cor_difference, aes(x = Metric, y = Correlation_Difference, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.4) +
  scale_fill_brewer(palette="Set2") +
  labs(
    y = "Correlation Difference of \nContext-Insensitive Compared \nto Context-Sensitive Data",
    fill = ""
  ) +
  theme_minimal() +
  scale_x_discrete(labels=c("BLEU-1", "BLEU-2", "BLEU-3", "BLEU-4", "METEOR", "ROUGE-L")) +
  theme(
    axis.text = element_text(size = 20),
    legend.text=element_text(size=22),
    axis.title=element_text(size=22),
    axis.text.x = element_text(colour = "black"),
    axis.title.x=element_blank(),
    legend.position = c(.82, .90),
    legend.direction = "horizontal",
    # legend.position = "top"
    plot.margin = margin(0.6,0.2,0.2,0.2, "cm")
  )
