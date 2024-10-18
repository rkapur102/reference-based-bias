library(here)
library(tidyverse)

# SETUP

# From Kreiss et al. (2023) "Context Matters for Image Descriptions for Accessibility: Challenges for Referenceless Evaluation Metrics"
# https://github.com/elisakreiss/contextual-description-evaluation
df_blv <- read_csv("./behavioral_data/blv_data_all.csv")
df_sighted <- read_csv("./behavioral_data/sighted_data_all.csv")

blv <- df_blv %>% 
  group_by(context, img_id, description) %>% 
  summarize(hypothesis_avg_overall_blv_rating = mean(q_overall)) %>%
  ungroup() %>%
  rename(hypothesis = description)

blv_and_all_references <- blv %>%
  group_by(img_id) %>%
  mutate(all_references = list(unique(df_sighted$description))) %>%
  ungroup

sighted <- df_sighted %>%
  group_by(context, img_id, description) %>%
  summarize(hypothesis_avg_overall_sighted_rating = mean(q_overall.postimg)) %>%
  ungroup() %>%
  rename(hypothesis = description)

get_overall_sighted_rating <- function(hypothesis) {
  return(sighted$hypothesis_avg_overall_sighted_rating[which(sighted$hypothesis == hypothesis)])
}

get_avg_reference_length <- function(references) {
  references_vector <- unlist(references)
  lengths <- nchar(references_vector)
  return(mean(lengths))
}

get_max_reference_length <- function(references) {
  references_vector <- unlist(references)
  lengths <- nchar(references_vector)
  return(max(lengths))
}

get_min_reference_length <- function(references) {
  references_vector <- unlist(references)
  lengths <- nchar(references_vector)
  return(min(lengths))
}

# CONTEXT SENSITIVE
# use only the descriptions in the BLV, and then references can be expanded to sighted

# remove test images
blv_and_all_references <- blv_and_all_references[-(which(blv_and_all_references$img_id == "sculpture.png")),] 
blv_and_all_references <- blv_and_all_references[-(which(blv_and_all_references$img_id == "guitar.png")),]

library(purrr)

ids = unique(df_sighted$img_id)
for (id in ids) {
  all_for_image = unique(df_sighted$description[which(df_sighted$img_id == id)])
  print(length(all_for_image))
}

# all references are in the same image-context pair
remove_hypothesis <- function(hypothesis, context, img_id, descriptions) {
  # get descriptions for current img_id
  # current_img_id = img_id
  all_for_image = unique(df_sighted$description[which(df_sighted$img_id == img_id)])
  
  # gather all descriptions from different contexts
  to_exclude = unique(df_sighted$description[which(df_sighted$context != context)])
  to_exclude = append(to_exclude, hypothesis) # also want to exclude hypothesis
  
  # exclude any overlap
  return(list(all_for_image[!(all_for_image %in% to_exclude)]))
}

sensitive_to_context <- blv_and_all_references %>%
  rowwise() %>%
  mutate(references = remove_hypothesis(hypothesis, context, img_id, unlist(all_references))) %>%
  select(-all_references)

# 

remove_hypothesis_2 <- function(hypothesis, context, img_id, descriptions) {
  # get descriptions for current img_id
  # current_img_id = img_id
  all_for_image = unique(df_sighted$description[which(df_sighted$img_id == img_id)])
  
  # gather all descriptions from different contexts
  to_exclude = unique(df_sighted$description[which(df_sighted$context != context)])
  to_exclude = append(to_exclude, hypothesis) # also want to exclude hypothesis
  
  # exclude any overlap
  return(all_for_image[!(all_for_image %in% to_exclude)])
}

generate_rows <- function(hypothesis, context, img_id, hypothesis_avg_overall_blv_rating) {
  valid_references <- remove_hypothesis_2(hypothesis, context, img_id)
  reference_rows <- list()
  
  for (i in seq_along(valid_references)) {
    combinations <- combn(valid_references, i, simplify = FALSE)
    random_combination <- sample(combinations, 1)
    reference_rows <- append(reference_rows, list(data.frame(
      hypothesis = hypothesis,
      context = context,
      img_id = img_id,
      hypothesis_avg_overall_blv_rating = hypothesis_avg_overall_blv_rating,
      reference_count = i,
      references = I(list(random_combination[[1]]))
    )))
  }
  
  return(do.call(rbind, reference_rows))
}

generate_rows_all_combns <- function(hypothesis, context, img_id, hypothesis_avg_overall_blv_rating) {
  valid_references <- remove_hypothesis_2(hypothesis, context, img_id)
  reference_rows <- list()
  
  for (i in seq_along(valid_references)) {
    combinations <- combn(valid_references, i, simplify = FALSE)
    for (comb in combinations) {
      reference_rows <- append(reference_rows, list(data.frame(
        hypothesis = hypothesis,
        context = context,
        img_id = img_id,
        hypothesis_avg_overall_blv_rating = hypothesis_avg_overall_blv_rating,
        reference_count = i,
        references = I(list(comb))
      )))
    }
  }
  
  return(do.call(rbind, reference_rows))
}

# Generate dataframe with all combinations
sensitive_to_context_varied_reference_count_one_combn <- blv_and_all_references %>%
  rowwise() %>%
  do(generate_rows(.$hypothesis, .$context, .$img_id, .$hypothesis_avg_overall_blv_rating)) %>%
  ungroup() # %>%
# select(-all_references) # Remove unnecessary columns

library(writexl)
# sensitive_to_context_varied_reference_count[] <- lapply(sensitive_to_context_varied_reference_count, function(x) if(class(x) == 'AsIs') unlist(x) else x)

sensitive_to_context_varied_reference_count_one_combn$hypothesis_avg_overall_sighted_rating = lapply(sensitive_to_context_varied_reference_count_one_combn$hypothesis, get_overall_sighted_rating)

# add hypothesis character length columns
sensitive_to_context_varied_reference_count_one_combn <- sensitive_to_context_varied_reference_count_one_combn %>%
  # mutate(reference_count = length(unlist(references))) %>%
  mutate(hypothesis_length = nchar(hypothesis)) 

# add in to get avg reference length, max reference length, min reference

sensitive_to_context_varied_reference_count_one_combn$avg_reference_length = lapply(sensitive_to_context_varied_reference_count_one_combn$references, get_avg_reference_length)
sensitive_to_context_varied_reference_count_one_combn$max_reference_length = lapply(sensitive_to_context_varied_reference_count_one_combn$references, get_max_reference_length)
sensitive_to_context_varied_reference_count_one_combn$min_reference_length = lapply(sensitive_to_context_varied_reference_count_one_combn$references, get_min_reference_length)

write_csv(sensitive_to_context_varied_reference_count_one_combn, "./sensitive_to_context_varied_reference_count_one_combn.csv")
fwrite(sensitive_to_context_varied_reference_count_one_combn, file = "./sensitive_to_context_varied_reference_count_one_combn.csv", sep = ",", row.names = TRUE)

library(tidyverse)
library(data.table)

sensitive_to_context_varied_reference_count_one_combn %>% 
  rowwise() %>% 
  mutate_if(is.list, ~paste(unlist(.), collapse = '|')) %>% 
  write.csv('./sensitive_to_context_varied_reference_count_one_combn.csv', row.names = TRUE)

# mutate_if(~any(str_detect(., fixed('|'))), ~str_split(., fixed('|'))) to reverse

library(openxlsx)

write.xlsx(sensitive_to_context_varied_reference_count_all_combns, "./sensitive_to_context_varied_reference_count_all_combns.xlsx")



# View the resulting dataframe
print(new_dataframe)

# now, grab sighted overall rating from sighted df and insert into the above as a new column

sensitive_to_context$hypothesis_avg_overall_sighted_rating = lapply(sensitive_to_context$hypothesis, get_overall_sighted_rating)

# add number of references and hypothesis character length columns
sensitive_to_context <- sensitive_to_context %>%
  mutate(reference_count = length(unlist(references))) %>%
  mutate(hypothesis_length = nchar(hypothesis)) 

# add in to get avg reference length, max reference length, min reference

sensitive_to_context$avg_reference_length = lapply(sensitive_to_context$references, get_avg_reference_length)
sensitive_to_context$max_reference_length = lapply(sensitive_to_context$references, get_max_reference_length)
sensitive_to_context$min_reference_length = lapply(sensitive_to_context$references, get_min_reference_length)

# GRAPH

bleu1_reflength <- ggplot(data = sensitive_to_context, aes(x = avg_reference_length, y = Bleu_1)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

# CONTEXT INSENSITIVE
# use only the descriptions in the BLV, and then references can be expanded to sighted 

remove_hypothesis_and_context <- function(hypothesis, context, img_id, descriptions) {
  # get descriptions for current img_id
  # current_img_id = img_id
  all_for_image = unique(df_sighted$description[which(df_sighted$img_id == img_id)])
  
  # gather all descriptions from current context
  # current_context = context
  to_exclude = unique(df_sighted$description[which(df_sighted$context == context)])
  
  # exclude any overlap
  return(list(all_for_image[!(all_for_image %in% to_exclude)]))
}

different_context <- blv_and_all_references %>%
  rowwise() %>%
  mutate(references = remove_hypothesis_and_context(hypothesis, context, img_id, unlist(all_references))) %>%
  select(-all_references)

# now, grab sighted overall rating from sighted df and insert into the above as a new column

different_context$hypothesis_avg_overall_sighted_rating = lapply(different_context$hypothesis, get_overall_sighted_rating)

# add number of references and hypothesis character length columns
different_context <- different_context %>%
  mutate(reference_count = length(unlist(references))) %>%
  mutate(hypothesis_length = nchar(hypothesis)) 

# add in to get avg reference length, max reference length, min reference

different_context$avg_reference_length = lapply(different_context$references, get_avg_reference_length)
different_context$max_reference_length = lapply(different_context$references, get_max_reference_length)
different_context$min_reference_length = lapply(different_context$references, get_min_reference_length)

# fix mislabeling?? sculpture.png and guitar.png

# WRITE CSVs

library(openxlsx)

write.xlsx(sensitive_to_context, "sensitive_to_context.xlsx")
write.xlsx(different_context, "different_context.xlsx")

write_csv(sensitive_to_context, "sensitive_to_context.csv")
write_csv(different_context, "different_context.csv")

# GRAPHS

metrics_sensitive_to_context = read.xlsx("./metrics/metrics_sensitive_to_context.xlsx", cols=c(2:7))
metrics_different_context = read.xlsx("./metrics/metrics_different_context.xlsx", cols=c(2:7))

# concatenate

sensitive_to_context = cbind(sensitive_to_context, metrics_sensitive_to_context)
different_context = cbind(different_context, metrics_different_context)

# PLOTS

library(ggplot2)
library(ggpubr)

# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/ for below

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# overall blv vs. bleu 1 corrplot

sensitive_to_context$hypothesis_avg_overall_sighted_rating <- unlist(sensitive_to_context$hypothesis_avg_overall_sighted_rating)
sensitive_to_context$avg_reference_length <- unlist(sensitive_to_context$avg_reference_length)

bleu1_blv <- ggplot(data = sensitive_to_context, aes(x = hypothesis_avg_overall_blv_rating, y = Bleu_1)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1), xlim=c(1,5)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) + 
  # stat_cor(aes(label = after_stat(r.label)), color = "red", geom = "label", label.x = 1, label.y = 0.8) + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8)
bleu1_blv

bleu1_sighted <- ggplot(data = sensitive_to_context, aes(x = hypothesis_avg_overall_sighted_rating, y = Bleu_1)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1), xlim=c(1,5)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())
bleu1_sighted

bleu1_refcount <- ggplot(data = sensitive_to_context, aes(x = reference_count, y = Bleu_1)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

bleu1_hyplength <- ggplot(data = sensitive_to_context, aes(x = hypothesis_length, y = Bleu_1)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

bleu1_reflength <- ggplot(data = sensitive_to_context, aes(x = avg_reference_length, y = Bleu_1)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

row1 <- ggarrange(bleu1_blv, bleu1_sighted, bleu1_refcount, bleu1_hyplength, bleu1_reflength, 
                  ncol = 5, nrow = 1)
row1 <- annotate_figure(row1, left = text_grob("BLEU-1", face = "bold", rot = 90))

# ROW 2

bleu2_blv <- ggplot(data = sensitive_to_context, aes(x = hypothesis_avg_overall_blv_rating, y = Bleu_2)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1), xlim=c(1,5)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + 
  xlab("Avg. Overall BLV Rating") + theme(
    axis.title.y = element_blank())
bleu2_blv

bleu2_sighted <- ggplot(data = sensitive_to_context, aes(x = hypothesis_avg_overall_sighted_rating, y = Bleu_2)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1), xlim=c(1,5)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + 
  xlab("Avg. Overall Sighted Rating") + theme(
    axis.title.y = element_blank())
bleu2_sighted

new <- ggarrange(bleu2_blv, bleu2_sighted,
                 ncol = 2, nrow = 1)
new <- annotate_figure(new, left = text_grob("BLEU-2", rot = 90))
new

bleu2_refcount <- ggplot(data = sensitive_to_context, aes(x = reference_count, y = Bleu_2)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

bleu2_hyplength <- ggplot(data = sensitive_to_context, aes(x = hypothesis_length, y = Bleu_2)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

bleu2_reflength <- ggplot(data = sensitive_to_context, aes(x = avg_reference_length, y = Bleu_2)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

row2 <- ggarrange(bleu2_blv, bleu2_sighted, bleu2_refcount, bleu2_hyplength, bleu2_reflength, 
                  ncol = 5, nrow = 1)
row2 <- annotate_figure(row2, left = text_grob("BLEU-2", face = "bold", rot = 90))

# ROW 3

bleu3_blv <- ggplot(data = sensitive_to_context, aes(x = hypothesis_avg_overall_blv_rating, y = Bleu_3)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1), xlim=c(1,5)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

bleu3_sighted <- ggplot(data = sensitive_to_context, aes(x = hypothesis_avg_overall_sighted_rating, y = Bleu_3)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1), xlim=c(1,5)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

bleu3_refcount <- ggplot(data = sensitive_to_context, aes(x = reference_count, y = Bleu_3)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

bleu3_hyplength <- ggplot(data = sensitive_to_context, aes(x = hypothesis_length, y = Bleu_3)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

bleu3_reflength <- ggplot(data = sensitive_to_context, aes(x = avg_reference_length, y = Bleu_3)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

row3 <- ggarrange(bleu3_blv, bleu3_sighted, bleu3_refcount, bleu3_hyplength, bleu3_reflength, 
                  ncol = 5, nrow = 1)
row3 <- annotate_figure(row3, left = text_grob("BLEU-3", face = "bold", rot = 90))

# ROW 4

bleu4_blv <- ggplot(data = sensitive_to_context, aes(x = hypothesis_avg_overall_blv_rating, y = Bleu_4)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1), xlim=c(1,5)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

bleu4_sighted <- ggplot(data = sensitive_to_context, aes(x = hypothesis_avg_overall_sighted_rating, y = Bleu_4)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1), xlim=c(1,5)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

bleu4_refcount <- ggplot(data = sensitive_to_context, aes(x = reference_count, y = Bleu_4)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

bleu4_hyplength <- ggplot(data = sensitive_to_context, aes(x = hypothesis_length, y = Bleu_4)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

bleu4_reflength <- ggplot(data = sensitive_to_context, aes(x = avg_reference_length, y = Bleu_4)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

row4 <- ggarrange(bleu4_blv, bleu4_sighted, bleu4_refcount, bleu4_hyplength, bleu4_reflength, 
                  ncol = 5, nrow = 1)
row4 <- annotate_figure(row4, left = text_grob("BLEU-4", face = "bold", rot = 90))

# ROW 5

meteor_blv <- ggplot(data = sensitive_to_context, aes(x = hypothesis_avg_overall_blv_rating, y = METEOR)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1), xlim=c(1,5)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

meteor_sighted <- ggplot(data = sensitive_to_context, aes(x = hypothesis_avg_overall_sighted_rating, y = METEOR)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1), xlim=c(1,5)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

meteor_refcount <- ggplot(data = sensitive_to_context, aes(x = reference_count, y = METEOR)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + theme(
    axis.title.x = element_blank(),
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

meteor_reflength <- ggplot(data = orig_sensitive, aes(x = avg_reference_length, y = METEOR)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

row5 <- ggarrange(meteor_blv, meteor_sighted, meteor_refcount, meteor_hyplength, meteor_reflength, 
                  ncol = 5, nrow = 1)
row5 <- annotate_figure(row5, left = text_grob("METEOR", face = "bold", rot = 90))

# ROW 6

rouge_blv <- ggplot(data = sensitive_to_context, aes(x = hypothesis_avg_overall_blv_rating, y = ROUGE_L)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1), xlim=c(1,5)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + 
  xlab("Avg. BLV Overall Rating") + 
  theme(
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_blank())

rouge_sighted <- ggplot(data = sensitive_to_context, aes(x = hypothesis_avg_overall_sighted_rating, y = ROUGE_L)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1), xlim=c(1,5)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + 
  xlab("Avg. Sighted Overall Rating") + 
  theme(
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_blank())

rouge_refcount <- ggplot(data = sensitive_to_context, aes(x = reference_count, y = ROUGE_L)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + 
  xlab("Reference Count") + 
  theme(
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_blank())

rouge_hyplength <- ggplot(data = sensitive_to_context, aes(x = hypothesis_length, y = ROUGE_L)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + 
  xlab("Hypothesis Length") + 
  theme(
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_blank())

rouge_reflength <- ggplot(data = sensitive_to_context, aes(x = avg_reference_length, y = ROUGE_L)) +
  geom_point() +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_smooth(method = "lm", se=F) +
  geom_jitter(alpha=0.1) +
  stat_cor(method = "pearson", label.y.npc=0.2, color = "red", geom = "label", label.x = 1, label.y = 0.8) + 
  xlab("Avg. Reference Length") + 
  theme(
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_blank())

row6 <- ggarrange(rouge_blv, rouge_sighted, rouge_refcount, rouge_hyplength, rouge_reflength, 
                  ncol = 5, nrow = 1, align = "v")
row6 <- annotate_figure(row6, left = text_grob("ROUGE-L", face = "bold", rot = 90))

TOTAL <- ggarrange(row1, row2, row3, row4, row5, row6, 
                   ncol = 1, nrow = 6, align = "v")
TOTAL <- annotate_figure(TOTAL, top = text_grob("Dataset 1 Correlations: Sensitive to Context", face = "bold", size = 14),)
TOTAL