# Author: Rhea Kapur
# Date: 6/9/23
# Version: 1.0

from nlgeval import compute_individual_metrics, NLGEval
import pandas as pd

nlgeval = NLGEval(metrics_to_omit=['CIDEr', 'SkipThoughtCS', 'EmbeddingAverageCosineSimilarity', 'VectorExtremaCosineSimilarity',
                                    'GreedyMatchingScore'])  # loads the models

input_path = "PATH_TO_SENSITIVE/INSENSITIVE.csv"
output_path = "PATH_TO_SENSITIVE/INSENSITIVE_WITH_CORRELATIONS.csv"

bare_df = pd.read_csv(input_path)

row_num = len(bare_df.index)
metrics_sensitive_to_context = pd.DataFrame(0.0, index=range(0), columns=range(7))
metrics_sensitive_to_context.columns = ['Bleu_1', 'Bleu_2', 'Bleu_3', 'Bleu_4', 'METEOR', 'ROUGE_L', 'Spice']

for index in range(row_num):
    hypothesis = bare_df.loc[index].at["hypothesis"]
    references = bare_df.loc[index].at["references"].split(".|") # stored as one string with separator

    metrics_dict = nlgeval.compute_individual_metrics(references, hypothesis)
    new_row = pd.DataFrame([metrics_dict])

    metrics_sensitive_to_context = pd.concat([metrics_sensitive_to_context, new_row])

merged_df = pd.concat([bare_df.reset_index(drop=True), metrics_sensitive_to_context.reset_index(drop=True)], axis=1)
merged_df.to_csv(output_path)