Replication materials for "Audience Engagement and the Dynamics of Online Activism: Far-Right Mobilization on Facebook", forthcoming in *Mobilization*.

A pre-print is available here: https://osf.io/preprints/socarxiv/rwfjc

# Organization

## Data
All necessary data to replicate the results are stored in the `data` directory. This also includes various intermediate datasets created by running the code below.

The `weekly_dataset.csv` file contains the final version of the data used for time series modeling. The `bf_posts_raw.csv` file contains the original text of BF's Facebook posts.

## Code
All code is contained in the `code` directory. Specific instructions are included within each file. The files are either in R or RMarkdown format.

### Topic models
The code used to run the topic model is contained in `estimate_topic_model.Rmd`. The fitted model is stored in the `topic_model` directory. The `process-topic-model-results.Rmd` script is used to obtain weekly topic proportions.

### Main results
The main time series regression analyses can be reproduced by running `main-results.Rmd`. This script produces Tables 2-4 and Figure 5, as well as additional supplementary material. Note that the tables require some additional formatting to match published versions.

### Post-level results
The negative binomial regression models estimating the effect of topic proportions and engagement bait on engagement can be reproduced by running `post-level-regressions.R`. This produces Figure 6 and the full table reported in the appendix.

### Additional figures
The `additional-visualizations.Rmd` script can be used to recreate Figures 2-4 and additional supplementary figures.

## Tables and Figures
All tables are stored in `tables` and figures are stored in `figures`.
