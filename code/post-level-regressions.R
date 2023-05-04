# Run this file to estimate regressions at the post-level predicting effect of (metatopics)
# and engagement bait on likes.

library(tidyverse)
library(lubridate)
library(fixest)
library(scico)
library(egg)
library(stringi)
library(modelsummary)
library(scales)
library(extrafont)

data <- read_csv("../data/topic_model_30_deduped.csv")
data$time <- ymd_hms(data$time)

data <- dplyr::filter(data, time >= as.Date("2014-04-01") & time <= as.Date("2017-05-31"))

# N = 36300
data <- data %>% filter(type %in% c("added_photos", 
                                    "added_video", "shared_story")) %>%
    mutate(type = relevel(as.factor(type), ref = "shared_story"))

# N = 36,196 # Removed uncommon post types to focus on main three

# Making a time of day variable, since we expect people to be online at different times
# And a day of week variable to account for seasonality
data <- data %>% mutate(hour = hour(time),
                        dow = wday(time, label = T),
                        words = stri_count_words(text))

# Converting to percentages for interpretability
data$M_motivational <- data$M_motivational*100
data$M_repression <- data$M_repression*100
data$M_promotional <- data$M_promotional*100
data$T_protests <- data$T_protests*100
data$M_islam <- data$M_islam*100
data$M_terrorism <- data$M_terrorism*100
data$M_politics <- data$M_politics*100
data$T_crime <- data$T_crime*100
data$T_military <- data$T_military*100
data$M_immigration <- data$M_immigration*100

# Running negative binomial models estimated engagements as function of topics percentages and controls
m.likes.nb <- fenegbin(likes ~ type + hour + I(hour^2) + words + I(words^2) +
                     M_motivational + M_repression + M_promotional +
                     T_protests + M_islam + M_terrorism + 
                     M_politics + T_crime + T_military + M_immigration| date ,
                 data = data)
summary(m.likes.nb)

m.shares.nb <- fenegbin(shares ~ type + hour + I(hour^2) + words + I(words^2) +
                      M_motivational + M_repression + M_promotional +
                      T_protests + M_islam + M_terrorism + 
                      M_politics + T_crime + T_military + M_immigration| date ,
                  data = data)
summary(m.shares.nb)

m.comments.nb <- fenegbin(comments ~ type + hour + I(hour^2) + words + I(words^2) +
                        M_motivational + M_repression + M_promotional +
                        T_protests + M_islam + M_terrorism + 
                        M_politics + T_crime + T_military + M_immigration| date ,
                    data = data)
summary(m.comments.nb)

likes_tidy.nb <- broom::tidy(m.likes.nb, conf.int = T) %>% 
    mutate(model = "Likes")
shares_tidy.nb <- broom::tidy(m.shares.nb, conf.int = T) %>% 
    mutate(model = "Shares")
comments_tidy.nb <- broom::tidy(m.comments.nb, conf.int = T) %>% 
    mutate(model = "Comments")
results.nb <- bind_rows(likes_tidy.nb, shares_tidy.nb, comments_tidy.nb) %>% 
    filter(grepl("M_|T_", term))

results.nb <- results.nb %>% mutate(
    model = factor(model, levels = c("Likes", "Shares", "Comments")),
    term = factor(term, levels = c("M_terrorism", "M_islam", "M_politics",
                                   "M_immigration", "T_crime", "T_military",
                                   "M_motivational", "M_repression", "M_promotional",
                                   "T_protests"))
) %>% mutate(estimate = exp(estimate),
             conf.low = exp(conf.low),
             conf.high = exp(conf.high))

# Now running models for engagement bait
data <- data %>% mutate(like_mention = ifelse(grepl("like", tolower(text)), 1,0),
                        share_mention = ifelse(grepl("share", tolower(text)), 1,0),
                        comment_mention = ifelse(grepl("comment", tolower(text)), 1,0),
                        any_mention = ifelse(grepl("like|share|comment", tolower(text)), 1,0))

# Running EB version
m.likes.nb.eb <- fenegbin(likes ~ type + hour + I(hour^2) + words + I(words^2) +
                           M_motivational + M_repression + M_promotional +
                           T_protests + M_islam + M_terrorism + 
                           M_politics + T_crime + T_military + M_immigration + like_mention | date ,
                       data = data)
summary(m.likes.nb.eb)

m.shares.nb.eb <- fenegbin(shares ~ type + hour + I(hour^2) + words + I(words^2) +
                            M_motivational + M_repression + M_promotional +
                            T_protests + M_islam + M_terrorism + 
                            M_politics + T_crime + T_military + M_immigration + share_mention| date ,
                        data = data)
summary(m.shares.nb.eb)



m.comments.nb.eb <- fenegbin(comments ~ type + hour + I(hour^2) + words + I(words^2) +
                              M_motivational + M_repression + M_promotional +
                              T_protests + M_islam + M_terrorism + 
                              M_politics + T_crime + T_military + M_immigration + comment_mention | date ,
                          data = data)
summary(m.comments.nb.eb)

# Making plot with EB
likes_tidy.nb.eb <- broom::tidy(m.likes.nb.eb, conf.int = T) %>% 
    mutate(model = "Likes")
shares_tidy.nb.eb  <- broom::tidy(m.shares.nb.eb, conf.int = T) %>% 
    mutate(model = "Shares")
comments_tidy.nb.eb  <- broom::tidy(m.comments.nb.eb, conf.int = T) %>% 
    mutate(model = "Comments")

results.nb.eb  <- bind_rows(likes_tidy.nb.eb , shares_tidy.nb.eb , comments_tidy.nb.eb ) %>% 
    filter(grepl("_mention", term))

results.nb.eb  <- results.nb.eb %>% mutate(
    model = factor(model, levels = c("Likes", "Shares", "Comments")),
    term = factor(term, levels = c("comment_mention", "share_mention", "like_mention"))
) %>% mutate(estimate = exp(estimate),
             conf.low = exp(conf.low),
             conf.high = exp(conf.high))

# PLOT

# Loading fonts and setting custom theme
loadfonts(device = c("all"))

# Defining custom ggplot2 theme to use times new roman font in all plots, must be called for each (sub)plot
th <- theme(axis.title.y = element_text(size = 10, family = "Times New Roman"),
            axis.text.y = element_text(size = 10, family = "Times New Roman"),
            axis.title.x = element_text(size = 10, family = "Times New Roman"),
            axis.text.x = element_text(size = 10, family = "Times New Roman"),
            legend.text = element_text(size = 10, family = "Times New Roman"),
            strip.text.x = element_text(size = 10, family = "Times New Roman"))

# Making two plots, first for topic covariates then for EB
a <- ggplot(aes(y = term, x = estimate, color = term), data = results.nb) + 
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high), alpha = 1) +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high), alpha = 1, pch = 21, color = "black") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    scale_y_discrete(labels = c("Terrorism (M)", "Islam (M)", "Politics (M)", 
                                "Immigration (M)", "Crime (T)", "Military (T)",
                                "Motivational (M)", "Repression (M)", 
                                "Promotional (M)", "Protests (T)")) +
    scale_x_continuous(labels = scales::number_format(accuracy = 0.01),
                       breaks = c(0.95, 1, 1.05)) +
    facet_grid(~model) + scale_color_scico_d(palette = "grayC", direction = -1) + 
    theme_classic() + theme(legend.position = "none") + 
    labs(x = "", y = "Topic") + th

b <- ggplot(aes(y = term, x = estimate, color = term), data = results.nb.eb) + 
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high), alpha = 1, color = "black") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    scale_y_discrete(labels = c("Comment", "Share", "Like")) +
    scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
    facet_grid(~model) + scale_color_scico_d(palette = "roma") + 
    theme_classic() + theme(legend.position = "none") + 
    labs(x = "Estimate", y = "Engagement bait") + th

# Comining and saving plots
arr <- ggarrange(a, b, nrow = 2, heights = c(10, 2), labels = c("A", "B"))

# Uncomment and run to store file
ggsave("../figures/eb_topic_coefficient_plot_nb_final.pdf", arr, width = 6.3, height = 6, device = cairo_pdf)


# REGRESSION TABLE
coef_names <- c( # Vector of variable names
    "T_protests"  = "Protests",
    "M_promotional" = "Promotional",
    "M_repression" = "Repression",
    "M_motivational" = "Motivational",
    "T_military"  = "Military",
    "T_crime" = "Crime",
    "M_immigration" = "Immigration",
    "M_politics" = "Politics",
    "M_islam" = "Islam",
    "M_terrorism" = "Terrorism",
    "like_mention" = "Like EB",
    "share_mention" = "Share EB",
    "comment_mention" = "Comment EB",
    "typeadded_photos" = "Photo",
    "typeadded_video" = "Video",
    "hour" = "Hour of day",
    "I(hour^2)" = "Hour of day$^2$",
    "words" = "Word count",
    "I(words^2)" = "Word count$^2$"
)  


modelsummary(list("Likes I" = m.likes.nb, 
                  "Likes II" = m.likes.nb.eb, 
                  "Shares I" = m.shares.nb, 
                  "Shares II" = m.shares.nb.eb,
                  "Comments I" = m.comments.nb,
                  "Comments II" = m.comments.nb.eb), 
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001), 
             coef_map = coef_names, 
             gof_omit = "Log.Lik.|RMSE|R2 Within|R2 WithiN Adj.|AIC|BIC|Std.Errors",
             fmt = 2,
             exponentiate = T,
             output = "../tables/topic_reg_table.docx"
             )

