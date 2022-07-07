if (!require("pacman")) install.packages("pacman") ## This code installs pacman package, pacman automatically download required packages if you don't have them and load them for you.
pacman::p_load(tidyverse, here, DT, ggpubr) # These are the packages you need and to download

## Read CSV
lcat_p1 <- read_csv(here("tidy","2022-07-01_fiber_study_lcat_plate1.csv"))

lcat_p2 <- read_csv(here("tidy","2022-07-02_fiber_study_lcat_plate2.csv"))

## Change plate to factor
lcat_p1$plate <- as.factor(lcat_p1$plate)
lcat_p2$plate <- as.factor(lcat_p2$plate)

## Calculate mean LCAT
lcat_p1 <- lcat_p1 %>%
        rowwise() %>%
        mutate(lcat_activity = mean(c(lcat_1, lcat_2, lcat_3)))

lcat_p2 <- lcat_p2 %>%
        rowwise() %>%
        mutate(lcat_activity = mean(c(lcat_1, lcat_2, lcat_3)))

## Summarize
lcat_p1_summary <- lcat_p1 %>%
        group_by(id, plate) %>%
        summarize(mean_lcat = mean(lcat_activity),
                  sd_lcat = sd(lcat_activity),
                  cv_lcat = sd_lcat/mean_lcat *100)

lcat_p2_summary <- lcat_p2 %>%
        group_by(id, plate) %>%
        summarize(mean_lcat = mean(lcat_activity),
                  sd_lcat = sd(lcat_activity),
                  cv_lcat = sd_lcat/mean_lcat *100)

## Quality control values by plate
qc_p1 <- mean(lcat_p1_summary$mean_lcat[lcat_p1_summary$id == "qc"])
qc_p2 <- mean(lcat_p2_summary$mean_lcat[lcat_p2_summary$id == "qc"])


## Normalize Summarize Data to QC
lcat_p1_summary <- lcat_p1_summary %>%
        mutate(norm_lcat = mean_lcat/qc_p1)

lcat_p2_summary <- lcat_p2_summary %>%
        mutate(norm_lcat = mean_lcat/qc_p2)

## Combine Tables
lcat_summary <- rbind(lcat_p1_summary, lcat_p2_summary)

## Data Table
datatable(lcat_summary)

## CVs
ggplot(lcat_summary, aes(x = cv_lcat)) +
       geom_histogram(color="black", fill="white") +
        labs(x = "CV (%)") +
        geom_vline(aes(xintercept=mean(cv_lcat)),
                   color="blue", linetype="dashed", size=1)

## Plate Analysis
p <- ggboxplot(lcat_summary, x = "plate", y = "mean_lcat",
          color = "plate", palette = "jco",
          add = "jitter")
# Change method
p + stat_compare_means(method = "t.test")

## Plate norm
p_normalize <- ggboxplot(lcat_summary, x = "plate", y = "norm_lcat",
               color = "plate", palette = "jco",
               add = "jitter")
# Change method
p_normalize + stat_compare_means(method = "t.test")
