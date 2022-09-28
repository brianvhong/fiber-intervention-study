## Library
if(!any(rownames(installed.packages()) == 'pacman'))
        install.packages('pacman')

if(!any(rownames(installed.packages()) == 'devtools'))
        install.packages('devtools')

if(!any(rownames(installed.packages()) == 'HTSet'))
        devtools::install_github("zhuchcn/HTSet")

pacman::p_load(HTSet,
               tidyverse,
               pheatmap,
               readxl,
               DT,
               insight,
               here)


# ID mapping + clinic metadata

mapping <- read.csv(here("subject_characteristics","final_samples.csv"))[,1:2] %>%
        `colnames<-`(c("Sample", "Sample.ID")) %>%
        mutate(Sample = as.character(Sample))

clinic <- read_xlsx(here("subject_characteristics","USANA Study Clinical Meta Data Sheet.xlsx"), sheet = 1) %>%
        mutate(
                Sample.ID = paste0(subject, `timepoint coded`),
                treatment = ifelse(treatment=="A", "fiber", "placebo")
        ) 

metadata <- left_join(mapping, clinic)
```

# Plate layout

layout1 <- c(as.matrix(read_xlsx(here("LCAT","layout","LCAT_layout.xlsx"), range = "B1:L9"))) %>%
        sub(" *", "", .)
layout2 <- c(as.matrix(read_xlsx(here("LCAT","layout","LCAT_layout.xlsx"), range = "B12:L20"))) %>%
        sub(" *", "", .)

layout1 <- layout1[!is.na(layout1)]
layout1[layout1=="ercc"] <- "ercc.plate1"
layout1[layout1=="blank"] <- "blank.plate1"
layout2 <- layout2[!is.na(layout2)]
layout2[layout2=="ercc"] <- "ercc.plate2"
layout2[layout2=="blank"] <- "blank.plate2"
```

# Read and tidy LCAT assay results

ranges <- c("C145:M153", "C157:M165", "C169:M177")

plate1 <- lapply(ranges, function(x){
        c(as.matrix(read_xlsx(here("LCAT","raw","2022-07-01_LCAT Fiber.xlsx"), range = x)))
}) %>%
        `names<-`(c("ratio1", "ratio2", "ratio3")) %>%
        do.call(cbind, .) %>%
        as.data.frame() %>%
        filter(complete.cases(.)) %>%
        mutate(Sample = layout1,
               Batch = "1") %>%
        filter(!(Sample %in% "blank.plate1")) %>%
        rowwise() %>%
        mutate(LCAT.ratio = mean(ratio1, ratio2, ratio3)) %>%
        ungroup() %>%
        mutate(norm.LCAT.ratio = LCAT.ratio/LCAT.ratio[Sample == "ercc.plate1"]) %>%
        filter(Sample != "ercc.plate1") %>%
        group_by(Sample, Batch) %>%
        summarize(LCAT.ratio = mean(LCAT.ratio),
                  norm.LCAT.ratio = mean(norm.LCAT.ratio)) 



plate2 <- lapply(ranges, function(x){
        c(as.matrix(read_xlsx(here("LCAT","raw","2022-07-02_LCAT Fiber.xlsx"), range = x)))
}) %>%
        `names<-`(c("ratio1", "ratio2", "ratio3")) %>%
        do.call(cbind, .) %>%
        as.data.frame() %>%
        filter(complete.cases(.)) %>%
        mutate(Sample = layout2, 
               Batch = "2") %>%
        filter(!(Sample %in% "blank.plate2")) %>%
        rowwise() %>%
        mutate(LCAT.ratio = mean(ratio1, ratio2, ratio3)) %>%
        ungroup() %>%
        mutate(norm.LCAT.ratio = LCAT.ratio/LCAT.ratio[Sample == "ercc.plate2"]) %>%
        filter(Sample != "ercc.plate2") %>%
        group_by(Sample, Batch) %>%
        summarize(LCAT.ratio = mean(LCAT.ratio),
                  norm.LCAT.ratio = mean(norm.LCAT.ratio))

plate.merge <- rbind(plate1, plate2) %>%
        left_join(metadata) %>%
        mutate(EU = paste(subject, timepoint, treatment, sep = ".")) %>%
        filter(Sample != "74") %>% # Filter outlier
        mutate(Sample = as.integer(Sample)) %>%
        arrange(Sample)

## Save File
#saveRDS(plate.merge, here("LCAT","data","LCAT_summary_filter_223B.rds"))