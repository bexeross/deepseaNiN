library(dplyr)

sedimMeans <- read.csv(file.path(dataPath,"inputs\\sedimMeans.csv"))


sedimMeans <- sedimMeans %>% mutate(FI = 
                                      case_when(grepl("mud|spi", tolower(Substrate)) ~ "F",
                                    TRUE ~ "NF"))

sedimSumm <- sedimMeans %>% group_by(SampID, FI) %>% summarize(sum=sum(mean_percent))

sedimSumm <- sedimSumm %>% filter (FI=="F") %>%
  mutate(
  S3F = case_when(sum==0 ~ "0",
                  sum<10 ~ "a",
                  between(sum,10,30) ~ "b",
                  between(sum,30,60) ~ "c",
                  TRUE ~ "circle"
                  )
)

