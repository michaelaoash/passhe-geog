library(tidyverse)
options(scipen=10000)

wsMap  <- read_csv("wsMap-density.csv")


wsMap %>% filter(`Measure for Color` > 0.8) %>% select(`Geo Name2`,`TT Selected Measure, Mode, Univ`, `TT Measure Count when Density Selected`,`Measure for Color`)


test  <- wsMap %>% group_by(`TT Selected Measure, Mode, Univ`) %>%
    transmute(
        County = `Geo Name2`,
        university = gsub(" Employee Density", "",`TT Selected Measure, Mode, Univ`),
        count = `TT Measure Count when Density Selected`,
        density = `Measure for Color`/100,
        share = count / sum(count)
    )
test  <- test %>% arrange(university, desc(share)) %>% mutate(runshare = cumsum(share))

pt1  <- test %>% filter(runshare<0.8)
pt2  <- test %>% filter(runshare>=0.8) %>% filter(row_number()==1)

test  <- bind_rows(pt1,pt2) %>% group_by(university) %>% arrange(university, desc(share))
