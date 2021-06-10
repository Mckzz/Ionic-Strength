library(tidyverse)
library(ggplot2)

IS_NaCl <- read_csv("~/student documents/UBC/Research/chaoborus imaging/Square capillary/2021_06_08 IS using NaCl/IS_NaCl.csv")
view(IS_NaCl)

ISlong <-
  pivot_longer(
    IS_NaCl,
    cols = c(`1`, `2`, `3`, `4`),
    names_to = "larva", values_to = "area") 
  #mutate(larva = as_factor(larva))

print(ISlong, n= 20)

#making % change column
ISlong.pct <- ISlong %>%
  group_by(larva) %>%
  mutate(
    area.pct.change = ((area - area[1]) / area[1]
    )*100) %>%
  ungroup() #why the ungroup? 

print(ISlong.pct, n= 40)

pct.meansd <- ISlong.pct %>%
  group_by(mM) %>% 
  dplyr::summarise(
    pct.mean = mean(area.pct.change), 
    pct.sd = sd(area.pct.change))

print(pct.meansd, n= 25)


#plot means/ sd
ggplot(data = pct.meansd, aes(x= mM)) +
  geom_point(aes(y= pct.mean)) +
  geom_line(aes(y= pct.mean)) +
  geom_errorbar(aes(x= mM,
                    ymin= pct.mean - pct.sd, 
                    ymax= pct.mean + pct.sd), 
                group= "ant.post",
                width= 0.1) +
  scale_color_manual(values=c("#D55E00", "#009e73")) +
  scale_x_log10() +
  labs(x = "mM", 
       y = "Area % change") +
  #ggtitle("") +
  theme_classic()
