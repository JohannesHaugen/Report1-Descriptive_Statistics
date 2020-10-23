df <- read_csv("./data/hypertrophy.csv") %>%
  select(CLUSTER, FAST_NUCLEI_T1, FAST_NUCLEI_T3) %>%
  filter(!is.na(CLUSTER)) %>%
  
  mutate(change = FAST_NUCLEI_T3 - FAST_NUCLEI_T1) %>%
  group_by(CLUSTER) %>%
  summarise(mean.change = mean(change)) %>%
  print()

bxp <- ggpaired(df, x = "CLUSTER", y = "mean.change", 
                order = c("LOW", "HIGH"),
                ylab = "Weight", xlab = "CLUSTER")
bxp


# Prepare the data
dfhypertrophy <- read_csv("./data/hypertrophy.csv") %>%
  select(CLUSTER, FAST_NUCLEI_T1, FAST_NUCLEI_T3) %>%
  filter(!is.na(CLUSTER)) %>%
  mutate(change = FAST_NUCLEI_T3 - FAST_NUCLEI_T1) %>%
  group_by(CLUSTER) %>%
  summarise(mean.change = mean(change)) %>%
  print()


df.figure <- dfhypertrophy %>%
  pivot_longer(names_to = "CLUSTER",
               values_to = "change",
               cols =  FAST_NUCLEI_T3:FAST_NUCLEI_T1) %>%
  mutate(timepoint = factor(timepoint, levels = c("HIGH", "LOW"))) %>%
  ggplot(aes(timepoint, sj.max)) + geom_boxplot()
# calculate the t-test, paired data
ttest <- t.test(FAST_NUCLEI_T3, FAST_NUCLEI_T1, paired = TRUE)

# plot the data to see corresponding data
sj.figure <- sj.max %>%
  pivot_longer(names_to = "timepoint",
               values_to = "sj.max", 
               cols = pre:meso3) %>%
  mutate(timepoint = factor(timepoint, levels = c("pre", "meso3"))) %>%
  ggplot(aes(timepoint, sj.max)) + geom_boxplot()

# create a summary statistic
sj.summary <- sj.max %>%
  pivot_longer(names_to = "timepoint",
               values_to = "sj.max", 
               cols = pre:meso3) %>%
  group_by(timepoint) %>%
  summarise(m = mean(sj.max, na.rm = TRUE), 
            s = sd(sj.max, na.rm = TRUE))

install.packages("ggpubr")

library(ggpubr)