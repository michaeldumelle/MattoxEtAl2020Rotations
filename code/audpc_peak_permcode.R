library(tidyverse)
library(broom)
#permutation test code

set.seed(9719)

t1_y1 <- read_csv("audpc_peak_2016_trial1.csv")
t1_y2 <- read_csv("audpc_peak_2017_trial1.csv") %>% filter(complete.cases(.))
t2_y1 <- read_csv("audpc_peak_2016_trial2.csv")
t2_y2 <- read_csv("audpc_peak_2017_trial2.csv")





perm_sim <- function(n_sim, response, group) {
  raw_mod <- glance(aov(response ~ group))
  raw_f <- raw_mod$statistic
  sim_f <- rep(0, n_sim)
  for (i in 1:n_sim) {
    sim_f[i] <- glance(aov(response ~ sample(group, size = length(group), replace = FALSE)))$statistic
  }
  raw_p <- raw_mod$p.value
  sim_p <- mean(sim_f >= raw_f)
  return(tibble(raw_p = raw_p, sim_p = sim_p))
}


n_sim <- 100000
perm_list <- list(perm_sim(n_sim = n_sim, response = t1_y1$Peak, group = t1_y1$Group) %>% mutate(reference = "t1_y1_peak"),
                  perm_sim(n_sim = n_sim, response = t1_y1$AUDPC, group = t1_y1$Group) %>% mutate(reference = "t1_y1_audpc"),
                  perm_sim(n_sim = n_sim, response = t1_y2$Peak, group = t1_y2$Group) %>% mutate(reference = "t1_y2_peak"),
                  perm_sim(n_sim = n_sim, response = t1_y2$AUDPC, group = t1_y2$Group) %>% mutate(reference = "t1_y2_audpc"),
                  perm_sim(n_sim = n_sim, response = t2_y1$Peak, group = t2_y1$Group) %>% mutate(reference = "t2_y1_peak"),
                  perm_sim(n_sim = n_sim, response = t2_y1$AUDPC, group = t2_y1$Group) %>% mutate(reference = "t2_y1_audpc"),
                  perm_sim(n_sim = n_sim, response = t2_y2$Peak, group = t2_y2$Group) %>% mutate(reference = "t2_y2_peak"),
                  perm_sim(n_sim = n_sim, response = t2_y2$AUDPC, group = t2_y2$Group) %>% mutate(reference = "t2_y2_audpc")
                  )
perm_results <- bind_rows(perm_list)
perm_results
write_csv(perm_results, "perm_results.csv")