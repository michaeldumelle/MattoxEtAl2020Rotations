library(tidyverse)
library(nlme)

#permutation test code

set.seed(9719)

t1_y1 <- read_csv("data/audpc_peak_2016_trial1_data.csv")
t1_y2 <- read_csv("data/audpc_peak_2017_trial1_data.csv") %>% filter(complete.cases(.))
t2_y1 <- read_csv("data/audpc_peak_2016_trial2_data.csv")
t2_y2 <- read_csv("data/audpc_peak_2017_trial2_data.csv")





perm_sim <- function(n_sim, response, group, block) {
  raw_mod <- lme(response ~ group, random = ~ 1|as.factor(block))
  anova_raw_mod <- anova(raw_mod, type = "marginal")
  raw_f <- anova_raw_mod[2, 3]
  sim_f <- rep(0, n_sim)
  for (i in 1:n_sim) {
    new_response <- sample(response, size = length(response), replace = FALSE)
    new_mod <- lme(new_response ~ group, random = ~ 1|as.factor(block))
    anova_new_mod <- anova(new_mod, type = "marginal")
    sim_f[i] <- anova_new_mod[2, 3]
  }
  raw_p <- anova_raw_mod[2, 4]
  sim_p <- mean(sim_f >= raw_f)
  return(tibble(raw_p = raw_p, sim_p = sim_p))
}


n_sim <- 100000
perm_list <- list(perm_sim(n_sim = n_sim, response = t1_y1$Peak, group = t1_y1$Group, block = t1_y1$Block) %>% mutate(reference = "t1_y1_peak"),
                  perm_sim(n_sim = n_sim, response = t1_y1$AUDPC, group = t1_y1$Group, block = t1_y1$Block) %>% mutate(reference = "t1_y1_audpc"),
                  perm_sim(n_sim = n_sim, response = t1_y2$Peak, group = t1_y2$Group, block = t1_y2$Block) %>% mutate(reference = "t1_y2_peak"),
                  perm_sim(n_sim = n_sim, response = t1_y2$AUDPC, group = t1_y2$Group, block = t1_y2$Block) %>% mutate(reference = "t1_y2_audpc"),
                  perm_sim(n_sim = n_sim, response = t2_y1$Peak, group = t2_y1$Group, block = t2_y1$Block) %>% mutate(reference = "t2_y1_peak"),
                  perm_sim(n_sim = n_sim, response = t2_y1$AUDPC, group = t2_y1$Group, block = t2_y1$Block) %>% mutate(reference = "t2_y1_audpc"),
                  perm_sim(n_sim = n_sim, response = t2_y2$Peak, group = t2_y2$Group, block = t2_y2$Block) %>% mutate(reference = "t2_y2_peak"),
                  perm_sim(n_sim = n_sim, response = t2_y2$AUDPC, group = t2_y2$Group, block = t2_y2$Block) %>% mutate(reference = "t2_y2_audpc")
                  )
perm_results <- bind_rows(perm_list)
perm_results
write_csv(perm_results, "perm_results.csv")