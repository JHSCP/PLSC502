library(tidyverse)
##### 2. Bayesian #####
set.seed(502502)
N <- 1000

pE <- 0.7
pW_given_E <- 0.8
pW_given_Enot <- 0.4
# assume candidates only have two opposing stance: support or not.
pE_not <- 1 - pE

## getting P(W) by using the Law of Total Probability 
pW <- pW_given_E * pE + pW_given_Enot * pE_not

# Since we assumed that the candidate has either support or not support (binary: 1 = supports, 0 = not), 
# I use prior belief of E, and rely on the binomial distribution function in R to 
# generate random variable for supporting environmental policy.

E <- rbinom(N, 1, pE)  

## Simulation
W <- numeric(N)
W[E == 1] <- rbinom(sum(E == 1), 1, pW_given_E)
W[E == 0] <- rbinom(sum(E == 0), 1, pW_given_Enot)

mean(E[W == 1]) %>% print()


##### 3. Voter Turnout #####

set.seed(20250922)  
library(dplyr)
library(ggplot2)

  
# Parameters
n_total      <- 100
n_campaign   <- n_total / 2 
intercept    <- 0.3
beta_campaign<- 0.5
sigma_eps    <- 0.1

# Design: 50 with active campaign (X=1), 50 without (X=0)
X <- c(rep(1, n_campaign), rep(0, n_campaign))

# Error term and outcome
eps <- rnorm(n_total, mean = 0, sd = sigma_eps)
Y   <- intercept + beta_campaign * X + eps

# Package into a tibble
problem3_b <- tibble(
  Campaign = factor(X, levels = c(0,1), labels = c("No Political Campaign","Political Campaign")),
  Turnout  = Y
)


# plotting

ggplot(problem3_b, aes(Turnout, fill = Campaign)) +
  geom_density(alpha = 0.35) +
  labs(
    title = "Distribution of Voter Turnout by Campaign Presence",
    x = "Turnout", y = "Density"
  ) +
  theme_minimal() + 
  scale_fill_brewer(palette = "Dark2")



# manually calculate

# ---- Simple difference in means ----
mean_no_campaign <- problem3_b %>% filter(Campaign == "No Political Campaign") %>% summarize(m = mean(Turnout)) %>% pull(m)
mean_campaign    <- problem3_b %>% filter(Campaign == "Political Campaign")    %>% summarize(m = mean(Turnout)) %>% pull(m)
diff_means       <- mean_campaign - mean_no_campaign

cat("\nMean turnout (No Campaign): ", round(mean_no_campaign, 3), "\n",
    "Mean turnout (Campaign):    ", round(mean_campaign, 3), "\n",
    "Difference in means:        ", round(diff_means, 3), "\n", sep = "")


# regression
m <- lm(Turnout ~ I(Campaign == "Political Campaign"), data = problem3_b)
summary(m)




##### 6. Candidate A and Candidate B. #####
#a.
set.seed(0922)
candidate <- c("A","B")
probability <- c(0.55, 0.45)

simulated_candidate <- sample(candidate, size = 10000, 
                              replace = TRUE, prob = probability)

#b.
probs <- prop.table(table(simulated_candidate))

print(probs)


#c. 
barplot(table(simulated_candidate)/10000, col = "tan",
        #main = "Simulated (Discrete Distribution)",
        xlab = "Candidates", ylab = "Proportion")



##### 7. political ideologies #####

set.seed(502)
n <- 10000

ideology_prob <- c(liberal = 0.40, 
                   moderate = 0.35, 
                   conservative = 0.25)
prob_dem_given_ideology <- c(liberal = 0.70, 
                             moderate = 0.50, 
                             conservative = 0.30)


voters <- tibble(ideology = sample(names(ideology_prob),
                                   size = n,
                                   replace = TRUE,
                                   prob = ideology_prob))


voters <- voters %>%
  mutate(prob_dem = prob_dem_given_ideology[ideology],
    voted_dem_numeric = rbinom(n(), size = 1, prob = prob_dem),
    vote = factor(voted_dem_numeric, levels = c(0, 1), labels = c("Other", "Democrat")))

voters %>%
  group_by(ideology) %>%
  summarise(n = n(),
    prob_dem_given_ideology = mean(voted_dem_numeric)) %>%
  mutate(ideology_proportion = n / sum(n)) %>% print()

empirical_prob <- mean(voters$voted_dem_numeric) %>% print()

ggplot(voters, aes(x = ideology, fill = vote)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Democrat" = "blue", "Other" = "darkorange")) +
  labs(title = "Simulated Voting Behavior by Political Ideology",
    x = "Voter Ideology",
    y = "Count of Voters") +
  theme_minimal()


#### 8. ####
#(a)
set.seed(502502502)

n <- 10000
p_initial_support <- 0.6
p_conversion <- 0.75

voters <- tibble(
  voter_id = 1:n,
  saw_ad = c(rep(TRUE, n / 2), rep(FALSE, n / 2)),
  initial_support = sample(
    c("supportive", "not_supportive"),
    size = n,
    replace = TRUE,
    prob = c(p_initial_support, 1 - p_initial_support)))

voters <- voters %>%
  mutate(final_support = initial_support,
    is_conversion_candidate = (saw_ad == TRUE & initial_support == "not_supportive"))

conversion_indices <- which(voters$is_conversion_candidate)
conversions <- sample(
  c("supportive", "not_supportive"),
  size = length(conversion_indices),
  replace = TRUE,
  prob = c(p_conversion, 1 - p_conversion)
)
voters$final_support[conversion_indices] <- conversions


voters %>%
  group_by(saw_ad) %>%
  summarise(count = n(),
    prob_support = mean(final_support == "supportive")) %>%
  mutate(group_proportion = count / sum(count)) %>% print()


#(b)
voters %>%
  group_by(saw_ad) %>%
  summarise(prob_support = mean(final_support == "supportive")) %>%
  mutate(group = ifelse(saw_ad, "Saw Ad", "Did Not See Ad")) %>% 
  ggplot(aes(x = group, y = prob_support, fill = group)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "Impact of Ad on Voter Support",
       x = "",
       y = "Probability of Supporting Candidate") +
  theme_minimal()




