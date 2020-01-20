knitr::opts_chunk$set(echo = TRUE)
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, skimr, GGally, plotly, viridis, caret, randomForest, e1071, rpart, 
               xgboost, h2o, corrplot, rpart.plot, corrgram, ggplot2, highcharter, 
               ggthemes, psych, scales, treemap, treemapify, repr, cowplot, magrittr, ggpubr,
               RColorBrewer, plotrix, ggrepel, tidyverse, gridExtra.)
mydata<-read.csv(file = "c:/real_estate_db.csv")
mydata <- subset(mydata, hc_mortgage_mean != "NaN" & !(is.na(hc_mortgage_mean)),)
mydata <- subset(mydata, rent_mean != "NaN" & !(is.na(rent_mean)),)
mydata <- subset(mydata, state != "NaN" & !(is.na(state)),)
mydata <- subset(mydata, male_pop != "NaN" & !(is.na(male_pop)),)
mydata <- subset(mydata, female_pop != "NaN" & !(is.na(female_pop)),)
mydata <- subset(mydata, hs_degree_male != "NaN" & !(is.na(hs_degree_male)),)
mydata <- subset(mydata, hs_degree_female != "NaN" & !(is.na(hs_degree_female)),)
mydata <- subset(mydata, type != "NaN" & !(is.na(type)),)
testprop<-sample(mydata,size = 300, replace = TRUE)
testofproportions <- subset(mydata, type != "NaN" & !(is.na(type)),)
testofproportions <- subset(mydata, pop != "NaN" & !(is.na(pop)),)
testofproportions <- subset(mydata, city != "NaN" & !(is.na(city)),)
head(mydata)
summary(mydata)
numerics <- select_if(mydata, is.numeric)
colnames(numerics)
# What is the distribution of the family mean

options(repr.plot.width=8, repr.plot.height=7)

# numerics %>% filter(!is.na(family_mean)) %>% 
#   summarize(mean=mean(family_mean), sd=sd(family_mean))

subset.rent <- numerics %>% filter(!is.na(rent_mean))

p1 <- ggplot(data=subset.rent, aes(x=rent_mean))+
  geom_histogram(aes(y=..density..), bins = 40, fill="#81F781")+
  stat_function(fun=dnorm, color="black",
                args=list(mean=mean(subset.rent$rent_mean), 
                          sd=sd(subset.rent$rent_mean))) + theme_minimal() + 
  theme(plot.title=element_text(hjust=0.5)) + labs(title="Rent Mean Distribution", 
                                                   x="Rent Mean", y="Probability")
p1

subset.female <- numerics %>%
  filter(!is.na(female_pop))


p2 <- ggplot(data=subset.female, aes(x=female_pop))+
  geom_histogram(aes(y=..density..), bins = 40, fill="#FAAC58")+
  stat_function(fun=dnorm, color="black",
                args=list(mean=mean(subset.female$female_pop[subset.female$STATEID == "2"]), 
                          sd=sd(subset.female$female_pop[subset.female$STATEID == "2"]))) + theme_minimal() + 
  theme(plot.title=element_text(hjust=0.5)) + labs(title="Normal Distribution", 
                                                   x="Female Population", y="Probability")
p2
ggplot(mydata, aes(mydata$pop, mydata$male_pop)) + geom_point(shape = 7) + geom_smooth(method = "lm" ,col = "Purple" ,xlab = "population", ylab = "male_pop")
subset.debt <- numerics %>%
  filter(!is.na(debt))


p3 <- ggplot(data=subset.debt, aes(x=debt))+
  geom_histogram(aes(y=..density..), bins = 40, fill="#FA5858")+
  stat_function(fun=dnorm, color="black",
                args=list(mean=mean(subset.debt$debt), 
                          sd=sd(subset.debt$debt))) + theme_minimal() + 
  theme(plot.title=element_text(hjust=0.5)) + labs(title="Left Skewed Distribution", 
                                                   x="Debt", y="Probability")
plot_grid(p1, p2, p3, align='h', nrow=3)
cols <- numerics %>% select(debt, rent_mean, female_age_mean) %>% 
  filter(!is.na(debt), !is.na(rent_mean), !is.na(female_age_mean))

do.call(cbind, lapply(cols, summary))
options(repr.plot.width=8, repr.plot.height=4)
# windows(height = 7, width = 3.5)
# Lines: Mean is the blue line and Median the green line

# First Subplot
p4 <- hist(subset.rent$rent_mean, col="LightBlue", xlab="Rent", main="Distribution of Rent")
abline(v = mean(subset.rent$rent_mean), col = "blue", lwd = 2, lty="dashed")
abline(v = median(subset.rent$rent_mean), col = "Red", lwd = 2, lty="dashed")
legend(x = c(4000, 3200), y = c(8000, 3600), legend=c("Mean", "Median"), col=c("blue","red"), cex=0.7, 
       lty="dashed", lwd=1, y.intersp = 3.9, x.intersp=3.8, xjust=-1.8)
four_states <- mydata %>% select(state, rent_mean) %>% filter(!is.na(rent_mean)) %>% 
  filter(state == "New York" | state == "California"| state == "Florida" | state == "Texas") %>%
  group_by(state) %>% do(sample_n(., size=250))

ggplot(four_states, aes(x=state, y=rent_mean, fill=state)) + geom_boxplot() + 
  stat_summary(fun.y=mean, colour="orange", geom="point", size=1) + 
  theme_minimal() + theme(plot.title=element_text(hjust=0.5, size=12)) + 
  labs(title="Difference in Independent Categorical Means \n (Sample Size 250)", x="States", y="Average Rent") + 
  scale_fill_brewer(palette="Set3")
# Third Subplot
p6 <- hist(subset.debt$debt, col="#F78181", xlab="Debt", main="Distribution of Debt")
abline(v = mean(subset.debt$debt), col = "blue", lwd = 2, lty="dashed")
abline(v = median(subset.debt$debt), col = "green", lwd = 2, lty="dashed")
legend(x = c(0.85, 1), y = c(5000, 3500), legend=c("Mean", "Median"), col=c("blue","green"), cex=0.8, 
       lty="dashed", lwd=1, y.intersp = 2, x.intersp=0.7, xjust=0.5)
ggplot(mydata, aes(type, ..count..)) + geom_bar(aes(fill = pop), position = "dodge")
# Use boxplots to explain better the concepts of quartiles

# We will use type of place
t.place <- mydata %>% select(rent_mean, type) %>% 
  filter(!is.na(rent_mean), !is.na(type)) %>%
  ggplot(aes(x=type, y=rent_mean)) + geom_boxplot(fill="white", colour="black", 
                                                  outlier.colour = "red", outlier.shape = 1) + 
  theme_minimal() + theme(plot.title=element_text(hjust=0.5)) + coord_flip() + 
  labs(title="Distribution of Average Rent by Type of Place", x="Type", y="Average Rent")

t.place + scale_fill_manual(values=c("#999999", "#E69F00"))
hist(mydata$female_pop[mydata$state == "Alaska"], col="blue", main = 'Distribution of Female Population in Alaska', ylab = 'Frequency', xlab = 'Female population')

qqnorm(mydata$female_pop[mydata$state == "Alaska"], col="blue")
qqline(mydata$female_pop[mydata$state == "Alaska"], col="red")

plot(mydata$female_pop[mydata$state == "Alaska"], type="l" , ylab = "Female population")
set.seed(1234)
t.test(mydata$female_pop[mydata$state == "Alaska"], mu = 1800)
x_bar <- mean(mydata$female_pop[mydata$state == "Alaska"])
# null hypothesized population mean
mu_0 <- 1800
# sample st. dev
s <- sd(mydata$female_pop[mydata$state == "Alaska"])
# sample size
n <- length(mydata$female_pop[mydata$state == "Alaska"])
# t-test test statistic
t <- (x_bar - mu_0)/(s/sqrt(n))
# two-sided p-value so multiply by 2
one_sided_t_pval <- pt(q = t, df = n-1, lower.tail = F)*2
one_sided_t_pval
# lower bound
x_bar+(qt(0.025, n-1)*(s/sqrt(n))) # alternately you can use x_bar-(qt(0.975, n-1)*(s/sqrt(n))
# upper bound
x_bar+(qt(0.975, n-1)*(s/sqrt(n))) # alternately you can use x_bar-(qt(0.025, n-1)*(s/sqrt(n)))
plot(x = seq(-4, 4, length = 100), dnorm(seq(-4, 4, length = 100)),type = 'l')
abline(v = qt(0.975, n-1),col = "Red")
abline(v = qt(0.025, n-1), col = "Blue")

num_sims <- 10000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  results[i] <- mean(sample(x = mydata$rent_mean[mydata$state == "Alaska"],
                            size = n,
                            replace = TRUE))
}
# Finally plot the results
hist(results, freq = FALSE, main='Sampling Distribution of the Sample Mean', xlab = 'Average Mean Rent in New York', ylab = 'Density')
# estimate a normal curve over it - this looks pretty good!
lines(x = seq(1100, 1300, .1), dnorm(seq(1100, 1300, .1), mean = x_bar, sd(results)))
# Shifting the sample so that the null hypothesis is true
time_given_H0_true <- mydata$rent_mean[mydata$state == "Alaska"] - mean(mydata$rent_mean[mydata$state == "Alaska"]) + mu_0
# This data is pretty skewed so even though n is large, We are going to perform a lot of simulations
num_sims <- 10000
# A vector to store my results
results_given_H0_true <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims)
{
  results_given_H0_true[i] <- mean(sample(x = time_given_H0_true,
                                          size = n,
                                          replace = TRUE))
}
# Finally plot the results
hist(results_given_H0_true, freq = FALSE, main='Sampling Distribution of the Sample Measure
n, Given Null Hypothesis is True', xlab = 'Average Rent', ylab = 'Density', xlim = c(1400,2500))
# adding line to show values more extreme on upper end
abline(v=x_bar , col = "red",)
# adding line to show values more extreme on lower end
low_end_extreme <- mean(results_given_H0_true)+(mean(results_given_H0_true)-x_bar)
abline(v=low_end_extreme, col="red")

low_end_extreme
high_end_extreme <- mean(results_given_H0_true) + x_bar
high_end_extreme
abline(v=x_bar, col = "red")
abline(v=low_end_extreme, col="blue")
# counts of values more extreme than the test statistic in our original sample, given H0 is true
# two sided given the alternate hypothesis
count_of_more_extreme_lower_tail <- sum(results_given_H0_true <= low_end_extreme)
count_of_more_extreme_upper_tail <- sum(results_given_H0_true >= high_end_extreme)
bootstrap_pvalue <- (count_of_more_extreme_lower_tail + count_of_more_extreme_upper_tail)/num_sims
bootstrap_pvalue
# need the standard error which is the standard deviation of the results
bootstrap_SE_X_bar <- sd(results)
# an estimate is to use the formula statistic +/- 2*SE
c(x_bar - 2*bootstrap_SE_X_bar, x_bar + 2*bootstrap_SE_X_bar)

# you can also use the 5th and 97.5th quantiles to determine the bounds:
c(quantile(results, c(.025, .975)))
# compare to our t-methods
c(x_bar+(qt(0.025, n-1)*(s/sqrt(n))), x_bar+(qt(0.975, n-1)*(s/sqrt(n))))
x <- sum(testofproportions$pop[testofproportions$city == 'Anchorage'])
n <- sum(testofproportions$pop[testofproportions$state  =='Alaska'])
p_cap <- x/n
p_cap
p_0 = 0.3333
z <- (p_cap - p_0)/sqrt((.3333*(1-.3333))/n)
p_val <- pnorm(z, lower.tail = F)
p_val
binom.test(x=x, n = n, p=(1/3), alternative="greater")
binom.test(x=x, n = n, p=(1/3), alternative="greater")$conf.int
binom.test(x=x, n = n, p=(1/3), alternative="two.sided")

Anchorage <- rep(c(1,0), c(33, 100-33))
Anchorage

table(Anchorage)
# This data is pretty skewed so even though n is large, We are going to do a lot of simulations
num_sims <- 10000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  results[i] <- mean(sample(x = Anchorage,
                            size = 100,
                            replace = TRUE))
}
# Finally plot the results
hist(results, freq = FALSE, main='Sampling Distribution of the Sample Proportion', xlab
     = 'Proportion of Anchorage', ylab = 'Density')
# estimating a normal curve over it - this looks pretty good!
lines(x = seq(.1, .75, .001), dnorm(seq(.1, .75, .001), mean = mean(results), sd = sd(results)))
c(quantile(results, c(.05, 1)))
cat("exact binomial test")
binom.test(x=33, n = 100, p=(1/3), alternative="greater")$conf.int
# Under the assumption that the null hypothesis is true, we have 33% population in Anchorage
Anchorage<- rep(c(1, 0), c(33, 100-33))
num_sims <- 10000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  results[i] <- mean(sample(x = Anchorage,
                            size = 100,
                            replace = TRUE))
}
# Finally plot the results
hist(results, freq = FALSE, main='Sampling Distribution of the Sample Proportion under H
_0:p=0.3', xlab = 'Proportion of People in Anchorage', ylab = 'Density')
# estimate a normal curve over it - this looks pretty good!
lines(x = seq(.15, .75, .001), dnorm(seq(.15, .75, .001), mean = mean(results), sd = sd
                                     (results)))
abline(v=.33, col="red")
count_of_more_extreme_upper_tail <- sum(results >= .33)
bootstrap_pvalue <- count_of_more_extreme_upper_tail/num_sims
cat("Bootstrap p-value")
bootstrap_pvalue
binom.test(x=33, n = 100, p=(1/3), alternative="greater")$p.value
c(quantile(results, c(.05, 1)))
set.seed(0)
options(repr.plot.width=8, repr.plot.height=5)


south_states <- mydata %>% select(state, rent_mean) %>% filter(state == "Delaware" | state == "Alaska") %>%
  ggplot(aes(x=state, y=rent_mean, fill=state)) + geom_boxplot() + 
  stat_summary(fun.y=mean, colour="orange", geom="point", size=1) + 
  theme_minimal() + 
  theme(plot.title=element_text(hjust=0.5, size=10)) + 
  labs(title="Difference Between Two Independent Means", y="Rent Mean", x="States") + 
  scale_fill_brewer(palette="Set3")

south_states
qqnorm(mydata$rent_mean[mydata$state == "Delaware"],col = 'blue')
qqline(mydata$rent_mean[mydata$state == "Delaware"] ,col = 'red')
qqnorm(mydata$rent_mean[mydata$state == "Alaska"],col = 'blue')
qqline(mydata$rent_mean[mydata$state == "Alaska"],col = 'red')
t.test(mydata$rent_mean[mydata$state == "Delaware"], mydata$rent_mean[mydata$state == "Alaska"])
x1_bar <- mean(mydata$rent_mean[mydata$state == "Delaware"])
x2_bar <- mean(mydata$rent_mean[mydata$state == "Alaska"])
n<-min(length(mydata$rent_mean[mydata$state == "Delaware"]),mydata$rent_mean[mydata$state == "Alaska"])
se <- sd(mydata$rent_mean[mydata$state == "Delaware"])**2/length(mydata$rent_mean[mydata$state == "Delaware"]) + sd(mydata$rent_mean[mydata$state == "Alaska"])**2/length(mydata$rent_mean[mydata$state == "Alaska"])

t <- (x1_bar - x2_bar) / sqrt(se)

two_sided_t_pval <- pt(t, df=n-1, lower.tail = T)*2
two_sided_t_pval
c((x1_bar - x2_bar)+qt(0.025,df=n-1)*sqrt(se),(x1_bar - x2_bar)+qt(0.975,df=n-1)*sqrt(se))
num_sims <- 10000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  mean_dela <- mean(sample(x = mydata$rent_mean[mydata$state == "Delaware"],
                           size = 34,
                           replace = TRUE))
  mean_alas <- mean(sample(x = mydata$rent_mean[mydata$state == "Alaska"],
                           size = 34,
                           replace = TRUE))
  results[i] <-   mean_dela - mean_alas
}
# Finally plot the results
hist(results, freq = FALSE, main='Sampling Distribution of the Sample Mean', xlab = 'Ave
rage Difference in Rent', ylab = 'Density')
c(quantile(results, c(.025, .975)))
t.test(mydata$rent_mean[mydata$state == "Delaware"], mydata$rent_mean[mydata$state == "Alaska"])$conf.int
transform(mydata, state=sample(state))
num_sims <- 1000
# A vector to store my results
results_given_H0_true <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  # idea here is if there is no relationshipm we should be able to shuffle the groups
  shuffled_groups <- transform(mydata, state=sample(state))
  mean_delaware <- mean(shuffled_groups$rent_mean[shuffled_groups$state=="Delaware"])
  mean_alaska <- mean(shuffled_groups$rent_mean[shuffled_groups$state=="Alaska"])
  results_given_H0_true[i] <-mean_delaware - mean_alaska
}

# Finally plot the results
hist(results_given_H0_true, freq = FALSE,
     main='Dist. of the Diff in Sample Means Under Null',
     xlab = 'Average Difference Rent  under Null',
     ylab = 'Density')

diff_in_sample_means <- mean(mydata$rent_mean[mydata$state == "Delaware"]) - mean(mydata$rent_mean[mydata$state == "Alaska"])
diff_in_sample_means
lower_extreme <- 
  abline(v=diff_in_sample_means, col = "blue")
abline(v=abs(diff_in_sample_means), col = "red")

count_of_more_extreme_lower_tail <- sum(results_given_H0_true <= diff_in_sample_means)
count_of_more_extreme_lower_tail
count_of_more_extreme_upper_tail <- sum(results_given_H0_true > abs(diff_in_sample_means))
count_of_more_extreme_upper_tail
bootstrap_pvalue <- (count_of_more_extreme_lower_tail + count_of_more_extreme_upper_tail)/num_sims
cat("Bootstrap p-value")

bootstrap_pvalue
t.test(mydata$rent_mean[mydata$state == "Delaware"], mydata$rent_mean[mydata$state == "Alaska"])
c((x1_bar - x2_bar)+qt(0.025,df=n-1)*sqrt(se),(x1_bar - x2_bar)+qt(0.975,df=n-1)*sqrt(se))
# the parts of the test statistic
# sample props
p_hat_c <- sum(mydata$female_pop[mydata$city == 'Anchorage'])/sum(mydata$female_pop[mydata$state == 'Alaska'])
p_hat_t <- sum(mydata$male_pop[mydata$city =='Anchorage'])/sum(mydata$male_pop[mydata$state == 'Alaska'])
p_hat_c
p_hat_t
# null hypothesized population prop difference between the two groups
p_0 <- 0
# sample size
n_c <- sum(mydata$female_pop[mydata$state == 'Alaska'])
n_t <- sum(mydata$male_pop[mydata$state == 'Alaska'])
# sample variances
den_p_c <- (p_hat_c*(1-p_hat_c))/n_c
den_p_t <- (p_hat_t*(1-p_hat_t))/n_t
# z-test test statistic
z <- (p_hat_c - p_hat_t - p_0)/sqrt(den_p_c + den_p_t)
z
# two sided p-value
two_sided_diff_prop_pval <- pnorm(q = z, lower.tail = FALSE)*2
two_sided_diff_prop_pval
# lower bound
(p_hat_c - p_hat_t)+(qnorm(0.025)*sqrt(den_p_c + den_p_t))
# upper bound
(p_hat_c - p_hat_t)+(qnorm(0.975)*sqrt(den_p_c + den_p_t))
# Make the data
females <- rep(c(1,0), c(sum(mydata$female_pop[mydata$city == 'Anchorage']), n_c - sum(mydata$female_pop[mydata$city == 'Anchorage'])))
males <- rep(c(1,0), c(sum(mydata$male_pop[mydata$city == 'Anchorage']), 
                       n_t- sum(mydata$male_pop[mydata$city == 'Anchorage'])))

num_sims <- 1000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  prop_female <- mean(sample(females,
                             size = n_c,
                             replace = TRUE))
  prop_male <- mean(sample(x = males,
                           size = n_t,
                           replace = TRUE))
  results[i] <- prop_female - prop_male
}
# Finally plot the results
hist(results, freq = FALSE, main='Dist. of the Diff in Prop', xlab = 'Difference in Pro
p. of Free Throws Made', ylab = 'Density')

c(quantile(results, c(.025, .975)))
c((p_hat_c - p_hat_t)+(qnorm(0.025)*sqrt(den_p_c + den_p_t)), (p_hat_c- p_hat_t)+(qnorm
                                                                                  (0.975)*sqrt(den_p_c + den_p_t)))
# Make the data
df_combined <- data.frame("population_proportion" = c(females, males),
                          "Anchorage" = rep(c("females", "males"), c(n_c, n_t)))
num_sims <- 1000
# A vector to store my results
results_given_H0_true <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  # idea here is if there is no relationshipm we should be able to shuffle the groups
  shuffled_groups <- transform(df_combined, Anchorage=sample(Anchorage))
  prop_females <- mean(shuffled_groups$population_proportion[shuffled_groups$Anchorage=="females"])
  prop_males <- mean(shuffled_groups$population_proportion[shuffled_groups$Anchorage=="males"])
  results_given_H0_true[i] <- prop_females - prop_males
}
results_given_H0_true
# Finally plot the results
hist(results_given_H0_true, freq = FALSE,
     main='Dist. of the Diff in Sample Sample Props Under Null',
     xlab = 'Average Difference in Prop. of females and males Made under Null',
     ylab = 'Density',xlim = c(-0.010,0.010) )
diff_in_sample_props <- p_hat_c - p_hat_t
abline(v=diff_in_sample_props, col = "blue")
abline(v=-diff_in_sample_props, col = "red")
count_of_more_extreme_lower_tail <- sum(results_given_H0_true <= -diff_in_sample_props)
count_of_more_extreme_upper_tail <- sum(results_given_H0_true > diff_in_sample_props)
bootstrap_pvalue <- (count_of_more_extreme_lower_tail + count_of_more_extreme_upper_tail)/num_sims
cat("Bootstrap p-value")
bootstrap_pvalue
two_sided_diff_prop_pval
c(quantile(results,c(0.025, 0.975)))
set.seed(315)
newdata<-sample(mydata$type, size = 500)
newdata
sum(((table(newdata) - 80)^2)/80)
pchisq(541.7, df = 6-1, lower.tail = FALSE