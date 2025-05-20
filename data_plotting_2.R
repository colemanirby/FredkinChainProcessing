setwd("../Desktop/School/data_processing")
install.packages("httpgd")
install.packages("rjson")
install.packages("ggplot2")
install.packages("plotrix")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggforce")
install.packages("ggiraph")
install.packages("ggpmisc")
install.packages("investr")
library(investr)
library(ggiraph)
library("dplyr")
library("tidyr")
library("ggplot2")
library("rjson")
library("plotrix")
library("ggforce")
library("ggpmisc")

#########Log Linear Saturation Plotting#################
z_0 <- 3.18
df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(!is.na(mean)) %>%
  mutate(ratio = (2 * ss) / (chain_size - 2)) %>%
  mutate(modifed_y = log10(mean/(chain_size^(1+z_0)))) %>%
  filter(between(ratio, 0, 1))
plot<- ggplot(df, aes(x = ratio, y = modifed_y, color = chain_size)) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = expression("Bond lifetimes as a function of Saturation 0 " < frac(2*s,N-2) ~"" <= "1.0"),
       x = expression(frac(2*s,N-2)), y = expression("log("*frac(tau, "N"^(1+z_0))*")")) +
  theme_minimal()
plot
ggsave("Plots/saturation_0_100_log_linear_N_oneplus_z_scaled.jpg", plot=plot)
#########ENDLog Linear Saturation PlottingEND#################
#########Log Log Saturation Plotting#################
z_0 <- 3.18
df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(!is.na(mean)) %>%
  mutate(ratio = log10((2 * ss) / (chain_size - 2))) %>%
  mutate(modifed_y = log10(mean/(chain_size^(1+z_0)))) %>%
  filter(between(ratio, 0, 1))
plot<- ggplot(df, aes(x = ratio, y = modifed_y, color = chain_size)) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = expression("Bond lifetimes as a function of Saturation 0 " < frac(2*s,N-2) ~"" <= "1.0"),
       x = expression(log(frac(2*s,N-2))), y = expression("log("*frac(tau, "N"^(1+z_0))*")")) +
  theme_minimal()
plot
ggsave("Plots/saturation_0_100_log_log_N_oneplus_z_scaled.jpg", plot=plot)
#########ENDLog Linear Saturation PlottingEND#################
#########Weighted NLS Fitting with Taylor Series Assumption#################
z_0 <- 3.099
c_0 <- 0.0014
c_1 <- 0.01
z_1 <- -1
default_filename <- "Plots/Analysis/lifetime_s+_non_linear_ts_assumption.jpg"
default_title <- "Lifetime of Excited Bonds in s = + with Best Fit Line"
spin_sector <- seq(from = 2, to = 50, by = 1)
c_0_coll <- c(0.0059996143)
c_1_coll <- c(0.02861064)
z_0_coll <- c(3.168815)
z_1_coll <- c(-2.114317)
for (s in spin_sector) {
  file_str <- gsub("+", toString(s), default_filename, fixed = TRUE)
  plot_title <- gsub("+", toString(s), default_title, fixed = TRUE)
  df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(ss == s) %>%
  filter(!is.na(mean)) %>%
  mutate(ratio = mean/chain_size) %>%
  mutate(se_ratio = err/chain_size) %>%
  mutate(se_log = se_ratio/(ratio*log(10))) %>%
  mutate(x = chain_size) %>%
  mutate(y = log10(ratio))

x_range <- seq(from = 2*(s+1), to = 300, by = 2)
non_lin_model <- nls(data = df, y ~ (z_0 + z_1/chain_size)*log10(chain_size) + log10(c_0 + c_1/chain_size),  algorithm = "port", lower = c(c_1 = 0, z_1 = -100, c_0 = 0, z_0 = -100), start = list(c_1 = c_1, z_1 = z_1, c_0 = c_0, z_0 = z_0), control = nls.control(maxiter = 500, warnOnly = TRUE), weights = 1/(se_log^2))
c_1 <- coef(non_lin_model)["c_1"]
z_1 <- coef(non_lin_model)["z_1"]
c_0 <- coef(non_lin_model)["c_0"]
z_0 <- coef(non_lin_model)["z_0"]

c_1_coll <- c(c_1_coll, c_1)
c_0_coll <- c(c_0_coll, c_0)
z_1_coll <- c(z_1_coll, z_1)
z_0_coll <- c(z_0_coll, z_0)
  print("#############")
  print(s)
  print(c_1)
  print(z_1)
  print(z_0)
  print(c_0)
  print("#############")
# f <- function(x) (z_0 + z_1/x)*log10(x) + log10(c_0 + c_1/x)
# bar_width <- 0.02 * diff(range(log10(df$x)))
# plot <- ggplot(df, aes(x = log10(x), y = y)) +
#   geom_point(size = 2) +
#   geom_errorbar(aes(ymin = y - se_log, ymax = y + se_log), width = bar_width) +
#   labs(title = plot_title,
#        x = expression(log(N)), y = expression(log(tau/N))) +
#   theme_minimal() +
#   geom_line(aes(y = f(x_range), x = log10(x_range)))

# ggsave(file_str, plot=plot)
}
df_coeffs <- data.frame(c_1 = c_1_coll, c_0 = c_0_coll, z_1 = z_1_coll, z_0 = z_0_coll)
df_coeffs$x <- seq(from = 1, to = 50, by = 1)
plot <- ggplot(df_coeffs, aes(x = x, y = z_0)) +
      geom_point(size = 2)
ggsave(plot = plot, "Plots/Analysis/CoeffsPlots/z_0_weighted_ts.jpg")
plot <- ggplot(df_coeffs, aes(x = x, y = z_1)) +
      geom_point(size = 2)
ggsave(plot = plot, "Plots/Analysis/CoeffsPlots/z_1_weighted_ts.jpg")
plot <- ggplot(df_coeffs, aes(x = x, y = c_0)) +
      geom_point(size = 2)
ggsave(plot = plot, "Plots/Analysis/CoeffsPlots/c_0_weighted_ts.jpg")
plot <- ggplot(df_coeffs, aes(x = x, y = c_1)) +
      geom_point(size = 2)
ggsave(plot = plot, "Plots/Analysis/CoeffsPlots/c_1_weighted_ts.jpg")
plot <- ggplot(df_coeffs, aes(x = x)) +
  geom_point(aes(y = z_1), color = "red") +
  geom_point(aes(y = z_0), color = "blue") +
  labs(y = "z_1(red) and z_0(blue)", x = "spin sector")
ggsave(plot = plot, "Plots/Analysis/CoeffsPlots/z_1_z_0_weighted_ts.jpg")
plot

plot <- ggplot(df_coeffs, aes(x = x)) +
  geom_point(aes(y = c_1), color = "red") +
  geom_point(aes(y = c_0), color = "blue") +
  labs(y = "c_1(red) and c_0(blue)", x = "spin sector")
plot
ggsave(plot = plot, "Plots/Analysis/CoeffsPlots/c_1_c_0_weighted_ts.jpg")

plot <- ggplot(df_coeffs, aes(x = x, y = z_0)) +
      geom_point(size = 2) +
      annotate("segment", x=6, xend=6, y=0, yend = 3.6974706) +
      annotate("segment", x=0, xend=6, y=3.6974706, yend = 3.6974706) +
      annotate("label", x = 15, y = 1, label = "z_0 = 3.697(4)") +
      annotate("segment", x=7, xend=7, y=0, yend = 3.9812140) +
      annotate("segment", x=0, xend=7, y=3.9812140, yend = 3.9812140) +
      annotate("label", x = 15, y = 4, label = "z_0_max = 3.981(2)") +
      annotate("segment", x = 1, xend = 1, y = 0, yend = 3.1688150) +
      annotate("segment", x = 0, xend = 1, y=3.1688150, yend = 3.1688150) +
      annotate("label", x = 5, y = 2, label = "<- z_0 = 3.168(8)")
plot

ggsave(plot = plot, "Plots/Analysis/CoeffsPlots/z_0_labeled_weighted_ts_assumption.jpg")
#########Weighted NLS Fitting with Taylor Series Assumption#################
#########Weighted NLS Fitting with Log-Normal Approximation#################
z_0 <- 3.099
c_0 <- 0.0014
c_1 <- 0.01
z_1 <- -1
default_filename <- "Plots/Analysis/lifetime_s+_non_linear_log_normal.jpg"
default_title <- "Lifetime of Excited Bonds in s = + with Best Fit Line"
spin_sector <- seq(from = 2, to = 50, by = 1)
c_0_coll <- c(0.0059996143)
c_1_coll <- c(0.02861064)
z_0_coll <- c(3.168815)
z_1_coll <- c(-2.114317)
for (s in spin_sector) {
  file_str <- gsub("+", toString(s), default_filename, fixed = TRUE)
  plot_title <- gsub("+", toString(s), default_title, fixed = TRUE)
  df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(ss == s) %>%
  filter(!is.na(mean)) %>%
  mutate(ratio = mean/chain_size) %>%
  mutate(se_ratio = err/chain_size) %>%
  mutate(se_log = sqrt(log(1+(err/mean)^2))/log(10)) %>%
  mutate(x = chain_size) %>%
  mutate(y = log10(ratio))

x_range <- seq(from = 2*(s+1), to = 300, by = 2)
non_lin_model <- nls(data = df, y ~ (z_0 + z_1/chain_size)*log10(chain_size) + log10(c_0 + c_1/chain_size),  algorithm = "port", lower = c(c_1 = 0, z_1 = -100, c_0 = 0, z_0 = -100), start = list(c_1 = c_1, z_1 = z_1, c_0 = c_0, z_0 = z_0), control = nls.control(maxiter = 500, warnOnly = TRUE), weights = 1/(se_log^2))
c_1 <- coef(non_lin_model)["c_1"]
z_1 <- coef(non_lin_model)["z_1"]
c_0 <- coef(non_lin_model)["c_0"]
z_0 <- coef(non_lin_model)["z_0"]

c_1_coll <- c(c_1_coll, c_1)
c_0_coll <- c(c_0_coll, c_0)
z_1_coll <- c(z_1_coll, z_1)
z_0_coll <- c(z_0_coll, z_0)
  print("#############")
  print(s)
  print(c_1)
  print(z_1)
  print(z_0)
  print(c_0)
  print("#############")
# f <- function(x) (z_0 + z_1/x)*log10(x) + log10(c_0 + c_1/x)
# bar_width <- 0.02 * diff(range(log10(df$x)))
# plot <- ggplot(df, aes(x = log10(x), y = y)) +
#   geom_point(size = 2) +
#   geom_errorbar(aes(ymin = y - se_log, ymax = y + se_log), width = bar_width) +
#   labs(title = plot_title,
#        x = expression(log(N)), y = expression(log(tau/N))) +
#   theme_minimal() +
#   geom_line(aes(y = f(x_range), x = log10(x_range)))

# ggsave(file_str, plot=plot)
}
df_coeffs <- data.frame(c_1 = c_1_coll, c_0 = c_0_coll, z_1 = z_1_coll, z_0 = z_0_coll)
df_coeffs$x <- seq(from = 1, to = 50, by = 1)
plot <- ggplot(df_coeffs, aes(x = x, y = z_0)) +
      geom_point(size = 2)
ggsave(plot = plot, "Plots/Analysis/CoeffsPlots/z_0_weighted_ln.jpg")
plot <- ggplot(df_coeffs, aes(x = x, y = z_1)) +
      geom_point(size = 2)
ggsave(plot = plot, "Plots/Analysis/CoeffsPlots/z_1_weighted_ln.jpg")
plot <- ggplot(df_coeffs, aes(x = x, y = c_0)) +
      geom_point(size = 2)
ggsave(plot = plot, "Plots/Analysis/CoeffsPlots/c_0_weighted_ln.jpg")
plot <- ggplot(df_coeffs, aes(x = x, y = c_1)) +
      geom_point(size = 2)
ggsave(plot = plot, "Plots/Analysis/CoeffsPlots/c_1_weighted_ln.jpg")
plot <- ggplot(df_coeffs, aes(x = x)) +
  geom_point(aes(y = z_1), color = "red") +
  geom_point(aes(y = z_0), color = "blue") +
  labs(y = "z_1(red) and z_0(blue)", x = "spin sector")
ggsave(plot = plot, "Plots/Analysis/CoeffsPlots/z_1_z_0_weighted_ln.jpg")
plot

plot <- ggplot(df_coeffs, aes(x = x)) +
  geom_point(aes(y = c_1), color = "red") +
  geom_point(aes(y = c_0), color = "blue") +
  labs(y = "c_1(red) and c_0(blue)", x = "spin sector")
plot
ggsave(plot = plot, "Plots/Analysis/CoeffsPlots/c_1_c_0_weighted_ln.jpg")

plot <- ggplot(df_coeffs, aes(x = x, y = z_0)) +
      geom_point(size = 2) +
      annotate("segment", x=6, xend=6, y=0, yend = 3.6972279) +
      annotate("segment", x=0, xend=6, y=3.6972279, yend = 3.6972279) +
      annotate("label", x = 15, y = 1, label = "z_0 = 3.697(2)") +
      annotate("segment", x=7, xend=7, y=0, yend = 3.9813811) +
      annotate("segment", x=0, xend=7, y=3.9813811, yend = 3.9813811) +
      annotate("label", x = 15, y = 4, label = "z_0_max = 3.981(4)") +
      annotate("segment", x = 1, xend = 1, y = 0, yend = 3.1688150) +
      annotate("segment", x = 0, xend = 1, y=3.1688150, yend = 3.1688150) +
      annotate("label", x = 5, y = 2, label = "z_0 = 3.168(8)")
plot

ggsave(plot = plot, "Plots/Analysis/CoeffsPlots/z_0_labeled_weighted_ln.jpg")
#########Weighted NLS Fitting with Taylor Series Assumption#################