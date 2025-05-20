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

####################################
# Plot tau vs ss for various N      #
####################################
df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)

main_title <- "Avg. Lifetimes for chains 4 \U2264 N \U2264 64"
plot <- ggplot(df, aes(x = ss, y = log10(mean), color = chain_size,
                       tooltip = paste("Size:", chain_size, "<br>SS:",
                                       ss, "<br>Lifetime:", round(mean, 2)),
                       group = chain_size, data_id = chain_size)) +
  geom_line_interactive(size = 1.2, hover_nearest = TRUE) +
  geom_point_interactive(size = 0.8, alpha = 0.7) +
  # facet_zoom(ylim = c(1, 100)) +
  labs(x = "Spin Sector", y = expression(tau), title = main_title) +
  theme_classic() +
  theme(legend.position = "none")
interactive_plot <- girafe(ggobj = plot)
interactive_plot <- girafe_options(
  interactive_plot,
  opts_hover(css = "stroke:black;cursor:pointer;"),
  opts_selection(type = "single", css = "fill:red;stroke:black;"),
  opts_toolbar(saveaspng = TRUE)
)

htmltools::save_html(interactive_plot,
                     "Plots/geom-line-interactive-tau_vs_ss_all_cs.html"
                     , libdir = "deps")

###################################
#Plot tau vs N for various ss     #
###################################
df <- read.table(file = "per_ss_pivot.txt",
                 sep = "\t", header = TRUE)
main_title <- "Avg. Lifetimes based on spin sector"
plot <- ggplot(df, aes(x = chainsize, y = log10(mean),
                       color = ss, tooltip = paste("Sector:", ss, "<br>N:",
                                                   chainsize, "<br>Lifetime:",
                                                   round(mean, 2)),group = ss,
                       data_id = ss)) +
  geom_line_interactive(size = 1.2, hover_nearest = TRUE) +
  geom_point_interactive(size=0.8, alpha = 0.7) +
  #facet_zoom(ylim=c(1,300), xlim=c(58,64)) +
  labs(x = "N", y = expression(tau), title = main_title) +
  theme_classic() +
  theme(legend.position =  "none")

interactive_plot <- girafe(ggobj = plot)
interactive_plot <- girafe_options(
  interactive_plot,
  opts_hover(css = "stroke:black;cursor:pointer;"),
  opts_selection(type = "single", css = "fill:red;stroke:black;"),
  opts_toolbar(saveaspng = TRUE)
)
htmltools::save_html(interactive_plot,
                     "Plots/geom-line-interactive_avg _life_ss.html",
                     libdir = "deps")

###################################
#Saturation Plotting              #
###################################

df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(!is.na(mean)) %>%
  mutate(ratio = (2 * ss) / (chain_size - 2)) %>%
  filter(between(ratio, 0, 0.25))
plot <- ggplot(df, aes(x = ratio, y = log10(mean), color = chain_size)) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = expression("Bond lifetimes as a function of Saturation 0 <"*frac(2*s,N-2) <= "0.25"),
       x = expression(frac(2*s,N-2)), y = expression("log("*tau*")")) +
  theme_minimal()

ggsave("Plots/saturation_0_25.jpg", plot=plot)

df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(!is.na(mean)) %>%
  mutate(ratio = (2 * ss) / (chain_size - 2)) %>%
  filter(between(ratio, 0.25, 0.50))
plot <- ggplot(df, aes(x = ratio, y = log10(mean), color = chain_size)) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = expression("Bond lifetimes as a function of Saturation 0.25 " <= frac(2*s,N-2) ~"" <= "0.50"),
       x = expression(frac(2*s,N-2)), y = expression("log("*tau*")")) +
  theme_minimal()
ggsave("Plots/saturation_25_50.jpg", plot=plot)


df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(!is.na(mean)) %>%
  mutate(ratio = (2 * ss) / (chain_size - 2)) %>%
  filter(between(ratio, 0.50, 0.75))
plot <- ggplot(df, aes(x = ratio, y = log10(mean), color = chain_size)) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = expression("Bond lifetimes as a function of Saturation 0.50 " <= frac(2*s,N-2) ~"" <= "0.75"),
       x = expression(frac(2*s,N-2)), y = expression("log("*tau*")")) +
  theme_minimal()
ggsave("Plots/saturation_50_75.jpg", plot=plot)

df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(!is.na(mean)) %>%
  mutate(ratio = (2 * ss) / (chain_size - 2)) %>%
  filter(between(ratio, 0.75, 1))
plot<- ggplot(df, aes(x = ratio, y = log10(mean), color = chain_size)) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = expression("Bond lifetimes as a function of Saturation 0.50 " <= frac(2*s,N-2) ~"" <= "0.75"),
       x = expression(frac(2*s,N-2)), y = expression("log("*tau*")")) +
  theme_minimal()
ggsave("Plots/saturation_75_100.jpg", plot=plot)

df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(!is.na(mean)) %>%
  mutate(ratio = (2 * ss) / (chain_size - 2)) %>%
  filter(between(ratio, 0, 1))
plot<- ggplot(df, aes(x = ratio, y = log10(mean), color = chain_size)) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = expression("Bond lifetimes as a function of Saturation 0.50 " <= frac(2*s,N-2) ~"" <= "0.75"),
       x = expression(frac(2*s,N-2)), y = expression("log("*tau*")")) +
  theme_minimal()
ggsave("Plots/saturation_0_100.jpg", plot=plot)

df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(!is.na(mean)) %>%
  mutate(ratio = (2 * ss) / (chain_size - 2)) %>%
  filter(between(ratio, 0, 1))
plot<- ggplot(df, aes(x = ratio, y = mean/chain_size, color = chain_size)) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = expression("Bond lifetimes as a function of Saturation 0.50 " <= frac(2*s,N-2) ~"" <= "0.75"),
       x = expression(frac(2*s,N-2)), y = expression(frac(tau,"N"))) +
  theme_minimal()
ggsave("Plots/saturation_0_100_linear_linear_scaled.jpg", plot=plot)

df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(!is.na(mean)) %>%
  mutate(ratio = (2 * ss) / (chain_size - 2)) %>%
  filter(between(ratio, 0, 1))
plot<- ggplot(df, aes(x = ratio, y = log10(mean/chain_size), color = chain_size)) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = expression("Bond lifetimes as a function of Saturation 0 " < frac(2*s,N-2) ~"" <= "1.0"),
       x = expression(frac(2*s,N-2)), y = expression("log("*frac(tau, "N")*")")) +
  theme_minimal()
ggsave("Plots/saturation_0_100_log_linear_Nscaled.jpg", plot=plot)

df <- df %>%
  filter(!is.na(mean)) %>%
  mutate(ratio = (2 * ss) / (chain_size - 2)) %>%
  filter(between(ratio, 0, 1))
plot<- ggplot(df, aes(x = ratio, y = log10(chain_size/mean), color = chain_size)) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = expression("Bond lifetimes as a function of Saturation 0 " < frac(2*s,N-2) ~"" <= "1.0"),
       x = expression(frac(2*s,N-2)), y = expression("log("*frac(tau, "N")*")")) +
  theme_minimal()
ggsave("Plots/saturation_0_100_log_linear_Tauscaled.jpg", plot=plot)

########################################################################################################

df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(chain_size == 300)
plot <- ggplot(df, aes(x = ss, y = log10(mean))) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = "Lifetime of Excited Bonds as a fuction of spin sector for N = 300",
       x = "s", y = expression(log(tau))) +
  theme_minimal()
ggsave("Plots/all_sectors_300.jpg", plot)

df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(ss == 1)
plot <- ggplot(df, aes(x = log10(chain_size), y = log10(mean))) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = "Lifetime of Excited Bonds in s = 1 with Best Fit Line",
       x = expression(log(N)), y = expression(log(tau))) +
  theme_minimal() + geom_smooth(method = "lm", se = TRUE) +
  stat_poly_eq(formula = y ~ x,
                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)

ggsave("Plots/lifetime_s1_bestfit.jpg", plot=plot)
df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(ss == 2)
plot <- ggplot(df, aes(x = log10(chain_size), y = log10(mean))) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = "Lifetime of Excited Bonds in s = 1 with Best Fit Line",
       x = expression(log(N)), y = expression(log(tau))) +
  theme_minimal() + geom_smooth(method = "lm", se = TRUE) +
  stat_poly_eq(formula = y ~ x,
                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)

ggsave("Plots/lifetime_s2_bestfit.jpg", plot=plot)

df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(ss == 3)
plot <- ggplot(df, aes(x = log10(chain_size), y = log10(mean))) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = "Lifetime of Excited Bonds in s = 1 with Best Fit Line",
       x = expression(log(N)), y = expression(log(tau))) +
  theme_minimal() + geom_smooth(method = "lm", se = TRUE) +
  stat_poly_eq(formula = y ~ x,
                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)

ggsave("Plots/lifetime_s3_bestfit.jpg", plot=plot)

df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(ss == 10)
plot<-ggplot(df, aes(x = log10(chain_size), y = log10(mean))) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = "Lifetime of Excited Bonds in s = 10 with Best Fit Line",
       x = expression(log(N)), y = expression(log(tau))) +
  theme_minimal() + geom_smooth(method = "lm", se = TRUE) +
  stat_poly_eq(formula = y ~ x,
                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)

ggsave("Plots/lifetime_s10_bestfit.jpg", plot=plot)

df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(ss == 20)
plot <- ggplot(df, aes(x = log10(chain_size), y = log10(mean))) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = "Lifetime of Excited Bonds in s = 20 with Best Fit Line",
       x = expression(log(N)), y = expression(log(tau))) +
  theme_minimal() + geom_smooth(method = "lm", se = TRUE) +
  stat_poly_eq(formula = y ~ x,
                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
ggsave("Plots/lifetime_s20_bestfit.jpg", plot=plot)

df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(ss == 100)
plot <- ggplot(df, aes(x = log10(chain_size), y = log10(mean))) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = "Lifetime of Excited Bonds in s = 100 with Best Fit Line",
       x = expression(log(N)), y = expression(log(tau))) +
  theme_minimal() + geom_smooth(method = "lm", se = TRUE) +
  stat_poly_eq(formula = y ~ x,
                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
plot
ggsave("Plots/lifetime_s100_bestfit.jpg")

df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(ss == 1)
y <- log(df$mean)
x <- log(df$chain_size)
lmodel <- lm(y ~ x)
summary(lmodel)
plot(x,y)
abline(a = coef(lmodel)[1], b = coef(lmodel)[2])


##############################################

df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(!is.na(mean)) %>%
  mutate(ratio = (2 * ss) / (chain_size - 2)) %>%
  filter(chain_size == 300)
plot<- ggplot(df, aes(x = ratio, y = log10(mean), color = chain_size)) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = expression("Bond lifetimes as a function of Saturation 0.50 " <= frac(2*s,N-2) ~"" <= "0.75"),
       x = expression(frac(2*s,N-2)), y = expression("log("*tau*")")) +
  theme_minimal()
ggsave("Plots/saturation_0_100.jpg", plot=plot)
###############################################
# Linear regression
###############################################
default_filename <- "Plots/lifetime_s+_bestfit.jpg"
default_title <- "Lifetime of Excited Bonds in s = + with Best Fit Line"
for (s in 1:149) {
  file_str <- gsub("+", toString(s), default_filename, fixed = TRUE)
  plot_title <- gsub("+", toString(s), default_title, fixed = TRUE)
  df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(ss == s)
plot <- ggplot(df, aes(x = log10(chain_size), y = log10(mean))) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = plot_title,
       x = expression(log(N)), y = expression(log(tau))) +
  theme_minimal() + geom_smooth(method = "lm", se = TRUE) +
  stat_poly_eq(formula = y ~ x,
                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
ggsave(file_str, plot=plot)
}
dev.off()
z_values <- c()
spin_sector <- seq(from=1, to=80, by=1)
# df_z <- data.frame(x_axis )
for (s in spin_sector) {
  df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
  df <- df %>% filter(ss == s)

  y <- log(df$mean)
  x <- log(df$chain_size)
  lmodel <- lm(y ~ x)
  coefs <- coef(lmodel)
  z <- coefs[2] - 1
  z_values <- c(z_values, z)
}

df_z <- data.frame(spin_sector, z_values)
plot <- ggplot(df_z, aes(x = spin_sector, y = z_values)) +
  geom_point(size = 2) +
  scale_color_viridis_c() +
  labs(title = "How z changes as a function of Spin Sector, Linear fits",
       x = "ss", y = "z") +
  theme_minimal()
plot
ggsave("Plots/behavior_of_z.jpg", plot=plot)
dev.off()


default_filename <- "Plots/lifetime_s+_bestfit_quad.jpg"
default_title <- "Lifetime of Excited Bonds in s = + with Quadratic Best Fit Line"
for (s in 1:149) {
  file_str <- gsub("+", toString(s), default_filename, fixed = TRUE)
  plot_title <- gsub("+", toString(s), default_title, fixed = TRUE)
  df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(ss == s)
plot <- ggplot(df, aes(x = log10(chain_size), y = log10(mean))) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = plot_title,
       x = expression(log(N)), y = expression(log(tau))) +
  theme_minimal() + geom_smooth(method = "lm", se = TRUE) +
  stat_poly_eq(formula = y ~ x + I(x^2),
                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
ggsave(file_str, plot=plot)
}

df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(ss == 9)
y <- log(df$mean)
x <- log(df$chain_size)
lmodel <- lm(y ~ x + I(x^2))
summary(lmodel)
predicted_fit <- predict(lmodel, list(x = x, x2 = I(x^2)))
plot(x, y, main = "Quadratic Regression", pch = 16)
lines(x, predicted_fit, col = "red", lwd = 2)

ggplot(df, aes(x = x, y = y)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = TRUE, color = "blue", fill = "lightblue") +
  labs(title = "Quadratic Regression", x = "log(chain_size)", y = "log(mean)") +
  theme_minimal()

default_filename <- "Plots/lifetime_s+_bestfit_quad.jpg"
default_title <- "Lifetime of Excited Bonds in s = + with Quadratic Best Fit Line"

for (s in 1:149) {
df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
file_str <- gsub("+", toString(s), default_filename, fixed = TRUE)
plot_title <- gsub("+", toString(s), default_title, fixed = TRUE)
df <- df %>%
  filter(ss == s)

plot <- ggplot(df, aes(x = log10(chain_size), y = log10(mean))) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = plot_title,
       x = expression(log(N)), y = expression(log(tau))) +
  theme_minimal() + 
  geom_smooth(method = "lm", se = TRUE, formula = y ~x + I(x^2)) +
  stat_poly_eq(formula = y ~ x + I(x^2),
                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
  theme_minimal()
  ggsave(file_str, plot=plot)
}

default_filename <- "Plots/lifetime_s+_bestfit_combo.jpg"
default_title <- "Lifetime of Excited Bonds in s = + with Best Fit Lines"
for (s in 1:149) {
df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
file_str <- gsub("+", toString(s), default_filename, fixed = TRUE)
plot_title <- gsub("+", toString(s), default_title, fixed = TRUE)
df <- df %>%
  filter(ss == s)

plot <- ggplot(df, aes(x = log10(chain_size), y = log10(mean))) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = plot_title,
       x = expression(log(N)), y = expression(log(tau))) +
  theme_minimal() + 
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x + I(x^2), col = "red") +
  geom_smooth(method = "lm", se = TRUE) +
  stat_poly_eq(formula = y ~ x + I(x^2), col = "red",
                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), label.x.npc = "left", label.y.npc = "top", parse = TRUE) +
  stat_poly_eq(formula = y ~ x,  aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),  label.x.npc = "left", label.y.npc = "top", vjust = 3.5, parse = TRUE)+
  theme_minimal()
  ggsave(file_str, plot=plot)
}

spin_sectors <- seq(from = 1, to = 80, by = 1)
z_collection <- c()
for (s in spin_sectors) {
df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(ss == s)
y <- log10(df$mean)
x <- log10(df$chain_size)
lmodel <- lm(y ~ x + I(x^2))
# coefs <- coef(lmodel)
### take derivative and find slope at x = 2.477 ~ log_10(300) (deeper into asymptotic limit)
z <- (2 * coef(lmodel)[3] * 2.477) + coef(lmodel)[2] - 1
z_collection <- c(z_collection, z)
}

df <- data.frame(spin_sectors, z_collection)

plot<- ggplot(df, aes(x = spin_sectors, y = z_collection)) +
  geom_point(size = 2) +
  scale_color_viridis_c() +
  labs(title = "How z changes as a function of Spin Sector, Quadratic fits",
       x = "s", y = "z") +
  theme_minimal()
ggsave("Plots/z_quadratic_derivative.jpg", plot=plot)

df <- read.table("per_N_pivot.txt", header = TRUE)
# Define parameters
Nmax <- 300
z <- 3.16
a <- 0.5
b <- 1.6
c <- 0.4
d <- 5.2
Nmin <- 50

# Filter and transform data
df_filtered <- df %>%
  filter(!is.na(mean)) %>%
  filter(chain_size > Nmin) %>%
  mutate(
    x = ((2 * ss / (chain_size - 2)) * chain_size^(1.0 / z) + chain_size^(-a)),
    y = (chain_size - 2) / mean
  )

# Define x-range
x_min <- 0.08
x_max <- 1 + Nmax^(1.0 / z)
x_vals <- seq(x_min, x_max, length.out = 500)

# Define functions
f1 <- function(x) c * x^d
f2 <- function(x) 1
f3 <- function(x) 1 / (1 + (1 / c)^(1 / b) * x^(-d / b))^b

# Evaluate functions
func_df <- data.frame(
  x = x_vals,
  f1 = f1(x_vals),
  f2 = f2(x_vals),
  f3 = f3(x_vals)
)

# Plot
plot <- ggplot() +
  geom_line(data = func_df, aes(x = x, y = f1), color = "red", linetype = "dashed") +
  geom_line(data = func_df, aes(x = x, y = f2), color = "green", linetype = "dotted") +
  geom_line(data = func_df, aes(x = x, y = f3), color = "purple") +
  geom_point(data = df_filtered, aes(x = x, y = y), color = "grey", shape = ".") +
  coord_cartesian(xlim = c(x_min, x_max), ylim = c(0, 2)) +
  labs(x = expression(frac(2*s, N-2) * N^z^-1 + frac(1, sqrt(N))), y = expression(frac(N-2, s)), title = "Data collapse") +
  theme_minimal()

df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)

z <- 3.19

df <- df %>% mutate(energy = ((chain_size/2)-1)/(mean + 1))  %>% mutate(N = chain_size)
# df <- df %>% filter(ss <= 10)
df <- df %>% arrange(N, ss)
plot <- ggplot() + 
  geom_point(data = df, aes(x = ss, y = energy, color = N)) +
  scale_color_gradient(low = "purple", high = "orange") +
  geom_line(data = df, aes(x = ss, y = energy, group = N),
            color = "black", alpha = 0.3, size = 0.5)
ggsave("Plots/energy_levels_linear_linear.jpg", plot=plot)

df <- df %>% arrange(N, ss)
plot <- ggplot() + 
  geom_point(data = df, aes(x = ss, y = log10((N^z)*energy), color = N)) +
  scale_color_gradient(low = "purple", high = "orange") +
  geom_line(data = df, aes(x = ss, y = log10((N^z)*energy), group = N),
            color = "black", alpha = 0.3, size = 0.5)
ggsave("Plots/energy_levels_log_linear.jpg", plot=plot)

plot <- ggplot() + 
  geom_point(data = df, aes(x = log10(ss), y = log10((N^z)*energy), color = N)) +
  geom_line(data = df, aes(x = log10(ss), y = log10((N^z)*energy), group = N),
            color = "black", alpha = 0.3, size = 0.5) + 
  scale_color_gradient(low = "purple", high = "orange")
ggsave("Plots/energy_levels_log_log.jpg", plot=plot)

plot <- ggplot() + 
  geom_line(data = df, aes(x = chain_size, y = energy, color = chain_size)) +
  scale_color_gradient(low = "purple", high = "orange")

plot <- ggplot() + 
  geom_line(data = df, aes(x = chain_size, y = log10(energy), color = chain_size)) +
  scale_color_gradient(low = "purple", high = "orange")

plot <- ggplot() + 
  geom_line(data = df, aes(x = log10(chain_size), y = log10(energy), color = chain_size)) +
  scale_color_gradient(low = "purple", high = "orange")


x_values <- seq(from = 1, to = 149, by = 1)
N <- 300
L <- 0.5
k <- 0.2
x_0 <- 80
f <- function(x) L/(1 + exp(-k*(x-x_0)))
ggplot() + 
geom_line(aes(x = x_values, y = f(x_values)))

# More nuanced z dependence collection

spin_sectors <- seq(from = 1, to = 80, by = 1)
z_collection <- c()
for (s in spin_sectors) {
df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(ss == s)
y <- log10(df$mean)
x <- log10(df$chain_size)
lmodel <- lm(y ~ x + I(x^2))
# coefs <- coef(lmodel)
### take derivative and find slope at x = 2.477 ~ log_10(300) (deeper into asymptotic limit)
z <- (2 * coef(lmodel)[3] * 2.477) + coef(lmodel)[2] - 1
z_collection <- c(z_collection, z)
}
df <- data.frame(spin_sectors, z_collection)

plot <- ggplot(df, aes(x = spin_sectors, y = z_collection)) +
  geom_point(size = 1) +
  scale_color_viridis_c() +
  labs(title = "How z changes as a function of Spin Sector, Quadratic fits",
       x = "s", y = "z") +
  geom_line(aes(y = 3.533757), linetype = "dashed") +
  annotate("label", x = 20, y = 3.45, label = "z ~ 3.534") +
  geom_line(aes(y = 0.1369335), linetype = "dashed") +
  annotate("label", x = 20, y = 0.2, label = "z ~ 0.137") +
  geom_line(aes(y = 3.221026), linetype = "dashed") +
  annotate("label", x = 20, y = 3.1, label = "z ~ 3.221") +
  theme_minimal() + 
  geom_line(aes(x = 6)) +
  annotate("segment", x=6, xend=6, y=0, yend = 3.533757) +
  annotate("label", x = 2, y = 1, label = "s = 6") + 
  annotate("segment", x = 9, xend=9, y = 0, yend = 3.221) +
  annotate("label", x = 13, y = 1, label = "s = 9")
ggsave("Plots/z_derivative_deeper.jpg", plot=plot)

z_values <- c()
spin_sector <- seq(from=1, to=80, by=1)
# df_z <- data.frame(x_axis )
for (s in spin_sector) {
  df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
  df <- df %>% filter(ss == s)

  y <- log10(df$mean)
  x <- log10(df$chain_size)
  lmodel <- lm(y ~ x)
  coefs <- coef(lmodel)
  z <- coefs[2] - 1
  z_values <- c(z_values, z)
}

df_z <- data.frame(spin_sector, z_values)
y <- z_values
x <- spin_sector
lmodel <- lm( y ~ log(x))

model <- nls (y ~ d + (a - d) / (1 + g *(x^b - 1)), start = list(a = 3.150103, b = 2.011552, g = 0.005315, d = 0.005317))
alpha <- 3.150103
beta <- 2.011552
gamma <- 0.005315
delta <- 0.034001
f <- function(x) delta + ((alpha - delta) / (1 + gamma*(x^beta - 1)))
x_range <- seq(from = 1, to = 80, by = 1)
plot <- ggplot(df_z, aes(x = spin_sector, y = z_values)) +
  geom_point(size = 2) +
  scale_color_viridis_c() +
  labs(title = "How z changes as a function of Spin Sector, Linear fits",
       x = "ss", y = "z") +
  theme_minimal() +
  geom_line(aes(y = 3.188141), linetype = "dashed") +
  annotate("label", x = 10, y = 3.148, label = "z ~ 3.188")+
  geom_line(aes(y = 0.1216337), linetype = "dashed") +
  annotate("label", x = 10, y = 0.2, label = "z ~ 0.122")+
  geom_line(aes(y = f(x_range), x = x_range))+
  geom_text(x = 60, y =2, label = expression(z(s) == delta + ((alpha - delta)/(1 + gamma*(x^beta -1)))), size = 7) +
  geom_text(x = 60, y = 1.8, label = expression( alpha == 3.150103), size = 7) +
  geom_text(x = 60, y = 1.6, label = expression( beta == 2.011552), size = 7) +
  geom_text(x = 60, y = 1.4, label = expression( gamma == 0.005315), size = 7) +
  geom_text(x = 60, y = 1.2, label = expression( delta == 0.034001), size = 7) 
  plot
ggsave("Plots/behavior_of_z_annotated.jpg", plot=plot)

##Numerology plot
f <- function(x) (pi/100) + (99*pi) / (100 + (x^2 - 1)/1.8)
x_range <- seq(from = 1, to = 80, by = 1)
plot <- ggplot(df_z, aes(x = spin_sector, y = z_values)) +
  geom_point(size = 2) +
  scale_color_viridis_c() +
  labs(title = "How z changes as a function of Spin Sector, Linear fits",
       x = "ss", y = "z") +
  theme_minimal() +
  geom_line(aes(y = 3.188141), linetype = "dashed") +
  annotate("label", x = 10, y = 3.148, label = "z ~ 3.188")+
  geom_line(aes(y = 0.1216337), linetype = "dashed") +
  annotate("label", x = 10, y = 0.2, label = "z ~ 0.122")+
  geom_text(x = 60, y = 2, label = expression(z(s) == pi/100 + (99*pi) / (100 + (s^2 - 1)/1.8)), size = 6)+
  geom_line(aes(y = f(x_range), x = x_range))
  plot
  ggsave("Plots/behavior_of_z_numerology_2.jpg", plot=plot)

z_0 = 3.19
##Slightly more sceintific plot
f <- function(x) (z_0/100) + (99*z_0) / (100 + (x^2)/1.8)
x_range <- seq(from = 1, to = 80, by = 1)
plot <- ggplot(df_z, aes(x = spin_sector, y = z_values)) +
  geom_point(size = 2) +
  scale_color_viridis_c() +
  labs(title = "How z changes as a function of Spin Sector, Linear fits",
       x = "ss", y = "z") +
  theme_minimal() +
  geom_line(aes(y = 3.188141), linetype = "dashed") +
  annotate("label", x = 10, y = 3.148, label = "z ~ 3.188")+
  geom_line(aes(y = 0.1216337), linetype = "dashed") +
  annotate("label", x = 10, y = 0.2, label = "z ~ 0.122")+
  geom_text(x = 60, y = 2, label = expression(z(s) == z_0/100 + (99*z_0) / (100 + (s^2)/1.8)), size = 6)+
  geom_line(aes(y = f(x_range), x = x_range))
  plot
  ggsave("Plots/behavior_of_z_scientific.jpg", plot=plot)


x_range <- seq(from = -80, to = 80, by = 1)

plot <- ggplot(data.frame( x = x_range), aes(x = x)) +
  stat_function(fun = f) +
   geom_text(x = 60, y = 2, label = expression(z(s) == z_0/100 + (99*z_0) / (100 + (s^2)/1.8)), size = 6) +
   annotate("label", x = 0, y = 0.5, label = "z_0 = 3.19")

plot

ggsave("Plots/mirrored_fit_function.jpg", plot=plot)

################# Retry s fits using log(tau/N) = (z_0 + z_1/N)log(N) + log(c_0 + c_1/N)######################

default_filename <- "Plots/Analysis/lifetime_s+_bestfit.jpg"
default_title <- "Lifetime of Excited Bonds in s = + with Best Fit Line"
spin_sector <- seq(from = 1, to = 149, by = 1)
for (s in spin_sector) {
  file_str <- gsub("+", toString(s), default_filename, fixed = TRUE)
  plot_title <- gsub("+", toString(s), default_title, fixed = TRUE)
  df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(ss == s) %>%
  filter(!is.na(mean))

ratio <- df$mean / df$chain_size
se_ratio <- df$err/df$chain_size
se_log <- se_ratio/(log(10) * ratio)
df$se_log <- se_log
df$x <- log10(df$chain_size)
df$y <- log10(ratio)
# df$y_p <- log10((df$mean + df$err)/df$chain_size)
# df$y_m <- log10((df$mean - df$err)/df$chain_size)
bar_width <- 0.02 * diff(range(df$x))
plot <- ggplot(df, aes(x = x, y = y)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = y - se_log, ymax = y + se_log), width = bar_width) +
  labs(title = plot_title,
       x = expression(log(N)), y = expression(log(tau/N))) +
  theme_minimal()
ggsave(file_str, plot=plot)
}

df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df_1 <- df %>%
  filter(ss == 1)

y <- log10(df_1$mean/df_1$chain_size)
x <- df_1$chain_size
lin_model <- lm(y~log10(x))

z_0 <- 3.188
c_0 <- 0.0052
x_range <- seq(from = 4, to = 300, by = 2)
f <- function (x) log10(c_0) + z_0*log10(x) 
plot <- ggplot(df_1, aes(x = log10(chain_size), y = log10(mean/chain_size))) +
geom_point(size = 2) +
geom_line(aes(y = f(x_range), x = log10(x_range)))
plot



df_2 <- df %>%
  filter(ss == 3) %>%
  filter(!is.na(mean))
y <- log10(df_2$mean/df_2$chain_size)
x <- df_2$chain_size

# z_0 <- 3.188
# c_0 <- 0.0015
kick_off_model <- lm(y~log10(x))
ratio <- df_2$mean / df_2$chain_size
se_ratio <- df_2$err/df_2$chain_size
se_log <- se_ratio/(log(10) * ratio)
non_lin_model <- nls(y ~ (z_0 + z_1/x)*log10(x) + log10(c_0 + c_1/x), algorithm = "port", lower = c(c_1 = 0, z_1 = -100, z_0 = -100, c_0 = 0),start = list(c_1 = 2.793e-3, z_1 = 1.828, z_0 = 3.304, c_0 = 4.386e-4))



#### An Introduction to Error Analysis: The Study of Uncertainties in Physical Measurements John R. Taylor
#### https://www.nist.gov/pml/nist-technical-note-1297
#### https://physics.stackexchange.com/questions/619143/error-propagation-with-log-base-10
se_ratio <- df_2$err/df_2$chain_size
ratio <- df_2$mean/df_2$chain_size

se_log <- se_ratio/(log(10)*ratio)
df_2$se_log <- se_log
x_range <- seq(from = 8, to = 300, by = 2)
df_2$x_range <- x_range
f <- function(x) (z_0 + z_1/x)*log10(x) + log10(c_0 + c_1/x)
z_0 <- 3.428
c_0 <- 5.407e-5
z_1 <- 6.463
c_1 <- 3.538e-4
plot<- ggplot(df_2, aes(x = log10(chain_size), y = log10(mean/chain_size))) +
geom_point(size = 2) +
geom_errorbar(aes(ymin = y - se_log, ymax = y + se_log), width = 0.01)+
geom_line(aes(y = f(x_range), x = log10(x_range)))
plot

z_0 <- 3.099
c_0 <- 0.0014
c_1 <- 0.01
z_1 <- -1
default_filename <- "Plots/Analysis/lifetime_s+_non_linear.jpg"
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
  filter(!is.na(mean))



ratio <- df$mean / df$chain_size
se_ratio <- df$err/df$chain_size
se_log <- se_ratio/(log(10) * ratio)
df$se_log <- se_log
df$x <- df$chain_size
df$y <- log10(ratio)
# y <- df$y
# x <- df$x
# df$y_p <- log10((df$mean + df$err)/df$chain_size)
# df$y_m <- log10((df$mean - df$err)/df$chain_size)

x_range <- seq(from = 2*(s+1), to = 300, by = 2)
non_lin_model <- nls(data = df, y ~ (z_0 + z_1/chain_size)*log10(chain_size) + log10(c_0 + c_1/chain_size),  algorithm = "port", lower = c(c_1 = 0, z_1 = -100, c_0 = 0, z_0 = -100), start = list(c_1 = c_1, z_1 = z_1, c_0 = c_0, z_0 = z_0), control = nls.control(maxiter = 500, warnOnly = TRUE))
c_1 <- coef(non_lin_model)["c_1"]
z_1 <- coef(non_lin_model)["z_1"]
c_0 <- coef(non_lin_model)["c_0"]
z_0 <- coef(non_lin_model)["z_0"]

c_1_coll <- c(c_1_coll, c_1)
c_0_coll <- c(c_0_coll , c_0)
z_1_coll <- c(z_1_coll, z_1)
z_0_coll <- c(z_0_coll, z_0)


  print("#############")
  print(s)
  print(c_1)
  print(z_1)
  print(z_0)
  print(c_0)
  print("#############")
f <- function(x) (z_0 + z_1/x)*log10(x) + log10(c_0 + c_1/x)
bar_width <- 0.02 * diff(range(log10(df$x)))
plot <- ggplot(df, aes(x = log10(x), y = y)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = y - se_log, ymax = y + se_log), width = bar_width) +
  labs(title = plot_title,
       x = expression(log(N)), y = expression(log(tau/N))) +
  theme_minimal() +
  geom_line(aes(y = f(x_range), x = log10(x_range)))

ggsave(file_str, plot=plot)
}
df_coeffs <- data.frame(c_1 = c_1_coll, c_0 = c_0_coll, z_1 = z_1_coll, z_0 = z_0_coll)
df_coeffs$x <- seq(from = 1, to = 50, by = 1)
plot <- ggplot(df_coeffs, aes(x = x, y = z_0)) +
      geom_point(size = 2)
ggsave(plot = plot, "Plots/Analysis/CoeffsPlots/z_0.jpg")
plot <- ggplot(df_coeffs, aes(x = x, y = z_1)) +
      geom_point(size = 2)
ggsave(plot = plot, "Plots/Analysis/CoeffsPlots/z_1.jpg")
plot <- ggplot(df_coeffs, aes(x = x, y = c_0)) +
      geom_point(size = 2)
ggsave(plot = plot, "Plots/Analysis/CoeffsPlots/c_0.jpg")
plot <- ggplot(df_coeffs, aes(x = x, y = c_1)) +
      geom_point(size = 2)
ggsave(plot = plot, "Plots/Analysis/CoeffsPlots/c_1.jpg")

plot <- ggplot(df_coeffs, aes(x = x)) +
  geom_point(aes(y = z_1), color = "red") +
  geom_point(aes(y = z_0), color = "blue") +
  labs(y = "z_1(red) and z_0(blue)", x = "spin sector")
ggsave(plot = plot, "Plots/Analysis/CoeffsPlots/z_1_z_0.jpg")
plot

plot <- ggplot(df_coeffs, aes(x = x)) +
  geom_point(aes(y = c_1), color = "red") +
  geom_point(aes(y = c_0), color = "blue") +
  labs(y = "c_1(red) and c_0(blue)", x = "spin sector")
plot
ggsave(plot = plot, "Plots/Analysis/CoeffsPlots/c_1_c_0.jpg")

plot <- ggplot(df_coeffs, aes(x = x, y = z_0)) +
      geom_point(size = 2) +
      annotate("segment", x=10, xend=10, y=0, yend = 3.8735261) +
      annotate("segment", x=0, xend=10, y=3.8735261, yend = 3.8735261) +
      annotate("label", x = 15, y = 1, label = "z_0_max = 3.8735261") +
      annotate("segment", x = 1, xend = 1, y = 0, yend = 3.1688150) +
      annotate("segment", x = 0, xend = 1, y=3.1688150, yend = 3.1688150) +
      annotate("label", x = 5, y = 2, label = "z_0 = 3.168(8)")
plot

ggsave(plot = plot, "Plots/Analysis/CoeffsPlots/z_0_labeled.jpg")


##########################################################################

# interval <- as_tibble(predFit(non_lin_model, newdata = x_range, interval = "confidence", level = 0.9)) %>%
#   mutate(x = x_range)

interval <- confint(non_lin_model , level = 0.9)

# default_filename <- "Plots/Analysis/DeviationsAndErrors/error_percentages_per_N_ss_+.jpg"
# default_title <- "errors as % of mean in s = +"
# spin_sector <- seq(from = 1, to = 149, by = 1)
# for (s in spin_sector) {
#   file_str <- gsub("+", toString(s), default_filename, fixed = TRUE)
#   plot_title <- gsub("+", toString(s), default_title, fixed = TRUE)
#   df <- read.table(file = "per_N_pivot.txt",
#                  sep = "\t", header = TRUE)
# df <- df %>%
#   filter(ss == s) %>%
#   filter(!is.na(mean))

# df$mean_scaled <- df$mean/df$chain_size
# df$err_scaled <- df$err/df$chain_size

# plot <- ggplot(df, aes(x = chain_size, y = 100*err_scaled/mean_scaled)) +
#   geom_point(size = 3) +
#   scale_color_viridis_c() +
#   labs(title = plot_title,
#        x = "chain_size", y = expression(error/(chain_size * mean))) +
#   theme_minimal()
# ggsave(file_str, plot=plot)
# }

default_filename <- "Plots/Analysis/Errors/standard_dev_ss_+.jpg"
default_title <- "standard deviations in s = +"
for (s in spin_sector) {
  file_str <- gsub("+", toString(s), default_filename, fixed = TRUE)
  plot_title <- gsub("+", toString(s), default_title, fixed = TRUE)
  df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(ss == s) %>%
  filter(!is.na(mean))
plot <- ggplot(df, aes(x = chain_size, y = 100*(dev/mean))) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  labs(title = plot_title,
       x = "chain_size", y = expression(sigma/mean)) +
  theme_minimal()
ggsave(file_str, plot=plot)
}

df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)

df <- df %>%
  filter(ss == 13) %>%
  filter(!is.na(mean))
plot <- ggplot(df, aes(x = chain_size, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - err, ymax = mean + err))

plot

ggsave(filename = "mean_vs_N_ss13.jpg", plot = plot)

df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)

df <- df %>%
  filter(ss == 13) %>%
  filter(!is.na(mean))
df$err_scaled <- df$err/df$chain_size
df$mean_scaled <- df$mean/df$chain_size
plot <- ggplot(df, aes(x = chain_size, y = mean_scaled)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_scaled - err_scaled, ymax = mean_scaled + err_scaled))

plot

ggsave(filename = "mean_vs_N_ss13_scaled.jpg", plot = plot)

df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(ss == 28) %>%
  filter(!is.na(mean))
plot <- ggplot(df, aes(x = chain_size, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - err, ymax = mean + err)) 

plot

ggsave(filename = "mean_vs_N_ss28.jpg", plot = plot)

df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(ss == 33) %>%
  filter(!is.na(mean))
plot <- ggplot(df, aes(x = chain_size, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - err, ymax = mean + err))

plot

ggsave(filename = "mean_vs_N_ss33.jpg", plot = plot)

df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(ss == 1) %>%
  filter(!is.na(mean))
plot <- ggplot(df, aes(x = chain_size, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - err, ymax = mean + err))

plot

ggsave(filename = "mean_vs_N_ss1.jpg", plot = plot)

df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(ss == 2) %>%
  filter(!is.na(mean))
plot <- ggplot(df, aes(x = chain_size, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - err, ymax = mean + err))

plot

ggsave(filename = "mean_vs_N_ss2.jpg", plot = plot)

df <- read.table(file = "per_N_pivot.txt",
                 sep = "\t", header = TRUE)
df <- df %>%
  filter(ss == 48) %>%
  filter(!is.na(mean))
plot <- ggplot(df, aes(x = chain_size, y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - err, ymax = mean + err))

plot

ggsave(filename = "mean_vs_N_ss48.jpg", plot = plot)