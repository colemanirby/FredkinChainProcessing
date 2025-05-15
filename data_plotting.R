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
### take derivative and find slope at x = 2.3 (deeper into asymptotic limit)
z <- (2 * coef(lmodel)[3] * 2.3) + coef(lmodel)[2] - 1
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
