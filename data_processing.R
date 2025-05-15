setwd("../Desktop/School/data_processing")
install.packages("httpgd")
install.packages("rjson")
install.packages("ggplot2")
install.packages("plotrix")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggforce")
install.packages("ggiraph")
library(ggiraph)
library("dplyr")
library("tidyr")
library("ggplot2")
library("rjson")
library("plotrix")
library("ggforce")
###################################
#Plot tau vs ss for N=300          #
###################################
base_file_str_200 <- "../run_data/run_ss_+_cs_#_200.json"
base_file_str_300 <- "../run_data/run_ss_+_cs_#_300.json"

# x_axis <- seq(from = 1, to = 149, by = 1)
# y_axis <- c()
# std_axis <- c()
# smallest_chain <- 202
# for (ss in x_axis) {

#   min_size <- 2 * (ss + 1)
#   if (min_size > smallest_chain) {
#     smallest_chain <- min_size
#   }
#   file_str <- gsub("+", toString(ss), base_file_str_300, fixed=TRUE)
#   file_str <- gsub("#", toString(smallest_chain), file_str, fixed=TRUE)
#   current_data <- fromJSON(file = file_str)
#   ss_data <- current_data[["runs"]][["300"]]
#   std_err <- std.error(ss_data)
#   std_axis <- c(std_axis, std_err)
#   y_avg <- mean(ss_data, trim = 0)
#   y_axis <- c(y_axis, y_avg)
# }
# df <- data.frame(ss = x_axis, tau = y_axis, sd = std_axis)
# #p <- ggplot(data=df, aes(x=ss, y=tau, group=2)) 
# #+ geom_errorbar(aes(ymin=tau-sd, ymax=tau+sd), width=.7)
# #+ geom_line() + geom_point()
# p <- ggplot(data = df, aes(x = ss, y = log(tau), group = 2)) +
#   geom_line() + geom_point()
# p + labs(x = "Spin Sector", y = expression(tau),
#          title = ("Lifetimes in spin sector for N=300"))

####################################
# Per N Pivot                       #
####################################

x_axis <- seq(from = 1L, to = 149L, by = 1L)
chain_sizes <- seq(from = 4L, to = 300L, by = 2L)
y_axis <- c()

df <- data.frame(ss = x_axis)

names <- c()
error_names <- c()
std_error_current <- c()

for (current_cs in chain_sizes) {
  y_axis_current <- c()
  std_error_current <- c()
  std_dev_current <- c()
  file_str <- c()
  smallest_chain <- 4
  for (ss in x_axis) {
    if (current_cs <= 200) {
      file_str <-  base_file_str_200
    } else {
      file_str <- base_file_str_300
      smallest_chain <- 202
    }
    min_size <- 2 * (ss + 1)
    if (min_size > smallest_chain) {
      smallest_chain <- min_size
    }
    if (smallest_chain > current_cs) {
      y_axis_current <- c(y_axis_current, NA)
      std_error_current <- c(std_error_current, NA)
      std_dev_current <- c(std_dev_current, NA)
    } else {
      file_str <- gsub("+", toString(ss), file_str, fixed = TRUE)
      file_str <- gsub("#", toString(smallest_chain), file_str, fixed = TRUE)
      current_file <- fromJSON(file = file_str)
      if (toString(current_cs) %in% names(current_file[["runs"]])) {
        chain <- current_file[["runs"]][[toString(current_cs)]]
        y_avg <- mean(chain, trim = 0)
        std_error <- std.error(chain)
        std_dev <- sd(chain)
        std_error_current <- c(std_error_current, std_error)
        std_dev_current <- c(std_dev_current, std_dev)
        y_axis_current <- c(y_axis_current, y_avg)
      } else {
        y_axis_current <- c(y_axis_current, NA)
        std_error_current <- c(std_error_current, NA)
        std_dev_current <- c(std_dev_current, NA)
      }
    }
  }

  err_name <- sprintf("err.%d", current_cs)
  dev_name <- sprintf("dev.%d", current_cs)
  mean_name <- sprintf("mean.%d", current_cs)

  df[mean_name] <- y_axis_current
  df[dev_name] <- std_dev_current
  df[err_name] <- std_error_current
}

write.table(df, file = "per_N.txt", quote = FALSE,
            sep = "\t", row.names = FALSE, col.names = TRUE)

df <- df %>% pivot_longer(cols = -ss,
                          names_to = c(".value", "chain_size"),
                          names_sep = "\\.")
write.table(df, file = "per_N_pivot.txt", quote = FALSE,
            sep = "\t", row.names = FALSE, col.names = TRUE)


###################################
# per ss pivot                    #
###################################

x_axis <- seq(from = 4L, to = 300L, by = 2L)
spin_sectors <- seq(from = 1L, to = 149L, by = 1L)
y_axis <- c()


smallest_cs_200 <- 4
smallest_cs_300 <- 202
current_cs <- 64

df <- data.frame(chainsize=x_axis)

names <- c()

for (ss in spin_sectors) {
  file_str <- c()
  y_axis_current <- c()
  std_err_current <- c()
  std_dev_current <- c()
  smallest_chain <- 4
  min_size <- 2 * (ss + 1)
  for (current_cs in x_axis) {
    if (current_cs <= 200) {
      file_str <- gsub("+", toString(ss), base_file_str_200, fixed = TRUE)
    } else {
      file_str <- gsub("+", toString(ss), base_file_str_300, fixed = TRUE)
      smallest_chain <- 202
    }
    if (min_size > smallest_chain) {
      smallest_chain <- min_size
    }
    if (smallest_chain <= current_cs) {
      file_str <- gsub("#", toString(smallest_chain), file_str, fixed = TRUE)
      current_file <- fromJSON(file = file_str)
      chain <- current_file[["runs"]][[toString(current_cs)]]
      y_avg <- mean(chain, trim = 0)
      y_axis_current <- c(y_axis_current, y_avg)
      std_err <- std.error(chain)
      std_err_current <- c(std_err_current,std_err)
      std_dev <- sd(chain)
      std_dev_current <- c(std_dev_current, std_dev)
    } else {
      y_axis_current <- c(y_axis_current, NA)
      std_err_current <- c(std_err_current, NA)
      std_dev_current <- c(std_dev_current, NA)
    }
  }

  std_dev_name <- sprintf("dev.%d",ss)
  std_err_name <- sprintf("err.%d",ss)
  mean_name <- sprintf("mean.%d",ss)
  df[mean_name] <- y_axis_current
  df[std_dev_name] <- std_dev_current
  df[std_err_name] <- std_err_current
}
write.table(df, file = "per_ss.txt", quote = FALSE, 
            sep = "\t", row.names = FALSE, col.names = TRUE)
df <- df %>% pivot_longer(cols = -chainsize,
                          names_to = c(".value", "ss"),
                          names_sep = "\\.")
write.table(df, file = "per_ss_pivot.txt", quote = FALSE,
            sep = "\t", row.names = FALSE, col.names = TRUE)