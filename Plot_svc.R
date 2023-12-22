# Plot scale variance componets for all analysis runs

library(tidyverse)
library(purrr)
library(ggpubr)

# load data
files <- list.files(path = "../results", pattern = "svc.*csv", ignore.case = TRUE, recursive = TRUE)

# define function to append all results to a single data frame
append_run <- function(run) {
  read_csv(file.path("../results", run)) %>% 
    mutate(run_id = str_extract(run, pattern = "(.*)(?= - )"), 
           type = str_extract(run, pattern = "(?<=_)(.*)(?=\\.)"))
}

# loop through result files and append to a single data frame
df <- map_df(files, append_run)

# plot all results faceted by run
# df %>% ggplot(aes(x=scale, y=sv_cumulative, color=type)) +
#   geom_line() +
#   geom_point() +
#   scale_x_log10() +
#   facet_wrap(factor(df$run_id)) +
#   xlab("cumulative variance (%)") +
#   ylab("scale (logarithmic scale")

scales <- c("500", "1000", "2000", "4000", "8000", "16000", "32000", "64000", "128000")

# plot all results on single plot colored by type
df %>% ggplot(aes(x=scale, y=sv_cumulative, color=type, group=interaction(run_id, type))) +
  geom_line() +
  geom_point() +
  geom_smooth(aes(group = type), size = 1.5, method = "loess", se = FALSE) +
  scale_x_log10(breaks=as.numeric(scales), labels=scales, minor_breaks=NULL) +
  ylab("cumulative variance (%)") +
  xlab("scale (m)")



# plot all results on single plot colored by type
p1 <- df %>% 
  filter(run_id == "R2") %>% 
  ggplot(aes(x=scale, y=sv_cumulative, color=type, group=interaction(run_id, type))) +
  geom_line() +
  geom_point() +
  #geom_smooth(aes(group = type), size = 1.5, method = "loess", se = FALSE) +
  scale_x_log10(breaks=as.numeric(scales), labels=scales, minor_breaks=NULL) +
  ylab("cumulative variance (%)") +
  xlab("scale (m)")

# plot all results on single plot colored by type
p2 <- df %>% 
  filter(run_id == "R5") %>% 
  ggplot(aes(x=scale, y=sv_cumulative, color=type, group=interaction(run_id, type))) +
  geom_line() +
  geom_point() +
  #geom_smooth(aes(group = type), size = 1.5, method = "loess", se = FALSE) +
  scale_x_log10(breaks=as.numeric(scales), labels=scales, minor_breaks=NULL) +
  ylab("cumulative variance (%)") +
  xlab("scale (m)")

# plot all results on single plot colored by type
p3 <- df %>% 
  filter(run_id == "R8") %>% 
  ggplot(aes(x=scale, y=sv_cumulative, color=type, group=interaction(run_id, type))) +
  geom_line() +
  geom_point() +
  #geom_smooth(aes(group = type), size = 1.5, method = "loess", se = FALSE) +
  scale_x_log10(breaks=as.numeric(scales), labels=scales, minor_breaks=NULL) +
  ylab("cumulative variance (%)") +
  xlab("scale (m)")

ggarrange(p1, p2, p3, nrow=1, ncol=3, labels="auto", common.legend = TRUE, legend = 'bottom')
