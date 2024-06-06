# Plot scale variance components for all analysis runs

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
df <- map_df(files, append_run) %>% 
  separate_wider_delim(type, del="_", names = c("type", "q"))

# define scales
scales <- c("500", "1000", "2000", "4000", "8000", "16000", "32000", "64000", "128000")
# get unique run_ids
df$run_id <- factor(df$run_id)
runs <- df$run_id %>% unique()

# summarize all runs
summary_table <- df %>%
  group_by(run_id, type, q) %>%
  filter(scale != min(scale)) %>% 
  summarize(Max_SV = max(scale_variance), 
            Scale_at_Max = scale[which.max(scale_variance)]) %>%
  ungroup()
# export summary table
summary_table %>% write_csv("../results/meta/result_summary.csv")


# summary plot
summary_table %>% 
  ggplot(aes(x = as.factor(Scale_at_Max), fill = type)) +
  geom_bar(position = 'stack', stat='count') +
  facet_wrap(vars(q))

# function to plot results for specified run on single plot (cumulative)
plot_svc_cum <- function(run) {
  df %>% filter(run_id == run) %>% 
    ggplot(aes(x=scale, y=sv_cumulative, color=type, linetype=q, shape=q, group=interaction(type, q))) +
    geom_line(linewidth=0.5) +
    geom_point(size = 2.5) +
    geom_segment(aes(x = 500, y = 1/7, xend = 64000, yend = 1), color='white', linewidth=1, linetype='dashed') +
    scale_linetype_manual(breaks=c("q0", "q1", "q2"), values=c(3, 5, 1)) +
    scale_shape_manual(breaks=c("q0", "q1", "q2"), values=c(4,1,16)) +
    scale_x_log10(breaks=as.numeric(scales), labels=scales, minor_breaks=NULL) +
    scale_y_continuous(sec.axis = sec_axis(~ ., name = "scale variance")) +
    ylab("cumulative scale variance (%)") +
    xlab("scale (m)") +
    ggtitle(run) +
    ylim(0,1)
}

# plot cumulative results for all runs
p <- map(runs, plot_svc_cum)

# plot cumulative scale variance for specified runs (faceted by run)
p1 <- df %>% filter(run_id %in% c("R1", "R3", "R5")) %>% 
  ggplot(aes(x=scale, y=sv_cumulative, color=type, linetype=q, shape=q, group=interaction(type, q))) +
  geom_line(linewidth=0.5) +
  geom_point(size = 2.5) +
  geom_segment(aes(x = 500, y = 1/7, xend = 64000, yend = 1), color='white', linewidth=1, linetype='dashed') +
  scale_linetype_manual(breaks=c("q0", "q1", "q2"), values=c(3, 5, 1)) +
  scale_shape_manual(breaks=c("q0", "q1", "q2"), values=c(4,1,16)) +
  scale_x_log10(breaks=as.numeric(scales), labels=scales, minor_breaks=NULL) +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "scale variance")) +
  ylab("cumulative scale variance (%)") +
  xlab("scale (m)") +
  ylim(0,1) +
  theme(legend.position = 'bottom') +
  facet_wrap(vars(run_id))
p1

# function to plot results for specified run on single plot
plot_svc <- function(type_name, run) {
  df %>% 
    filter(run_id == run) %>% 
    filter(type == type_name) %>% 
    ggplot(aes(x = scale, y = scale_variance,  ymax = scale_variance, ymin=0, 
               color = type, linetype=q, shape=q)) +
    geom_linerange() +
    geom_point(size = 3) +
    scale_linetype_manual(breaks=c("q0", "q1", "q2"), values=c(3, 5, 1)) +
    scale_shape_manual(breaks=c("q0", "q1", "q2"), values=c(4,1,16)) +
    scale_x_log10(breaks=as.numeric(scales), labels=scales, minor_breaks=NULL) +
    # axis labels
    labs(subtitle = type_name,
         y = "scale variance", 
         x = "scale") +
    theme(legend.position="none")
}

# plot scale variance results for specified run (faceted by type)
p2 <- df %>% 
  filter(run_id == "R1") %>% 
  ggplot(aes(x = scale, y = scale_variance,  ymax = scale_variance, ymin=0, 
             color = type, linetype=q, shape=q)) +
  # Draw lines from y=0 to the value at each category
  geom_linerange() +
  # Draw the points/dots
  geom_point(size = 3) +
  scale_linetype_manual(breaks=c("q0", "q1", "q2"), values=c(3, 5, 1)) +
  scale_shape_manual(breaks=c("q0", "q1", "q2"), values=c(4,1,16)) +
  scale_x_log10(breaks=as.numeric(scales), labels=scales, minor_breaks=NULL) +
  # axis labels
  labs(y = "scale variance (%)", 
       x = "scale") +
  theme(legend.position="none") +
  facet_wrap(vars(factor(type, levels=c("OBS", "EBK", "OK"))))

# arrange plot showing cumulative (first row, line graph) results for multiple sites 
# and scale variance (second row, lollipop graph) results for one site
ggarrange(p1, p2, nrow = 2, ncol = 1, labels='auto')
