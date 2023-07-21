# plot estimated diversity
i_freq1 <- obs %>%
  filter(level1 == 19286) %>%
  st_drop_geometry() %>%
  group_by(common_name, sampling_event_identifier) %>%
  summarize(present = n(), .groups = "keep") %>%
  mutate(present = 1) %>%
  pivot_wider(
    names_from = sampling_event_identifier,
    values_from = present,
    values_fill = 0
  ) %>%
  column_to_rownames(var = "common_name") %>%
  as.data.frame() %>%
  as.incfreq()

i_freq2 <- obs %>%
  filter(level1 == 19678) %>%
  st_drop_geometry() %>%
  group_by(common_name, sampling_event_identifier) %>%
  summarize(present = n(), .groups = "keep") %>%
  mutate(present = 1) %>%
  pivot_wider(
    names_from = sampling_event_identifier,
    values_from = present,
    values_fill = 0
  ) %>%
  column_to_rownames(var = "common_name") %>%
  as.data.frame() %>%
  as.incfreq()

i_freq <- list(i_freq1, i_freq2)

inxt <- iNEXT(i_freq, q=c(0,1,2), datatype = "incidence_freq")
ggiNEXT(inxt, type=1, facet.var = "Assemblage", color.var = "Order.q")