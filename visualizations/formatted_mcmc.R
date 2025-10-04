mcmc <- arrow::read_parquet("./experiments/space_vs_time_ortho/space_vs_time_ortho-sugar maple-mcmc.parquet") %>%
  select(p5, p9, .iteration, .chain) %>%
  pivot_longer(p5:p9, names_to = "parameter", values_to = "value") %>%
  mutate(parameter = ifelse(parameter == "p5","recent N dep.","antecedent N dep."))

p <- mcmc |> 
  ggplot(aes(x = .iteration, y = value, color = factor(.chain))) + 
  geom_line() + 
  facet_wrap(~parameter, scales = "free") +
  labs(color = "chain", x = "iteration") +
  theme_bw() +
  ggtitle("Acer saccharum")
p

ggsave(filename = paste0("./visualizations/acer-saccharum-Ndep-mcmc.png"), plot = p, device = "png",
       height = 4, width = 6, units = "in")
