# Fake histogram plot

my_col <- c(RColorBrewer::brewer.pal(3, "Blues")[3],"orange")

# all fert
h1 <- data.frame(ante = rnorm(1000, mean = -0.5, sd = 0.1),
                 st = rnorm(1000, mean = -0.4, sd = 0.1)) %>%
  pivot_longer(ante:st, names_to = "dep_type", values_to = "growth_change")

p_h1 <- ggplot(data = h1, aes(x = growth_change, group = dep_type, color = dep_type, fill = dep_type))+
  geom_density(alpha = 0.5)+
  xlim(c(-1,1))+
  geom_vline(xintercept = 0)+
  scale_color_manual(values = my_col, labels = c("antecedent N dep.","short-term change in N dep."), name = "")+
  scale_fill_manual(values = my_col, labels = c("antecedent N dep.","short-term change in N dep."), name = "")+
  theme_bw()+
  xlab(expression(paste("change in growth (kg C ", y^-1," ",ind^-1,")")))

ggsave(p_h1,filename = "./visualizations/H1.png",
       device = "png", height = 2, width = 5, units = "in")

# all stress
h2 <- data.frame(ante = rnorm(1000, mean = 0.5, sd = 0.1),
                 st = rnorm(1000, mean = 0.4, sd = 0.1)) %>%
  pivot_longer(ante:st, names_to = "dep_type", values_to = "growth_change")

p_h2 <- ggplot(data = h2, aes(x = growth_change, group = dep_type, color = dep_type, fill = dep_type))+
  geom_density(alpha = 0.5)+
  xlim(c(-1,1))+
  geom_vline(xintercept = 0)+
  scale_color_manual(values = my_col, labels = c("antecedent N dep.","short-term change in N dep."), name = "")+
  scale_fill_manual(values = my_col, labels = c("antecedent N dep.","short-term change in N dep."), name = "")+
  theme_bw()+
  xlab(expression(paste("change in growth (kg C ", y^-1," ",ind^-1,")")))

ggsave(p_h2,filename = "./visualizations/H2.png",
       device = "png", height = 2, width = 5, units = "in")

# long-term stress, short term fertilization
h3 <- data.frame(ante = rnorm(1000, mean = 0.5, sd = 0.1),
                 st = rnorm(1000, mean = -0.5, sd = 0.1)) %>%
  pivot_longer(ante:st, names_to = "dep_type", values_to = "growth_change")

p_h3 <- ggplot(data = h3, aes(x = growth_change, group = dep_type, color = dep_type, fill = dep_type))+
  geom_density(alpha = 0.5)+
  xlim(c(-1,1))+
  geom_vline(xintercept = 0)+
  scale_color_manual(values = my_col, labels = c("antecedent N dep.","short-term change in N dep."), name = "")+
  scale_fill_manual(values = my_col, labels = c("antecedent N dep.","short-term change in N dep."), name = "")+
  theme_bw()+
  xlab(expression(paste("change in growth (kg C ", y^-1," ",ind^-1,")")))

ggsave(p_h3,filename = "./visualizations/H3.png",
       device = "png", height = 2, width = 5, units = "in")

# long-term fertilization, long-term stress
h4 <- data.frame(ante = rnorm(1000, mean = -0.5, sd = 0.1),
                 st = rnorm(1000, mean = 0.5, sd = 0.1)) %>%
  pivot_longer(ante:st, names_to = "dep_type", values_to = "growth_change")

p_h4 <- ggplot(data = h4, aes(x = growth_change, group = dep_type, color = dep_type, fill = dep_type))+
  geom_density(alpha = 0.5)+
  xlim(c(-1,1))+
  geom_vline(xintercept = 0)+
  scale_color_manual(values = my_col, labels = c("antecedent N dep.","short-term change in N dep."), name = "")+
  scale_fill_manual(values = my_col, labels = c("antecedent N dep.","short-term change in N dep."), name = "")+
  theme_bw()+
  xlab(expression(paste("change in growth (kg C ", y^-1," ",ind^-1,")")))

ggsave(p_h4,filename = "./visualizations/H4.png",
       device = "png", height = 2, width = 5, units = "in")
