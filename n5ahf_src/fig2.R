
# Import Average Data -----
df_temps <- read_csv("dat_timecourse.csv") %>%
  mutate(ymin = (mu-sd),
         ymax = (mu+sd)) %>%
  select(-X5)

# Fig2a with error bars -------
Fig2bar =   ggplot(df_temps,
                   aes(x=time,
                       group=cond,
                       color=cond,
                       y=mu,ymin=ymin,
                       ymax=ymax))+
  #geom_ribbon(alpha=.55) +
  geom_errorbar(color="black",alpha=.5,
                position = position_dodge(width = 0.25))+
  geom_line(size=1.05)+
  scale_color_manual(values = c("black","red",
                                "green","blue",
                                "darkgrey")) +
  #facet_wrap(~cond) +
  labs(x='Time Post Start of Wheel (h)',
       y='Core Temperature (°C)') +
  scale_y_continuous(limits = c(30,44),
                     breaks = seq(30,44,2), 
                     expand = c(0, 0)) + 
  scale_x_continuous(limits = c(-1,26),
                     breaks = seq(-1,26,2), 
                     expand = c(0, 0)) +
  theme(legend.title = element_blank())     

# Fig2a ------
Fig2line =   ggplot(df_temps,
                    aes(
                      x = time,
                      group = cond,
                      color = cond,
                      y = mu,
                      ymin = ymin,
                      ymax = ymax
                    )) +
  geom_line(size = 1.05, alpha = .5)+
  scale_color_manual(values = c("black","red",
                                "green","blue",
                                "darkgrey")) +
  #facet_wrap(~cond) +
  labs(x='Time Post Start of Wheel (h)',
       y='Core Temperature (°C)') +
  scale_y_continuous(limits = c(30,44),
                     breaks = seq(30,44,2), 
                     expand = c(0, 0)) + 
  scale_x_continuous(limits = c(-1,26),
                     breaks = seq(-1,26,2), 
                     expand = c(0, 0)) +
  theme(legend.title = element_blank())

# Individual Data ------------
## EHI0 ---------
EHI0temps = read.csv("EHI0temps.csv",
                     nrows=55) %>%
  select(starts_with("Tc"))  %>%
  mutate(time = seq(-1,26,.5),
         cond = "EHI0") %>%
  pivot_longer(
    cols = starts_with("Tc"),
    names_to = "sub",
    names_prefix = "tc",
    values_to = "temp",
    values_drop_na = TRUE
 ) %>%
  mutate(sub = as.factor(sub))

## EHI1 ---------
EHI1temps = read.csv("EHI1temps.csv",
                     nrows=55) %>%
  select(starts_with("Tc"))  %>%
  mutate(time = seq(-1,26,.5),
         cond = "EHI1") %>%
  pivot_longer(
    cols = starts_with("Tc"),
    names_to = "sub",
    names_prefix = "tc",
    values_to = "temp",
    values_drop_na = TRUE
  ) %>%
  mutate(sub = as.factor(sub))

## EHI3 ---------
EHI3temps = read.csv("EHI3temps.csv",
                     nrows=55) %>%
  select(starts_with("Tc"))  %>%
  mutate(time = seq(-1,26,.5),
         cond = "EHI3") %>%
  pivot_longer(
    cols = starts_with("Tc"),
    names_to = "sub",
    names_prefix = "tc",
    values_to = "temp",
    values_drop_na = TRUE
  ) %>%
  mutate(sub = as.factor(sub))

## EHI7 ---------
EHI7temps = read.csv("EHI7temps.csv",
                     nrows=55) %>%
  select(starts_with("Tc"))  %>%
  mutate(time = seq(-1,26,.5),
         cond = "EHI7") %>%
  pivot_longer(
    cols = starts_with("Tc"),
    names_to = "sub",
    names_prefix = "tc",
    values_to = "temp",
    values_drop_na = TRUE
  ) %>%
  mutate(sub = as.factor(sub))

## EXC ---------
EXCtemps = read.csv("EXCtemps.csv",
                    nrows=55) %>%
  select(starts_with("Tc"))  %>%
  mutate(time = seq(-1,26,.5),
         cond = "EXC") %>%
  pivot_longer(
    cols = starts_with("Tc"),
    names_to = "sub",
    names_prefix = "tc",
    values_to = "temp",
    values_drop_na = TRUE
  ) %>%
  mutate(sub = as.factor(sub))

## Combined ----
ind_temps = bind_rows(EHI0temps,
                      EHI1temps,
                      EHI3temps,
                      EHI7temps,
                      EXCtemps) %>%
  mutate(cond = as.factor(cond))

# Fig2b: Individual data lines ----
fig2_ind = ggplot(ind_temps,
       aes(x=time,y=temp,group=sub,color=cond)) +
  geom_line(alpha=.5) +
  facet_wrap(~cond) +
  scale_color_manual(values = c("black","red",
                                "green","blue",
                                "darkgrey")) +
  labs(x='Time Post Start of Wheel (h)',
       y='Core Temperature (°C)') +
  scale_y_continuous(limits = c(28.75,44),
                     breaks = seq(30,44,2), 
                     expand = c(0, 0)) + 
  scale_x_continuous(limits = c(-1,26),
                     breaks = seq(-1,26,2),
                     expand = c(0, 0)) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45))

# Averages by gam ---
fig2gam = ggplot(ind_temps,
       aes(x=time,y=temp,color=cond)) +
  geom_smooth(alpha=.5,
              method = 'gam',
              formula = y ~ s(x, bs = "cs"),
              se = FALSE) +
  scale_color_manual(values = c("black",
                                "red",
                                "green",
                                "blue",
                                "darkgrey")) +
  labs(x='Time Post Start of Wheel (h)',
       y='Core Temperature (°C)') +
  scale_y_continuous(limits = c(28.75,44),
                     breaks = seq(30,44,2),
                     expand = c(0, 0)) + 
  scale_x_continuous(limits = c(-1,26),
                     breaks = seq(-1,26,2), 
                     expand = c(0, 0)) +
  theme(legend.title = element_blank())

# Figure 2 ---------------
(Fig2 <- ggarrange(Fig2line,fig2_ind,
                   labels = c("A","B"),
                   heights= c(1,2),
                   ncol = 1,
                   common.legend = TRUE))
# Export ------------
ggsave(plot = Fig2, "Fig2.pdf",
       encoding="MacRoman",
       width = 9,
       height = 9,
       device = "pdf")
