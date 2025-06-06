# Figure 3
# HSP70-Plasma and Corticosterone

# Organize Data -------------
df_fig3 = df_inflam %>% 
  mutate(
  logHSP70PL = 100*log(HSP70PL),
  logCort = 100*log(Cort)
) 

df_anno_fig3a = read.csv("df_anno_fig3a.csv") %>%
  mutate(Recovery2 = "EHI0")  %>%
  mutate(SacTime2 = factor(SacTime2,
                           levels = c("min-30",
                                      "hr-3",
                                      "day-1",
                                      "day-7")))

# Panel A: HSP70 in Plasma -----------
Fig3A = ggplot(df_fig3, aes(y = logHSP70PL, x = Recovery2, 
                             color = Recovery2, group = Recovery2)) + 
  geom_jitter(alpha = .5,
              position = pos_1) +
  stat_summary(
    fun.data = "mean_sd",
    geom = "errorbar",
    mapping = aes(group = Recovery2),
    position = position_nudge(x = .25),
    size = 0.9,
    width = .25,
    
  )  +
  stat_summary(
    aes(y = logHSP70PL, group = Recovery2, shape = Recovery2),
    position = position_nudge(x = .25),
    fun = mean,
    geom = "point",
    size = 2
  ) +
    facet_wrap(. ~ SacTime2, ncol = 4,
               strip.position = "bottom") +
    scale_shape_manual(values = c(15,16,17,18))+
    scale_color_manual(values = c("black","red","green","blue")) +
    # scale_color_grey(start=0.1, end=0.7)  +
    labs(x='Recovery Time',
         y=bquote("100\u2022"~log[e]~"(HSP70-Plasma)"))+
    annotate("rect", xmin = -Inf, xmax = Inf, 
             ymin = 0, ymax = 48, 
             fill = "grey", alpha = .3, 
             color = NA)+
    geom_hline(yintercept = 24)+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank())+
    geom_text(data = df_anno_fig3a,
              aes(x=xstar,y=ystar, label=labs),
              size=3.25,
              fontface="bold",
              show.legend= FALSE)+
    geom_segment(data = df_anno_fig3a, aes(x = x1, xend = x2, 
                                           y = yval, yend = yval),
                 colour = "darkred")



df_anno_fig3b = read.csv("df_anno_fig3b.csv") %>%
  mutate(Recovery2 = "EHI0")  %>%
  mutate(SacTime2 = factor(SacTime2,
                           levels = c("min-30",
                                      "hr-3",
                                      "day-1",
                                      "day-7")))

# Panel B: Corticosterone --------------
Fig3B = ggplot(df_fig3, aes(y = logCort, x = Recovery2, 
                            color = Recovery2, group = Recovery2)) + 
  geom_jitter(alpha = .5, 
              position = pos_1) +
  stat_summary(fun.data = "mean_sd",
               geom = "errorbar",
               mapping = aes(group = Recovery2),
               position = position_nudge(x=.25), 
               size = 1,
               width = .25) +
  stat_summary(aes(y = logCort, group = Recovery2, shape = Recovery2),
               position = position_nudge(x=.25),
               fun = mean, geom = "point", 
               size = 2) +
  facet_wrap(. ~ SacTime2, ncol = 4,
             strip.position = "bottom") +
  scale_shape_manual(values = c(15,16,17,18))+
  scale_color_manual(values = c("black","red","green","blue")) +
  labs(x='Recovery Time',
       y=bquote("100\u2022"~log[e]~"(Corticosterone)"))+ 
  annotate("rect", xmin = -Inf, xmax = Inf, 
           ymin = (997-103), ymax = (997+103), 
           fill = "grey", alpha = .3, 
           color = NA)+
  geom_hline(yintercept = 997)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.title = element_blank()) +
    geom_text(data = df_anno_fig3b,
              aes(x=xstar,y=ystar, label=labs),
              size=3.25,fontface="bold",
              show.legend= FALSE)+
    geom_segment(data = df_anno_fig3b, aes(x = x1, xend = x2, 
                                           y = yval, yend = yval),
                 colour = "darkred") 
# Figure 3 -------------
Fig3 <- ggarrange(Fig3A,Fig3B, labels = c("A","B"),
                  common.legend = TRUE)

ggsave(plot = Fig3, "Fig3.pdf",
       encoding="MacRoman",
       width = 8.25,
       height = 5,
       device = "pdf")
