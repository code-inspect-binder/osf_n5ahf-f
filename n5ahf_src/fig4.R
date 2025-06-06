# Figure 4
# CK, I-FABP2, AST, HSP70-Liver 

# Get summary data ---------------
summcon_log_CK = df_HIT %>% filter(Type == "EXC") %>%
  summarise(mean = mean(log_CK,na.rm=TRUE),
            sd_min = mean(log_CK,na.rm=TRUE)-sd(log_CK,na.rm=TRUE),
            sd_max = mean(log_CK,na.rm=TRUE)+sd(log_CK,na.rm=TRUE))

summcon_log_AST = df_HIT %>% filter(Type == "EXC") %>%
  summarise(mean = mean(log_AST,na.rm=TRUE),
            sd_min = mean(log_AST,na.rm=TRUE)-sd(log_AST,na.rm=TRUE),
            sd_max = mean(log_AST,na.rm=TRUE)+sd(log_AST,na.rm=TRUE))

# Organize Data --------------
df_fig4 = df_inflam %>%
  mutate(
    log_FABP2 = 100*log(FABP2),
    log_HSP70LIV = 100*log(HSP70LIV+1)
  ) 

df_anno_fig4a = read.csv("df_anno_fig4a.csv") %>%
  mutate(Recovery2 = "EHI0")  %>%
  mutate(SacTime2 = factor(SacTime2,
                           levels = c("min-30",
                                      "hr-3",
                                      "day-1",
                                      "day-7")))


# Panel A: Creatine Kinase ----------
Fig4A = ggplot(df_HITvis, aes(y = log_CK, x = Recovery2, 
                             color = Recovery2, group = Recovery2)) + 
    geom_jitter(alpha = .5, 
                position = pos_1)+
    stat_summary(aes(y = log_CK, group = Recovery2, shape = Recovery2),
                 position = position_nudge(x=.25),
                 fun.y = mean, geom = "point", 
                 size = 2) +
    stat_summary(fun.data = "mean_sd",
                 geom = "errorbar",
                 mapping = aes(group = Recovery2),
                 position = position_nudge(x=.25), 
                 size = 1,
                 width = .25) +
    facet_wrap(. ~ SacTime2, ncol = 4,
               strip.position = "bottom") +
    scale_shape_manual(values = c(15,16,17,18))+
    scale_color_manual(values = c("black","red","green","blue")) +
    labs(x='Recovery Time',
         y=bquote("100\u2022"~log[e]~"(CK)"))+
    annotate("rect", xmin = -Inf, xmax = Inf, 
             ymin = summcon_log_CK$sd_min, ymax = summcon_log_CK$sd_max, 
             fill = "grey", alpha = .3, 
             color = NA)+
    geom_hline(yintercept = summcon_log_CK$mean)+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank())+
    geom_text(data = df_anno_fig4a,
              aes(x=xstar,y=ystar, label=labs),
              size=3.25,
              fontface="bold",
              show.legend = FALSE)+
    geom_segment(data = df_anno_fig4a, aes(x = x1, xend = x2, 
                                           y = yval, yend = yval),
                 colour = "darkred")


df_anno_fig4b = read.csv("df_anno_fig4b.csv") %>%
  mutate(Recovery2 = "EHI0")  %>%
  mutate(SacTime2 = factor(SacTime2,
                           levels = c("min-30",
                                      "hr-3",
                                      "day-1",
                                      "day-7")))

# Panel B: FABP2 ------------
Fig4B = ggplot(df_fig4, aes(y = log_FABP2, x = Recovery2, 
                               color = Recovery2, group = Recovery2)) + 
    geom_jitter(alpha = .5, 
                position = pos_1)+
    stat_summary(aes(y = log_FABP2, group = Recovery2, shape = Recovery2),
                 position = position_nudge(x=.25),
                 fun.y = mean, geom = "point", 
                 size = 2) +
    stat_summary(fun.data = "mean_sd",
                 geom = "errorbar",
                 mapping = aes(group = Recovery2),
                 position = position_nudge(x=.25), 
                 size = 1,
                 width = .25) +
    facet_wrap(. ~ SacTime2, ncol = 4,
               strip.position = "bottom") +
    scale_shape_manual(values = c(15,16,17,18))+
    scale_color_manual(values = c("black","red","green","blue")) +
    labs(x='Recovery Time',
         y=bquote("100\u2022"~log[e]~"(I-FABP2)"))+
    annotate("rect", xmin = -Inf, xmax = Inf, 
             ymin = 175, ymax = 225, 
             fill = "grey", alpha = .3, 
             color = NA)+
    geom_hline(yintercept = 200)+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank())+
    geom_text(data = df_anno_fig4b,
              aes(x=xstar,y=ystar, label=labs),
              size=3.25,
              fontface="bold",
              show.legend = FALSE)+
    geom_segment(data = df_anno_fig4b, aes(x = x1, xend = x2, 
                                           y = yval, yend = yval),
                 colour = "darkred")

df_anno_fig4c = read.csv("df_anno_fig4c.csv") %>%
  mutate(Recovery2 = "EHI0") %>%
  mutate(SacTime2 = factor(SacTime2,
                           levels = c("min-30",
                                      "hr-3",
                                      "day-1",
                                      "day-7")))


# Panel D: Liver HSP70 ----------------
Fig4C = ggplot(df_fig4, aes(y = log_HSP70LIV, x = Recovery2, 
                             color = Recovery2, group = Recovery2)) + 
    geom_jitter(alpha = .5, 
                position = pos_1)+
    stat_summary(aes(y = log_HSP70LIV, group = Recovery2, shape = Recovery2),
                 position = position_nudge(x=.25),
                 fun.y = mean, geom = "point", 
                 size = 2) +
    stat_summary(fun.data = "mean_sd",
                 geom = "errorbar",
                 mapping = aes(group = Recovery2),
                 position = position_nudge(x=.25), 
                 size = 1,
                 width = .25) +
    facet_wrap(. ~ SacTime2, ncol = 4,
               strip.position = "bottom") +
    scale_shape_manual(values = c(15,16,17,18))+
    scale_color_manual(values = c("black","red","green","blue")) +
    labs(x='Recovery Time',
         y=bquote("100\u2022"~log[e]~"(HSP70-Liver)"))+
    annotate("rect", xmin = -Inf, xmax = Inf, 
             ymin = (20-13), ymax = (20+13), 
             fill = "grey", alpha = .3, 
             color = NA)+
    geom_hline(yintercept = 20)+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank())+
    geom_text(data = df_anno_fig4c,
              aes(x=xstar,y=ystar, label=labs),
              size=3.25,
              fontface="bold",
              show.legend = FALSE)+
    geom_segment(data = df_anno_fig4c, aes(x = x1, xend = x2, 
                                           y = yval, yend = yval),
                 colour = "darkred")

df_anno_fig4d = read.csv("df_anno_fig4d.csv") %>%
  mutate(Recovery2 = "EHI0")  %>%
  mutate(SacTime2 = factor(SacTime2,
                           levels = c("min-30",
                                      "hr-3",
                                      "day-1",
                                      "day-7")))


Fig4D = ggplot(df_HITvis, aes(y = log_AST, x = Recovery2, 
                             color = Recovery2, group = Recovery2)) + 
    geom_jitter(alpha = .5, 
                position = pos_1)+
    stat_summary(aes(y = log_AST, group = Recovery2, shape = Recovery2),
                 position = position_nudge(x=.25),
                 fun.y = mean, geom = "point", 
                 size = 2) +
    stat_summary(fun.data = "mean_sd",
                 geom = "errorbar",
                 mapping = aes(group = Recovery2),
                 position = position_nudge(x=.25), 
                 size = 1,
                 width = .25) +
    facet_wrap(. ~ SacTime2, ncol = 4,
               strip.position = "bottom") +
    scale_shape_manual(values = c(15,16,17,18))+
    scale_color_manual(values = c("black","red","green","blue")) +
    labs(x='Recovery Time',
         y=bquote("100\u2022"~log[e]~"(AST)"))+
    annotate("rect", xmin = -Inf, xmax = Inf, 
             ymin = summcon_log_AST$sd_min, ymax = summcon_log_AST$sd_max, 
             fill = "grey", alpha = .3, 
             color = NA)+
    geom_hline(yintercept = summcon_log_AST$mean)+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank())+
    geom_text(data = df_anno_fig4d,
              aes(x=xstar,y=ystar, label=labs),
              size=3.15,
              fontface="bold",
              show.legend = FALSE)+
    geom_segment(data = df_anno_fig4d, aes(x = x1, xend = x2, 
                                           y = yval, yend = yval),
                 colour = "darkred")

# Note: order changed in manuscript C & D flipped (as noted below)
# Figure 4 --------------
Fig4 <- ggarrange(Fig4A, Fig4B, Fig4D, Fig4C,
                  labels = c("A","B","C","D"),
                  common.legend = TRUE)

ggsave(plot = Fig4, "Fig4.pdf",
       encoding="MacRoman",
       width = 10.65,
       height = 6.5,
       device = "pdf")
