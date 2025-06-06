# Figure 5
# IL-6, IL-10, IP-10

df_fig5 = df_inflam %>%
  mutate(log_IL10 = 100*log(IL10),
         log_IL6 = 100*log(IL6),
         log_IP10 = 100*log(IP10))

df_anno_IL10 = read.csv("df_anno_IL10.csv") %>%
  mutate(Recovery2 = "EHI0")  %>%
  mutate(SacTime2 = factor(SacTime2,
                           levels = c("min-30",
                                      "hr-3",
                                      "day-1",
                                      "day-7")))

# Figure IL-10 ------------------
(FigIL10 = ggplot(df_fig5, aes(y = log_IL10, x = Recovery2, 
                               color = Recovery2, group = Recovery2)) + 
    geom_jitter(alpha = .5, 
                position = pos_1)+
    stat_summary(aes(y = log_IL10, group = Recovery2, shape = Recovery2),
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
         y=bquote("100\u2022"~log[e]~"(IL-10)"))+
    annotate("rect", xmin = -Inf, xmax = Inf, 
             ymin = (168-26), ymax = (168+26), 
             fill = "grey", alpha = .3, 
             color = NA)+
    geom_hline(yintercept = 168)+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank())+
    geom_text(data = df_anno_IL10,
              aes(x=xstar,y=ystar, label=labs),
              size=3.25,
              fontface="bold",
              show.legend = FALSE)+
    geom_segment(data = df_anno_IL10, 
                 aes(x = x1, xend = x2, 
                     y = yval, yend = yval),
                 colour = "darkred"))

df_anno_IL6 = read.csv("df_anno_IL6.csv") %>%
  mutate(Recovery2 = "EHI0") %>%
  mutate(SacTime2 = factor(SacTime2,
                           levels = c("min-30",
                                      "hr-3",
                                      "day-1",
                                      "day-7")))
  

# Figure IL6 -----------
(FigIL6 = ggplot(df_fig5, aes(y = log_IL6, 
                              x = Recovery2, 
                             color = Recovery2, 
                             group = Recovery2)) + 
    geom_jitter(alpha = .5, 
                position = pos_1)+
    stat_summary(aes(y = log_IL6, 
                     group = Recovery2, 
                     shape = Recovery2),
                 position = position_nudge(x=.25),
                 fun = mean, geom = "point", 
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
         y=bquote("100\u2022"~log[e]~"(IL-6)"))+
    annotate("rect", xmin = -Inf, xmax = Inf, 
             ymin = (156-58), ymax = (156+58), 
             fill = "grey", alpha = .3, 
             color = NA)+
    geom_hline(yintercept = 156)+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank()) +
    geom_text(data = df_anno_IL6,
              aes(x=xstar,y=ystar, label=labs),
              size=3.25,
              fontface="bold",
              show.legend = FALSE)+
    geom_segment(data = df_anno_IL6, 
                 aes(x = x1,
                     xend = x2,
                     y = yval,
                     yend = yval), 
                 colour = "darkred"))

df_anno_IP10 = read.csv("df_anno_IP10.csv") %>%
  mutate(Recovery2 = "EHI0")  %>%
  mutate(SacTime2 = factor(SacTime2,
                           levels = c("min-30",
                                      "hr-3",
                                      "day-1",
                                      "day-7")))

# Figure IP10 --------------
(FigIP10 = ggplot(df_fig5, aes(y = log_IP10, x = Recovery2, 
                             color = Recovery2, group = Recovery2)) + 
    geom_jitter(alpha = .5, 
                position = pos_1)+
    stat_summary(aes(y = log_IP10, group = Recovery2, shape = Recovery2),
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
         y=bquote("100\u2022"~log[e]~"(IP-10)"))+
    annotate("rect", xmin = -Inf, xmax = Inf, 
             ymin = (mean(100*log(df_inflam_CON$IP10),na.rm=TRUE)-sd(100*log(df_inflam_CON$IP10),na.rm=TRUE)), 
             ymax = (mean(100*log(df_inflam_CON$IP10),na.rm=TRUE)+sd(100*log(df_inflam_CON$IP10),na.rm=TRUE)), 
             fill = "grey", alpha = .3, 
             color = NA)+
    geom_hline(yintercept = mean(100*log(df_inflam_CON$IP10),na.rm=TRUE))+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank()) +
    geom_text(data = df_anno_IP10,
              aes(x=xstar,y=ystar, label=labs),
              size=3.25,
              fontface="bold",
              show.legend = FALSE)+
    geom_segment(data = df_anno_IP10,
                 aes(
                   x = x1,
                   xend = x2,
                   y = yval,
                   yend = yval
                 ),
                 colour = "darkred"))



# Figure 5 ------------------
(Fig5 <- ggarrange(FigIL6, FigIL10, FigIP10,
                   labels = c("A","B","C"),
                   ncol = 1,
                   common.legend = TRUE))
ggsave(plot = Fig5, "Fig5.pdf",
       encoding="MacRoman",
       width = 6,
       height = 10,
       device = "pdf")
  