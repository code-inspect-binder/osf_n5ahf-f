# Figure 6
# CHEMOKINES (Fig 6): MIP1b, MIP2, G/CSF, KC

df_Fig6 = df_inflam %>%
  mutate(log_MIP1b = 100*log(MIP1b),
         log_MIP2 = 100*log(MIP2),
         log_GCSF = 100*log(GCSF),
         log_KC = 100*log(KC))

df_anno_MIP1b = read.csv("df_anno_MIP1b.csv") %>%
  mutate(Recovery2 = "EHI0")  %>%
  mutate(SacTime2 = factor(SacTime2,
                           levels = c("min-30",
                                      "hr-3",
                                      "day-1",
                                      "day-7")))
# Figure MIP-Beta -----------------------
FigMIP1b = ggplot(df_Fig6, aes(y = log_MIP1b, x = Recovery2, 
                                color = Recovery2, group = Recovery2)) + 
    geom_jitter(alpha = .5, 
                position = pos_1)+
    stat_summary(aes(y = log_MIP1b, group = Recovery2, shape = Recovery2),
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
         y=bquote("100\u2022"~log[e]~"(MIP-1"~beta*")"))+
    annotate("rect", xmin = -Inf, xmax = Inf, 
             ymin = (mean(100*log(df_inflam_CON$MIP1b),na.rm=TRUE)-sd(100*log(df_inflam_CON$MIP1b),na.rm=TRUE)), 
             ymax = (mean(100*log(df_inflam_CON$MIP1b),na.rm=TRUE)+sd(100*log(df_inflam_CON$MIP1b),na.rm=TRUE)), 
             fill = "grey", alpha = .3, 
             color = NA)+
    geom_hline(yintercept = mean(100*log(df_inflam_CON$MIP1b),na.rm=TRUE))+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank())  +
    geom_text(data = df_anno_MIP1b,
              aes(x=xstar,y=ystar, label=labs),
              size=3.25,
              fontface="bold",
              show.legend = FALSE)+
    geom_segment(data = df_anno_MIP1b, aes(x = x1, xend = x2, 
                                          y = yval, yend = yval),
                 colour = "darkred")

df_anno_MIP2 = read.csv("df_anno_MIP2.csv") %>%
  mutate(Recovery2 = "EHI0")  %>%
  mutate(SacTime2 = factor(SacTime2,
                           levels = c("min-30",
                                      "hr-3",
                                      "day-1",
                                      "day-7")))
# Figure MIP2 --------------
FigMIP2 = ggplot(df_Fig6, aes(y = log_MIP2, x = Recovery2, 
                             color = Recovery2, group = Recovery2)) + 
    geom_jitter(alpha = .5, 
                position = pos_1)+
    stat_summary(aes(y = log_MIP2, group = Recovery2, shape = Recovery2),
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
         y=bquote("100\u2022"~log[e]~"(MIP-2)"))+
    annotate("rect", xmin = -Inf, xmax = Inf, 
             ymin = mean(100*log(df_inflam_CON$MIP2),na.rm=TRUE)-sd(100*log(df_inflam_CON$MIP2),na.rm=TRUE), 
             ymax = mean(100*log(df_inflam_CON$MIP2),na.rm=TRUE)+sd(100*log(df_inflam_CON$MIP2),na.rm=TRUE), 
             fill = "grey", alpha = .3, 
             color = NA)+
    geom_hline(yintercept = mean(100*log(df_inflam_CON$MIP2),na.rm=TRUE))+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank()) +
    geom_text(data = df_anno_MIP2,
              aes(x=xstar,y=ystar, label=labs),
              size=3.25,
              fontface="bold",
              show.legend = FALSE)+
    geom_segment(data = df_anno_MIP2, aes(x = x1, xend = x2, 
                                           y = yval, yend = yval),
                 colour = "darkred")


df_anno_GCSF = read.csv("df_anno_GCSF.csv") %>%
  mutate(Recovery2 = "EHI0")  %>%
  mutate(SacTime2 = factor(SacTime2,
                           levels = c("min-30",
                                      "hr-3",
                                      "day-1",
                                      "day-7")))

# Figure GCSF -------------------
FigGCSF = ggplot(df_Fig6, aes(y = log_GCSF, x = Recovery2, 
                             color = Recovery2, group = Recovery2)) + 
    geom_jitter(alpha = .5, 
                position = pos_1)+
    stat_summary(aes(y = log_GCSF, group = Recovery2, shape = Recovery2),
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
         y=bquote("100\u2022"~log[e]~"(G/CSF)"))+
    annotate("rect", xmin = -Inf, xmax = Inf, 
             ymin = (mean(100*log(df_inflam_CON$GCSF),na.rm=TRUE)-sd(100*log(df_inflam_CON$GCSF),na.rm=TRUE)), 
             ymax = (mean(100*log(df_inflam_CON$GCSF),na.rm=TRUE)+sd(100*log(df_inflam_CON$GCSF),na.rm=TRUE)), 
             fill = "grey", alpha = .3, 
             color = NA)+
    geom_hline(yintercept = mean(100*log(df_inflam_CON$GCSF),na.rm=TRUE))+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank()) +
    geom_text(data = df_anno_GCSF,
              aes(x=xstar,y=ystar, label=labs),
              size=3.25,
              fontface="bold",
              show.legend = FALSE)+
    geom_segment(data = df_anno_GCSF, aes(x = x1, xend = x2, 
                                           y = yval, yend = yval),
                 colour = "darkred")

df_anno_KC = read.csv("df_anno_KC.csv") %>%
  mutate(Recovery2 = "EHI0")  %>%
  mutate(SacTime2 = factor(SacTime2,
                           levels = c("min-30",
                                      "hr-3",
                                      "day-1",
                                      "day-7")))

# Figure KC -----------------------
FigKC = ggplot(df_Fig6, aes(y = log_KC, x = Recovery2, 
                             color = Recovery2, group = Recovery2)) + 
    geom_jitter(alpha = .5, 
                position = pos_1)+
    stat_summary(aes(y = log_KC, group = Recovery2, shape = Recovery2),
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
         y=bquote("100\u2022"~log[e]~"(KC)"))+
    annotate("rect", xmin = -Inf, xmax = Inf, 
             ymin = (mean(100*log(df_inflam_CON$KC),na.rm=TRUE)-sd(100*log(df_inflam_CON$KC),na.rm=TRUE)), 
             ymax = (mean(100*log(df_inflam_CON$KC),na.rm=TRUE)+sd(100*log(df_inflam_CON$KC),na.rm=TRUE)), 
             fill = "grey", alpha = .3, 
             color = NA)+
    geom_hline(yintercept = mean(100*log(df_inflam_CON$KC),na.rm=TRUE))+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank()) +
    geom_text(data = df_anno_KC,
              aes(x=xstar,y=ystar, label=labs),
              size=3.25,
              fontface="bold",
              show.legend = FALSE)+
    geom_segment(data = df_anno_KC, aes(x = x1, xend = x2, 
                                           y = yval, yend = yval),
                 colour = "darkred")


#  Figure 6 --------------
Fig6 <- ggarrange(FigMIP1b, 
                  FigMIP2, 
                  FigGCSF, 
                  FigKC,
                  labels = c("A", "B", "C", "D"),
                  common.legend = TRUE)

ggsave(plot = Fig6, "Fig6.pdf",
       encoding="MacRoman",
       width = 10,
       height = 6,
       device = "pdf")
