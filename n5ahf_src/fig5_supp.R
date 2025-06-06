# Figure 5
# IL-6, IL-10, IP-10

df_fig5 = df_inflam %>%
  mutate(log_IL10 = 100*log(IL10),
         log_IL6 = 100*log(IL6),
         log_IP10 = 100*log(IP10))


# Figure IL-10 ------------------
FigIL10_supp = ggplot(df_fig5, aes(y = IL10, 
                                   x = Recovery2, 
                               color = Recovery2, 
                               group = Recovery2)) + 
    geom_jitter(alpha = .5, 
                position = pos_1)+
    facet_wrap(. ~ SacTime2, ncol = 4,
               strip.position = "bottom") +
    scale_shape_manual(values = c(15,16,17,18))+
    scale_color_manual(values = c("black","red","green","blue")) +
    labs(x='Recovery Time',
         y="IL-10")+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank())
  

# Figure IL6 -----------
FigIL6_supp = ggplot(df_fig5, aes(y = IL6, 
                              x = Recovery2, 
                             color = Recovery2, 
                             group = Recovery2)) + 
    geom_jitter(alpha = .5, 
                position = pos_1)+
   facet_wrap(. ~ SacTime2, ncol = 4,
              strip.position = "bottom") +
    scale_shape_manual(values = c(15,16,17,18))+
    scale_color_manual(values = c("black","red","green","blue")) +
    labs(x='Recovery Time',
         y="IL-6")+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank()) 


# Figure IP10 --------------
FigIP10_supp = ggplot(df_fig5, aes(y = IP10, x = Recovery2, 
                             color = Recovery2, group = Recovery2)) + 
    geom_jitter(alpha = .5, 
                position = pos_1)+
    facet_wrap(. ~ SacTime2, ncol = 4,
               strip.position = "bottom") +
    scale_shape_manual(values = c(15,16,17,18))+
    scale_color_manual(values = c("black","red","green","blue")) +
    labs(x='Recovery Time',
         y="IP-10")+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank()) 



# Figure 5 ------------------
(Fig5_supp <- ggarrange(FigIL6_supp, FigIL10_supp, FigIP10_supp,
                   labels = c("A","B","C"),
                   ncol = 1,
                   common.legend = TRUE))
ggsave(plot = Fig5_supp, "Fig5_supp.pdf",
       width = 6,
       height = 10,
       device = "pdf")
  