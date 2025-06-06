# Figure 6
# CHEMOKINES (Fig 6): MIP1b, MIP2, G/CSF, KC

df_Fig6 = df_inflam %>%
  mutate(log_MIP1b = 100*log(MIP1b),
         log_MIP2 = 100*log(MIP2),
         log_GCSF = 100*log(GCSF),
         log_KC = 100*log(KC))


# Figure MIP-Beta -----------------------
FigMIP1b_supp = ggplot(df_Fig6, aes(y = MIP1b, x = Recovery2, 
                                color = Recovery2, group = Recovery2)) + 
    geom_jitter(alpha = .5, 
                position = pos_1)+
    facet_wrap(. ~ SacTime2, ncol = 4,
               strip.position = "bottom") +
    scale_shape_manual(values = c(15,16,17,18))+
    scale_color_manual(values = c("black","red","green","blue")) +
    labs(x='Recovery Time',
         y=bquote("MIP1-"~beta*""))+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank())  

# Figure MIP2 --------------
FigMIP2_supp = ggplot(df_Fig6, aes(y = MIP2, x = Recovery2, 
                             color = Recovery2, group = Recovery2)) + 
    geom_jitter(alpha = .5, 
                position = pos_1)+
    facet_wrap(. ~ SacTime2, ncol = 4,
               strip.position = "bottom") +
    scale_shape_manual(values = c(15,16,17,18))+
    scale_color_manual(values = c("black","red","green","blue")) +
    labs(x='Recovery Time',
         y="MIP-2")+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank()) 


# Figure GCSF -------------------
FigGCSF_supp = ggplot(df_Fig6, aes(y = GCSF, x = Recovery2, 
                             color = Recovery2, group = Recovery2)) + 
    geom_jitter(alpha = .5, 
                position = pos_1)+
    facet_wrap(. ~ SacTime2, ncol = 4,
               strip.position = "bottom") +
    scale_shape_manual(values = c(15,16,17,18))+
    scale_color_manual(values = c("black","red","green","blue")) +
    labs(x='Recovery Time',
         y="G/CSF")+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank()) 

# Figure KC -----------------------
FigKC_supp = ggplot(df_Fig6, aes(y = KC, x = Recovery2, 
                             color = Recovery2, group = Recovery2)) + 
    geom_jitter(alpha = .5, 
                position = pos_1)+
    facet_wrap(. ~ SacTime2, ncol = 4,
               strip.position = "bottom") +
    scale_shape_manual(values = c(15,16,17,18))+
    scale_color_manual(values = c("black","red","green","blue")) +
    labs(x='Recovery Time',
         y="KC")+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank())


#  Figure 6 --------------
Fig6_supp <- ggarrange(FigMIP1b_supp, 
                  FigMIP2_supp, 
                  FigGCSF_supp, 
                  FigKC_supp,
                  labels = c("A", "B", "C", "D"),
                  common.legend = TRUE)

ggsave(plot = Fig6_supp, "Fig6_supp.pdf",
       width = 10,
       height = 6,
       device = "pdf")
