# Figure 4 raw
# CK, I-FABP2, AST, HSP70-Liver 

# Get summary data ---------------


# Organize Data --------------
df_fig4 = df_inflam %>%
  mutate(
    log_FABP2 = 100*log(FABP2),
    log_HSP70LIV = 100*log(HSP70LIV+1)
  ) 

# Panel A: Creatine Kinase ----------
Fig4A_supp = ggplot(df_HITvis, aes(y = CK, x = Recovery2, 
                             color = Recovery2, group = Recovery2)) + 
    geom_jitter(alpha = .5, 
                position = pos_1)+
    facet_wrap(. ~ SacTime2, ncol = 4,
               strip.position = "bottom") +
    scale_shape_manual(values = c(15,16,17,18))+
    scale_color_manual(values = c("black","red","green","blue")) +
    labs(x='Recovery Time',
         y="CK")+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank())


# Panel B: FABP2 ------------
Fig4B_supp = ggplot(df_fig4, aes(y = FABP2, x = Recovery2, 
                               color = Recovery2, group = Recovery2)) + 
    geom_jitter(alpha = .5, 
                position = pos_1)+
    facet_wrap(. ~ SacTime2, ncol = 4,
               strip.position = "bottom") +
    scale_shape_manual(values = c(15,16,17,18))+
    scale_color_manual(values = c("black","red","green","blue")) +
    labs(x='Recovery Time',
         y="I-FABP2")+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank())
    
# Panel D: Liver HSP70 ----------------
Fig4C_supp = ggplot(df_fig4, aes(y = HSP70LIV, x = Recovery2, 
                             color = Recovery2, group = Recovery2)) + 
    geom_jitter(alpha = .5, 
                position = pos_1)+
    facet_wrap(. ~ SacTime2, ncol = 4,
               strip.position = "bottom") +
    scale_shape_manual(values = c(15,16,17,18))+
    scale_color_manual(values = c("black","red","green","blue")) +
    labs(x='Recovery Time',
         y="HSP70-Liver")+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank())


Fig4D_supp = ggplot(df_HITvis, aes(y = AST, x = Recovery2, 
                             color = Recovery2, group = Recovery2)) + 
    geom_jitter(alpha = .5, 
                position = pos_1)+
   
    facet_wrap(. ~ SacTime2, ncol = 4,
               strip.position = "bottom") +
    scale_shape_manual(values = c(15,16,17,18))+
    scale_color_manual(values = c("black","red","green","blue")) +
    labs(x='Recovery Time',
         y="AST")+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank())


# Note: order changed in manuscript C & D flipped (as noted below)
# Figure 4 --------------
Fig4_supp <- ggarrange(Fig4A_supp, Fig4B_supp, Fig4D_supp, Fig4C_supp,
                  labels = c("A","B","C","D"),
                  common.legend = TRUE)

ggsave(plot = Fig4_supp, "Fig4_supp.pdf",
       width = 10.65,
       height = 6.5,
       device = "pdf")
