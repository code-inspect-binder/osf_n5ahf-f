# Figure 3 Raw
# HSP70-Plasma and Corticosterone

# Organize Data -------------
df_fig3 = df_inflam %>% 
  mutate(
  logHSP70PL = 100*log(HSP70PL),
  logCort = 100*log(Cort)
) 


# Panel A: HSP70 in Plasma -----------
Fig3A_supp = ggplot(df_fig3, aes(y = HSP70PL, x = Recovery2, 
                             color = Recovery2, group = Recovery2)) + 
  geom_jitter(alpha = .5,
              position = pos_1) +
    facet_wrap(. ~ SacTime2, ncol = 4,
               strip.position = "bottom") +
    scale_shape_manual(values = c(15,16,17,18))+
    scale_color_manual(values = c("black","red","green","blue")) +
    # scale_color_grey(start=0.1, end=0.7)  +
    labs(x='Recovery Time',
         y="HSP70-Plasma")+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank())


# Panel B: Corticosterone --------------
Fig3B_supp = ggplot(df_fig3, aes(y = Cort, x = Recovery2, 
                            color = Recovery2, group = Recovery2)) + 
  geom_jitter(alpha = .5, 
              position = pos_1) +
  facet_wrap(. ~ SacTime2, ncol = 4,
             strip.position = "bottom") +
  scale_shape_manual(values = c(15,16,17,18))+
  scale_color_manual(values = c("black","red","green","blue")) +
  labs(x='Recovery Time',
       y="Corticosterone")+ 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.title = element_blank()) 
# Figure 3 -------------
Fig3_supp <- ggarrange(Fig3A_supp,
                       Fig3B_supp, 
                       labels = c("A","B"),
                  common.legend = TRUE)

ggsave(plot = Fig3_supp, "Fig3_supp.pdf",
       width = 8.25,
       height = 5,
       device = "pdf")
