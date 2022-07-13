
#---------------------------------------------------------------
## Pacific Sardine ##

# ### Select data for estimation, replace N/A landings to zero #
# 
# dataset_psdn <- dataset %>% 
#   dplyr::select(VESSEL_NUM, PSDN_SDM_60, PSDN_Landings, PSDN_Price, MSQD_Price,
#                 PORT_AREA_ID, LANDING_YEAR, MSQD_SDM_90, MSQD_SPAWN_SDM_90, group) %>%
#   filter(LANDING_YEAR >= 2000) %>% 
#   dplyr::mutate(PSDN_Landings = coalesce(PSDN_Landings, 0)) %>% #filter(PSDN_Landings > 0) %>% 
#   mutate(PSDN_Landings = ifelse(PSDN_Landings<=0, 0 ,PSDN_Landings)) %>% 
#   mutate(Closure = ifelse(LANDING_YEAR >= 2015,1,0)) %>%
#   mutate(RelPrice = PSDN_Price / MSQD_Price) %>% drop_na() 
# dataset_psdn$port_ID <- as.factor(
#   udpipe::unique_identifier(dataset_psdn, fields = "PORT_AREA_ID", start_from = 1))
# dataset_psdn$cluster <- as.factor(
#   udpipe::unique_identifier(dataset_psdn, fields = "group", start_from = 1))
# # dataset_psdn_port_names <- dataset_psdn %>% dplyr::select(PORT_AREA_CODE, port_ID) %>% unique()
# 
# 
# fit_qPSDN <- brm(bf(PSDN_Landings ~ PSDN_SDM_60 + MSQD_SPAWN_SDM_90 + (1 | cluster) + (1 | port_ID), 
#                     hu ~ (1 | cluster) + (1 | port_ID)),
#                  data = dataset_psdn, 
#                  prior = c(set_prior("cauchy(0,2)", class = "sd")),
#                  family = hurdle_gamma(), 
#                  chains = 4, cores = 4, warmup = "1000", iter = "2000",
#                  control = list(adapt_delta = 0.95))
# 
# plot(fit_qPSDN)
# plot(fit_qPSDN, pars = c("PSDN_SDM_60"))
# coef(fit_qPSDN)
# 
#
#------------------------------------------------------------------------ 
# Results


### Own species distribution effect
# 
# ```{r by_port_sdm, eval=FALSE, fig.cap=, include=FALSE}
# # PSDN plots
# conditions <- data.frame(port_ID = unique(est_data_psdn$port_ID))
# rownames(conditions) <- unique(est_data_psdn$Port)
# conditions_psdn <- conditions %>% 
#   rownames_to_column('port_name') %>%
#   filter(port_ID == 1 | port_ID == 2 | port_ID == 3) %>%
#   column_to_rownames('port_name')
# 
# c_eff_psdn <- (conditional_effects
#                (fit_qPSDN_price, "PSDN_SDM_60", surface=TRUE, conditions = conditions_psdn, re_formula = NULL))
# #, transform = log, method = "posterior_predict"))
# g1 <- plot(c_eff_psdn, plot = FALSE, nrow = 3, ncol = 1)[[1]] + ggtitle('(a) Pacific sardine')+ 
#   theme(plot.title = element_text(size=9, face="bold.italic"), 
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
#   scale_x_continuous(name = element_blank()) +
#   scale_y_continuous(name = "Landings (tons)")
# 

# 
# # NANC plots
# conditions3 <- data.frame(port_ID = unique(est_data_nanc$port_ID))
# rownames(conditions3) <- unique(est_data_nanc$Port)
# conditions_nanc <- conditions3 %>% 
#   rownames_to_column('port_name') %>%
#   filter(port_ID == 3  | port_ID == 4  | port_ID == 5) %>%
#   column_to_rownames('port_name')
# c_eff_nanc <- (conditional_effects
#                (fit_qNANC_price, "NANC_SDM_20", surface=TRUE, conditions = conditions_nanc, re_formula = NULL))
# #, transform = log, method = "posterior_predict"))
# g3 <- plot(c_eff_nanc, plot = FALSE, nrow = 3, ncol = 1)[[1]] + ggtitle('(c) Northern anchovy') + 
#   theme(plot.title = element_text(size=9, face="bold.italic"), 
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
#   scale_x_continuous(name = element_blank()) +
#   scale_y_continuous(name = element_blank()) 
# 
# # Merge plots
# g1 + g2 + g3


#--------------------------------------------------------- 
## Interaction effects
# 
# ```{r int_effect_PSDN_by_port, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE}
# c_eff_int_psdn_msqd <- (conditional_effects(fit_qPSDN_price, "PSDN_SDM_60:MSQD_SDM_90", surface=TRUE, 
#                                             conditions = conditions_psdn, re_formula = NULL))
# c_eff_int_psdn_nanc <- (conditional_effects(fit_qPSDN_price, "PSDN_SDM_60:NANC_SDM_20", surface=TRUE, 
#                                             conditions = conditions_psdn, re_formula = NULL))
# c_eff_int_msqd_nanc <- (conditional_effects(fit_qPSDN_price, "MSQD_SDM_90:NANC_SDM_20", surface=TRUE, 
#                                             conditions = conditions_psdn, re_formula = NULL))
# 
# g1_PSDN <-  plot(c_eff_int_psdn_msqd, plot = FALSE)[[1]] + ggtitle('(a) Pacific sardine x Market squid') + 
#   theme(
#     plot.title = element_text(size=9, face="bold.italic"), 
#     axis.text = element_text(size = 7), 
#     axis.title = element_text(size = 8),
#     legend.title = element_text(size = 9), 
#     legend.text = element_text(size=8)
#   ) + guides(colour=guide_legend(title="Landings: PSDN")) +
#   scale_x_continuous(name = "P(Pres): PSDN") + scale_y_continuous(name = "P(Pres): MSQD") 
# 
# g2_PSDN <-  plot(c_eff_int_psdn_nanc, plot = FALSE)[[1]] + ggtitle('(b) Pacific sardine x Northern anchovy') + 
#   theme(
#     plot.title = element_text(size=9, face="bold.italic"), 
#     axis.text = element_text(size = 7), 
#     axis.title = element_text(size = 8),
#     legend.title = element_text(size = 9), 
#     legend.text = element_text(size=8)
#   ) + guides(colour=guide_legend(title="Landings: PSDN")) + 
#   scale_x_continuous(name = "P(Pres): PSDN") + scale_y_continuous(name = "P(Pres): NANC") 
# 
# g3_PSDN <-  plot(c_eff_int_msqd_nanc, plot = FALSE)[[1]] + ggtitle('(c) Northern anchovy x Market squid') + 
#   theme(
#     plot.title = element_text(size=9, face="bold.italic"), 
#     axis.text = element_text(size = 7), 
#     axis.title = element_text(size = 8),
#     legend.title = element_text(size = 9), 
#     legend.text = element_text(size=8)
#   ) + guides(colour=guide_legend(title="Landings: PSDN")) + 
#   scale_x_continuous(name = "P(Pres): MSQD") + scale_y_continuous(name = "P(Pres): NANC") 
# save.image (file = "stan_fit.RData")
# ```
# 
# ```{r int_effect_PSDN_by_port_PLOT, eval=FALSE, fig.cap=, message=FALSE, warning=FALSE, include=FALSE}
# g1_PSDN / g2_PSDN
# ```
# 
# ```{r int_effect_MSQD_by_port, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE}
# c_eff_int_msqd_psdn <- (conditional_effects(fit_qMSQD_price, "MSQD_SDM_90:PSDN_SDM_60_dOpen", surface=TRUE, 
#                                             conditions = conditions_msqd, re_formula = NULL))
# c_eff_int_msqd_nanc <- (conditional_effects(fit_qMSQD_price, "MSQD_SDM_90:NANC_SDM_20", surface=TRUE, 
#                                             conditions = conditions_msqd, re_formula = NULL))
# c_eff_int_psdn_nanc <- (conditional_effects(fit_qMSQD_price, "PSDN_SDM_60_dOpen:NANC_SDM_20", surface=TRUE, 
#                                             conditions = conditions_msqd, re_formula = NULL))
# 
# g1_MSQD <-  plot(c_eff_int_msqd_psdn, plot = FALSE)[[1]] + ggtitle('(a) Market squid x Pacific sardine') + 
#   theme(
#     plot.title = element_text(size=9, face="bold.italic"), 
#     axis.text = element_text(size = 7), 
#     axis.title = element_text(size = 8),
#     legend.title = element_text(size = 9), 
#     legend.text = element_text(size=8)
#   ) + guides(colour=guide_legend(title="Landings: MSQD")) +
#   scale_x_continuous(name = "P(Pres): MSQD") + scale_y_continuous(name = "P(Pres): PSDN") 
# 
# g2_MSQD <-  plot(c_eff_int_msqd_nanc, plot = FALSE)[[1]] + ggtitle('(b) Market squid x Northern anchovy') + 
#   theme(
#     plot.title = element_text(size=9, face="bold.italic"), 
#     axis.text = element_text(size = 7), 
#     axis.title = element_text(size = 8),
#     legend.title = element_text(size = 9), 
#     legend.text = element_text(size=8)
#   ) + guides(colour=guide_legend(title="Landings: MSQD")) + 
#   scale_x_continuous(name = "P(Pres): MSQD") + scale_y_continuous(name = "P(Pres): NANC") 
# 
# g3_MSQD <-  plot(c_eff_int_psdn_nanc, plot = FALSE)[[1]] + ggtitle('(c) Pacific sardine x Northern anchovy') + 
#   theme(
#     plot.title = element_text(size=9, face="bold.italic"), 
#     axis.text = element_text(size = 7), 
#     axis.title = element_text(size = 8),
#     legend.title = element_text(size = 9), 
#     legend.text = element_text(size=8)
#   ) + guides(colour=guide_legend(title="Landings: MSQD")) + 
#   scale_x_continuous(name = "P(Pres): PSDN") + scale_y_continuous(name = "P(Pres): NANC") 
# save.image (file = "stan_fit.RData")
# ```
# 
# ```{r int_effect_MSQD_by_port_PLOT, eval=FALSE, fig.cap=, message=FALSE, warning=FALSE, include=FALSE}
# g1_MSQD / g2_MSQD 
# ```
# 
# ```{r int_effect_NANC_by_port, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE}
# conditions_nanc <- conditions_nanc %>% filter(port_ID == 3  | port_ID == 5)
# 
# c_eff_int_nanc_psdn <- (conditional_effects(fit_qNANC_price, "NANC_SDM_20:PSDN_SDM_60_dOpen", surface=TRUE, 
#                                             conditions = conditions_nanc, re_formula = NULL))
# c_eff_int_nanc_msqd <- (conditional_effects(fit_qNANC_price, "NANC_SDM_20:MSQD_SDM_90", surface=TRUE, 
#                                             conditions = conditions_nanc, re_formula = NULL))
# c_eff_int_psdn_msqd <- (conditional_effects(fit_qNANC_price, "PSDN_SDM_60_dOpen:MSQD_SDM_90", surface=TRUE, 
#                                             conditions = conditions_nanc, re_formula = NULL))
# 
# g1_NANC <-  plot(c_eff_int_nanc_psdn, plot = FALSE)[[1]] + ggtitle('(a) Northern anchovy x Pacific sardine') + 
#   theme(
#     plot.title = element_text(size=9, face="bold.italic"), 
#     axis.text = element_text(size = 7), 
#     axis.title = element_text(size = 8),
#     legend.title = element_text(size = 9), 
#     legend.text = element_text(size=8)
#   ) + guides(colour=guide_legend(title="Landings: PSDN")) +
#   scale_x_continuous(name = "P(Pres): NANC") + scale_y_continuous(name = "P(Pres): PSDN") 
# 
# g2_NANC <-  plot(c_eff_int_nanc_msqd, plot = FALSE)[[1]] + ggtitle('(b) Northern anchovy x Market squid') + 
#   theme(
#     plot.title = element_text(size=9, face="bold.italic"), 
#     axis.text = element_text(size = 7), 
#     axis.title = element_text(size = 8),
#     legend.title = element_text(size = 9), 
#     legend.text = element_text(size=8)
#   ) + guides(colour=guide_legend(title="Landings: PSDN")) + 
#   scale_x_continuous(name = "P(Pres): NANC") + scale_y_continuous(name = "P(Pres): MSQD") 
# 
# g3_NANC <-  plot(c_eff_int_psdn_msqd, plot = FALSE)[[1]] + ggtitle('(c) Pacific sardine x Market squid') + 
#   theme(
#     plot.title = element_text(size=9, face="bold.italic"), 
#     axis.text = element_text(size = 7), 
#     axis.title = element_text(size = 8),
#     legend.title = element_text(size = 9), 
#     legend.text = element_text(size=8)
#   ) + guides(colour=guide_legend(title="Landings: PSDN")) + 
#   scale_x_continuous(name = "P(Pres): PSDN") + scale_y_continuous(name = "P(Pres): MSQD") 
# save.image (file = "stan_fit.RData")
# ```
# 
# ```{r int_effect_NANC_by_port_PLOT, eval=FALSE, fig.cap=, message=FALSE, warning=FALSE, include=FALSE}
# g1_NANC / g2_NANC 
# ```
# 
# ### Pacific sardine closure
# 
# ```{r by_port_msqd_dclose, eval=FALSE, fig.cap=, include=FALSE}
# # conditions_dClose <- data.frame(port_ID = unique(est_data_msqd$dClose))
# # rownames(conditions_dClose) <- unique(est_data_msqd$dClose)
# c_eff_close_msqd <- (conditional_effects(fit_qMSQD_price, "dClose", conditions = conditions_msqd, re_formula = NULL))
# g1 <-  plot(c_eff_close_msqd, plot = FALSE)[[1]] + ggtitle('(a) Market squid') + 
#   theme(plot.title = element_text(size=9, face="bold.italic"), 
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
#   scale_x_discrete(name = "") +
#   scale_y_continuous(name = "Landings (tons)") 
# 
# c_eff_close_nanc <- (conditional_effects(fit_qNANC_price, "dClose", conditions = conditions_nanc, re_formula = NULL))
# g2 <- plot(c_eff_close_nanc, plot = FALSE)[[1]] + ggtitle('(b) Northern anchovy') + 
#   theme(plot.title = element_text(size=9, face="bold.italic"), 
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
#   scale_x_discrete(name = "Closure? (1 = True; 0 = False)") +
#   scale_y_continuous(name = "Landings (tons)") 
# 
# g1 / g2
# ```
# 
# # OTHER CODE
# 
# <!-- ```{r int_effect_sep, eval=FALSE, include=FALSE} -->
#   
#   <!-- hu <- plot(conditional_effects(fit_qPSDN, "PSDN_SDM:MSQD_SDM", surface=TRUE, dpar="hu"), -->
#                     <!--            plot = FALSE, ask = FALSE) -->
#   <!-- mu <- plot(conditional_effects(fit_qPSDN, "PSDN_SDM:MSQD_SDM", surface=TRUE, dpar="mu"), -->
#                     <!--            plot = FALSE, ask = FALSE) -->
#   
#   <!-- mu[[1]] + hu[[1]] + plot_layout(nrow = 2) -->
#   
#   <!-- ``` -->
#   
#   <!-- ```{r int_effect_sep_msqd, eval=FALSE, fig.cap=, include=FALSE} -->
#   
#   <!-- hu <- plot(conditional_effects(fit_qMSQD_gamma, "PSDN_SDM:MSQD_SDM", surface=TRUE, dpar="hu"), -->
#                     <!--            plot = FALSE, ask = FALSE) -->
#   <!-- mu <- plot(conditional_effects(fit_qMSQD_gamma, "PSDN_SDM:MSQD_SDM", surface=TRUE, dpar="mu"), -->
#                     <!--            plot = FALSE, ask = FALSE) -->
#   
#   <!-- mu[[1]] + hu[[1]] + plot_layout(nrow = 2) -->
#   
