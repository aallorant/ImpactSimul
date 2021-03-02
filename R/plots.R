#' Module to produce graphical outputs to visualize simulations' results
#'
#' @param res object with simulations results (deaths, new infections, # patients off and on treatment, # of lost to follow-up)
#'  from both the baseline and intervention scenarios
#' @param h time horizon (in years) at which the results of the study need to be displayed
#' default = 3 years
#' @param daly object with simulations results in terms of DALYs from both the baseline and intervention scenarios
#' @return 
#' This function returns summary plots of the simulations' results. 
#' 
library(ggplot2)
library(data.table)
library(extrafont)
library(viridis)

result_comparison_plot <- function(res,h=3){
  
  tmp <- suppressWarnings(setDT(res) %>%
                            data.table::melt(id.vars = c("time","scenario","simul"),
                                             value.name = "outcome"))

  p1 <- tmp[(variable != "Death" & variable != "New infection" & variable != "simul"),
            list(mean = mean(outcome, na.rm = T),
                 `2.5%` = quantile(outcome, probs = .025,na.rm=T),
                 `97.5%` = quantile(outcome, probs = .975,na.rm=T)),by = 'time,scenario,variable']
  
  p2 <- tmp[(variable == "Death" | variable == "New infection"),
            list(sum = sum(outcome, na.rm = T)),by = 'scenario,variable,simul']
  
  gg1 <- ggplot(data = p1, aes(x = time, y = mean, group = scenario)) +
    geom_line(aes(colour = scenario)) +
    geom_ribbon(data = p1, aes(ymin= `2.5%`, ymax=`97.5%`, fill = scenario), linetype=2, alpha=0.1) +
    facet_wrap(~variable, scales = "free") +
    labs(x ='Week', y = 'Outcome',
         title = paste0("Simulation results")) +
    theme_bw() +
    scale_fill_manual(values = viridis(length(unique(tmp$scenario)))) +
    scale_color_manual(values=viridis(length(unique(tmp$scenario))))+
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, NA))+
    guides(fill=FALSE) +
    theme(strip.background = element_rect(color="white", fill="white", size=1.5, linetype="solid"),
          strip.text = element_text(size = 10),
          panel.grid.major = element_blank(), panel.grid.minor  = element_blank(),
          axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5, size = 14)) +
    geom_vline(xintercept = h*52, linetype = "dotted")
  
  gg2 <- ggplot(data = p2,
                aes(x=sum, fill=scenario)) + geom_density(alpha=.3)+
    facet_wrap(~variable, scales = "free") +
    theme_bw() +
    scale_fill_manual(values = viridis(length(unique(tmp$scenario)))) +
    scale_color_manual(values=viridis(length(unique(tmp$scenario))))+
    labs(x ='Outcome', y = 'Probability') +
    theme(strip.background = element_rect(color="white", fill="white", size=1.5, linetype="solid"),
          strip.text = element_text(size = 10),
          panel.grid.major = element_blank(), panel.grid.minor  = element_blank(),
          axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5, size = 14))
  
  gg <- gridExtra::grid.arrange(gg1,gg2)
  return(gg)
}

DALY_comparison <- function(daly){
  
  ggplot(daly, aes(x=dalys, fill=scenario)) + geom_density(alpha=.3)+
    theme_bw() +
    scale_fill_manual(values = viridis(length(unique(tmp$scenario)))) +
    scale_color_manual(values=viridis(length(unique(tmp$scenario))))+
    labs(x ='DALYs', y = 'Probability',
         title = paste0("Simulation results")) +
    theme(strip.background = element_rect(color="white", fill="white", size=1.5, linetype="solid"),
          strip.text = element_text(size = 10),
          panel.grid.major = element_blank(), panel.grid.minor  = element_blank(),
          axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5, size = 14))
  
}
