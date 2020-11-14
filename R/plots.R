#' Module to produce graphical outputs to visualize simulations' results
#'
#' @param res object with simulations results (deaths, new infections, # patients off and on treatment, # of lost to follow-up)
#'  from both the baseline and intervention scenarios
#' @param daly object with simulations results in terms of DALYs from both the baseline and intervention scenarios
#' @return 
#' This function returns summary plots of the simulations' results. 
#' 
library(ggplot2)
library(data.table)
library(extrafont)

result_comparison_plot <- function(res){
  p <- suppressWarnings(res %>%
    data.table() %>%
    melt.data.table(id.vars = c("time","scenario")) %>%
    group_by(time,scenario, variable) %>%
    summarize(mean = mean(value),
              q2.5 = quantile(value, probs = .025),
              q97.5 = quantile(value, probs = .975)))
  p1 <- p %>% filter(variable != "Death" & variable != "New infection")
  p2 <- suppressWarnings(res %>%
                           data.table() %>%
                           melt.data.table(id.vars = c("time","scenario","simul"))) %>%
                          filter(variable == "Death" | variable == "New infection") %>%
    group_by(scenario, variable,simul) %>%
    summarize(deaths_cum = sum(value))
  
  gg1 <- ggplot(data = p1, aes(x = time, y = mean, group = scenario)) +
    geom_line(aes(colour = scenario)) +
    geom_ribbon(data = p1, aes(ymin= q2.5, ymax=q97.5, fill = scenario), linetype=2, alpha=0.1) +
    facet_wrap(~variable, scales = "free") +
    labs(x ='Time', y = 'Outcome',
         title = paste0("Simulation results")) +
    theme_bw() +
    scale_fill_manual(values = c("#0d4e93", "#ffdc00")) +
    scale_color_manual(values=c("#0d4e93", "#ffdc00"))+
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, NA))+
    guides(fill=FALSE) +
    theme(strip.background = element_rect(color="white", fill="white", size=1.5, linetype="solid"),
          strip.text = element_text(size = 10),
          panel.grid.major = element_blank(), panel.grid.minor  = element_blank(),
          axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5, size = 14))
  
  gg2 <- ggplot(p2, aes(x=deaths_cum, fill=scenario)) + geom_density(alpha=.3)+
    facet_wrap(~variable, scales = "free") +
    theme_bw() +
    scale_fill_manual(values = c("#0d4e93", "#ffdc00")) +
    scale_color_manual(values=c("#0d4e93", "#ffdc00")) +
    labs(x ='Value', y = 'Frequency') +
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
    scale_fill_manual(values = c("#0d4e93", "#ffdc00")) +
    scale_color_manual(values=c("#0d4e93", "#ffdc00")) +
    labs(x ='DALYs', y = '',
         title = paste0("Simulation results")) +
    theme(strip.background = element_rect(color="white", fill="white", size=1.5, linetype="solid"),
          strip.text = element_text(size = 10),
          panel.grid.major = element_blank(), panel.grid.minor  = element_blank(),
          axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5, size = 14))
  
}
