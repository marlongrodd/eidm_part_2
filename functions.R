
#---------------
#  Function 1.
#---------------

trans_data <- function(data_FD,
                       inf            = "All",
                       sex            = "All",
                       age            = c(0, 99)) {
  
  # ---------------------------- cut the dataset ---------------------------------
  
  
  if(any(data_FD$admission_date > data_FD$intermediate_date, na.rm = T)) {
    
    data_FD = data_FD[-which(data_FD$admission_date > data_FD$intermediate_date),]
    
  }
  
  if(any(data_FD$admission_date > data_FD$discharge_date, na.rm = T)) {
    
    data_FD = data_FD[-which(data_FD$admission_date > data_FD$discharge_date),]
    
  }
  
  if(any(data_FD$intermediate_date > data_FD$discharge_date, na.rm = T)) {
    
    data_FD = data_FD[-which(data_FD$intermediate_date > data_FD$discharge_date),]
    
  }
  
  # ---------------------------- calc duration ---------------------------------
  
  data_FD$admission_date    <- as.Date(data_FD$admission_date)
  data_FD$intermediate_date <- as.Date(data_FD$intermediate_date)
  data_FD$discharge_date    <- as.Date(data_FD$discharge_date)
  
  data_FD$time_to_inter <- as.numeric(data_FD$intermediate_date - data_FD$admission_date)
  data_FD$time_to_disch <- as.numeric(data_FD$discharge_date - data_FD$admission_date)
  
  data_FD <- data_FD[, c("time_to_inter",
                         "time_to_disch",
                         "discharge_status",
                         "pathogen",
                         "age",
                         "sex")]
  
  data_FD$id <- 1:nrow(data_FD)
  
  # ---------------------------- subgroups ---------------------------------------
  
  # sex
  if(sex != "All") {
    
    # ("All", "Female", "Male", "Non-Binary")
    
    # if(sex == "f") {
    #   sex <- "f"
    # }else if(sex == "m") {
    #   sex <- "m"
    # }else {
    #   sex <- "d"
    # }
    
    data_FD <- data_FD[which(data_FD$sex == sex), ]
  }
  
  # age
  if(age[1] != age[2]) {
    data_FD <- data_FD[which(data_FD$age >= age[1] & data_FD$age <= age[2]), ]
  }else {
    data_FD$age <- rep(999, nrow(data_FD))
  }
  
  # ---------------------------- long format ---------------------------------------
  
  if(inf == "All") {
    
    data_trans <- data.frame(id   = data_FD$id,
                             from = 1,
                             to   = ifelse(!is.na(data_FD$time_to_inter),
                                           2,
                                           ifelse(data_FD$discharge_status == "d",
                                                  4,
                                                  3)),
                             time = ifelse(!is.na(data_FD$time_to_inter),
                                           data_FD$time_to_inter,
                                           data_FD$time_to_disch))
    
    acc <- data.frame(id   = data_FD[!is.na(data_FD$time_to_inter), "id"],
                      from = 2,
                      to   = ifelse(data_FD[!is.na(data_FD$time_to_inter), "discharge_status"] == "d",
                                    6,
                                    5),
                      time = data_FD[!is.na(data_FD$time_to_inter), "time_to_disch"])
    
    names(acc) <- names(data_trans)
    
    data_trans <- rbind(data_trans, acc)
    
  }else {

    data_trans <- data.frame(id   = data_FD$id,
                             from = 1,
                             to   = ifelse(!is.na(data_FD$time_to_inter) & (data_FD$pathogen == inf) & !is.na(data_FD$pathogen),
                                           2,
                                           ifelse(data_FD$discharge_status == "d",
                                                  4,
                                                  3)),
                             time = ifelse(!is.na(data_FD$time_to_inter) & (data_FD$pathogen == inf) & !is.na(data_FD$pathogen),
                                           data_FD$time_to_inter,
                                           data_FD$time_to_disch))

    acc <- data.frame(id   = data_FD[which(!is.na(data_FD$time_to_inter) & (data_FD$pathogen == inf) & !is.na(data_FD$id)), "id"],
                      from = rep(2, sum(!is.na(data_FD$time_to_inter) & (data_FD$pathogen == inf), na.rm = T)),
                      to   = ifelse(data_FD[which(!is.na(data_FD$time_to_inter) & (data_FD$pathogen == inf)), "discharge_status"] == "d",
                                    rep(6, sum(!is.na(data_FD$time_to_inter) & (data_FD$pathogen == inf), na.rm = T)),
                                    rep(5, sum(!is.na(data_FD$time_to_inter) & (data_FD$pathogen == inf), na.rm = T))),
                      time = data_FD[which(!is.na(data_FD$time_to_inter) & (data_FD$pathogen == inf)), "time_to_disch"])
    
    names(acc) <- names(data_trans)

    data_trans <- rbind(data_trans, acc)

  }

  # ---------------------------- fixing for etm ---------------------------------------

  # etm doesn't like to start at time 0
  if(any(data_trans$time == 0)) {

    data_trans[data_trans$time == 0, "time"] <- 0.5

  }

  # etm doesn't like double transisions
  data_trans <- data_trans[order(data_trans$id, data_trans$time), ]
  acc        <- data_trans[which(data_trans$to == 2), "id"]
  acc        <- data_trans[which(data_trans$id %in% acc), ]
  acc        <- acc %>% group_by(id) %>% summarise(test = diff(time))

  if(any(acc$test <= 0)) {

    acc <- acc[which(acc$test <= 0), ]$id
    data_trans[(data_trans$id %in% acc) & data_trans$from == 2, "time"] <- data_trans[(data_trans$id %in% acc) & data_trans$from == 2, "time"] + 1

  }

  # etm doesn't like transitions at time 0
  data_trans[data_trans$time == 0, "time"] <- data_trans[data_trans$time == 0, "time"] + 0.5

  # ---------------------------- covariates ---------------------------------------

  data_cov <- unique(data_FD[, c("id", "sex", "age")])


  data_trans <- merge(data_trans,
                      data_cov,
                      by.x = "id",
                      by.y = "id",
                      all.x = T)

  return(data_trans)
}

plot_trans <- function(data_trans,
                       color_scheme,
                       ord,
                       time_max = "last") {
  
  acc <- unique(data_trans[, c("from", "to")])
  
  tra <- matrix(ncol = 6, nrow = 6, FALSE)
  
  for (i in 1:nrow(acc)) {
    tra[acc[i, "from"], acc[i, "to"]] <-  T
  }
  
  tra.prob <- etm(data_trans, c("1", "2", "3", "4", "5", "6"), tra, "cens", 0, time_max)

  p <- c(1, 0, 0, 0, 0, 0)

  p_time <- data.frame(t = 0,
                       p,
                       name = c("Admitted",
                                "Intermediate",
                                "Discharge w/o Int.",
                                "Deceased w/o Int.",
                                "Discharge w/ Int.",
                                "Deceased w/ Int."))

  for (i in 1:dim(tra.prob$est)[3]) {
    acc <- data.frame(t = tra.prob$time[i],
                      p = (p %*% tra.prob$est[, , i])[1, ],
                      name = c("Admitted",
                               "Intermediate",
                               "Discharge w/o Int.",
                               "Deceased w/o Int.",
                               "Discharge w/ Int.",
                               "Deceased w/ Int."))

    p_time <- rbind(p_time, acc)
  }

  p_time <- p_time[order(p_time$name, p_time$t), ]

  p_time$name <- as.factor(p_time$name)

  p_time$name <- factor(p_time$name,
                        levels = ord)

  pl_2 <- ggplot(p_time,
                 aes(x    = t,
                     y    = p,
                     fill = name)) +

    labs(x = "\n Time from Admission",
         y = "Probability \n") +

    scale_fill_manual(values = color_scheme) +

    geom_area(alpha = 0.8) +
    theme_classic()


  acc <- data_trans[data_trans$time <= time_max, ] %>% group_by(from, to) %>% summarise(n = length(from))
  test <- as.numeric(c(n_distinct(data_trans$id) - sum(acc[acc$from == 1, "n"]),
                       acc[acc$to == 2, "n"] - sum(acc[acc$from == 2, "n"]),
                       sum(acc[acc$to == 3, "n"]),
                       sum(acc[acc$to == 4, "n"]),
                       sum(acc[acc$to == 5, "n"]),
                       sum(acc[acc$to == 6, "n"])))

  table_trans <- data.frame(State = c("Admitted",
                                      "Intermediate",
                                      "Discharge w/o Int.",
                                      "Deceased w/o Int.",
                                      "Discharge w/ Int.",
                                      "Deceased w/ Int."),
                            perc. = round(p %*% tra.prob$est[, , dim(tra.prob$est)[3]] * 100, 2)[1, ],
                            n     = test)

  return(list(pl_2, table_trans))
}

plot_trans_2 <- function(data_trans,
                         color_scheme,
                         ord,
                         time_max = "last") {
  
  acc        <- data_trans
  acc        <- acc[acc$id %in% acc[acc$to == 2, ]$id, ]
  acc        <- acc[order(acc$id, acc$from), ]
  acc_1      <- acc[acc$from == 2, ]
  acc_1$time <- acc[acc$from == 2, ]$time - acc[acc$from == 1, ]$time
  
  tra <- matrix(ncol = 3, nrow = 3, FALSE)
  
  acc <- unique(acc_1[, c("from", "to")])
  
  if(5 %in% acc$to) {
    tra[1, 2] <- T
  }
  
  if(6 %in% acc$to) {
    tra[1, 3] <- T
  }
  
  tra.prob <- etm(acc_1, c("2", "5", "6"), tra, "cens", 0, time_max)
  
  # return(list(0, tra.prob$trans))
  
  p <- c(1, 0, 0)

  p_time <- data.frame(t = 0,
                       p,
                       name = c("Intermediate",
                                "Discharge w/ Int.",
                                "Deceased w/ Int."))

  for (i in 1:dim(tra.prob$est)[3]) {
    acc <- data.frame(t = tra.prob$time[i],
                      p = (p %*% tra.prob$est[, , i])[1, ],
                      name = c("Intermediate",
                               "Discharge w/ Int.",
                               "Deceased w/ Int."))

    p_time <- rbind(p_time, acc)
  }

  p_time <- rbind(p_time, data.frame(t = unique(p_time$t),
                                     p = 0,
                                     name = "Discharge w/o Int."))

  p_time <- rbind(p_time, data.frame(t = unique(p_time$t),
                                     p = 0,
                                     name = "Admitted"))

  p_time <- rbind(p_time, data.frame(t = unique(p_time$t),
                                     p = 0,
                                     name = "Deceased w/o Int."))

  p_time <- p_time[order(p_time$name, p_time$t), ]

  p_time$name <- as.factor(p_time$name)

  p_time$name <- factor(p_time$name,
                        levels = ord)

  pl_2 <- ggplot(p_time,
                 aes(x    = t,
                     y    = p,
                     fill = name)) +

    labs(x = "\n Time from Admission",
         y = "Probability \n") +

    scale_fill_manual(values = color_scheme) +

    geom_area(alpha = 0.8) +
    theme_classic()


  table_trans <- data.frame(State = c("Admitted",
                                      "Discharge w/ Int.",
                                      "Deceased w/ Int."),
                            perc. = round(tra.prob$est[1, , dim(tra.prob$est)[3]] * 100, 2))

  rownames(table_trans) <- NULL

  return(list(pl_2, table_trans))
}

circular_plot <- function(data_FD,
                          text_size = 4) {
  
  acc    <- data_FD %>% group_by(pathogen) %>% summarise(value = length(pathogen))
  acc    <- acc[which(!is.na(acc$pathogen)), ]
  acc    <- acc[order(acc$value), ]
  acc$id <- seq(1, nrow(acc))
  
  # Get the name and the y position of each label
  label_data       <- acc
  number_of_bar    <- nrow(label_data)
  angle            <- 90 - 360 * (label_data$id - 0.5) / number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse( angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle + 180, angle)
  
  label_data       <- label_data[!is.na(label_data$pathogen), ]
  
  # Make the plot
  p <- ggplot(acc,
         aes(x    = as.factor(id),
             y    = value,
             fill = pathogen)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    
    geom_bar(stat  = "identity",
             alpha = 0.5) +
    
    ylim(-15, 50) +
    theme_minimal() +
    coord_polar() + 
    
    theme(
      legend.position = "none",
      axis.text       = element_blank(),
      axis.title      = element_blank(),
      panel.grid      = element_blank(),
      plot.margin     = unit(rep(-1, 4), "cm")) +
    
    geom_text(data = label_data,
              aes(x     = id,
                  y     = value + 5,
                  label = pathogen,
                  hjust = hjust),
              color       = "black",
              # fontface    = "bold",
              alpha       = 0.6,
              size        = text_size,
              angle       = label_data$angle,
              inherit.aes = FALSE)
  
  return(p)
  
}

data_cox <- function(data_trans) {
  
  data_cox <- data_trans
  data_cox$inf   <- as.numeric(data_cox$from == 2)
  data_cox$event <- as.numeric(data_cox$to != 2)
  
  data_cox$stop  <- data_cox$time
  data_cox$start <- 0
  data_cox[data_cox$from == 2, "start"] <- data_cox[data_cox$to == 2, "stop"]
  
  data_cox[data_cox$to == 6, "to"] <- 4
  data_cox[data_cox$to == 5, "to"] <- 3
  
  data_cox$sex <- as.factor(data_cox$sex)

  return(data_cox)
}

model_cox <- function(data_cox,
                      outcome) {
  
  data_cox_1 <- data_cox
  data_cox_1[data_cox_1$to != outcome, "event"] <- 0
  
  form <- "Surv(start, stop, event) ~ inf"
  
  if(n_distinct(data_cox_1$sex, na.rm = T) > 1) {
    form <- paste0(form, " + sex")
  }
  
  if(n_distinct(data_cox_1$age, na.rm = T) > 1) {
    form <- paste0(form, " + age")
  }
  
  form <- paste0(form, " + cluster(id)")
  
  form <- as.formula(form)
  
  cox_disch <- coxph(form,
                     data = data_cox_1)
  
  return(cox_disch)
  
  
}

forest_cox_data_line <- function(mdl, name, infl = 1) {
  
  test <- data.frame(Factor = name)
  
  test$est <- summary(mdl)$coefficients[infl, 2]
  test$low <- exp(confint(mdl))[infl, 1]
  test$hi <- exp(confint(mdl))[infl, 2]
  test$se <- exp(summary(mdl)$coefficients[infl, 3])
  
  test$` ` <- paste(rep(" ", 20), collapse = " ")
  
  test$`HR (95% CI)` = paste0(round(summary(mdl)$coefficients[infl, 2], 2), " (", paste0(round(exp(confint(mdl)[infl, ]), 2), collapse = " to "), ")")
  
  test$se <- test$se / 4
  
  return(test)
  
}

forest_cox <- function(mdl) {
  cox_forest <- forest_cox_data_line(mdl,
                                     "Intermediate",
                                     infl = 1)
  
  if(all(c("a", "g", "e") %in% strsplit(as.character(mdl$formula)[3], "")[[1]])) {
    
    cox_forest <- rbind(cox_forest,
                        forest_cox_data_line(mdl,
                                             "Age",
                                             infl = 3))
    
  }
  
  if(all(c("s", "e", "x") %in% strsplit(as.character(mdl$formula)[3], "")[[1]])) {
    
    cox_forest <- rbind(cox_forest,
                        forest_cox_data_line(mdl,
                                             "Sex (F)",
                                             infl = 2))
    
  }
  
  
  ticks <- round(c(1 - min(abs(c(mean(c(1, min(cox_forest$low))), mean(c(1, max(cox_forest$hi)))) - 1)),
                   1 + min(abs(c(mean(c(1, min(cox_forest$low))), mean(c(1, max(cox_forest$hi)))) - 1))),
                 ceiling(abs(log10(min(abs(c(mean(c(1, min(cox_forest$low))), mean(c(1, max(cox_forest$hi)))) - 1))))))
  
  if((ticks[1] < 1) & (ticks[2] > 1)) {
    ticks <- c(ticks, 1)
  }

  cox_forest$low <- ifelse(is.infinite(cox_forest$hi),
                          0,
                          cox_forest$low)

  cox_forest$hi <- ifelse(is.infinite(cox_forest$hi),
                          0,
                          cox_forest$hi)

  cox_forest$se <- ifelse(is.infinite(cox_forest$se),
                          0.00001,
                          cox_forest$se)

  p <- forest(cox_forest[, c(1, 6:7)],
              est = cox_forest$est,
              lower = cox_forest$low,
              upper = cox_forest$hi,
              sizes = cox_forest$se,
              ci_column = 2,
              ref_line = 1,
              xlim = c(min(cox_forest$low), max(cox_forest$hi)),
              ticks_at = c(1, ticks))

  return(p)
}

boot.clos <- function(data, state.names, tra, cens.name, s = 0, nboot) {
  res <- double(nboot)
  set.seed(2025)
  for (i in seq_len(nboot)) {
    index <- sample(unique(data$id), replace = TRUE)
    inds <- new.id <- NULL
    for (j in seq_along(index)){
      ind <- which(data$id == index[j])
      new.id <- c(new.id, rep(j, length(ind)))
      inds <- c(inds, ind)
    }
    dboot <- cbind(data[inds, ], new.id)
    dboot[, which(names(dboot) == "id")]
    dboot$id <- dboot$new.id
    tr.prob <- etm(dboot, state.names, tra, cens.name, s, cova = FALSE)
    res[i] <- etm::clos(tr.prob)$e.phi
  }
  res
}

clos_tab <- function(data_trans) {
  
  data_trans[data_trans$to == 5, "to"] <- 3
  data_trans[data_trans$to == 6, "to"] <- 4
  
  
  acc <- unique(data_trans[, c("from", "to")])
  
  tra <- matrix(ncol = 4, nrow = 4, FALSE)
  
  for (i in 1:nrow(acc)) {
    tra[acc[i, "from"], acc[i, "to"]] <-  T
  }
  
  
  tr.prob <- etm(data_trans, c("1","2","3","4"), tra, NULL, 0)
  
  mdl_cLOS <- clos(tr.prob)$e.phi[1, 1]
  
  acc <- sd(boot.clos(data_trans, c("1","2","3","4"), tra, NULL, 0,
                   nboot = 20))
  
  acc <- c(mdl_cLOS - qnorm(0.975) * acc, mdl_cLOS + qnorm(0.975) * acc)
  
  mdl_cLOS_ci <- paste0(c("(", paste0(round(acc, 2), collapse = "; "), ")"), collapse = "")
  
  
  ret_table <- data.frame(mdl_cLOS, mdl_cLOS_ci)
  names(ret_table) <- c("change in length of stay in days", "Conf. Interval")
  
  return(ret_table)
  
}

render_table_1 <- function(data_FD,
                           inf            = "All",
                           sex            = "All",
                           age            = c(0, 99)) {
  
  
  # ---------------------------- cut the dataset ---------------------------------
  
  
  if(any(data_FD$admission_date > data_FD$intermediate_date, na.rm = T)) {
    
    data_FD = data_FD[-which(data_FD$admission_date > data_FD$intermediate_date),]
    
  }
  
  if(any(data_FD$admission_date > data_FD$discharge_date, na.rm = T)) {
    
    data_FD = data_FD[-which(data_FD$admission_date > data_FD$discharge_date),]
    
  }
  
  if(any(data_FD$intermediate_date > data_FD$discharge_date, na.rm = T)) {
    
    data_FD = data_FD[-which(data_FD$intermediate_date > data_FD$discharge_date),]
    
  }
  
  # ---------------------------- calc duration ---------------------------------
  
  data_FD$admission_date    <- as.Date(data_FD$admission_date)
  data_FD$intermediate_date <- as.Date(data_FD$intermediate_date)
  data_FD$discharge_date    <- as.Date(data_FD$discharge_date)
  
  data_FD$time_to_inter <- as.numeric(data_FD$intermediate_date - data_FD$admission_date)
  data_FD$time_to_disch <- as.numeric(data_FD$discharge_date - data_FD$admission_date)
  
  data_FD <- data_FD[, c("id",
                         "time_to_inter",
                         "time_to_disch",
                         "discharge_status",
                         "pathogen",
                         "age",
                         "sex")]
  
  # ---------------------------- subgroups ---------------------------------------
  
  acc <- data_FD
  
  acc$nos_inf <- ifelse(is.na(acc$time_to_inter),
                        " w/o Intermediate",
                        " w/ Intermediate")
  
  

  acc <- acc[, c("nos_inf", "sex", "age")]

  names(acc) <- c("nos_inf", "Sex", "Age")

  return(table1(~ Sex + Age | nos_inf, data = acc))
  
  
  
}




check_func <- function(data_trans) {
  
  
  check_1 <- F
  check_2 <- F
  
  check_1 <- n_distinct(data_trans$to) < 5
  
  if(!check_1) {

    check_2 <- any(data_trans$age >= 900, na.rm = T)

  }

  check <- any(c(check_1,
                 check_2))
  
  return(check)
}


check_raw_func <- function(data_FD) {
  
  check_1 <- F
  check_2 <- F
  check_3 <- F
  check_4 <- F
  check_5 <- F
  check_6 <- F
  check_7 <- F
  
  check_1 <- any(!(c("admission_date",
                   "intermediate_date",
                   "discharge_date",
                   "discharge_status",
                   "pathogen",
                   "age",
                   "sex") %in% names(data_FD)))
  
  
  if(!check_1) {
    
    check_2 <- any(c(inherits(try(as.Date(data_FD$admission_date)), "try-error"),
                     inherits(try(as.Date(data_FD$intermediate_date)), "try-error"),
                     inherits(try(as.Date(data_FD$discharge_date)), "try-error")))
    
    if(!check_2) {
      
      check_3 <- any(c(as.Date(data_FD$admission_date) > as.Date(data_FD$intermediate_date),
                       as.Date(data_FD$admission_date) > as.Date(data_FD$discharge_date),
                       as.Date(data_FD$intermediate_date) > as.Date(data_FD$discharge_date)),
                     na.rm = T)
      
      if(!check_3) {
        
        check_4 <- any(c(is.na(data_FD$admission_date),
                         is.na(data_FD$discharge_date),
                         is.na(data_FD$discharge_status)))
        
        if(!check_4) {
          
          acc <- data_FD[, c("intermediate_date", "discharge_status")]
          acc$intermediate_date <- !is.na(acc$intermediate_date)
          acc <- unique(acc)
          check_5 <- nrow(acc) < 4
          
          if(!check_5) {
            
            check_6 <- !is.numeric(data_FD$age)
            
            if(!check_6) {
              
              check_7 <- any(data_FD$age > 120)
            }
          }
        }
      }
    }
  }
  
  check <- any(c(check_1,
                 check_2,
                 check_3,
                 check_4,
                 check_5,
                 check_6,
                 check_7))
  
  
  return(check)
}

check_cox <- function(data_FD, inter) {
  
  is(tryCatch(model_cox(data_cox(trans_data(data_FD  = data_FD,
                                            inf      = inter)),
                        3),error=function(e) e, warning=function(w) w), "warning")
  
}

