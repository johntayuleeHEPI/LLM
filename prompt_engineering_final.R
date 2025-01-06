# ---------------------------------------------------------------------------- #
# Grading Method : 1-100
# 2024/11/08 : OS改ZSL
# 2024/11/15 : 把圖的word變大()
# ---------------------------------------------------------------------------- #
# ============================================================================ #
# ============================================================================ #
# Packages
library(nlme)  # lme
library(lme4)  #lmer
library(tidyverse)
library(magrittr)
library(afex)
library(fastDummies)  # dummy variables
library(Matrix)
library(readxl)
library(dplyr)
# ============================================================================ #
# ================================= Data input =============================== #
# ============================================================================ #
# data directory
# setwd("C:/NTU/李達宇老師/prompt engineering data/data_collection/醫生回傳檔案/second")
file_path <- "C:/NTU/李達宇老師/prompt engineering data/data_collection/醫生回傳檔案/second/醫生回傳檔案_second_final.xlsx"

# worksheet names in Excel
sheet_names <- excel_sheets(file_path)
data_list <- list()

# input data (Prevention, Diagnosis, Treatment (including mediation), Recovery)
for (sheet in sheet_names[1:4]) {
  data <- read_excel(file_path, sheet = sheet, skip = 3, col_names = FALSE)
  data_list[[sheet]] <- data
}
# print(data_list)


# ============================================================================ #
# ============================ Data Preprocessing ============================ #
# ============================================================================ #
# 平均一個醫生有45個答案(5個題目*9個問法[3 model*3 prompt])
data_preprocess <- function(dt){
  data <- dt
  
  # -------------------------------------------------------------------------- #
  # 去除醫生名字的row
  data <- data[-c(1,27,53,79),]
  # turn into as data frame type
  data <- as.data.frame(data[,-c(1,2)])
  # set prompt index
  # colnames(data) <- rep(c("OS", "COT", "TOT"), 3)
  colnames(data) <- rep(c("ZSL", "COT", "TOT"), 3)
  # head(data)
  
  # Transpose the data frame
  data <- as.data.frame(t(data))
  
  # 平均一個醫生有45個答案
  colnames(data) <- rep(rep(c("Accuracy", "Hallucination", "Specificity & Relevance",
                              "Empathy & Understanding", "Actionability"), each=5), 4)
  
  data <- Filter(function(x) !any(is.na(x)), data)
  # -------------------------------------------------------------------------- #
  return(data)
}
preprocessed_datalist <- lapply(data_list, data_preprocess)


# ============================================================================ #
# ========================== Comparative Performance ========================= #
# ============================================================================ #
# Choose Stages
# Prevention
data <- preprocessed_datalist[["Prevention"]]

# Diagnosis
data <- preprocessed_datalist[["Diagnosis"]]

# Treatment (including mediation)
data <- preprocessed_datalist[["Treatment (including mediation)"]]

# Recovery
data <- preprocessed_datalist[["Recovery"]]


# --------------------------------- Analysis --------------------------------- #
analysis <- function(dt, goal){
  data <- dt
  # =============================== tidy up data ============================= #
  # 4 doctors reply --> rep到4
  P.ID <- rep(1:4, each = 45)
  
  # ----------------------------------- Accuracy ------------------------------- #
  # 平均一個醫生有45個答案
  dt_acc <- data.frame(P.ID = P.ID)
  
  sequence <- seq(1,5)
  for (i in seq(1, 55, by = 25)) {
    sequence <- c(sequence, (i + 25):(i + 29))
  }
  # print(sequence)
  # data[,c(sequence)]
  dt_acc$acc <- unlist(data[,c(sequence)], use.names = FALSE)
  dt_acc$prompt <- as.factor(rep(c("ZSL", "COT", "TOT"), 30))
  dt_acc$model <- as.factor(rep(c(rep(c("ChatGPT"),3),
                                  rep(c("Claude"),3),
                                  rep(c("Gemini"),3)),10))
  
  # -------------------------------- Hallucination ------------------------------ #
  # 平均一個醫生有45個答案
  dt_hal <- data.frame(P.ID = P.ID)
  
  sequence <- seq(6,10)
  for (i in seq(6, 60, by = 25)) {
    sequence <- c(sequence, (i + 25):(i + 29))
  }
  # print(sequence)
  # data[,sequence]
  dt_hal$hal <- unlist(data[,sequence], use.names = FALSE)
  dt_hal$prompt <- as.factor(rep(c("ZSL", "COT", "TOT"), 30))
  dt_hal$model <- as.factor(rep(c(rep(c("ChatGPT"),3),
                                  rep(c("Claude"),3),
                                  rep(c("Gemini"),3)),10))
  
  # ---------------------------- Specificity & Relevance ----------------------- #
  # 平均一個醫生有45個答案
  dt_spe <- data.frame(P.ID = P.ID)
  
  sequence <- seq(11,15)
  for (i in seq(11, 65, by = 25)) {
    sequence <- c(sequence, (i + 25):(i + 29))
  }
  # print(sequence)
  # data[,sequence]
  dt_spe$spe <- unlist(data[,sequence], use.names = FALSE)
  dt_spe$prompt <- as.factor(rep(c("ZSL", "COT", "TOT"), 30))
  dt_spe$model <- as.factor(rep(c(rep(c("ChatGPT"),3),
                                  rep(c("Claude"),3),
                                  rep(c("Gemini"),3)),10))
  
  # --------------------------- Empathy & Understanding ------------------------ #
  # 平均一個醫生有45個答案
  dt_emp <- data.frame(P.ID = P.ID)
  
  sequence <- seq(16,20)
  for (i in seq(16, 70, by = 25)) {
    sequence <- c(sequence, (i + 25):(i + 29))
  }
  # print(sequence)
  # data[,sequence]
  dt_emp$emp <- unlist(data[,sequence], use.names = FALSE)
  dt_emp$prompt <- as.factor(rep(c("ZSL", "COT", "TOT"), 30))
  dt_emp$model <- as.factor(rep(c(rep(c("ChatGPT"),3),
                                  rep(c("Claude"),3),
                                  rep(c("Gemini"),3)),10))
  
  # -------------------------------- Actionability ----------------------------- #
  # 平均一個醫生有45個答案
  dt_act <- data.frame(P.ID = P.ID)
  
  sequence <- seq(21,25)
  for (i in seq(21, 75, by = 25)) {
    sequence <- c(sequence, (i + 25):(i + 29))
  }
  # print(sequence)
  # data[,sequence]
  dt_act$act <- unlist(data[,sequence], use.names = FALSE)
  dt_act$prompt <- as.factor(rep(c("ZSL", "COT", "TOT"), 30))
  dt_act$model <- as.factor(rep(c(rep(c("ChatGPT"),3),
                                  rep(c("Claude"),3),
                                  rep(c("Gemini"),3)),10))

  
  # =========================== random intercept model ======================= #
  
  if (goal=='Prompt') {
    print('------------------ Accuracy ------------------')
    # ----------------------------------- Accuracy ------------------------------- #
    # default with baseline=0
    # Model_Acc <- lme(acc ~ prompt+model, random  = ~ 1 | P.ID, data = dt_acc, method = "REML")
    # summary(Model_Acc)
    
    ## Prompt
    Model_Acc <- lme(acc ~ prompt, random  = ~ 1 | P.ID, data = dt_acc, method = "REML")
    print(summary(Model_Acc))
    
    print('------------------ Hallucination ------------------')
    # -------------------------------- Hallucination ------------------------------ #
    # default with baseline=0
    # Model_Hal <- lme(hal ~ prompt+model, random  = ~ 1 | P.ID, data = dt_hal, method = "REML")
    # summary(Model_Hal)
    
    ## Prompt
    Model_Hal <- lme(hal ~ prompt, random  = ~ 1 | P.ID, data = dt_hal, method = "REML")
    print(summary(Model_Hal))
    
    print('------------------ Specificity & Relevance ------------------')
    # ---------------------------- Specificity & Relevance ----------------------- #
    # default with baseline=0
    # Model_Spe <- lme(spe ~ prompt+model, random  = ~ 1 | P.ID, data = dt_spe, method = "REML")
    # summary(Model_Spe)
    
    ## Prompt
    Model_Spe <- lme(spe ~ prompt, random  = ~ 1 | P.ID, data = dt_spe, method = "REML")
    print(summary(Model_Spe))
    
    print('------------------ Empathy & Understanding ------------------')
    # --------------------------- Empathy & Understanding ------------------------ #
    # default with baseline=0
    # Model_Emp <- lme(emp ~ prompt+model, random  = ~ 1 | P.ID, data = dt_emp, method = "REML")
    # summary(Model_Emp)
    
    ## Prompt
    Model_Emp <- lme(emp ~ prompt, random  = ~ 1 | P.ID, data = dt_emp, method = "REML")
    print(summary(Model_Emp))
    
    print('------------------ Actionability ------------------')
    # -------------------------------- Actionability ----------------------------- #
    # default with baseline=0
    # Model_Act <- lme(act ~ prompt+model, random  = ~ 1 | P.ID, data = dt_act, method = "REML")
    # summary(Model_Act)
    
    ## Prompt
    Model_Act <- lme(act ~ prompt, random  = ~ 1 | P.ID, data = dt_act, method = "REML")
    print(summary(Model_Act))
    
  }else{
    
    print('------------------ Accuracy ------------------')
    # ----------------------------------- Accuracy ------------------------------- #
    ## Model
    Model_Acc <- lme(acc ~ model, random  = ~ 1 | P.ID, data = dt_acc, method = "REML")
    print(summary(Model_Acc))
    
    print('------------------ Hallucination ------------------')
    # -------------------------------- Hallucination ------------------------------ #
    ## Model
    Model_Hal <- lme(hal ~ model, random  = ~ 1 | P.ID, data = dt_hal, method = "REML")
    print(summary(Model_Hal))
    
    print('------------------ Specificity & Relevance ------------------')
    # ---------------------------- Specificity & Relevance ----------------------- #
    ## Model
    Model_Spe <- lme(spe ~ model, random  = ~ 1 | P.ID, data = dt_spe, method = "REML")
    print(summary(Model_Spe))
    
    print('------------------ Empathy & Understanding ------------------')
    # --------------------------- Empathy & Understanding ------------------------ #
    ## Model
    Model_Emp <- lme(emp ~ model, random  = ~ 1 | P.ID, data = dt_emp, method = "REML")
    print(summary(Model_Emp))
    
    print('------------------ Actionability ------------------')
    # -------------------------------- Actionability ----------------------------- #
    ## Model
    Model_Act <- lme(act ~ model, random  = ~ 1 | P.ID, data = dt_act, method = "REML")
    print(summary(Model_Act))
    
  }
  dt_list <- list(dt_acc, dt_hal, dt_spe, dt_emp, dt_act)
  return(dt_list)
}

## Prompt
analysis(data, goal = 'Prompt')

## Model
analysis(data, goal = 'Model')


# ============================================================================ #
# ============================== Barplot with C.I. =========================== #
# ============================================================================ #
# https://stackoverflow.com/questions/77539385/grouped-barplot-with-95-confidence-intervals-in-r

# ---------------------- Base on 4 Medical Stages ---------------------------- #

# ============================ Data Preprocessing ============================ #
dt_total <- lapply(list(preprocessed_datalist[["Prevention"]], 
                        preprocessed_datalist[["Diagnosis"]], 
                        preprocessed_datalist[["Treatment (including mediation)"]], 
                        preprocessed_datalist[["Recovery"]]), 
                   analysis, goal = 'Prompt')

# ============================================================================ #

# ================================== Plots =================================== #
plotting_stage <- function(dt_list, goal, stage){
  
  i <- ifelse(stage == 'Prevention', 1, 
              ifelse(stage == 'Diagnosis', 2, 
                     ifelse(stage == 'Treatment (including mediation)', 3, 4)))
  dt_acc <- dt_list[[i]][[1]]
  dt_hal <- dt_list[[i]][[2]]
  dt_spe <- dt_list[[i]][[3]]
  dt_emp <- dt_list[[i]][[4]]
  dt_act <- dt_list[[i]][[5]]
  
  # -------------------------------------------------------------------------- #
  if (goal=='Prompt') {
    ## Prompt
    os.index <- which(dt_acc$prompt=='ZSL')
    cot.index <- which(dt_acc$prompt=='COT')
    tot.index <- which(dt_acc$prompt=='TOT')
    mean <- c(mean(dt_acc$acc[os.index]), mean(dt_acc$acc[cot.index]),
              mean(dt_acc$acc[tot.index]),
              mean(dt_hal$hal[os.index]), mean(dt_hal$hal[cot.index]),
              mean(dt_hal$hal[tot.index]),
              mean(dt_spe$spe[os.index]), mean(dt_spe$spe[cot.index]),
              mean(dt_spe$spe[tot.index]),
              mean(dt_emp$emp[os.index]), mean(dt_emp$emp[cot.index]),
              mean(dt_emp$emp[tot.index]),
              mean(dt_act$act[os.index]), mean(dt_act$act[cot.index]),
              mean(dt_act$act[tot.index]))
    len <- length(dt_acc$acc[os.index])
    stderr <- c(sd(dt_acc$acc[os.index])/sqrt(len),
                sd(dt_acc$acc[cot.index])/sqrt(len),
                sd(dt_acc$acc[tot.index])/sqrt(len),
                sd(dt_hal$hal[os.index])/sqrt(len),
                sd(dt_hal$hal[cot.index])/sqrt(len),
                sd(dt_hal$hal[tot.index])/sqrt(len),
                sd(dt_spe$spe[os.index])/sqrt(len),
                sd(dt_spe$spe[cot.index])/sqrt(len),
                sd(dt_spe$spe[tot.index])/sqrt(len),
                sd(dt_emp$emp[os.index])/sqrt(len),
                sd(dt_emp$emp[cot.index])/sqrt(len),
                sd(dt_emp$emp[tot.index])/sqrt(len),
                sd(dt_act$act[os.index])/sqrt(len),
                sd(dt_act$act[cot.index])/sqrt(len),
                sd(dt_act$act[tot.index])/sqrt(len))
    alpha <- 0.05
    t_value <- qt(1 - alpha/2, df = len - 1)
    conf_interval <- rep(mean, each =2) + rep(c(-1, 1)*t_value,15)*stderr
    # print(conf_interval)
    df <- data.frame(index = factor(rep(c('Accuracy','Hallucination', 'Specificity & Relevance', 'Empathy & Understanding', 'Actionability'), each=3), 
                                    levels = c('Accuracy','Hallucination', 'Specificity & Relevance', 'Empathy & Understanding', 'Actionability')),
                     prompt = factor(c("ZSL", "COT", "TOT"), levels = c("ZSL", "COT", "TOT")), 
                     mean = mean, 
                     Lower_CI = conf_interval[seq(1,30,2)], 
                     Upper_CI = conf_interval[seq(2,30,2)])
  }else{
    # -------------------------------------------------------------------------- #
    ## Model
    gpt.index <- which(dt_acc$model=='ChatGPT')
    cl.index <- which(dt_acc$model=='Claude')
    ge.index <- which(dt_acc$model=='Gemini')
    mean <- c(mean(dt_acc$acc[gpt.index]), mean(dt_acc$acc[cl.index]),
              mean(dt_acc$acc[ge.index]),
              mean(dt_hal$hal[gpt.index]), mean(dt_hal$hal[cl.index]),
              mean(dt_hal$hal[ge.index]),
              mean(dt_spe$spe[gpt.index]), mean(dt_spe$spe[cl.index]),
              mean(dt_spe$spe[ge.index]),
              mean(dt_emp$emp[gpt.index]), mean(dt_emp$emp[cl.index]),
              mean(dt_emp$emp[ge.index]),
              mean(dt_act$act[gpt.index]), mean(dt_act$act[cl.index]),
              mean(dt_act$act[ge.index]))
    len <- length(dt_acc$acc[gpt.index])
    stderr <- c(sd(dt_acc$acc[gpt.index])/sqrt(len),
                sd(dt_acc$acc[cl.index])/sqrt(len),
                sd(dt_acc$acc[ge.index])/sqrt(len),
                sd(dt_hal$hal[gpt.index])/sqrt(len),
                sd(dt_hal$hal[cl.index])/sqrt(len),
                sd(dt_hal$hal[ge.index])/sqrt(len),
                sd(dt_spe$spe[gpt.index])/sqrt(len),
                sd(dt_spe$spe[cl.index])/sqrt(len),
                sd(dt_spe$spe[ge.index])/sqrt(len),
                sd(dt_emp$emp[gpt.index])/sqrt(len),
                sd(dt_emp$emp[cl.index])/sqrt(len),
                sd(dt_emp$emp[ge.index])/sqrt(len),
                sd(dt_act$act[gpt.index])/sqrt(len),
                sd(dt_act$act[cl.index])/sqrt(len),
                sd(dt_act$act[ge.index])/sqrt(len))
    alpha <- 0.05
    t_value <- qt(1 - alpha/2, df = len - 1)
    conf_interval <- rep(mean, each =2) + rep(c(-1, 1)*t_value,15)*stderr
    # print(conf_interval)
    df <- data.frame(index = factor(rep(c('Accuracy','Hallucination', 'Specificity & Relevance', 'Empathy & Understanding', 'Actionability'), each=3), 
                                    levels = c('Accuracy','Hallucination', 'Specificity & Relevance', 'Empathy & Understanding', 'Actionability')),
                     prompt = factor(c("ChatGPT", "Claude", "Gemini"), levels = c("ChatGPT", "Claude", "Gemini")), 
                     mean = mean, 
                     Lower_CI = conf_interval[seq(1,30,2)], 
                     Upper_CI = conf_interval[seq(2,30,2)])
  }
  # -------------------------------------------------------------------------- #
  # Plots
  plt <- ggplot(df, aes(prompt, mean)) + 
    ylim(0, 80) +
    ggtitle(stage) + 
    theme_minimal(base_size = 25) +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_col(aes(fill = index),position = position_dodge(0.8), 
             color = "black", alpha = 0.7, width = 0.7) +
    geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI, group = index), 
                  position = position_dodge(0.8), width = 0.2) +
    geom_text(aes(x= prompt, y=mean+10, label=round(mean,1), group=index,
                  vjust = 18), position = position_dodge(width = 0.8))+
    theme(panel.grid.major.x = element_blank()) +
    labs(x = NULL, y = "Mean Score")
  print(plt)
  
  return(df)
}

## Prompt
cal_dt_prompt <- lapply(c('Prevention', 'Diagnosis', 'Treatment (including mediation)', 'Recovery'), 
                        plotting_stage, dt_list = dt_total, goal = 'Prompt')

## Model
cal_dt_model <- lapply(c('Prevention', 'Diagnosis', 'Treatment (including mediation)', 'Recovery'), 
                       plotting_stage, dt_list = dt_total, goal = 'Model')


# ----------------------- Base on 5 Comparative Performances ----------------- #

# ============================ Data Preprocessing ============================ #
reformat_df <- function(dt){
  data <- dt
  res <- rbind.data.frame(
    # Accuracy
    data[data$index=='Accuracy',],
    
    # Hallucination
    data[data$index=='Hallucination',],
    
    # Specificity & Relevance
    data[data$index=='Specificity & Relevance',],
    
    # Empathy & Understanding
    data[data$index=='Empathy & Understanding',],
    
    # Actionability
    data[data$index=='Actionability',])
  return(res)
}

## Prompt
res_data <- list_rbind(lapply(cal_dt_prompt, reformat_df))

## Model
res_data <- list_rbind(lapply(cal_dt_model, reformat_df))

res_data$stage <- factor(rep(c('Prevention', 'Diagnosis', 'Treatment (including mediation)', 'Recovery'), each=15),
                         levels = c('Prevention', 'Diagnosis', 'Treatment (including mediation)', 'Recovery'))
# ============================================================================ #

# ================================== Plots =================================== #
plotting_performance <- function(dt, goal, domain){
  data <- dt[dt$index==domain,]
  if (goal=='Prompt') {
    ## Prompt
    data$prompt <- factor(data$prompt, levels = c("ZSL", "COT", "TOT"))
    ggplot(data, aes(stage, mean)) + 
      ylim(0, 80) +
      ggtitle(domain) +
      theme_minimal(base_size = 20) +
      theme(plot.title = element_text(size = 20, hjust = 0.5),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            legend.text = element_text(size = 15),
            legend.title = element_text(size = 15)) +
      geom_col(aes(fill = prompt),position = position_dodge(0.8), 
               color = "black", alpha = 0.7, width = 0.6) +
      geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI, group = prompt), 
                    position = position_dodge(0.8), width = 0.2) +
      geom_text(aes(x=stage, y=mean+10, label=round(mean,1), group=prompt,
                    vjust = 18), position = position_dodge(width = 0.8), size = 4)+
      theme(panel.grid.major.x = element_blank()) +
      labs(x = NULL, y = "Mean Score")
  }else{
    ## Model
    data <- data[,  -which(colnames(data) %in% c("prompt"))]
    data$model <- factor(c("ChatGPT", "Claude", "Gemini"), levels = c("ChatGPT", "Claude", "Gemini"))
    ggplot(data, aes(stage, mean)) +
      ylim(0, 80) +
      ggtitle(domain) +
      theme_minimal(base_size = 20) +
      theme(plot.title = element_text(size = 20, hjust = 0.5),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            legend.text = element_text(size = 15),
            legend.title = element_text(size = 15)) +
      geom_col(aes(fill = model ),position = position_dodge(0.8), 
               color = "black", alpha = 0.7, width = 0.6) +
      geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI, group = model), 
                    position = position_dodge(0.8), width = 0.2) +
      geom_text(aes(x=stage, y=mean+10, label=round(mean,1), group=model,
                    vjust = 18), position = position_dodge(width = 0.8), size = 4)+
      theme(panel.grid.major.x = element_blank()) +
      labs(x = NULL, y = "Mean Score")
  }
  
}

## Prompt
lapply(c('Accuracy','Hallucination', 'Specificity & Relevance', 
         'Empathy & Understanding', 'Actionability'), 
       plotting_performance, goal = 'Prompt', dt = res_data)

## Model
lapply(c('Accuracy','Hallucination', 'Specificity & Relevance', 
         'Empathy & Understanding', 'Actionability'), 
       plotting_performance, goal = 'Model', dt = res_data)
