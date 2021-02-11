library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(rgl)
library(hrbrthemes)
library(orca)
library(processx)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
data<-read_excel("../data/simulation_results.xlsx")
data_complexity<-read_excel("../data/complexity_results.xlsx")

#Summary of results
summary <- summary(data%>%
                    select(`KRR-Mean`, `Huber-Mean`,`RKR-Mean`,`KGARD-Mean`))
summary



#Transform complexity
data_c <- data_complexity %>%
  select(`KRR`, `Huber`,`RKR`,`KGARD`,`n`) %>%
  pivot_longer(., cols = c(`KRR`, `Huber`,`RKR`,`KGARD`),
               names_to = "method", values_to = "time")

fig <- plot_ly(data = data_c, x = ~n, y = ~time, color = ~method, colors = "RdBu",
               width=1024, height=480)
fig <- fig %>% layout(yaxis = list(title="Time [s]",tickfont=list(size=14),
                                   titlefont=list(size=18),range=list(0,120)),
               xaxis = list(title="n",tickfont=list(size=14),titlefont=list(size=18),
                            range=list(0,1020)),
               legend = list(x=0.05,y=1,title=list(text='<b> Methods </b>'),font=list(size=16)),
               hovermode="compare")
fig
orca(fig,"complexity.pdf")



#Predict fits
data_fit<-select(data_complexity,c(n,KRR,Huber,RKR,KGARD))
data_pred<-tibble("n"=c(seq(50,3000,100),seq(4000,30000,1000)))
models <- c("KRR","RKR","Huber","KGARD")
for (model in models){
  #Train models
  d<-ifelse (model=="Huber", 2, 3)
  formula <- paste(model,"~","poly(n,d,raw=TRUE)")
  model_fit<-lm(formula, data=data_fit)
  
  #Predict new data
  data_pred[ , ncol(data_pred) + 1] <- model_fit$coefficients[1] + 
    tcrossprod(poly(data_pred$n,d,raw=TRUE),t(model_fit$coefficients[-1]))
  #Rename
  data_pred <- rename(data_pred, !!paste0(model):=V1)
}
data_pred
data_pred <- data_pred %>%
  pivot_longer(., cols = c(`KRR`, `Huber`,`RKR`,`KGARD`),
               names_to = "method", values_to = "time_pred")


fig <- plot_ly(data = data_c, x = ~n, y = ~time, type="scatter", mode ="markers", color=~method)
fig <- fig %>% add_trace(data = data_pred, y =~ time_pred,type="scatter", mode ="lines", color=~method)
fig

n<-50
model$coefficients[1]+model$coefficients[2]*n+model$coefficients[3]*n^2



n_input=150
p_input=1
gamma_input=0.05
test <- data
if(n_input!="All"){
  test <- test %>% filter(n==n_input)
}
if(p_input!="All"){
  test <- test %>% filter(p==p_input)
}
if(gamma_input!="All"){
  test <- test %>% filter(gamma==gamma_input)
}


test <- filter(data, n == 500, p == 5, gamma == 0.2) %>% 
  rename(`Huber`=`Huber-Mean`, `KRR` = `KRR-Mean`, `RKR`= `RKR-Mean`, `KGARD`=`KGARD-Mean`) %>%
  select(`KRR`, `Huber`,`RKR`,`KGARD`,`Shift`) %>%
  pivot_longer(., cols = c(`KRR`, `Huber`,`RKR`,`KGARD`),
               names_to = "method", values_to = "mean")
test

test_sd <- filter(data, gamma <0.2) %>% 
  rename(`Huber`=`Huber-Mean`, `KRR` = `KRR-Mean`, `RKR`= `RKR-Mean`, `KGARD`=`KGARD-Mean`) %>%
  select(`KRR`, `Huber`,`RKR`,`KGARD`)

len <- dim(test_sd)[1]
dim(test_sd %>% filter(Huber < KRR))[1]/len




#Funktionierender plot
max <- max(test$mean)*1.1
fig <- plot_ly(test, x = ~`Shift`, y = ~`mean`, color = ~`method`,
               mode = "lines+markers",colors="RdBu")
fig <- fig %>% layout(yaxis = list(title="MSE",titlefont=list(size=18),range=list(0,max),
                                   tickfont=list(size=14)),
                      xaxis = list(title="Outlier shift",titlefont=list(size=18),
                                   tickfont=list(size=14)),
                      legend = list(x=0.05,y=1,title=list(text='<b> Methods </b>'),
                                    font=list(size=16)),
                      hovermode="compare")
fig
#orca(fig, "result_plot_6.pdf")


#Plot mit error bars
fig <- plot_ly(test_merged, x = ~`Shift`, y = ~`mean`, color = ~`method`,
               mode = "lines+markers", error_y=~list(array=`SD`))
fig <- fig %>% layout(yaxis = list(title="MSE"), xaxis = list(title="Outlier shift"),
                      legend = list(x=0.05,y=1,title=list(text='<b> Methods </b>')),
                      hovermode="compare")
fig

#Plot mit SD
test <- filter(data, n == 150, p == 1, gamma == 0.05)
#KRR
fig <- plot_ly(test, x = ~Shift, y = ~`KRR-Mean`+`KRR-SD`, type = 'scatter', mode = 'lines',
               line = list(color = 'transparent')) 
fig <- fig %>% add_trace(y = ~`KRR-Mean`-`KRR-SD`, type = 'scatter', mode = 'lines',
                         fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)',
                         line = list(color = 'transparent'), showlegend = FALSE) 
fig <- fig %>% add_trace(x = ~Shift, y = ~`KRR-Mean`, type = 'scatter', mode = 'lines',
                         line = list(color='rgb(0,100,80)'),name="KRR") 
#KGARD
fig <- fig %>% add_trace(y = ~`KGARD-Mean`+`KGARD-SD`, type = 'scatter', mode = 'lines',
                         line = list(color = 'transparent'),showlegend = FALSE) 
fig <- fig %>% add_trace(y = ~`KGARD-Mean`-`KGARD-SD`, type = 'scatter', mode = 'lines',
                         fill = 'tonexty', fillcolor='rgba(217,95,2,0.2)',
                         line = list(color = 'transparent'), showlegend = FALSE) 
fig <- fig %>% add_trace(x = ~Shift, y = ~`KGARD-Mean`, type = 'scatter', mode = 'lines',
                        line = list(color="rgb(217,95,2)"),name="KGARD")
#RKR
fig <- fig %>% add_trace(y = ~`RKR-Mean`+`RKR-SD`, type = 'scatter', mode = 'lines',
                         line = list(color = 'transparent'),
                         showlegend = FALSE) 
fig <- fig %>% add_trace(y = ~`RKR-Mean`-`RKR-SD`, type = 'scatter', mode = 'lines',
                         fill = 'tonexty', line = list(color = 'transparent'),
                         showlegend = FALSE, fillcolor='rgba(231,41,138,0.2)') 
fig <- fig %>% add_trace(x = ~Shift, y = ~`RKR-Mean`, type = 'scatter', mode = 'lines',
                         name = 'RKR', line = list(color="rgb(231,41,138)"))
#Huber
fig <- fig %>% add_trace(y = ~`Huber-Mean`+`Huber-SD`, type = 'scatter', mode = 'lines',
                         line = list(color = 'transparent'),
                         showlegend = FALSE, name = 'Low 2014') 
fig <- fig %>% add_trace(y = ~`Huber-Mean`-`Huber-SD`, type = 'scatter', mode = 'lines',
                         fill = 'tonexty', line = list(color = 'transparent'),
                         showlegend = FALSE, fillcolor='rgba(117,112,179,0.2)') 
fig <- fig %>% add_trace(x = ~Shift, y = ~`Huber-Mean`, type = 'scatter', mode = 'lines',
                         name = 'Huber', line = list(color="rgb(117,112,179)"))

fig <- fig %>% layout(yaxis = list(title="MSE"), xaxis = list(title="Outlier shift"),
                      legend = list(x=0.05,y=1))

fig





data

test2 <- data %>% rename(`KRR` = `KRR-Mean`,
                          Huber = `Huber-Mean`,
                          RKR = `RKR-Mean`,
                          KGARD = `KGARD-Mean`
) %>%
  filter(gamma == 0.1) %>% 
  select(`KRR`, `Huber`,`RKR`,`KGARD`) %>%
  pivot_longer(., cols = c(`KRR`, `Huber`,`RKR`,`KGARD`),
                                                       names_to = "Var", values_to = "Val")
#Boxplot
ggplot(test2, aes(x=Var, y=Val)) + geom_boxplot()

fig <- plot_ly(test2, y =~Val , type = "box", color=~Var,
               boxpoints="suspectedoutliers",colors="RdBu")
fig <- fig %>% layout(yaxis = list(title="MSE",titlefont=list(size=18),tickfont=list(size=15)),
                                   xaxis = list(tickfont=list(size=18)),showlegend=FALSE)
fig
#orca(fig, "boxplot_0.1.pdf")




#Kde
c_scale <- RColorBrewer::brewer.pal(4, "RdBu")
fig <- ggplot(test2, aes(Val, fill = Var, colour = Var))+
  geom_density(outline.type="upper",
               alpha=0.3)+
  xlab("MSE")+ylab("Density")+
  theme_bw()+
  theme(legend.title=element_blank())
withr::with_options(
  list(ggplot2.discrete.fill = c_scale),
  print(ggplotly(fig) %>% layout(legend = list(x=0.8, y=1)))
)




#threeD <- filter(data, n == 150, p == 1)
threeD <- data %>% filter(n == 150, p == 1) %>% select(`KRR-Mean`, `Huber-Mean`,`RKR-Mean`,`KGARD-Mean`,`Shift`,`gamma`) %>% pivot_longer(., cols = c(`KRR-Mean`, `Huber-Mean`,`RKR-Mean`,`KGARD-Mean`),
                                                                                   names_to = "Var", values_to = "Val")

plot_ly(data=threeD,x=threeD$Shift, y=threeD$gamma, z=threeD$Val, type="scatter3d", mode = "markers", color=threeD$Var)


plot_ly(test2, y = ~Val, color = I("black"), 
        alpha = 0.1, boxpoints = "suspectedoutliers") %>% add_boxplot(x = "Overall")


