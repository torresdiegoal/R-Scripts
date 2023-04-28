##########################################
##############    ROC CURVES
##########################################
library(pROC)

png(paste(outpath,"Logis vs RandFor ROC Curve - PCA.png", sep='')) # Abrimos el dispositivo grafico

par(pty = 's') # plot_type = 'square'
#par(mar = c(1,1,1,1))
#logis_roc <- 
roc(response = train$CLIENTE,
    predictor = logis$fitted.values, # Grafica de la Regresion logistica
    percent = TRUE,
    plot = TRUE, 
    auc = TRUE,
    print.auc = TRUE, 
    legacy.axes=TRUE, 
    col = '#377eb8',lwd = 3,
    main = 'Logis vs RandFor ROC Curve - PCA') 

plot.roc(as.factor(train$CLIENTE),
         randomFor$votes[,1], #plot=TRUE,  # Grafica del Random Forest
         print.auc = TRUE,
         percent = TRUE,
         add = TRUE,
         col = '#4daf4a', lwd = 3,
         print.auc.y = 40
)  
#par(pty = 's')
legend('bottomright',
       legend = c('logistic Regression', 'random forest'), 
       col = c('#377eb8','#4daf4a'),
       lwd = 5)
#plot(roc_graph)
dev.off() # Cerramos el dispositivo grafico


# Volvemos al valor original con 
par(pty = 'm') # plot_type = 'Maximum'
