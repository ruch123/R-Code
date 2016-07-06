

plot <- sqlQuery(channel = 5, select * from [Sheet1$])
print(plot)


Boxplot( ~ times, data=plot, id.method="y")
Boxplot( ~ var1, data=plot, id.method="y", ylab="<auto>variable 1", 
  main="boxplot")
scatterplot(var1~times, reg.line=lm, smooth=TRUE, spread=TRUE, 
  id.method='mahal', id.n = 2, boxplots='xy', span=0.5, data=plot)
scatterplot(var1~times, reg.line=lm, smooth=TRUE, spread=TRUE, 
  id.method='mahal', id.n = 2, boxplots='xy', span=0.5, xlab="times", 
  ylab="variable 1", main="sample", data=plot)
plot$times
str(plot)
plot(plot$times, plot$var1,type="s",main="sample",sub="one",xlab="time",ylab="variable",asp=1/2)
points(plot$times ,plot$variable2, col="blue", cex=1, pch="+")

