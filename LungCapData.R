LungCapData<-read.table(file.choose(), header=T, sep=",")
attach(LungCapData)
levels(Smoke)
Height[1:10]
CatHeight[1:10]
rm=ls(list=ls())
levels(CatHeight)
CatHeight <- factor(CatHeight,order=T,levels=c("F","E","D","C","B","A"))
# create a variable of all 0's to store the categorical heights in
CatHeight <- rep(0, 725)
# run through a look, checking each row/value for height
for (i in 1:725){
  # if the height of person i is less than 50, then assign a value of "A" to the categorical height for person i
  if (Height[i]<50){CatHeight[i] <- "A"}
  # if the height of person i is less than 55 and greater/equal to 50, assign a value of "B" to the categorical height for person i
  if (Height[i]<55 & Height[i]>=50){CatHeight[i] <- "B"}
  # same for the rest...
  if (Height[i]<60 & Height[i]>=55){CatHeight[i] <- "C"}
  if (Height[i]<65 & Height[i]>=60){CatHeight[i] <- "D"}
  if (Height[i]<70 & Height[i]>=65){CatHeight[i] <- "E"}
  if (Height[i]>=70){CatHeight[i] <- "F"}
}
# check the heights of the first 5 people
Height[1:5]
[1] 62.1 74.7 69.7 71.0 56.9
# compare to the categorical height of the first 5 people, to make sure that the categories were assigned correctly
CatHeight[1:5]
#[1] "D" "F" "E" "F" "C"
mean(LungCap[CatHeight=="A"])
mean(LungCap[CatHeight=="B"])
mean(LungCap[CatHeight=="C"])
mean(LungCap[CatHeight=="D"])
mean(LungCap[CatHeight=="E"])
mean(LungCap[CatHeight=="F"])
mod<-lm(LungCap~CatHeight)
summary(mod)
mod1<-lm(LungCap~CatHeight+Age)
summary(mod1)
