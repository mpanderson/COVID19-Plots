library(readxl)
library(plotly)

code<-vector()
for(i in 1:55){
  
  state<-read_excel("C:\\Users\\mpand\\COVID19\\2020 03 26 - By state.xlsx", sheet=i)
  code[i]<-noquote(state[[1]][1])
  nam<-paste("state.dat.",code[i],sep="")
  assign(nam,read_excel("C:\\Users\\mpand\\COVID19\\2020 03 26 - By state.xlsx", sheet=i))
}

#Now all 50 states are imported and saved as state.dat."States two digit code"

#Get the differences


c.d.r.NY<-apply(as.matrix(state.dat.NY[,3:5]),2,diff)
c.d.r.CA<-apply(as.matrix(state.dat.CA[,3:5]),2,diff)
c.d.r.OR<-apply(as.matrix(state.dat.OR[,3:5]),2,diff)
c.d.r.OK<-apply(as.matrix(state.dat.OK[,3:5]),2,diff)



today<-Sys.Date()
tm <- seq(1, dim(state.dat.NY)[1]-1, by = 1)
tm2 <- seq(1, dim(state.dat.CA)[1]-1, by = 1)
tm3 <- seq(1, dim(state.dat.OR)[1]-1, by = 1)
tm4 <- seq(1, dim(state.dat.OK)[1]-1, by = 1)

x <- today - tm
x2 <- today - tm2
x3 <- today - tm3
x4 <- today - tm4

#y <- rnorm(length(x))
y <- c.d.r.NY[,1]
y2<-c.d.r.CA[,1]
y3<-c.d.r.OR[,1]
y4<-c.d.r.OK[,1]

s.y<-cumsum(y)
s.y2<-cumsum(y2)
s.y3<-cumsum(y3)
s.y4<-cumsum(y4)

fig<-plot_ly(x = ~x, y = ~rev(y), type='scatter',name="New York",mode = 'lines', text = paste(rev(s.y),"total cases",rev(tm), "days after 1st infection."))%>%
  layout(yaxis=list(title="Daily Cases",range=c(0,(max(y)+100))))%>%
  layout(xaxis=list(title="Date"))%>%
  add_lines(x=~x2,y=~rev(y2), name="California",mode = 'lines+markers',text = paste(rev(s.y2),"total cases",rev(tm2), "days after 1st infection."))%>%
  add_lines(x=~x3,y=~rev(y3), name='Oregon',mode = 'lines+markers',text = paste(rev(s.y3),"total cases",rev(tm3), "days after 1st infection."))%>%
  add_lines(x=~x4,y=~rev(y4), name='Oklahoma',mode = 'markers',text = paste(rev(s.y4),"total cases",rev(tm4), "days after 1st infection."))

htmlwidgets::saveWidget(fig, "C:\\Users\\mpand\\COVID19\\index.html")



fig<-fig%>% layout(
  title= "Daily State Case Numbers",yaxis2=list(overlaying="y", tickfont=list(color="orange")))

plot(x,y,type="l")
today <- Sys.Date()
tm <- seq(0, 600, by = 10)
x <- today - tm
y <- rnorm(length(x))
fig <- plot_ly(x = ~x, y = ~y, mode = 'lines', text = paste(tm,"days from today"))

fig