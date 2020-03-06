

Lights <- function(data,lco,hco){

LowerCutOff  <- lco
HigherCutOff <- hco

emp <- ecdf(data)

LowerTarget  <- emp(c(LowerCutOff))
MiddleTarget <- diff(emp(c(LowerCutOff,HigherCutOff)))
HigherTarget <- 1 - emp(c(HigherCutOff))

plot(emp)

t <- rbind(LowerTarget,MiddleTarget,HigherTarget)
colnames(t) <- 'Probability'

return(t)
}




Lights(dt$treatment,lco = 130,hco = 180)
p <- Lights(dt$treatment,lco = 130,hco = 180)

pp <- c(.3,.5,.2)

ppp <- cbind(p,pp)
pb <- barplot(ppp, main='StopLight Chart',
              col = c('red','yellow','green'),
              ylab="Probability",
              names.arg = c('Treat 1','Treat 2'))

text(pb,
     c(ppp[1,]/2,c(ppp[1,]+ppp[2,]/2),c(ppp[1,]+ppp[2,]+ppp[3,]/2)),
     labels = c(paste0(round(ppp[1,],2)*100,'%'),
                paste0(round(ppp[2,],2)*100,'%'),
                paste0(round(ppp[3,],2)*100,'%'))
     )

hist(dt$treatment)
plot(density(dt$treatment))



qqnorm(dt$treatment, pch = 1, frame = FALSE)
qqline(dt$treatment, col = "steelblue", lwd = 2)
