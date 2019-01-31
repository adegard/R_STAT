#https://datascienceplus.com/six-sigma-dmaic-series-in-r-part-2/

# gage R & R study 

# Create gage R and R data for 3 piston rings , 2 operators and each operator 3 measurements per piston
Operator<- factor(rep(1:2, each = 9))
Pistonring<- factor(rep(rep(1:3, each = 3), 2))
run<- factor(rep(1:3, 6))
diameter<-c(1.4727, 1.4206, 1.4754, 1.5083, 1.5739,
            1.4341, 1.5517, 1.5483, 1.4614, 1.3337,
            1.6078, 1.4767, 1.4066, 1.5951, 1.8419,
            1.7087, 1.8259, 1.5444)
pistondata<-data.frame(Operator,Pistonring,run,diameter)
View(pistondata)


#Load package
library("SixSigma")

#Perform gage R & R
my.rr <- ss.rr(var = diameter, part = Pistonring,
               appr = Operator,
               data = pistondata,
               main = "Six Sigma Gage R&R Measure",
               sub = "Piston ring MSA")
