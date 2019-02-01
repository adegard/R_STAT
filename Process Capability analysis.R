#https://datascienceplus.com/six-sigma-dmaic-series-in-r-part-2/
# Create data for Process capability analysis
x<-c(755.81, 750.54, 751.05, 749.52, 749.21, 748.38,
              748.11, 753.07, 749.56, 750.08, 747.16, 747.53,
              749.22, 746.76, 747.64, 750.46, 749.27, 750.33,
              750.26, 751.29)


ss.ca.cp(x,740, 760)
ss.ca.cpk(x,740, 760)


# perform Capability Study
ss.study.ca(x, LSL = 740, USL = 760,
            Target = 750, alpha = 0.5,
            f.su = "Food Sample Example")
