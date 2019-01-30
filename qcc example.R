#https://cran.r-project.org/web/packages/qcc/vignettes/qcc_a_quick_tour.html
#qcc example
data(pistonrings)
diameter = with(pistonrings, qcc.groups(diameter, sample))
head(diameter)

q1 = qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,])


plot(q1, chart.all=FALSE)


plot(q1, add.stats=FALSE)


q1 = qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,],
         confidence.level=0.99)
q1 = qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,], plot=FALSE)

#add warning limits
warn.limits = limits.xbar(q1$center, q1$std.dev, q1$sizes, 2)

plot(q1, restore.par = FALSE)
abline(h = warn.limits, lty = 3, col = "chocolate")



q1 = qcc(diameter[1:25,], type="xbar", nsigmas=3, plot=FALSE)

#Histogram and process capability analysis
process.capability(q1, spec.limits=c(73.95,74.05))


#pareto chart of defects:
defect = c(80, 27, 66, 94, 33)
names(defect) = c("price code", "schedule date", "supplier code", "contact num.", "part num.")
pareto.chart(defect, ylab = "Error frequency")


#Cause and Effect fishbones
cause.and.effect(cause = list(Measurements = c("Micrometers", "Microscopes", "Inspectors"),
                              Materials    = c("Alloys", "Lubricants", "Suppliers"),
                              Personnel    = c("Shifts", "Supervisors", "Training", "Operators"),
                              Environment  = c("Condensation", "Moisture"),
                              Methods      = c("Brake", "Engager", "Angle"),
                              Machines     = c("Speed", "Lathes", "Bits", "Sockets")),
                 effect = "Surface Flaws")