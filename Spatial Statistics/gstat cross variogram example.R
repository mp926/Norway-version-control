# Cross variogram example 

coordinates(meuse)=~x+y

 g = gstat(NULL, "log(zn)", log(zinc)~sqrt(dist), meuse)
 g = gstat(g, "log(cd)", log(cadmium)~sqrt(dist), meuse)
 g = gstat(g, "log(pb)", log(lead)~sqrt(dist), meuse)
 g = gstat(g, "log(cu)", log(copper)~sqrt(dist), meuse)
 v = variogram(g)
 g = gstat(g, model = vgm(1, "Exp", 300, 1), fill.all = TRUE)
 g.fit = fit.lmc(v, g)
 g.fit

 
 plot(v, g.fit)
 vgm.map = variogram(g, cutoff = 1500, width = 100, map = TRUE)
 plot(vgm.map, threshold = 5, col.regions = bpy.colors(), xlab = "", ylab = "")
 
 
 
 
