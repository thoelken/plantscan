
scan_ratio = function(filepath, cut.hue, min.hue=0.0, max.hue=1.0, min.sat=0.0, min.val=0.0) {
  par(mfrow=c(1,2))
  plant = jpeg::readJPEG(filepath)
  res = dim(plant)
  plant.hsv = plant
  for(r in 1:res[1]) {
    plant.hsv[r,,] = t(rgb2hsv(plant[r,,1], plant[r,,2], plant[r,,3], maxColorValue=1))
  }
  mask = matrix(T, ncol=res[2], nrow=res[1])
  # flt = matrix(c(0.3, 0.99, 0.6, 1, 0.0, 1), ncol=2, byrow=T)
  # mask[plant[,,1] > flt[1,1] & plant[,,1] < flt[1,2] & plant[,,2] > flt[2,1] & plant[,,2] < flt[2,2] & plant[,,3] > flt[3,1] & plant[,,3] < flt[3,2]] = F
  mask[plant.hsv[,,1] > min.hue & plant.hsv[,,1] < max.hue & plant.hsv[,,2] > min.sat & plant.hsv[,,3] > min.val] = F
  plant2 = plant
  plant2[,,1][mask] = 1
  plant2[,,2][mask] = 1
  plant2[,,3][mask] = 1
  plot(1,1,xlim=c(1,res[2]),ylim=c(1,res[1]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  rasterImage(plant2, 1, 1, res[2], res[1])
  hist(as.vector(plant.hsv[,,1][!mask]), breaks=c(0:255/255), col=hsv(0:255/255), border=hsv(0:255/255), xlim=c(min.hue, max.hue), main='Histogram', xlab='hue')
  abline(v=cut.hue)
  par(mfrow=c(1,1))
  return(c(sum(plant[,,1][!mask] < cut.hue), sum(plant[,,1][!mask] > cut.hue)))
}

ratio = scan_ratio('test001.jpg', cut.hue=0.175, min.hue=0.14, max.hue=0.3, min.sat=0.4, min.val=0.2)

dirs = list.dirs('~/project/plantscanner/data')
ratios = data.frame(file=character(), dir=character(), infected=integer(), healthy=integer())
for(d in dirs[2:length(dirs)]) {
  for(f in list.files(d, pattern='.jpg', full.names=T)[1:7]) {
    r = scan_ratio(f, cut.hue=0.175, min.hue=0.14, max.hue=0.3, min.sat=0.4, min.val=0.2)
    ratios = rbind(ratios, list(file=f, dir=d, infected=r[1], healthy=r[2]))
  }
}
plot(0, 0, type='n', xlim=c(1,7), ylim=range(c(ratios$infected, ratios$healthy)), log='y')
lines(1:7, ratios$infected[1:7], col='orange')
lines(1:7, ratios$infected[8:14], col='orange')
lines(1:7, ratios$healthy[1:7], col='green')
lines(1:7, ratios$healthy[8:14], col='green')
