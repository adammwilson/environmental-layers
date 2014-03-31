library(sp)
library(foreign)
library(reshape)
library(latticeExtra)

path = "/nobackupp4/aguzman4/climateLayers/output/"
tilelist = list.files(path,pattern="^[0-9]")

rm(dlst)
dlst.full=c()
for (t in tilelist){

  # t="30.0_-105.0"

  dbf = read.dbf (paste(path,t,"/monthly_covariates_ghcn_data_TMAX_2001_2011",t,".dbf",sep=""))

  dlst=melt(dbf,id.vars=c("station","month","TMax"),measure.vars=paste("mm",sprintf("%02d",1:12),sep="_"))
  dlst1=melt(dbf,id.vars=c("station","month","TMax"),measure.vars=paste("mm",sprintf("%02d",c(2:12,1)),sep="_"))
  dlst2=melt(dbf,id.vars=c("station","month","TMax"),measure.vars=paste("mm",sprintf("%02d",c(12,1:11)),sep="_"))

  dlst.m = cbind(dlst,dlst1,dlst2)
  dlst.m$lstm=as.numeric(gsub("mm_","",dlst.m$variable))

  dlst.m.sel=dlst.m[,c(1,2,3,5,10,15,16)]

  dlst.all=dlst.m.sel[dlst.m.sel$month==dlst.m.sel$lstm,c("station","month","TMax","value","value.1","value.2")]

  colnames(dlst.all)[colnames(dlst.all)=="value"]="lst"
  colnames(dlst.all)[colnames(dlst.all)=="value.1"]="lstp1"  # LST plus 1 month
  colnames(dlst.all)[colnames(dlst.all)=="value.2"]="lstm1"  # LST less 1 month

  dlst.all$tile=t
  dlst.full = rbind(dlst.all,dlst.full)

}

write.csv(dlst.full,file="/nobackup/awilso10/interp/lst_tmax.csv")

## copy to acrobates
system("scp /nobackup/awilso10/interp/lst_tmax.csv adamw@acrobates.eeb.yale.edu:/data/personal/adamw/projects/")

## Write some plots

xyplot (TMax~lst | c(as.factor(month)+as.factor(tile)) , data=dlst , ylab="Observations Tmax", xlab="LST"  )+layer(panel.abline(0,1))
+layer(panel.abline(lm (y~x),col="red"))

dev.off()

