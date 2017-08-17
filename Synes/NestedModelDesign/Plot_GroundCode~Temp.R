library(ggplot2)
library(grid)
library(gridExtra)


setwd("D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Results/3_DistStreams_VariableAtmosTrans/MicrositeLevel/GroundCode")

df <- read.csv("D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Results/3_DistStreams_VariableAtmosTrans/Merged_RasterAndResidualSensorData_2013.csv")
df$Point.Site <- substr(df$Point.loc_ID,5,6)




CategoryList <- c("c0 = Sensor on bare soil or duff layer, no shrub or tall herb vegetation within 1-2 m.",
                  "c1 = Sensor in grass or forb layer that could shade the sensor at least some of the time, , no shrub within 1-2 m",
                  "c2 = Sensor in a small gap or near edge of shrubs that are within 1-2m",
                  "c3 = Sensor underneath an open shrub or small tree canopy",
                  "c4 = Sensor under a dense shrub or small tree canopy",
                  "c5 = Among rocks",
                  "c6 = Among fallen logs")
CategoryText <- paste0("",unlist(CategoryList),collapse="\n")



for (day in 1:365) {
  for (dep in c("Max","Min","DiurnalRange")) {
    
    ObsColumn <- sprintf("Sensor.Day%s.%s",day, dep)
    
    if (ObsColumn %in% colnames(df)) {
      
      foo <- df[c("Point.loc_ID","Point.Site","Indep.GroundCode",ObsColumn)]
      
      names(foo)[names(foo) == ObsColumn] <- "TempResidual"
      
      MaxAbsValue <- ifelse(max(foo$TempResidual) > abs(min(foo$TempResidual)), max(foo$TempResidual), abs(min(foo$TempResidual)))
      
      p1 <- ggplot() + facet_wrap(~Point.Site, ncol=1) +
        geom_point(data=foo, aes(x=Indep.GroundCode, y=TempResidual), size=1.5, shape=3) +
        geom_hline(yintercept=0) +
        scale_y_continuous(limits=c(-MaxAbsValue, MaxAbsValue)) +
        labs(title=sprintf("Day %s\n%s temperature", day, dep), y=sprintf("%s temperature residual from median", dep))+
        theme_bw()
      
      
      p2 <- ggplot() + 
        labs(title = CategoryText) + theme(plot.title = element_text(hjust = 0))
      
      plot <- grid.arrange(arrangeGrob(p1,p2, heights=c(3, 1), ncol=1),
                           ncol=1)
      
      ggsave(file=sprintf("GroundCode~TemperatureResidual_%s_Day%s.png", dep, day), plot, width=10,height=6, dpi=300)
      
    }
  }
}






