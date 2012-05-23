library (raster)
DA_path<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles"
img_files<-list.files(path=DA_path, pattern="*.img$")

#Create empty vectors to hold two tiles to be mosaiced
N<- c(1:366)*NA
val<- c(1:length(img_files))*NA

for (i in 1:366){
   N[i]<- paste("Day",i,"_Tiles_DailyAvgs",sep="")
   assign (N[i],val)
}

#Separate pairs of tiles (h08v04 and h09v04) into vectors
for (i in 1:length(img_files)){
  if (length(grep("^day_1\\_.",img_files[i]) !=0)){
      img_files[i]-> Day1_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_2\\_.",img_files[i]) !=0)){
      img_files[i]-> Day2_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_3\\_.",img_files[i]) !=0)){
      img_files[i]-> Day3_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_4\\_.",img_files[i]) !=0)){
      img_files[i]-> Day4_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_5\\_.",img_files[i]) !=0)){
      img_files[i]-> Day5_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_6\\_.",img_files[i]) !=0)){
      img_files[i]-> Day6_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_7\\_.",img_files[i]) !=0)){
      img_files[i]-> Day7_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_8\\_.",img_files[i]) !=0)){
      img_files[i]-> Day8_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_9\\_.",img_files[i]) !=0)){
      img_files[i]-> Day9_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_10\\_.",img_files[i]) !=0)){
      img_files[i]-> Day10_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_11\\_.",img_files[i]) !=0)){
      img_files[i]-> Day11_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_12\\_.",img_files[i]) !=0)){
      img_files[i]-> Day12_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_13\\_.",img_files[i]) !=0)){
      img_files[i]-> Day13_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_14\\_.",img_files[i]) !=0)){
      img_files[i]-> Day14_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_15\\_.",img_files[i]) !=0)){
      img_files[i]-> Day15_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_16\\_.",img_files[i]) !=0)){
      img_files[i]-> Day16_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_17\\_.",img_files[i]) !=0)){
      img_files[i]-> Day17_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_18\\_.",img_files[i]) !=0)){
      img_files[i]-> Day18_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_19\\_.",img_files[i]) !=0)){
      img_files[i]-> Day19_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_20\\_.",img_files[i]) !=0)){
      img_files[i]-> Day20_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_21\\_.",img_files[i]) !=0)){
      img_files[i]-> Day21_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_22\\_.",img_files[i]) !=0)){
      img_files[i]-> Day22_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_23\\_.",img_files[i]) !=0)){
      img_files[i]-> Day23_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_24\\_.",img_files[i]) !=0)){
      img_files[i]-> Day24_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_25\\_.",img_files[i]) !=0)){
      img_files[i]-> Day25_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_26\\_.",img_files[i]) !=0)){
      img_files[i]-> Day26_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_27\\_.",img_files[i]) !=0)){
      img_files[i]-> Day27_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_28\\_.",img_files[i]) !=0)){
      img_files[i]-> Day28_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_29\\_.",img_files[i]) !=0)){
      img_files[i]-> Day29_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_30\\_.",img_files[i]) !=0)){
      img_files[i]-> Day30_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_31\\_.",img_files[i]) !=0)){
      img_files[i]-> Day31_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_32\\_.",img_files[i]) !=0)){
      img_files[i]-> Day32_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_33\\_.",img_files[i]) !=0)){
      img_files[i]-> Day33_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_34\\_.",img_files[i]) !=0)){
      img_files[i]-> Day34_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_35\\_.",img_files[i]) !=0)){
      img_files[i]-> Day35_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_36\\_.",img_files[i]) !=0)){
      img_files[i]-> Day36_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_37\\_.",img_files[i]) !=0)){
      img_files[i]-> Day37_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_38\\_.",img_files[i]) !=0)){
      img_files[i]-> Day38_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_39\\_.",img_files[i]) !=0)){
      img_files[i]-> Day39_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_40\\_.",img_files[i]) !=0)){
      img_files[i]-> Day40_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_41\\_.",img_files[i]) !=0)){
      img_files[i]-> Day41_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_42\\_.",img_files[i]) !=0)){
      img_files[i]-> Day42_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_43\\_.",img_files[i]) !=0)){
      img_files[i]-> Day43_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_44\\_.",img_files[i]) !=0)){
      img_files[i]-> Day44_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_45\\_.",img_files[i]) !=0)){
      img_files[i]-> Day45_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_46\\_.",img_files[i]) !=0)){
      img_files[i]-> Day46_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_47\\_.",img_files[i]) !=0)){
      img_files[i]-> Day47_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_48\\_.",img_files[i]) !=0)){
      img_files[i]-> Day48_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_49\\_.",img_files[i]) !=0)){
      img_files[i]-> Day49_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_50\\_.",img_files[i]) !=0)){
      img_files[i]-> Day50_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_51\\_.",img_files[i]) !=0)){
      img_files[i]-> Day51_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_52\\_.",img_files[i]) !=0)){
      img_files[i]-> Day52_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_53\\_.",img_files[i]) !=0)){
      img_files[i]-> Day53_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_54\\_.",img_files[i]) !=0)){
      img_files[i]-> Day54_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_55\\_.",img_files[i]) !=0)){
      img_files[i]-> Day55_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_56\\_.",img_files[i]) !=0)){
      img_files[i]-> Day56_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_57\\_.",img_files[i]) !=0)){
      img_files[i]-> Day57_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_58\\_.",img_files[i]) !=0)){
      img_files[i]-> Day58_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_59\\_.",img_files[i]) !=0)){
      img_files[i]-> Day59_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_60\\_.",img_files[i]) !=0)){
      img_files[i]-> Day60_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_61\\_.",img_files[i]) !=0)){
      img_files[i]-> Day61_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_62\\_.",img_files[i]) !=0)){
      img_files[i]-> Day62_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_63\\_.",img_files[i]) !=0)){
      img_files[i]-> Day63_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_64\\_.",img_files[i]) !=0)){
      img_files[i]-> Day64_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_65\\_.",img_files[i]) !=0)){
      img_files[i]-> Day65_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_66\\_.",img_files[i]) !=0)){
      img_files[i]-> Day66_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_67\\_.",img_files[i]) !=0)){
      img_files[i]-> Day67_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_68\\_.",img_files[i]) !=0)){
      img_files[i]-> Day68_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_69\\_.",img_files[i]) !=0)){
      img_files[i]-> Day69_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_70\\_.",img_files[i]) !=0)){
      img_files[i]-> Day70_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_71\\_.",img_files[i]) !=0)){
      img_files[i]-> Day71_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_72\\_.",img_files[i]) !=0)){
      img_files[i]-> Day72_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_73\\_.",img_files[i]) !=0)){
      img_files[i]-> Day73_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_74\\_.",img_files[i]) !=0)){
      img_files[i]-> Day74_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_75\\_.",img_files[i]) !=0)){
      img_files[i]-> Day75_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_76\\_.",img_files[i]) !=0)){
      img_files[i]-> Day76_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_77\\_.",img_files[i]) !=0)){
      img_files[i]-> Day77_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_78\\_.",img_files[i]) !=0)){
      img_files[i]-> Day78_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_79\\_.",img_files[i]) !=0)){
      img_files[i]-> Day79_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_80\\_.",img_files[i]) !=0)){
      img_files[i]-> Day80_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_81\\_.",img_files[i]) !=0)){
      img_files[i]-> Day81_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_82\\_.",img_files[i]) !=0)){
      img_files[i]-> Day82_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_83\\_.",img_files[i]) !=0)){
      img_files[i]-> Day83_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_84\\_.",img_files[i]) !=0)){
      img_files[i]-> Day84_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_85\\_.",img_files[i]) !=0)){
      img_files[i]-> Day85_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_86\\_.",img_files[i]) !=0)){
      img_files[i]-> Day86_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_87\\_.",img_files[i]) !=0)){
      img_files[i]-> Day87_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_88\\_.",img_files[i]) !=0)){
      img_files[i]-> Day88_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_89\\_.",img_files[i]) !=0)){
      img_files[i]-> Day89_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_90\\_.",img_files[i]) !=0)){
      img_files[i]-> Day90_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_91\\_.",img_files[i]) !=0)){
      img_files[i]-> Day91_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_92\\_.",img_files[i]) !=0)){
      img_files[i]-> Day92_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_93\\_.",img_files[i]) !=0)){
      img_files[i]-> Day93_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_94\\_.",img_files[i]) !=0)){
      img_files[i]-> Day94_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_95\\_.",img_files[i]) !=0)){
      img_files[i]-> Day95_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_96\\_.",img_files[i]) !=0)){
      img_files[i]-> Day96_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_97\\_.",img_files[i]) !=0)){
      img_files[i]-> Day97_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_98\\_.",img_files[i]) !=0)){
      img_files[i]-> Day98_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_99\\_.",img_files[i]) !=0)){
      img_files[i]-> Day99_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_100\\_.",img_files[i]) !=0)){
      img_files[i]-> Day100_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_101\\_.",img_files[i]) !=0)){
      img_files[i]-> Day101_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_102\\_.",img_files[i]) !=0)){
      img_files[i]-> Day102_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_103\\_.",img_files[i]) !=0)){
      img_files[i]-> Day103_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_104\\_.",img_files[i]) !=0)){
      img_files[i]-> Day104_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_105\\_.",img_files[i]) !=0)){
      img_files[i]-> Day105_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_106\\_.",img_files[i]) !=0)){
      img_files[i]-> Day106_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_107\\_.",img_files[i]) !=0)){
      img_files[i]-> Day107_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_108\\_.",img_files[i]) !=0)){
      img_files[i]-> Day108_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_109\\_.",img_files[i]) !=0)){
      img_files[i]-> Day109_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_110\\_.",img_files[i]) !=0)){
      img_files[i]-> Day110_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_111\\_.",img_files[i]) !=0)){
      img_files[i]-> Day111_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_112\\_.",img_files[i]) !=0)){
      img_files[i]-> Day112_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_113\\_.",img_files[i]) !=0)){
      img_files[i]-> Day113_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_114\\_.",img_files[i]) !=0)){
      img_files[i]-> Day114_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_115\\_.",img_files[i]) !=0)){
      img_files[i]-> Day115_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_116\\_.",img_files[i]) !=0)){
      img_files[i]-> Day116_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_117\\_.",img_files[i]) !=0)){
      img_files[i]-> Day117_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_118\\_.",img_files[i]) !=0)){
      img_files[i]-> Day118_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_119\\_.",img_files[i]) !=0)){
      img_files[i]-> Day119_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_120\\_.",img_files[i]) !=0)){
      img_files[i]-> Day120_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_121\\_.",img_files[i]) !=0)){
      img_files[i]-> Day121_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_122\\_.",img_files[i]) !=0)){
      img_files[i]-> Day122_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_123\\_.",img_files[i]) !=0)){
      img_files[i]-> Day123_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_124\\_.",img_files[i]) !=0)){
      img_files[i]-> Day124_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_125\\_.",img_files[i]) !=0)){
      img_files[i]-> Day125_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_126\\_.",img_files[i]) !=0)){
      img_files[i]-> Day126_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_127\\_.",img_files[i]) !=0)){
      img_files[i]-> Day127_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_128\\_.",img_files[i]) !=0)){
      img_files[i]-> Day128_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_129\\_.",img_files[i]) !=0)){
      img_files[i]-> Day129_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_130\\_.",img_files[i]) !=0)){
      img_files[i]-> Day130_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_131\\_.",img_files[i]) !=0)){
      img_files[i]-> Day131_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_132\\_.",img_files[i]) !=0)){
      img_files[i]-> Day132_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_133\\_.",img_files[i]) !=0)){
      img_files[i]-> Day133_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_134\\_.",img_files[i]) !=0)){
      img_files[i]-> Day134_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_135\\_.",img_files[i]) !=0)){
      img_files[i]-> Day135_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_136\\_.",img_files[i]) !=0)){
      img_files[i]-> Day136_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_137\\_.",img_files[i]) !=0)){
      img_files[i]-> Day137_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_138\\_.",img_files[i]) !=0)){
      img_files[i]-> Day138_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_139\\_.",img_files[i]) !=0)){
      img_files[i]-> Day139_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_140\\_.",img_files[i]) !=0)){
      img_files[i]-> Day140_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_141\\_.",img_files[i]) !=0)){
      img_files[i]-> Day141_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_142\\_.",img_files[i]) !=0)){
      img_files[i]-> Day142_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_143\\_.",img_files[i]) !=0)){
      img_files[i]-> Day143_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_144\\_.",img_files[i]) !=0)){
      img_files[i]-> Day144_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_145\\_.",img_files[i]) !=0)){
      img_files[i]-> Day145_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_146\\_.",img_files[i]) !=0)){
      img_files[i]-> Day146_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_147\\_.",img_files[i]) !=0)){
      img_files[i]-> Day147_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_148\\_.",img_files[i]) !=0)){
      img_files[i]-> Day148_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_149\\_.",img_files[i]) !=0)){
      img_files[i]-> Day149_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_150\\_.",img_files[i]) !=0)){
      img_files[i]-> Day150_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_151\\_.",img_files[i]) !=0)){
      img_files[i]-> Day151_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_152\\_.",img_files[i]) !=0)){
      img_files[i]-> Day152_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_153\\_.",img_files[i]) !=0)){
      img_files[i]-> Day153_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_154\\_.",img_files[i]) !=0)){
      img_files[i]-> Day154_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_155\\_.",img_files[i]) !=0)){
      img_files[i]-> Day155_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_156\\_.",img_files[i]) !=0)){
      img_files[i]-> Day156_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_157\\_.",img_files[i]) !=0)){
      img_files[i]-> Day157_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_158\\_.",img_files[i]) !=0)){
      img_files[i]-> Day158_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_159\\_.",img_files[i]) !=0)){
      img_files[i]-> Day159_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_160\\_.",img_files[i]) !=0)){
      img_files[i]-> Day160_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_161\\_.",img_files[i]) !=0)){
      img_files[i]-> Day161_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_162\\_.",img_files[i]) !=0)){
      img_files[i]-> Day162_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_163\\_.",img_files[i]) !=0)){
      img_files[i]-> Day163_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_164\\_.",img_files[i]) !=0)){
      img_files[i]-> Day164_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_165\\_.",img_files[i]) !=0)){
      img_files[i]-> Day165_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_166\\_.",img_files[i]) !=0)){
      img_files[i]-> Day166_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_167\\_.",img_files[i]) !=0)){
      img_files[i]-> Day167_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_168\\_.",img_files[i]) !=0)){
      img_files[i]-> Day168_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_169\\_.",img_files[i]) !=0)){
      img_files[i]-> Day169_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_170\\_.",img_files[i]) !=0)){
      img_files[i]-> Day170_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_171\\_.",img_files[i]) !=0)){
      img_files[i]-> Day171_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_172\\_.",img_files[i]) !=0)){
      img_files[i]-> Day172_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_173\\_.",img_files[i]) !=0)){
      img_files[i]-> Day173_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_174\\_.",img_files[i]) !=0)){
      img_files[i]-> Day174_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_175\\_.",img_files[i]) !=0)){
      img_files[i]-> Day175_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_176\\_.",img_files[i]) !=0)){
      img_files[i]-> Day176_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_177\\_.",img_files[i]) !=0)){
      img_files[i]-> Day177_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_178\\_.",img_files[i]) !=0)){
      img_files[i]-> Day178_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_179\\_.",img_files[i]) !=0)){
      img_files[i]-> Day179_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_180\\_.",img_files[i]) !=0)){
      img_files[i]-> Day180_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_181\\_.",img_files[i]) !=0)){
      img_files[i]-> Day181_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_182\\_.",img_files[i]) !=0)){
      img_files[i]-> Day182_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_183\\_.",img_files[i]) !=0)){
      img_files[i]-> Day183_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_184\\_.",img_files[i]) !=0)){
      img_files[i]-> Day184_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_185\\_.",img_files[i]) !=0)){
      img_files[i]-> Day185_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_186\\_.",img_files[i]) !=0)){
      img_files[i]-> Day186_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_187\\_.",img_files[i]) !=0)){
      img_files[i]-> Day187_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_188\\_.",img_files[i]) !=0)){
      img_files[i]-> Day188_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_189\\_.",img_files[i]) !=0)){
      img_files[i]-> Day189_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_190\\_.",img_files[i]) !=0)){
      img_files[i]-> Day190_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_191\\_.",img_files[i]) !=0)){
      img_files[i]-> Day191_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_192\\_.",img_files[i]) !=0)){
      img_files[i]-> Day192_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_193\\_.",img_files[i]) !=0)){
      img_files[i]-> Day193_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_194\\_.",img_files[i]) !=0)){
      img_files[i]-> Day194_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_195\\_.",img_files[i]) !=0)){
      img_files[i]-> Day195_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_196\\_.",img_files[i]) !=0)){
      img_files[i]-> Day196_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_197\\_.",img_files[i]) !=0)){
      img_files[i]-> Day197_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_198\\_.",img_files[i]) !=0)){
      img_files[i]-> Day198_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_199\\_.",img_files[i]) !=0)){
      img_files[i]-> Day199_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_200\\_.",img_files[i]) !=0)){
      img_files[i]-> Day200_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_201\\_.",img_files[i]) !=0)){
      img_files[i]-> Day201_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_202\\_.",img_files[i]) !=0)){
      img_files[i]-> Day202_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_203\\_.",img_files[i]) !=0)){
      img_files[i]-> Day203_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_204\\_.",img_files[i]) !=0)){
      img_files[i]-> Day204_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_205\\_.",img_files[i]) !=0)){
      img_files[i]-> Day205_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_206\\_.",img_files[i]) !=0)){
      img_files[i]-> Day206_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_207\\_.",img_files[i]) !=0)){
      img_files[i]-> Day207_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_208\\_.",img_files[i]) !=0)){
      img_files[i]-> Day208_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_209\\_.",img_files[i]) !=0)){
      img_files[i]-> Day209_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_210\\_.",img_files[i]) !=0)){
      img_files[i]-> Day210_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_211\\_.",img_files[i]) !=0)){
      img_files[i]-> Day211_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_212\\_.",img_files[i]) !=0)){
      img_files[i]-> Day212_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_213\\_.",img_files[i]) !=0)){
      img_files[i]-> Day213_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_214\\_.",img_files[i]) !=0)){
      img_files[i]-> Day214_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_215\\_.",img_files[i]) !=0)){
      img_files[i]-> Day215_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_216\\_.",img_files[i]) !=0)){
      img_files[i]-> Day216_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_217\\_.",img_files[i]) !=0)){
      img_files[i]-> Day217_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_218\\_.",img_files[i]) !=0)){
      img_files[i]-> Day218_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_219\\_.",img_files[i]) !=0)){
      img_files[i]-> Day219_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_220\\_.",img_files[i]) !=0)){
      img_files[i]-> Day220_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_221\\_.",img_files[i]) !=0)){
      img_files[i]-> Day221_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_222\\_.",img_files[i]) !=0)){
      img_files[i]-> Day222_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_223\\_.",img_files[i]) !=0)){
      img_files[i]-> Day223_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_224\\_.",img_files[i]) !=0)){
      img_files[i]-> Day224_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_225\\_.",img_files[i]) !=0)){
      img_files[i]-> Day225_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_226\\_.",img_files[i]) !=0)){
      img_files[i]-> Day226_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_227\\_.",img_files[i]) !=0)){
      img_files[i]-> Day227_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_228\\_.",img_files[i]) !=0)){
      img_files[i]-> Day228_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_229\\_.",img_files[i]) !=0)){
      img_files[i]-> Day229_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_230\\_.",img_files[i]) !=0)){
      img_files[i]-> Day230_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_231\\_.",img_files[i]) !=0)){
      img_files[i]-> Day231_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_232\\_.",img_files[i]) !=0)){
      img_files[i]-> Day232_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_233\\_.",img_files[i]) !=0)){
      img_files[i]-> Day233_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_234\\_.",img_files[i]) !=0)){
      img_files[i]-> Day234_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_235\\_.",img_files[i]) !=0)){
      img_files[i]-> Day235_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_236\\_.",img_files[i]) !=0)){
      img_files[i]-> Day236_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_237\\_.",img_files[i]) !=0)){
      img_files[i]-> Day237_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_238\\_.",img_files[i]) !=0)){
      img_files[i]-> Day238_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_239\\_.",img_files[i]) !=0)){
      img_files[i]-> Day239_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_240\\_.",img_files[i]) !=0)){
      img_files[i]-> Day240_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_241\\_.",img_files[i]) !=0)){
      img_files[i]-> Day241_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_242\\_.",img_files[i]) !=0)){
      img_files[i]-> Day242_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_243\\_.",img_files[i]) !=0)){
      img_files[i]-> Day243_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_244\\_.",img_files[i]) !=0)){
      img_files[i]-> Day244_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_245\\_.",img_files[i]) !=0)){
      img_files[i]-> Day245_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_246\\_.",img_files[i]) !=0)){
      img_files[i]-> Day246_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_247\\_.",img_files[i]) !=0)){
      img_files[i]-> Day247_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_248\\_.",img_files[i]) !=0)){
      img_files[i]-> Day248_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_249\\_.",img_files[i]) !=0)){
      img_files[i]-> Day249_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_250\\_.",img_files[i]) !=0)){
      img_files[i]-> Day250_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_251\\_.",img_files[i]) !=0)){
      img_files[i]-> Day251_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_252\\_.",img_files[i]) !=0)){
      img_files[i]-> Day252_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_253\\_.",img_files[i]) !=0)){
      img_files[i]-> Day253_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_254\\_.",img_files[i]) !=0)){
      img_files[i]-> Day254_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_255\\_.",img_files[i]) !=0)){
      img_files[i]-> Day255_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_256\\_.",img_files[i]) !=0)){
      img_files[i]-> Day256_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_257\\_.",img_files[i]) !=0)){
      img_files[i]-> Day257_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_258\\_.",img_files[i]) !=0)){
      img_files[i]-> Day258_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_259\\_.",img_files[i]) !=0)){
      img_files[i]-> Day259_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_260\\_.",img_files[i]) !=0)){
      img_files[i]-> Day260_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_261\\_.",img_files[i]) !=0)){
      img_files[i]-> Day261_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_262\\_.",img_files[i]) !=0)){
      img_files[i]-> Day262_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_263\\_.",img_files[i]) !=0)){
      img_files[i]-> Day263_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_264\\_.",img_files[i]) !=0)){
      img_files[i]-> Day264_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_265\\_.",img_files[i]) !=0)){
      img_files[i]-> Day265_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_266\\_.",img_files[i]) !=0)){
      img_files[i]-> Day266_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_267\\_.",img_files[i]) !=0)){
      img_files[i]-> Day267_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_268\\_.",img_files[i]) !=0)){
      img_files[i]-> Day268_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_269\\_.",img_files[i]) !=0)){
      img_files[i]-> Day269_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_270\\_.",img_files[i]) !=0)){
      img_files[i]-> Day270_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_271\\_.",img_files[i]) !=0)){
      img_files[i]-> Day271_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_272\\_.",img_files[i]) !=0)){
      img_files[i]-> Day272_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_273\\_.",img_files[i]) !=0)){
      img_files[i]-> Day273_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_274\\_.",img_files[i]) !=0)){
      img_files[i]-> Day274_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_275\\_.",img_files[i]) !=0)){
      img_files[i]-> Day275_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_276\\_.",img_files[i]) !=0)){
      img_files[i]-> Day276_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_277\\_.",img_files[i]) !=0)){
      img_files[i]-> Day277_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_278\\_.",img_files[i]) !=0)){
      img_files[i]-> Day278_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_279\\_.",img_files[i]) !=0)){
      img_files[i]-> Day279_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_280\\_.",img_files[i]) !=0)){
      img_files[i]-> Day280_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_281\\_.",img_files[i]) !=0)){
      img_files[i]-> Day281_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_282\\_.",img_files[i]) !=0)){
      img_files[i]-> Day282_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_283\\_.",img_files[i]) !=0)){
      img_files[i]-> Day283_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_284\\_.",img_files[i]) !=0)){
      img_files[i]-> Day284_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_285\\_.",img_files[i]) !=0)){
      img_files[i]-> Day285_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_286\\_.",img_files[i]) !=0)){
      img_files[i]-> Day286_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_287\\_.",img_files[i]) !=0)){
      img_files[i]-> Day287_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_288\\_.",img_files[i]) !=0)){
      img_files[i]-> Day288_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_289\\_.",img_files[i]) !=0)){
      img_files[i]-> Day289_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_290\\_.",img_files[i]) !=0)){
      img_files[i]-> Day290_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_291\\_.",img_files[i]) !=0)){
      img_files[i]-> Day291_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_292\\_.",img_files[i]) !=0)){
      img_files[i]-> Day292_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_293\\_.",img_files[i]) !=0)){
      img_files[i]-> Day293_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_294\\_.",img_files[i]) !=0)){
      img_files[i]-> Day294_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_295\\_.",img_files[i]) !=0)){
      img_files[i]-> Day295_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_296\\_.",img_files[i]) !=0)){
      img_files[i]-> Day296_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_297\\_.",img_files[i]) !=0)){
      img_files[i]-> Day297_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_298\\_.",img_files[i]) !=0)){
      img_files[i]-> Day298_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_299\\_.",img_files[i]) !=0)){
      img_files[i]-> Day299_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_300\\_.",img_files[i]) !=0)){
      img_files[i]-> Day300_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_301\\_.",img_files[i]) !=0)){
      img_files[i]-> Day301_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_302\\_.",img_files[i]) !=0)){
      img_files[i]-> Day302_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_303\\_.",img_files[i]) !=0)){
      img_files[i]-> Day303_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_304\\_.",img_files[i]) !=0)){
      img_files[i]-> Day304_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_305\\_.",img_files[i]) !=0)){
      img_files[i]-> Day305_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_306\\_.",img_files[i]) !=0)){
      img_files[i]-> Day306_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_307\\_.",img_files[i]) !=0)){
      img_files[i]-> Day307_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_308\\_.",img_files[i]) !=0)){
      img_files[i]-> Day308_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_309\\_.",img_files[i]) !=0)){
      img_files[i]-> Day309_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_310\\_.",img_files[i]) !=0)){
      img_files[i]-> Day310_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_311\\_.",img_files[i]) !=0)){
      img_files[i]-> Day311_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_312\\_.",img_files[i]) !=0)){
      img_files[i]-> Day312_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_313\\_.",img_files[i]) !=0)){
      img_files[i]-> Day313_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_314\\_.",img_files[i]) !=0)){
      img_files[i]-> Day314_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_315\\_.",img_files[i]) !=0)){
      img_files[i]-> Day315_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_316\\_.",img_files[i]) !=0)){
      img_files[i]-> Day316_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_317\\_.",img_files[i]) !=0)){
      img_files[i]-> Day317_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_318\\_.",img_files[i]) !=0)){
      img_files[i]-> Day318_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_319\\_.",img_files[i]) !=0)){
      img_files[i]-> Day319_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_320\\_.",img_files[i]) !=0)){
      img_files[i]-> Day320_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_321\\_.",img_files[i]) !=0)){
      img_files[i]-> Day321_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_322\\_.",img_files[i]) !=0)){
      img_files[i]-> Day322_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_323\\_.",img_files[i]) !=0)){
      img_files[i]-> Day323_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_324\\_.",img_files[i]) !=0)){
      img_files[i]-> Day324_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_325\\_.",img_files[i]) !=0)){
      img_files[i]-> Day325_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_326\\_.",img_files[i]) !=0)){
      img_files[i]-> Day326_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_327\\_.",img_files[i]) !=0)){
      img_files[i]-> Day327_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_328\\_.",img_files[i]) !=0)){
      img_files[i]-> Day328_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_329\\_.",img_files[i]) !=0)){
      img_files[i]-> Day329_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_330\\_.",img_files[i]) !=0)){
      img_files[i]-> Day330_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_331\\_.",img_files[i]) !=0)){
      img_files[i]-> Day331_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_332\\_.",img_files[i]) !=0)){
      img_files[i]-> Day332_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_333\\_.",img_files[i]) !=0)){
      img_files[i]-> Day333_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_334\\_.",img_files[i]) !=0)){
      img_files[i]-> Day334_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_335\\_.",img_files[i]) !=0)){
      img_files[i]-> Day335_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_336\\_.",img_files[i]) !=0)){
      img_files[i]-> Day336_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_337\\_.",img_files[i]) !=0)){
      img_files[i]-> Day337_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_338\\_.",img_files[i]) !=0)){
      img_files[i]-> Day338_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_339\\_.",img_files[i]) !=0)){
      img_files[i]-> Day339_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_340\\_.",img_files[i]) !=0)){
      img_files[i]-> Day340_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_341\\_.",img_files[i]) !=0)){
      img_files[i]-> Day341_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_342\\_.",img_files[i]) !=0)){
      img_files[i]-> Day342_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_343\\_.",img_files[i]) !=0)){
      img_files[i]-> Day343_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_344\\_.",img_files[i]) !=0)){
      img_files[i]-> Day344_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_345\\_.",img_files[i]) !=0)){
      img_files[i]-> Day345_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_346\\_.",img_files[i]) !=0)){
      img_files[i]-> Day346_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_347\\_.",img_files[i]) !=0)){
      img_files[i]-> Day347_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_348\\_.",img_files[i]) !=0)){
      img_files[i]-> Day348_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_349\\_.",img_files[i]) !=0)){
      img_files[i]-> Day349_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_350\\_.",img_files[i]) !=0)){
      img_files[i]-> Day350_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_351\\_.",img_files[i]) !=0)){
      img_files[i]-> Day351_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_352\\_.",img_files[i]) !=0)){
      img_files[i]-> Day352_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_353\\_.",img_files[i]) !=0)){
      img_files[i]-> Day353_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_354\\_.",img_files[i]) !=0)){
      img_files[i]-> Day354_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_355\\_.",img_files[i]) !=0)){
      img_files[i]-> Day355_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_356\\_.",img_files[i]) !=0)){
      img_files[i]-> Day356_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_357\\_.",img_files[i]) !=0)){
      img_files[i]-> Day357_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_358\\_.",img_files[i]) !=0)){
      img_files[i]-> Day358_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_359\\_.",img_files[i]) !=0)){
      img_files[i]-> Day359_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_360\\_.",img_files[i]) !=0)){
      img_files[i]-> Day360_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_361\\_.",img_files[i]) !=0)){
      img_files[i]-> Day361_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_362\\_.",img_files[i]) !=0)){
      img_files[i]-> Day362_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_363\\_.",img_files[i]) !=0)){
      img_files[i]-> Day363_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_364\\_.",img_files[i]) !=0)){
      img_files[i]-> Day364_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_365\\_.",img_files[i]) !=0)){
      img_files[i]-> Day365_Tiles_DailyAvgs[i]
  }else if (length(grep("^day_366\\_.",img_files[i]) !=0)){
      img_files[i]-> Day366_Tiles_DailyAvgs[i]
  }
}
          
      
#Get rid of NA's and add full path name to each file
Day1_Tiles_DailyAvgs<-Day1_Tiles_DailyAvgs[!is.na(Day1_Tiles_DailyAvgs)]
Day1_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day1_Tiles_DailyAvgs,sep="")
Day2_Tiles_DailyAvgs<-Day2_Tiles_DailyAvgs[!is.na(Day2_Tiles_DailyAvgs)]
Day2_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day2_Tiles_DailyAvgs,sep="")
Day3_Tiles_DailyAvgs<-Day3_Tiles_DailyAvgs[!is.na(Day3_Tiles_DailyAvgs)]
Day3_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day3_Tiles_DailyAvgs,sep="")
Day4_Tiles_DailyAvgs<-Day4_Tiles_DailyAvgs[!is.na(Day4_Tiles_DailyAvgs)]
Day4_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day4_Tiles_DailyAvgs,sep="")
Day5_Tiles_DailyAvgs<-Day5_Tiles_DailyAvgs[!is.na(Day5_Tiles_DailyAvgs)]
Day5_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day5_Tiles_DailyAvgs,sep="")
Day6_Tiles_DailyAvgs<-Day6_Tiles_DailyAvgs[!is.na(Day6_Tiles_DailyAvgs)]
Day6_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day6_Tiles_DailyAvgs,sep="")
Day7_Tiles_DailyAvgs<-Day7_Tiles_DailyAvgs[!is.na(Day7_Tiles_DailyAvgs)]
Day7_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day7_Tiles_DailyAvgs,sep="")
Day8_Tiles_DailyAvgs<-Day8_Tiles_DailyAvgs[!is.na(Day8_Tiles_DailyAvgs)]
Day8_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day8_Tiles_DailyAvgs,sep="")
Day9_Tiles_DailyAvgs<-Day9_Tiles_DailyAvgs[!is.na(Day9_Tiles_DailyAvgs)]
Day9_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day9_Tiles_DailyAvgs,sep="")
Day10_Tiles_DailyAvgs<-Day10_Tiles_DailyAvgs[!is.na(Day10_Tiles_DailyAvgs)]
Day10_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day10_Tiles_DailyAvgs,sep="")
Day11_Tiles_DailyAvgs<-Day11_Tiles_DailyAvgs[!is.na(Day11_Tiles_DailyAvgs)]
Day11_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day11_Tiles_DailyAvgs,sep="")
Day12_Tiles_DailyAvgs<-Day12_Tiles_DailyAvgs[!is.na(Day12_Tiles_DailyAvgs)]
Day12_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day12_Tiles_DailyAvgs,sep="")
Day13_Tiles_DailyAvgs<-Day13_Tiles_DailyAvgs[!is.na(Day13_Tiles_DailyAvgs)]
Day13_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day13_Tiles_DailyAvgs,sep="")
Day14_Tiles_DailyAvgs<-Day14_Tiles_DailyAvgs[!is.na(Day14_Tiles_DailyAvgs)]
Day14_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day14_Tiles_DailyAvgs,sep="")
Day15_Tiles_DailyAvgs<-Day15_Tiles_DailyAvgs[!is.na(Day15_Tiles_DailyAvgs)]
Day15_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day15_Tiles_DailyAvgs,sep="")
Day16_Tiles_DailyAvgs<-Day16_Tiles_DailyAvgs[!is.na(Day16_Tiles_DailyAvgs)]
Day16_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day16_Tiles_DailyAvgs,sep="")
Day17_Tiles_DailyAvgs<-Day17_Tiles_DailyAvgs[!is.na(Day17_Tiles_DailyAvgs)]
Day17_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day17_Tiles_DailyAvgs,sep="")
Day18_Tiles_DailyAvgs<-Day18_Tiles_DailyAvgs[!is.na(Day18_Tiles_DailyAvgs)]
Day18_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day18_Tiles_DailyAvgs,sep="")
Day19_Tiles_DailyAvgs<-Day19_Tiles_DailyAvgs[!is.na(Day19_Tiles_DailyAvgs)]
Day19_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day19_Tiles_DailyAvgs,sep="")
Day20_Tiles_DailyAvgs<-Day20_Tiles_DailyAvgs[!is.na(Day20_Tiles_DailyAvgs)]
Day20_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day20_Tiles_DailyAvgs,sep="")
Day21_Tiles_DailyAvgs<-Day21_Tiles_DailyAvgs[!is.na(Day21_Tiles_DailyAvgs)]
Day21_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day21_Tiles_DailyAvgs,sep="")
Day22_Tiles_DailyAvgs<-Day22_Tiles_DailyAvgs[!is.na(Day22_Tiles_DailyAvgs)]
Day22_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day22_Tiles_DailyAvgs,sep="")
Day23_Tiles_DailyAvgs<-Day23_Tiles_DailyAvgs[!is.na(Day23_Tiles_DailyAvgs)]
Day23_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day23_Tiles_DailyAvgs,sep="")
Day24_Tiles_DailyAvgs<-Day24_Tiles_DailyAvgs[!is.na(Day24_Tiles_DailyAvgs)]
Day24_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day24_Tiles_DailyAvgs,sep="")
Day25_Tiles_DailyAvgs<-Day25_Tiles_DailyAvgs[!is.na(Day25_Tiles_DailyAvgs)]
Day25_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day25_Tiles_DailyAvgs,sep="")
Day26_Tiles_DailyAvgs<-Day26_Tiles_DailyAvgs[!is.na(Day26_Tiles_DailyAvgs)]
Day26_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day26_Tiles_DailyAvgs,sep="")
Day27_Tiles_DailyAvgs<-Day27_Tiles_DailyAvgs[!is.na(Day27_Tiles_DailyAvgs)]
Day27_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day27_Tiles_DailyAvgs,sep="")
Day28_Tiles_DailyAvgs<-Day28_Tiles_DailyAvgs[!is.na(Day28_Tiles_DailyAvgs)]
Day28_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day28_Tiles_DailyAvgs,sep="")
Day29_Tiles_DailyAvgs<-Day29_Tiles_DailyAvgs[!is.na(Day29_Tiles_DailyAvgs)]
Day29_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day29_Tiles_DailyAvgs,sep="")
Day30_Tiles_DailyAvgs<-Day30_Tiles_DailyAvgs[!is.na(Day30_Tiles_DailyAvgs)]
Day30_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day30_Tiles_DailyAvgs,sep="")
Day31_Tiles_DailyAvgs<-Day31_Tiles_DailyAvgs[!is.na(Day31_Tiles_DailyAvgs)]
Day31_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day31_Tiles_DailyAvgs,sep="")
Day32_Tiles_DailyAvgs<-Day32_Tiles_DailyAvgs[!is.na(Day32_Tiles_DailyAvgs)]
Day32_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day32_Tiles_DailyAvgs,sep="")
Day33_Tiles_DailyAvgs<-Day33_Tiles_DailyAvgs[!is.na(Day33_Tiles_DailyAvgs)]
Day33_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day33_Tiles_DailyAvgs,sep="")
Day34_Tiles_DailyAvgs<-Day34_Tiles_DailyAvgs[!is.na(Day34_Tiles_DailyAvgs)]
Day34_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day34_Tiles_DailyAvgs,sep="")
Day35_Tiles_DailyAvgs<-Day35_Tiles_DailyAvgs[!is.na(Day35_Tiles_DailyAvgs)]
Day35_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day35_Tiles_DailyAvgs,sep="")
Day36_Tiles_DailyAvgs<-Day36_Tiles_DailyAvgs[!is.na(Day36_Tiles_DailyAvgs)]
Day36_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day36_Tiles_DailyAvgs,sep="")
Day37_Tiles_DailyAvgs<-Day37_Tiles_DailyAvgs[!is.na(Day37_Tiles_DailyAvgs)]
Day37_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day37_Tiles_DailyAvgs,sep="")
Day38_Tiles_DailyAvgs<-Day38_Tiles_DailyAvgs[!is.na(Day38_Tiles_DailyAvgs)]
Day38_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day38_Tiles_DailyAvgs,sep="")
Day39_Tiles_DailyAvgs<-Day39_Tiles_DailyAvgs[!is.na(Day39_Tiles_DailyAvgs)]
Day39_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day39_Tiles_DailyAvgs,sep="")
Day40_Tiles_DailyAvgs<-Day40_Tiles_DailyAvgs[!is.na(Day40_Tiles_DailyAvgs)]
Day40_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day40_Tiles_DailyAvgs,sep="")
Day41_Tiles_DailyAvgs<-Day41_Tiles_DailyAvgs[!is.na(Day41_Tiles_DailyAvgs)]
Day41_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day41_Tiles_DailyAvgs,sep="")
Day42_Tiles_DailyAvgs<-Day42_Tiles_DailyAvgs[!is.na(Day42_Tiles_DailyAvgs)]
Day42_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day42_Tiles_DailyAvgs,sep="")
Day43_Tiles_DailyAvgs<-Day43_Tiles_DailyAvgs[!is.na(Day43_Tiles_DailyAvgs)]
Day43_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day43_Tiles_DailyAvgs,sep="")
Day44_Tiles_DailyAvgs<-Day44_Tiles_DailyAvgs[!is.na(Day44_Tiles_DailyAvgs)]
Day44_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day44_Tiles_DailyAvgs,sep="")
Day45_Tiles_DailyAvgs<-Day45_Tiles_DailyAvgs[!is.na(Day45_Tiles_DailyAvgs)]
Day45_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day45_Tiles_DailyAvgs,sep="")
Day46_Tiles_DailyAvgs<-Day46_Tiles_DailyAvgs[!is.na(Day46_Tiles_DailyAvgs)]
Day46_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day46_Tiles_DailyAvgs,sep="")
Day47_Tiles_DailyAvgs<-Day47_Tiles_DailyAvgs[!is.na(Day47_Tiles_DailyAvgs)]
Day47_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day47_Tiles_DailyAvgs,sep="")
Day48_Tiles_DailyAvgs<-Day48_Tiles_DailyAvgs[!is.na(Day48_Tiles_DailyAvgs)]
Day48_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day48_Tiles_DailyAvgs,sep="")
Day49_Tiles_DailyAvgs<-Day49_Tiles_DailyAvgs[!is.na(Day49_Tiles_DailyAvgs)]
Day49_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day49_Tiles_DailyAvgs,sep="")
Day50_Tiles_DailyAvgs<-Day50_Tiles_DailyAvgs[!is.na(Day50_Tiles_DailyAvgs)]
Day50_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day50_Tiles_DailyAvgs,sep="")
Day51_Tiles_DailyAvgs<-Day51_Tiles_DailyAvgs[!is.na(Day51_Tiles_DailyAvgs)]
Day51_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day51_Tiles_DailyAvgs,sep="")
Day52_Tiles_DailyAvgs<-Day52_Tiles_DailyAvgs[!is.na(Day52_Tiles_DailyAvgs)]
Day52_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day52_Tiles_DailyAvgs,sep="")
Day53_Tiles_DailyAvgs<-Day53_Tiles_DailyAvgs[!is.na(Day53_Tiles_DailyAvgs)]
Day53_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day53_Tiles_DailyAvgs,sep="")
Day54_Tiles_DailyAvgs<-Day54_Tiles_DailyAvgs[!is.na(Day54_Tiles_DailyAvgs)]
Day54_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day54_Tiles_DailyAvgs,sep="")
Day55_Tiles_DailyAvgs<-Day55_Tiles_DailyAvgs[!is.na(Day55_Tiles_DailyAvgs)]
Day55_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day55_Tiles_DailyAvgs,sep="")
Day56_Tiles_DailyAvgs<-Day56_Tiles_DailyAvgs[!is.na(Day56_Tiles_DailyAvgs)]
Day56_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day56_Tiles_DailyAvgs,sep="")
Day57_Tiles_DailyAvgs<-Day57_Tiles_DailyAvgs[!is.na(Day57_Tiles_DailyAvgs)]
Day57_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day57_Tiles_DailyAvgs,sep="")
Day58_Tiles_DailyAvgs<-Day58_Tiles_DailyAvgs[!is.na(Day58_Tiles_DailyAvgs)]
Day58_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day58_Tiles_DailyAvgs,sep="")
Day59_Tiles_DailyAvgs<-Day59_Tiles_DailyAvgs[!is.na(Day59_Tiles_DailyAvgs)]
Day59_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day59_Tiles_DailyAvgs,sep="")
Day60_Tiles_DailyAvgs<-Day60_Tiles_DailyAvgs[!is.na(Day60_Tiles_DailyAvgs)]
Day60_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day60_Tiles_DailyAvgs,sep="")
Day61_Tiles_DailyAvgs<-Day61_Tiles_DailyAvgs[!is.na(Day61_Tiles_DailyAvgs)]
Day61_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day61_Tiles_DailyAvgs,sep="")
Day62_Tiles_DailyAvgs<-Day62_Tiles_DailyAvgs[!is.na(Day62_Tiles_DailyAvgs)]
Day62_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day62_Tiles_DailyAvgs,sep="")
Day63_Tiles_DailyAvgs<-Day63_Tiles_DailyAvgs[!is.na(Day63_Tiles_DailyAvgs)]
Day63_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day63_Tiles_DailyAvgs,sep="")
Day64_Tiles_DailyAvgs<-Day64_Tiles_DailyAvgs[!is.na(Day64_Tiles_DailyAvgs)]
Day64_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day64_Tiles_DailyAvgs,sep="")
Day65_Tiles_DailyAvgs<-Day65_Tiles_DailyAvgs[!is.na(Day65_Tiles_DailyAvgs)]
Day65_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day65_Tiles_DailyAvgs,sep="")
Day66_Tiles_DailyAvgs<-Day66_Tiles_DailyAvgs[!is.na(Day66_Tiles_DailyAvgs)]
Day66_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day66_Tiles_DailyAvgs,sep="")
Day67_Tiles_DailyAvgs<-Day67_Tiles_DailyAvgs[!is.na(Day67_Tiles_DailyAvgs)]
Day67_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day67_Tiles_DailyAvgs,sep="")
Day68_Tiles_DailyAvgs<-Day68_Tiles_DailyAvgs[!is.na(Day68_Tiles_DailyAvgs)]
Day68_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day68_Tiles_DailyAvgs,sep="")
Day69_Tiles_DailyAvgs<-Day69_Tiles_DailyAvgs[!is.na(Day69_Tiles_DailyAvgs)]
Day69_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day69_Tiles_DailyAvgs,sep="")
Day70_Tiles_DailyAvgs<-Day70_Tiles_DailyAvgs[!is.na(Day70_Tiles_DailyAvgs)]
Day70_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day70_Tiles_DailyAvgs,sep="")
Day71_Tiles_DailyAvgs<-Day71_Tiles_DailyAvgs[!is.na(Day71_Tiles_DailyAvgs)]
Day71_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day71_Tiles_DailyAvgs,sep="")
Day72_Tiles_DailyAvgs<-Day72_Tiles_DailyAvgs[!is.na(Day72_Tiles_DailyAvgs)]
Day72_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day72_Tiles_DailyAvgs,sep="")
Day73_Tiles_DailyAvgs<-Day73_Tiles_DailyAvgs[!is.na(Day73_Tiles_DailyAvgs)]
Day73_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day73_Tiles_DailyAvgs,sep="")
Day74_Tiles_DailyAvgs<-Day74_Tiles_DailyAvgs[!is.na(Day74_Tiles_DailyAvgs)]
Day74_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day74_Tiles_DailyAvgs,sep="")
Day75_Tiles_DailyAvgs<-Day75_Tiles_DailyAvgs[!is.na(Day75_Tiles_DailyAvgs)]
Day75_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day75_Tiles_DailyAvgs,sep="")
Day76_Tiles_DailyAvgs<-Day76_Tiles_DailyAvgs[!is.na(Day76_Tiles_DailyAvgs)]
Day76_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day76_Tiles_DailyAvgs,sep="")
Day77_Tiles_DailyAvgs<-Day77_Tiles_DailyAvgs[!is.na(Day77_Tiles_DailyAvgs)]
Day77_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day77_Tiles_DailyAvgs,sep="")
Day78_Tiles_DailyAvgs<-Day78_Tiles_DailyAvgs[!is.na(Day78_Tiles_DailyAvgs)]
Day78_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day78_Tiles_DailyAvgs,sep="")
Day79_Tiles_DailyAvgs<-Day79_Tiles_DailyAvgs[!is.na(Day79_Tiles_DailyAvgs)]
Day79_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day79_Tiles_DailyAvgs,sep="")
Day80_Tiles_DailyAvgs<-Day80_Tiles_DailyAvgs[!is.na(Day80_Tiles_DailyAvgs)]
Day80_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day80_Tiles_DailyAvgs,sep="")
Day81_Tiles_DailyAvgs<-Day81_Tiles_DailyAvgs[!is.na(Day81_Tiles_DailyAvgs)]
Day81_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day81_Tiles_DailyAvgs,sep="")
Day82_Tiles_DailyAvgs<-Day82_Tiles_DailyAvgs[!is.na(Day82_Tiles_DailyAvgs)]
Day82_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day82_Tiles_DailyAvgs,sep="")
Day83_Tiles_DailyAvgs<-Day83_Tiles_DailyAvgs[!is.na(Day83_Tiles_DailyAvgs)]
Day83_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day83_Tiles_DailyAvgs,sep="")
Day84_Tiles_DailyAvgs<-Day84_Tiles_DailyAvgs[!is.na(Day84_Tiles_DailyAvgs)]
Day84_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day84_Tiles_DailyAvgs,sep="")
Day85_Tiles_DailyAvgs<-Day85_Tiles_DailyAvgs[!is.na(Day85_Tiles_DailyAvgs)]
Day85_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day85_Tiles_DailyAvgs,sep="")
Day86_Tiles_DailyAvgs<-Day86_Tiles_DailyAvgs[!is.na(Day86_Tiles_DailyAvgs)]
Day86_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day86_Tiles_DailyAvgs,sep="")
Day87_Tiles_DailyAvgs<-Day87_Tiles_DailyAvgs[!is.na(Day87_Tiles_DailyAvgs)]
Day87_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day87_Tiles_DailyAvgs,sep="")
Day88_Tiles_DailyAvgs<-Day88_Tiles_DailyAvgs[!is.na(Day88_Tiles_DailyAvgs)]
Day88_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day88_Tiles_DailyAvgs,sep="")
Day89_Tiles_DailyAvgs<-Day89_Tiles_DailyAvgs[!is.na(Day89_Tiles_DailyAvgs)]
Day89_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day89_Tiles_DailyAvgs,sep="")
Day90_Tiles_DailyAvgs<-Day90_Tiles_DailyAvgs[!is.na(Day90_Tiles_DailyAvgs)]
Day90_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day90_Tiles_DailyAvgs,sep="")
Day91_Tiles_DailyAvgs<-Day91_Tiles_DailyAvgs[!is.na(Day91_Tiles_DailyAvgs)]
Day91_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day91_Tiles_DailyAvgs,sep="")
Day92_Tiles_DailyAvgs<-Day92_Tiles_DailyAvgs[!is.na(Day92_Tiles_DailyAvgs)]
Day92_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day92_Tiles_DailyAvgs,sep="")
Day93_Tiles_DailyAvgs<-Day93_Tiles_DailyAvgs[!is.na(Day93_Tiles_DailyAvgs)]
Day93_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day93_Tiles_DailyAvgs,sep="")
Day94_Tiles_DailyAvgs<-Day94_Tiles_DailyAvgs[!is.na(Day94_Tiles_DailyAvgs)]
Day94_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day94_Tiles_DailyAvgs,sep="")
Day95_Tiles_DailyAvgs<-Day95_Tiles_DailyAvgs[!is.na(Day95_Tiles_DailyAvgs)]
Day95_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day95_Tiles_DailyAvgs,sep="")
Day96_Tiles_DailyAvgs<-Day96_Tiles_DailyAvgs[!is.na(Day96_Tiles_DailyAvgs)]
Day96_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day96_Tiles_DailyAvgs,sep="")
Day97_Tiles_DailyAvgs<-Day97_Tiles_DailyAvgs[!is.na(Day97_Tiles_DailyAvgs)]
Day97_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day97_Tiles_DailyAvgs,sep="")
Day98_Tiles_DailyAvgs<-Day98_Tiles_DailyAvgs[!is.na(Day98_Tiles_DailyAvgs)]
Day98_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day98_Tiles_DailyAvgs,sep="")
Day99_Tiles_DailyAvgs<-Day99_Tiles_DailyAvgs[!is.na(Day99_Tiles_DailyAvgs)]
Day99_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day99_Tiles_DailyAvgs,sep="")
Day100_Tiles_DailyAvgs<-Day100_Tiles_DailyAvgs[!is.na(Day100_Tiles_DailyAvgs)]
Day100_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day100_Tiles_DailyAvgs,sep="")
Day101_Tiles_DailyAvgs<-Day101_Tiles_DailyAvgs[!is.na(Day101_Tiles_DailyAvgs)]
Day101_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day101_Tiles_DailyAvgs,sep="")
Day102_Tiles_DailyAvgs<-Day102_Tiles_DailyAvgs[!is.na(Day102_Tiles_DailyAvgs)]
Day102_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day102_Tiles_DailyAvgs,sep="")
Day103_Tiles_DailyAvgs<-Day103_Tiles_DailyAvgs[!is.na(Day103_Tiles_DailyAvgs)]
Day103_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day103_Tiles_DailyAvgs,sep="")
Day104_Tiles_DailyAvgs<-Day104_Tiles_DailyAvgs[!is.na(Day104_Tiles_DailyAvgs)]
Day104_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day104_Tiles_DailyAvgs,sep="")
Day105_Tiles_DailyAvgs<-Day105_Tiles_DailyAvgs[!is.na(Day105_Tiles_DailyAvgs)]
Day105_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day105_Tiles_DailyAvgs,sep="")
Day106_Tiles_DailyAvgs<-Day106_Tiles_DailyAvgs[!is.na(Day106_Tiles_DailyAvgs)]
Day106_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day106_Tiles_DailyAvgs,sep="")
Day107_Tiles_DailyAvgs<-Day107_Tiles_DailyAvgs[!is.na(Day107_Tiles_DailyAvgs)]
Day107_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day107_Tiles_DailyAvgs,sep="")
Day108_Tiles_DailyAvgs<-Day108_Tiles_DailyAvgs[!is.na(Day108_Tiles_DailyAvgs)]
Day108_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day108_Tiles_DailyAvgs,sep="")
Day109_Tiles_DailyAvgs<-Day109_Tiles_DailyAvgs[!is.na(Day109_Tiles_DailyAvgs)]
Day109_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day109_Tiles_DailyAvgs,sep="")
Day110_Tiles_DailyAvgs<-Day110_Tiles_DailyAvgs[!is.na(Day110_Tiles_DailyAvgs)]
Day110_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day110_Tiles_DailyAvgs,sep="")
Day111_Tiles_DailyAvgs<-Day111_Tiles_DailyAvgs[!is.na(Day111_Tiles_DailyAvgs)]
Day111_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day111_Tiles_DailyAvgs,sep="")
Day112_Tiles_DailyAvgs<-Day112_Tiles_DailyAvgs[!is.na(Day112_Tiles_DailyAvgs)]
Day112_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day112_Tiles_DailyAvgs,sep="")
Day113_Tiles_DailyAvgs<-Day113_Tiles_DailyAvgs[!is.na(Day113_Tiles_DailyAvgs)]
Day113_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day113_Tiles_DailyAvgs,sep="")
Day114_Tiles_DailyAvgs<-Day114_Tiles_DailyAvgs[!is.na(Day114_Tiles_DailyAvgs)]
Day114_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day114_Tiles_DailyAvgs,sep="")
Day115_Tiles_DailyAvgs<-Day115_Tiles_DailyAvgs[!is.na(Day115_Tiles_DailyAvgs)]
Day115_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day115_Tiles_DailyAvgs,sep="")
Day116_Tiles_DailyAvgs<-Day116_Tiles_DailyAvgs[!is.na(Day116_Tiles_DailyAvgs)]
Day116_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day116_Tiles_DailyAvgs,sep="")
Day117_Tiles_DailyAvgs<-Day117_Tiles_DailyAvgs[!is.na(Day117_Tiles_DailyAvgs)]
Day117_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day117_Tiles_DailyAvgs,sep="")
Day118_Tiles_DailyAvgs<-Day118_Tiles_DailyAvgs[!is.na(Day118_Tiles_DailyAvgs)]
Day118_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day118_Tiles_DailyAvgs,sep="")
Day119_Tiles_DailyAvgs<-Day119_Tiles_DailyAvgs[!is.na(Day119_Tiles_DailyAvgs)]
Day119_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day119_Tiles_DailyAvgs,sep="")
Day120_Tiles_DailyAvgs<-Day120_Tiles_DailyAvgs[!is.na(Day120_Tiles_DailyAvgs)]
Day120_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day120_Tiles_DailyAvgs,sep="")
Day121_Tiles_DailyAvgs<-Day121_Tiles_DailyAvgs[!is.na(Day121_Tiles_DailyAvgs)]
Day121_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day121_Tiles_DailyAvgs,sep="")
Day122_Tiles_DailyAvgs<-Day122_Tiles_DailyAvgs[!is.na(Day122_Tiles_DailyAvgs)]
Day122_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day122_Tiles_DailyAvgs,sep="")
Day123_Tiles_DailyAvgs<-Day123_Tiles_DailyAvgs[!is.na(Day123_Tiles_DailyAvgs)]
Day123_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day123_Tiles_DailyAvgs,sep="")
Day124_Tiles_DailyAvgs<-Day124_Tiles_DailyAvgs[!is.na(Day124_Tiles_DailyAvgs)]
Day124_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day124_Tiles_DailyAvgs,sep="")
Day125_Tiles_DailyAvgs<-Day125_Tiles_DailyAvgs[!is.na(Day125_Tiles_DailyAvgs)]
Day125_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day125_Tiles_DailyAvgs,sep="")
Day126_Tiles_DailyAvgs<-Day126_Tiles_DailyAvgs[!is.na(Day126_Tiles_DailyAvgs)]
Day126_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day126_Tiles_DailyAvgs,sep="")
Day127_Tiles_DailyAvgs<-Day127_Tiles_DailyAvgs[!is.na(Day127_Tiles_DailyAvgs)]
Day127_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day127_Tiles_DailyAvgs,sep="")
Day128_Tiles_DailyAvgs<-Day128_Tiles_DailyAvgs[!is.na(Day128_Tiles_DailyAvgs)]
Day128_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day128_Tiles_DailyAvgs,sep="")
Day129_Tiles_DailyAvgs<-Day129_Tiles_DailyAvgs[!is.na(Day129_Tiles_DailyAvgs)]
Day129_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day129_Tiles_DailyAvgs,sep="")
Day130_Tiles_DailyAvgs<-Day130_Tiles_DailyAvgs[!is.na(Day130_Tiles_DailyAvgs)]
Day130_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day130_Tiles_DailyAvgs,sep="")
Day131_Tiles_DailyAvgs<-Day131_Tiles_DailyAvgs[!is.na(Day131_Tiles_DailyAvgs)]
Day131_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day131_Tiles_DailyAvgs,sep="")
Day132_Tiles_DailyAvgs<-Day132_Tiles_DailyAvgs[!is.na(Day132_Tiles_DailyAvgs)]
Day132_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day132_Tiles_DailyAvgs,sep="")
Day133_Tiles_DailyAvgs<-Day133_Tiles_DailyAvgs[!is.na(Day133_Tiles_DailyAvgs)]
Day133_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day133_Tiles_DailyAvgs,sep="")
Day134_Tiles_DailyAvgs<-Day134_Tiles_DailyAvgs[!is.na(Day134_Tiles_DailyAvgs)]
Day134_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day134_Tiles_DailyAvgs,sep="")
Day135_Tiles_DailyAvgs<-Day135_Tiles_DailyAvgs[!is.na(Day135_Tiles_DailyAvgs)]
Day135_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day135_Tiles_DailyAvgs,sep="")
Day136_Tiles_DailyAvgs<-Day136_Tiles_DailyAvgs[!is.na(Day136_Tiles_DailyAvgs)]
Day136_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day136_Tiles_DailyAvgs,sep="")
Day137_Tiles_DailyAvgs<-Day137_Tiles_DailyAvgs[!is.na(Day137_Tiles_DailyAvgs)]
Day137_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day137_Tiles_DailyAvgs,sep="")
Day138_Tiles_DailyAvgs<-Day138_Tiles_DailyAvgs[!is.na(Day138_Tiles_DailyAvgs)]
Day138_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day138_Tiles_DailyAvgs,sep="")
Day139_Tiles_DailyAvgs<-Day139_Tiles_DailyAvgs[!is.na(Day139_Tiles_DailyAvgs)]
Day139_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day139_Tiles_DailyAvgs,sep="")
Day140_Tiles_DailyAvgs<-Day140_Tiles_DailyAvgs[!is.na(Day140_Tiles_DailyAvgs)]
Day140_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day140_Tiles_DailyAvgs,sep="")
Day141_Tiles_DailyAvgs<-Day141_Tiles_DailyAvgs[!is.na(Day141_Tiles_DailyAvgs)]
Day141_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day141_Tiles_DailyAvgs,sep="")
Day142_Tiles_DailyAvgs<-Day142_Tiles_DailyAvgs[!is.na(Day142_Tiles_DailyAvgs)]
Day142_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day142_Tiles_DailyAvgs,sep="")
Day143_Tiles_DailyAvgs<-Day143_Tiles_DailyAvgs[!is.na(Day143_Tiles_DailyAvgs)]
Day143_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day143_Tiles_DailyAvgs,sep="")
Day144_Tiles_DailyAvgs<-Day144_Tiles_DailyAvgs[!is.na(Day144_Tiles_DailyAvgs)]
Day144_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day144_Tiles_DailyAvgs,sep="")
Day145_Tiles_DailyAvgs<-Day145_Tiles_DailyAvgs[!is.na(Day145_Tiles_DailyAvgs)]
Day145_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day145_Tiles_DailyAvgs,sep="")
Day146_Tiles_DailyAvgs<-Day146_Tiles_DailyAvgs[!is.na(Day146_Tiles_DailyAvgs)]
Day146_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day146_Tiles_DailyAvgs,sep="")
Day147_Tiles_DailyAvgs<-Day147_Tiles_DailyAvgs[!is.na(Day147_Tiles_DailyAvgs)]
Day147_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day147_Tiles_DailyAvgs,sep="")
Day148_Tiles_DailyAvgs<-Day148_Tiles_DailyAvgs[!is.na(Day148_Tiles_DailyAvgs)]
Day148_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day148_Tiles_DailyAvgs,sep="")
Day149_Tiles_DailyAvgs<-Day149_Tiles_DailyAvgs[!is.na(Day149_Tiles_DailyAvgs)]
Day149_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day149_Tiles_DailyAvgs,sep="")
Day150_Tiles_DailyAvgs<-Day150_Tiles_DailyAvgs[!is.na(Day150_Tiles_DailyAvgs)]
Day150_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day150_Tiles_DailyAvgs,sep="")
Day151_Tiles_DailyAvgs<-Day151_Tiles_DailyAvgs[!is.na(Day151_Tiles_DailyAvgs)]
Day151_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day151_Tiles_DailyAvgs,sep="")
Day152_Tiles_DailyAvgs<-Day152_Tiles_DailyAvgs[!is.na(Day152_Tiles_DailyAvgs)]
Day152_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day152_Tiles_DailyAvgs,sep="")
Day153_Tiles_DailyAvgs<-Day153_Tiles_DailyAvgs[!is.na(Day153_Tiles_DailyAvgs)]
Day153_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day153_Tiles_DailyAvgs,sep="")
Day154_Tiles_DailyAvgs<-Day154_Tiles_DailyAvgs[!is.na(Day154_Tiles_DailyAvgs)]
Day154_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day154_Tiles_DailyAvgs,sep="")
Day155_Tiles_DailyAvgs<-Day155_Tiles_DailyAvgs[!is.na(Day155_Tiles_DailyAvgs)]
Day155_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day155_Tiles_DailyAvgs,sep="")
Day156_Tiles_DailyAvgs<-Day156_Tiles_DailyAvgs[!is.na(Day156_Tiles_DailyAvgs)]
Day156_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day156_Tiles_DailyAvgs,sep="")
Day157_Tiles_DailyAvgs<-Day157_Tiles_DailyAvgs[!is.na(Day157_Tiles_DailyAvgs)]
Day157_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day157_Tiles_DailyAvgs,sep="")
Day158_Tiles_DailyAvgs<-Day158_Tiles_DailyAvgs[!is.na(Day158_Tiles_DailyAvgs)]
Day158_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day158_Tiles_DailyAvgs,sep="")
Day159_Tiles_DailyAvgs<-Day159_Tiles_DailyAvgs[!is.na(Day159_Tiles_DailyAvgs)]
Day159_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day159_Tiles_DailyAvgs,sep="")
Day160_Tiles_DailyAvgs<-Day160_Tiles_DailyAvgs[!is.na(Day160_Tiles_DailyAvgs)]
Day160_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day160_Tiles_DailyAvgs,sep="")
Day161_Tiles_DailyAvgs<-Day161_Tiles_DailyAvgs[!is.na(Day161_Tiles_DailyAvgs)]
Day161_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day161_Tiles_DailyAvgs,sep="")
Day162_Tiles_DailyAvgs<-Day162_Tiles_DailyAvgs[!is.na(Day162_Tiles_DailyAvgs)]
Day162_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day162_Tiles_DailyAvgs,sep="")
Day163_Tiles_DailyAvgs<-Day163_Tiles_DailyAvgs[!is.na(Day163_Tiles_DailyAvgs)]
Day163_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day163_Tiles_DailyAvgs,sep="")
Day164_Tiles_DailyAvgs<-Day164_Tiles_DailyAvgs[!is.na(Day164_Tiles_DailyAvgs)]
Day164_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day164_Tiles_DailyAvgs,sep="")
Day165_Tiles_DailyAvgs<-Day165_Tiles_DailyAvgs[!is.na(Day165_Tiles_DailyAvgs)]
Day165_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day165_Tiles_DailyAvgs,sep="")
Day166_Tiles_DailyAvgs<-Day166_Tiles_DailyAvgs[!is.na(Day166_Tiles_DailyAvgs)]
Day166_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day166_Tiles_DailyAvgs,sep="")
Day167_Tiles_DailyAvgs<-Day167_Tiles_DailyAvgs[!is.na(Day167_Tiles_DailyAvgs)]
Day167_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day167_Tiles_DailyAvgs,sep="")
Day168_Tiles_DailyAvgs<-Day168_Tiles_DailyAvgs[!is.na(Day168_Tiles_DailyAvgs)]
Day168_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day168_Tiles_DailyAvgs,sep="")
Day169_Tiles_DailyAvgs<-Day169_Tiles_DailyAvgs[!is.na(Day169_Tiles_DailyAvgs)]
Day169_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day169_Tiles_DailyAvgs,sep="")
Day170_Tiles_DailyAvgs<-Day170_Tiles_DailyAvgs[!is.na(Day170_Tiles_DailyAvgs)]
Day170_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day170_Tiles_DailyAvgs,sep="")
Day171_Tiles_DailyAvgs<-Day171_Tiles_DailyAvgs[!is.na(Day171_Tiles_DailyAvgs)]
Day171_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day171_Tiles_DailyAvgs,sep="")
Day172_Tiles_DailyAvgs<-Day172_Tiles_DailyAvgs[!is.na(Day172_Tiles_DailyAvgs)]
Day172_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day172_Tiles_DailyAvgs,sep="")
Day173_Tiles_DailyAvgs<-Day173_Tiles_DailyAvgs[!is.na(Day173_Tiles_DailyAvgs)]
Day173_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day173_Tiles_DailyAvgs,sep="")
Day174_Tiles_DailyAvgs<-Day174_Tiles_DailyAvgs[!is.na(Day174_Tiles_DailyAvgs)]
Day174_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day174_Tiles_DailyAvgs,sep="")
Day175_Tiles_DailyAvgs<-Day175_Tiles_DailyAvgs[!is.na(Day175_Tiles_DailyAvgs)]
Day175_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day175_Tiles_DailyAvgs,sep="")
Day176_Tiles_DailyAvgs<-Day176_Tiles_DailyAvgs[!is.na(Day176_Tiles_DailyAvgs)]
Day176_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day176_Tiles_DailyAvgs,sep="")
Day177_Tiles_DailyAvgs<-Day177_Tiles_DailyAvgs[!is.na(Day177_Tiles_DailyAvgs)]
Day177_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day177_Tiles_DailyAvgs,sep="")
Day178_Tiles_DailyAvgs<-Day178_Tiles_DailyAvgs[!is.na(Day178_Tiles_DailyAvgs)]
Day178_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day178_Tiles_DailyAvgs,sep="")
Day179_Tiles_DailyAvgs<-Day179_Tiles_DailyAvgs[!is.na(Day179_Tiles_DailyAvgs)]
Day179_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day179_Tiles_DailyAvgs,sep="")
Day180_Tiles_DailyAvgs<-Day180_Tiles_DailyAvgs[!is.na(Day180_Tiles_DailyAvgs)]
Day180_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day180_Tiles_DailyAvgs,sep="")
Day181_Tiles_DailyAvgs<-Day181_Tiles_DailyAvgs[!is.na(Day181_Tiles_DailyAvgs)]
Day181_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day181_Tiles_DailyAvgs,sep="")
Day182_Tiles_DailyAvgs<-Day182_Tiles_DailyAvgs[!is.na(Day182_Tiles_DailyAvgs)]
Day182_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day182_Tiles_DailyAvgs,sep="")
Day183_Tiles_DailyAvgs<-Day183_Tiles_DailyAvgs[!is.na(Day183_Tiles_DailyAvgs)]
Day183_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day183_Tiles_DailyAvgs,sep="")
Day184_Tiles_DailyAvgs<-Day184_Tiles_DailyAvgs[!is.na(Day184_Tiles_DailyAvgs)]
Day184_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day184_Tiles_DailyAvgs,sep="")
Day185_Tiles_DailyAvgs<-Day185_Tiles_DailyAvgs[!is.na(Day185_Tiles_DailyAvgs)]
Day185_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day185_Tiles_DailyAvgs,sep="")
Day186_Tiles_DailyAvgs<-Day186_Tiles_DailyAvgs[!is.na(Day186_Tiles_DailyAvgs)]
Day186_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day186_Tiles_DailyAvgs,sep="")
Day187_Tiles_DailyAvgs<-Day187_Tiles_DailyAvgs[!is.na(Day187_Tiles_DailyAvgs)]
Day187_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day187_Tiles_DailyAvgs,sep="")
Day188_Tiles_DailyAvgs<-Day188_Tiles_DailyAvgs[!is.na(Day188_Tiles_DailyAvgs)]
Day188_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day188_Tiles_DailyAvgs,sep="")
Day189_Tiles_DailyAvgs<-Day189_Tiles_DailyAvgs[!is.na(Day189_Tiles_DailyAvgs)]
Day189_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day189_Tiles_DailyAvgs,sep="")
Day190_Tiles_DailyAvgs<-Day190_Tiles_DailyAvgs[!is.na(Day190_Tiles_DailyAvgs)]
Day190_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day190_Tiles_DailyAvgs,sep="")
Day191_Tiles_DailyAvgs<-Day191_Tiles_DailyAvgs[!is.na(Day191_Tiles_DailyAvgs)]
Day191_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day191_Tiles_DailyAvgs,sep="")
Day192_Tiles_DailyAvgs<-Day192_Tiles_DailyAvgs[!is.na(Day192_Tiles_DailyAvgs)]
Day192_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day192_Tiles_DailyAvgs,sep="")
Day193_Tiles_DailyAvgs<-Day193_Tiles_DailyAvgs[!is.na(Day193_Tiles_DailyAvgs)]
Day193_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day193_Tiles_DailyAvgs,sep="")
Day194_Tiles_DailyAvgs<-Day194_Tiles_DailyAvgs[!is.na(Day194_Tiles_DailyAvgs)]
Day194_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day194_Tiles_DailyAvgs,sep="")
Day195_Tiles_DailyAvgs<-Day195_Tiles_DailyAvgs[!is.na(Day195_Tiles_DailyAvgs)]
Day195_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day195_Tiles_DailyAvgs,sep="")
Day196_Tiles_DailyAvgs<-Day196_Tiles_DailyAvgs[!is.na(Day196_Tiles_DailyAvgs)]
Day196_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day196_Tiles_DailyAvgs,sep="")
Day197_Tiles_DailyAvgs<-Day197_Tiles_DailyAvgs[!is.na(Day197_Tiles_DailyAvgs)]
Day197_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day197_Tiles_DailyAvgs,sep="")
Day198_Tiles_DailyAvgs<-Day198_Tiles_DailyAvgs[!is.na(Day198_Tiles_DailyAvgs)]
Day198_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day198_Tiles_DailyAvgs,sep="")
Day199_Tiles_DailyAvgs<-Day199_Tiles_DailyAvgs[!is.na(Day199_Tiles_DailyAvgs)]
Day199_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day199_Tiles_DailyAvgs,sep="")
Day200_Tiles_DailyAvgs<-Day200_Tiles_DailyAvgs[!is.na(Day200_Tiles_DailyAvgs)]
Day200_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day200_Tiles_DailyAvgs,sep="")
Day201_Tiles_DailyAvgs<-Day201_Tiles_DailyAvgs[!is.na(Day201_Tiles_DailyAvgs)]
Day201_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day201_Tiles_DailyAvgs,sep="")
Day202_Tiles_DailyAvgs<-Day202_Tiles_DailyAvgs[!is.na(Day202_Tiles_DailyAvgs)]
Day202_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day202_Tiles_DailyAvgs,sep="")
Day203_Tiles_DailyAvgs<-Day203_Tiles_DailyAvgs[!is.na(Day203_Tiles_DailyAvgs)]
Day203_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day203_Tiles_DailyAvgs,sep="")
Day204_Tiles_DailyAvgs<-Day204_Tiles_DailyAvgs[!is.na(Day204_Tiles_DailyAvgs)]
Day204_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day204_Tiles_DailyAvgs,sep="")
Day205_Tiles_DailyAvgs<-Day205_Tiles_DailyAvgs[!is.na(Day205_Tiles_DailyAvgs)]
Day205_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day205_Tiles_DailyAvgs,sep="")
Day206_Tiles_DailyAvgs<-Day206_Tiles_DailyAvgs[!is.na(Day206_Tiles_DailyAvgs)]
Day206_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day206_Tiles_DailyAvgs,sep="")
Day207_Tiles_DailyAvgs<-Day207_Tiles_DailyAvgs[!is.na(Day207_Tiles_DailyAvgs)]
Day207_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day207_Tiles_DailyAvgs,sep="")
Day208_Tiles_DailyAvgs<-Day208_Tiles_DailyAvgs[!is.na(Day208_Tiles_DailyAvgs)]
Day208_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day208_Tiles_DailyAvgs,sep="")
Day209_Tiles_DailyAvgs<-Day209_Tiles_DailyAvgs[!is.na(Day209_Tiles_DailyAvgs)]
Day209_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day209_Tiles_DailyAvgs,sep="")
Day210_Tiles_DailyAvgs<-Day210_Tiles_DailyAvgs[!is.na(Day210_Tiles_DailyAvgs)]
Day210_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day210_Tiles_DailyAvgs,sep="")
Day211_Tiles_DailyAvgs<-Day211_Tiles_DailyAvgs[!is.na(Day211_Tiles_DailyAvgs)]
Day211_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day211_Tiles_DailyAvgs,sep="")
Day212_Tiles_DailyAvgs<-Day212_Tiles_DailyAvgs[!is.na(Day212_Tiles_DailyAvgs)]
Day212_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day212_Tiles_DailyAvgs,sep="")
Day213_Tiles_DailyAvgs<-Day213_Tiles_DailyAvgs[!is.na(Day213_Tiles_DailyAvgs)]
Day213_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day213_Tiles_DailyAvgs,sep="")
Day214_Tiles_DailyAvgs<-Day214_Tiles_DailyAvgs[!is.na(Day214_Tiles_DailyAvgs)]
Day214_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day214_Tiles_DailyAvgs,sep="")
Day215_Tiles_DailyAvgs<-Day215_Tiles_DailyAvgs[!is.na(Day215_Tiles_DailyAvgs)]
Day215_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day215_Tiles_DailyAvgs,sep="")
Day216_Tiles_DailyAvgs<-Day216_Tiles_DailyAvgs[!is.na(Day216_Tiles_DailyAvgs)]
Day216_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day216_Tiles_DailyAvgs,sep="")
Day217_Tiles_DailyAvgs<-Day217_Tiles_DailyAvgs[!is.na(Day217_Tiles_DailyAvgs)]
Day217_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day217_Tiles_DailyAvgs,sep="")
Day218_Tiles_DailyAvgs<-Day218_Tiles_DailyAvgs[!is.na(Day218_Tiles_DailyAvgs)]
Day218_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day218_Tiles_DailyAvgs,sep="")
Day219_Tiles_DailyAvgs<-Day219_Tiles_DailyAvgs[!is.na(Day219_Tiles_DailyAvgs)]
Day219_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day219_Tiles_DailyAvgs,sep="")
Day220_Tiles_DailyAvgs<-Day220_Tiles_DailyAvgs[!is.na(Day220_Tiles_DailyAvgs)]
Day220_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day220_Tiles_DailyAvgs,sep="")
Day221_Tiles_DailyAvgs<-Day221_Tiles_DailyAvgs[!is.na(Day221_Tiles_DailyAvgs)]
Day221_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day221_Tiles_DailyAvgs,sep="")
Day222_Tiles_DailyAvgs<-Day222_Tiles_DailyAvgs[!is.na(Day222_Tiles_DailyAvgs)]
Day222_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day222_Tiles_DailyAvgs,sep="")
Day223_Tiles_DailyAvgs<-Day223_Tiles_DailyAvgs[!is.na(Day223_Tiles_DailyAvgs)]
Day223_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day223_Tiles_DailyAvgs,sep="")
Day224_Tiles_DailyAvgs<-Day224_Tiles_DailyAvgs[!is.na(Day224_Tiles_DailyAvgs)]
Day224_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day224_Tiles_DailyAvgs,sep="")
Day225_Tiles_DailyAvgs<-Day225_Tiles_DailyAvgs[!is.na(Day225_Tiles_DailyAvgs)]
Day225_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day225_Tiles_DailyAvgs,sep="")
Day226_Tiles_DailyAvgs<-Day226_Tiles_DailyAvgs[!is.na(Day226_Tiles_DailyAvgs)]
Day226_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day226_Tiles_DailyAvgs,sep="")
Day227_Tiles_DailyAvgs<-Day227_Tiles_DailyAvgs[!is.na(Day227_Tiles_DailyAvgs)]
Day227_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day227_Tiles_DailyAvgs,sep="")
Day228_Tiles_DailyAvgs<-Day228_Tiles_DailyAvgs[!is.na(Day228_Tiles_DailyAvgs)]
Day228_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day228_Tiles_DailyAvgs,sep="")
Day229_Tiles_DailyAvgs<-Day229_Tiles_DailyAvgs[!is.na(Day229_Tiles_DailyAvgs)]
Day229_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day229_Tiles_DailyAvgs,sep="")
Day230_Tiles_DailyAvgs<-Day230_Tiles_DailyAvgs[!is.na(Day230_Tiles_DailyAvgs)]
Day230_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day230_Tiles_DailyAvgs,sep="")
Day231_Tiles_DailyAvgs<-Day231_Tiles_DailyAvgs[!is.na(Day231_Tiles_DailyAvgs)]
Day231_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day231_Tiles_DailyAvgs,sep="")
Day232_Tiles_DailyAvgs<-Day232_Tiles_DailyAvgs[!is.na(Day232_Tiles_DailyAvgs)]
Day232_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day232_Tiles_DailyAvgs,sep="")
Day233_Tiles_DailyAvgs<-Day233_Tiles_DailyAvgs[!is.na(Day233_Tiles_DailyAvgs)]
Day233_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day233_Tiles_DailyAvgs,sep="")
Day234_Tiles_DailyAvgs<-Day234_Tiles_DailyAvgs[!is.na(Day234_Tiles_DailyAvgs)]
Day234_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day234_Tiles_DailyAvgs,sep="")
Day235_Tiles_DailyAvgs<-Day235_Tiles_DailyAvgs[!is.na(Day235_Tiles_DailyAvgs)]
Day235_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day235_Tiles_DailyAvgs,sep="")
Day236_Tiles_DailyAvgs<-Day236_Tiles_DailyAvgs[!is.na(Day236_Tiles_DailyAvgs)]
Day236_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day236_Tiles_DailyAvgs,sep="")
Day237_Tiles_DailyAvgs<-Day237_Tiles_DailyAvgs[!is.na(Day237_Tiles_DailyAvgs)]
Day237_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day237_Tiles_DailyAvgs,sep="")
Day238_Tiles_DailyAvgs<-Day238_Tiles_DailyAvgs[!is.na(Day238_Tiles_DailyAvgs)]
Day238_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day238_Tiles_DailyAvgs,sep="")
Day239_Tiles_DailyAvgs<-Day239_Tiles_DailyAvgs[!is.na(Day239_Tiles_DailyAvgs)]
Day239_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day239_Tiles_DailyAvgs,sep="")
Day240_Tiles_DailyAvgs<-Day240_Tiles_DailyAvgs[!is.na(Day240_Tiles_DailyAvgs)]
Day240_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day240_Tiles_DailyAvgs,sep="")
Day241_Tiles_DailyAvgs<-Day241_Tiles_DailyAvgs[!is.na(Day241_Tiles_DailyAvgs)]
Day241_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day241_Tiles_DailyAvgs,sep="")
Day242_Tiles_DailyAvgs<-Day242_Tiles_DailyAvgs[!is.na(Day242_Tiles_DailyAvgs)]
Day242_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day242_Tiles_DailyAvgs,sep="")
Day243_Tiles_DailyAvgs<-Day243_Tiles_DailyAvgs[!is.na(Day243_Tiles_DailyAvgs)]
Day243_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day243_Tiles_DailyAvgs,sep="")
Day244_Tiles_DailyAvgs<-Day244_Tiles_DailyAvgs[!is.na(Day244_Tiles_DailyAvgs)]
Day244_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day244_Tiles_DailyAvgs,sep="")
Day245_Tiles_DailyAvgs<-Day245_Tiles_DailyAvgs[!is.na(Day245_Tiles_DailyAvgs)]
Day245_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day245_Tiles_DailyAvgs,sep="")
Day246_Tiles_DailyAvgs<-Day246_Tiles_DailyAvgs[!is.na(Day246_Tiles_DailyAvgs)]
Day246_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day246_Tiles_DailyAvgs,sep="")
Day247_Tiles_DailyAvgs<-Day247_Tiles_DailyAvgs[!is.na(Day247_Tiles_DailyAvgs)]
Day247_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day247_Tiles_DailyAvgs,sep="")
Day248_Tiles_DailyAvgs<-Day248_Tiles_DailyAvgs[!is.na(Day248_Tiles_DailyAvgs)]
Day248_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day248_Tiles_DailyAvgs,sep="")
Day249_Tiles_DailyAvgs<-Day249_Tiles_DailyAvgs[!is.na(Day249_Tiles_DailyAvgs)]
Day249_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day249_Tiles_DailyAvgs,sep="")
Day250_Tiles_DailyAvgs<-Day250_Tiles_DailyAvgs[!is.na(Day250_Tiles_DailyAvgs)]
Day250_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day250_Tiles_DailyAvgs,sep="")
Day251_Tiles_DailyAvgs<-Day251_Tiles_DailyAvgs[!is.na(Day251_Tiles_DailyAvgs)]
Day251_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day251_Tiles_DailyAvgs,sep="")
Day252_Tiles_DailyAvgs<-Day252_Tiles_DailyAvgs[!is.na(Day252_Tiles_DailyAvgs)]
Day252_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day252_Tiles_DailyAvgs,sep="")
Day253_Tiles_DailyAvgs<-Day253_Tiles_DailyAvgs[!is.na(Day253_Tiles_DailyAvgs)]
Day253_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day253_Tiles_DailyAvgs,sep="")
Day254_Tiles_DailyAvgs<-Day254_Tiles_DailyAvgs[!is.na(Day254_Tiles_DailyAvgs)]
Day254_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day254_Tiles_DailyAvgs,sep="")
Day255_Tiles_DailyAvgs<-Day255_Tiles_DailyAvgs[!is.na(Day255_Tiles_DailyAvgs)]
Day255_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day255_Tiles_DailyAvgs,sep="")
Day256_Tiles_DailyAvgs<-Day256_Tiles_DailyAvgs[!is.na(Day256_Tiles_DailyAvgs)]
Day256_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day256_Tiles_DailyAvgs,sep="")
Day257_Tiles_DailyAvgs<-Day257_Tiles_DailyAvgs[!is.na(Day257_Tiles_DailyAvgs)]
Day257_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day257_Tiles_DailyAvgs,sep="")
Day258_Tiles_DailyAvgs<-Day258_Tiles_DailyAvgs[!is.na(Day258_Tiles_DailyAvgs)]
Day258_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day258_Tiles_DailyAvgs,sep="")
Day259_Tiles_DailyAvgs<-Day259_Tiles_DailyAvgs[!is.na(Day259_Tiles_DailyAvgs)]
Day259_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day259_Tiles_DailyAvgs,sep="")
Day260_Tiles_DailyAvgs<-Day260_Tiles_DailyAvgs[!is.na(Day260_Tiles_DailyAvgs)]
Day260_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day260_Tiles_DailyAvgs,sep="")
Day261_Tiles_DailyAvgs<-Day261_Tiles_DailyAvgs[!is.na(Day261_Tiles_DailyAvgs)]
Day261_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day261_Tiles_DailyAvgs,sep="")
Day262_Tiles_DailyAvgs<-Day262_Tiles_DailyAvgs[!is.na(Day262_Tiles_DailyAvgs)]
Day262_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day262_Tiles_DailyAvgs,sep="")
Day263_Tiles_DailyAvgs<-Day263_Tiles_DailyAvgs[!is.na(Day263_Tiles_DailyAvgs)]
Day263_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day263_Tiles_DailyAvgs,sep="")
Day264_Tiles_DailyAvgs<-Day264_Tiles_DailyAvgs[!is.na(Day264_Tiles_DailyAvgs)]
Day264_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day264_Tiles_DailyAvgs,sep="")
Day265_Tiles_DailyAvgs<-Day265_Tiles_DailyAvgs[!is.na(Day265_Tiles_DailyAvgs)]
Day265_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day265_Tiles_DailyAvgs,sep="")
Day266_Tiles_DailyAvgs<-Day266_Tiles_DailyAvgs[!is.na(Day266_Tiles_DailyAvgs)]
Day266_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day266_Tiles_DailyAvgs,sep="")
Day267_Tiles_DailyAvgs<-Day267_Tiles_DailyAvgs[!is.na(Day267_Tiles_DailyAvgs)]
Day267_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day267_Tiles_DailyAvgs,sep="")
Day268_Tiles_DailyAvgs<-Day268_Tiles_DailyAvgs[!is.na(Day268_Tiles_DailyAvgs)]
Day268_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day268_Tiles_DailyAvgs,sep="")
Day269_Tiles_DailyAvgs<-Day269_Tiles_DailyAvgs[!is.na(Day269_Tiles_DailyAvgs)]
Day269_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day269_Tiles_DailyAvgs,sep="")
Day270_Tiles_DailyAvgs<-Day270_Tiles_DailyAvgs[!is.na(Day270_Tiles_DailyAvgs)]
Day270_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day270_Tiles_DailyAvgs,sep="")
Day271_Tiles_DailyAvgs<-Day271_Tiles_DailyAvgs[!is.na(Day271_Tiles_DailyAvgs)]
Day271_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day271_Tiles_DailyAvgs,sep="")
Day272_Tiles_DailyAvgs<-Day272_Tiles_DailyAvgs[!is.na(Day272_Tiles_DailyAvgs)]
Day272_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day272_Tiles_DailyAvgs,sep="")
Day273_Tiles_DailyAvgs<-Day273_Tiles_DailyAvgs[!is.na(Day273_Tiles_DailyAvgs)]
Day273_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day273_Tiles_DailyAvgs,sep="")
Day274_Tiles_DailyAvgs<-Day274_Tiles_DailyAvgs[!is.na(Day274_Tiles_DailyAvgs)]
Day274_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day274_Tiles_DailyAvgs,sep="")
Day275_Tiles_DailyAvgs<-Day275_Tiles_DailyAvgs[!is.na(Day275_Tiles_DailyAvgs)]
Day275_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day275_Tiles_DailyAvgs,sep="")
Day276_Tiles_DailyAvgs<-Day276_Tiles_DailyAvgs[!is.na(Day276_Tiles_DailyAvgs)]
Day276_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day276_Tiles_DailyAvgs,sep="")
Day277_Tiles_DailyAvgs<-Day277_Tiles_DailyAvgs[!is.na(Day277_Tiles_DailyAvgs)]
Day277_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day277_Tiles_DailyAvgs,sep="")
Day278_Tiles_DailyAvgs<-Day278_Tiles_DailyAvgs[!is.na(Day278_Tiles_DailyAvgs)]
Day278_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day278_Tiles_DailyAvgs,sep="")
Day279_Tiles_DailyAvgs<-Day279_Tiles_DailyAvgs[!is.na(Day279_Tiles_DailyAvgs)]
Day279_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day279_Tiles_DailyAvgs,sep="")
Day280_Tiles_DailyAvgs<-Day280_Tiles_DailyAvgs[!is.na(Day280_Tiles_DailyAvgs)]
Day280_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day280_Tiles_DailyAvgs,sep="")
Day281_Tiles_DailyAvgs<-Day281_Tiles_DailyAvgs[!is.na(Day281_Tiles_DailyAvgs)]
Day281_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day281_Tiles_DailyAvgs,sep="")
Day282_Tiles_DailyAvgs<-Day282_Tiles_DailyAvgs[!is.na(Day282_Tiles_DailyAvgs)]
Day282_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day282_Tiles_DailyAvgs,sep="")
Day283_Tiles_DailyAvgs<-Day283_Tiles_DailyAvgs[!is.na(Day283_Tiles_DailyAvgs)]
Day283_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day283_Tiles_DailyAvgs,sep="")
Day284_Tiles_DailyAvgs<-Day284_Tiles_DailyAvgs[!is.na(Day284_Tiles_DailyAvgs)]
Day284_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day284_Tiles_DailyAvgs,sep="")
Day285_Tiles_DailyAvgs<-Day285_Tiles_DailyAvgs[!is.na(Day285_Tiles_DailyAvgs)]
Day285_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day285_Tiles_DailyAvgs,sep="")
Day286_Tiles_DailyAvgs<-Day286_Tiles_DailyAvgs[!is.na(Day286_Tiles_DailyAvgs)]
Day286_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day286_Tiles_DailyAvgs,sep="")
Day287_Tiles_DailyAvgs<-Day287_Tiles_DailyAvgs[!is.na(Day287_Tiles_DailyAvgs)]
Day287_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day287_Tiles_DailyAvgs,sep="")
Day288_Tiles_DailyAvgs<-Day288_Tiles_DailyAvgs[!is.na(Day288_Tiles_DailyAvgs)]
Day288_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day288_Tiles_DailyAvgs,sep="")
Day289_Tiles_DailyAvgs<-Day289_Tiles_DailyAvgs[!is.na(Day289_Tiles_DailyAvgs)]
Day289_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day289_Tiles_DailyAvgs,sep="")
Day290_Tiles_DailyAvgs<-Day290_Tiles_DailyAvgs[!is.na(Day290_Tiles_DailyAvgs)]
Day290_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day290_Tiles_DailyAvgs,sep="")
Day291_Tiles_DailyAvgs<-Day291_Tiles_DailyAvgs[!is.na(Day291_Tiles_DailyAvgs)]
Day291_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day291_Tiles_DailyAvgs,sep="")
Day292_Tiles_DailyAvgs<-Day292_Tiles_DailyAvgs[!is.na(Day292_Tiles_DailyAvgs)]
Day292_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day292_Tiles_DailyAvgs,sep="")
Day293_Tiles_DailyAvgs<-Day293_Tiles_DailyAvgs[!is.na(Day293_Tiles_DailyAvgs)]
Day293_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day293_Tiles_DailyAvgs,sep="")
Day294_Tiles_DailyAvgs<-Day294_Tiles_DailyAvgs[!is.na(Day294_Tiles_DailyAvgs)]
Day294_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day294_Tiles_DailyAvgs,sep="")
Day295_Tiles_DailyAvgs<-Day295_Tiles_DailyAvgs[!is.na(Day295_Tiles_DailyAvgs)]
Day295_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day295_Tiles_DailyAvgs,sep="")
Day296_Tiles_DailyAvgs<-Day296_Tiles_DailyAvgs[!is.na(Day296_Tiles_DailyAvgs)]
Day296_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day296_Tiles_DailyAvgs,sep="")
Day297_Tiles_DailyAvgs<-Day297_Tiles_DailyAvgs[!is.na(Day297_Tiles_DailyAvgs)]
Day297_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day297_Tiles_DailyAvgs,sep="")
Day298_Tiles_DailyAvgs<-Day298_Tiles_DailyAvgs[!is.na(Day298_Tiles_DailyAvgs)]
Day298_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day298_Tiles_DailyAvgs,sep="")
Day299_Tiles_DailyAvgs<-Day299_Tiles_DailyAvgs[!is.na(Day299_Tiles_DailyAvgs)]
Day299_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day299_Tiles_DailyAvgs,sep="")
Day300_Tiles_DailyAvgs<-Day300_Tiles_DailyAvgs[!is.na(Day300_Tiles_DailyAvgs)]
Day300_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day300_Tiles_DailyAvgs,sep="")
Day301_Tiles_DailyAvgs<-Day301_Tiles_DailyAvgs[!is.na(Day301_Tiles_DailyAvgs)]
Day301_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day301_Tiles_DailyAvgs,sep="")
Day302_Tiles_DailyAvgs<-Day302_Tiles_DailyAvgs[!is.na(Day302_Tiles_DailyAvgs)]
Day302_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day302_Tiles_DailyAvgs,sep="")
Day303_Tiles_DailyAvgs<-Day303_Tiles_DailyAvgs[!is.na(Day303_Tiles_DailyAvgs)]
Day303_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day303_Tiles_DailyAvgs,sep="")
Day304_Tiles_DailyAvgs<-Day304_Tiles_DailyAvgs[!is.na(Day304_Tiles_DailyAvgs)]
Day304_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day304_Tiles_DailyAvgs,sep="")
Day305_Tiles_DailyAvgs<-Day305_Tiles_DailyAvgs[!is.na(Day305_Tiles_DailyAvgs)]
Day305_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day305_Tiles_DailyAvgs,sep="")
Day306_Tiles_DailyAvgs<-Day306_Tiles_DailyAvgs[!is.na(Day306_Tiles_DailyAvgs)]
Day306_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day306_Tiles_DailyAvgs,sep="")
Day307_Tiles_DailyAvgs<-Day307_Tiles_DailyAvgs[!is.na(Day307_Tiles_DailyAvgs)]
Day307_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day307_Tiles_DailyAvgs,sep="")
Day308_Tiles_DailyAvgs<-Day308_Tiles_DailyAvgs[!is.na(Day308_Tiles_DailyAvgs)]
Day308_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day308_Tiles_DailyAvgs,sep="")
Day309_Tiles_DailyAvgs<-Day309_Tiles_DailyAvgs[!is.na(Day309_Tiles_DailyAvgs)]
Day309_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day309_Tiles_DailyAvgs,sep="")
Day310_Tiles_DailyAvgs<-Day310_Tiles_DailyAvgs[!is.na(Day310_Tiles_DailyAvgs)]
Day310_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day310_Tiles_DailyAvgs,sep="")
Day311_Tiles_DailyAvgs<-Day311_Tiles_DailyAvgs[!is.na(Day311_Tiles_DailyAvgs)]
Day311_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day311_Tiles_DailyAvgs,sep="")
Day312_Tiles_DailyAvgs<-Day312_Tiles_DailyAvgs[!is.na(Day312_Tiles_DailyAvgs)]
Day312_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day312_Tiles_DailyAvgs,sep="")
Day313_Tiles_DailyAvgs<-Day313_Tiles_DailyAvgs[!is.na(Day313_Tiles_DailyAvgs)]
Day313_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day313_Tiles_DailyAvgs,sep="")
Day314_Tiles_DailyAvgs<-Day314_Tiles_DailyAvgs[!is.na(Day314_Tiles_DailyAvgs)]
Day314_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day314_Tiles_DailyAvgs,sep="")
Day315_Tiles_DailyAvgs<-Day315_Tiles_DailyAvgs[!is.na(Day315_Tiles_DailyAvgs)]
Day315_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day315_Tiles_DailyAvgs,sep="")
Day316_Tiles_DailyAvgs<-Day316_Tiles_DailyAvgs[!is.na(Day316_Tiles_DailyAvgs)]
Day316_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day316_Tiles_DailyAvgs,sep="")
Day317_Tiles_DailyAvgs<-Day317_Tiles_DailyAvgs[!is.na(Day317_Tiles_DailyAvgs)]
Day317_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day317_Tiles_DailyAvgs,sep="")
Day318_Tiles_DailyAvgs<-Day318_Tiles_DailyAvgs[!is.na(Day318_Tiles_DailyAvgs)]
Day318_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day318_Tiles_DailyAvgs,sep="")
Day319_Tiles_DailyAvgs<-Day319_Tiles_DailyAvgs[!is.na(Day319_Tiles_DailyAvgs)]
Day319_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day319_Tiles_DailyAvgs,sep="")
Day320_Tiles_DailyAvgs<-Day320_Tiles_DailyAvgs[!is.na(Day320_Tiles_DailyAvgs)]
Day320_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day320_Tiles_DailyAvgs,sep="")
Day321_Tiles_DailyAvgs<-Day321_Tiles_DailyAvgs[!is.na(Day321_Tiles_DailyAvgs)]
Day321_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day321_Tiles_DailyAvgs,sep="")
Day322_Tiles_DailyAvgs<-Day322_Tiles_DailyAvgs[!is.na(Day322_Tiles_DailyAvgs)]
Day322_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day322_Tiles_DailyAvgs,sep="")
Day323_Tiles_DailyAvgs<-Day323_Tiles_DailyAvgs[!is.na(Day323_Tiles_DailyAvgs)]
Day323_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day323_Tiles_DailyAvgs,sep="")
Day324_Tiles_DailyAvgs<-Day324_Tiles_DailyAvgs[!is.na(Day324_Tiles_DailyAvgs)]
Day324_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day324_Tiles_DailyAvgs,sep="")
Day325_Tiles_DailyAvgs<-Day325_Tiles_DailyAvgs[!is.na(Day325_Tiles_DailyAvgs)]
Day325_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day325_Tiles_DailyAvgs,sep="")
Day326_Tiles_DailyAvgs<-Day326_Tiles_DailyAvgs[!is.na(Day326_Tiles_DailyAvgs)]
Day326_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day326_Tiles_DailyAvgs,sep="")
Day327_Tiles_DailyAvgs<-Day327_Tiles_DailyAvgs[!is.na(Day327_Tiles_DailyAvgs)]
Day327_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day327_Tiles_DailyAvgs,sep="")
Day328_Tiles_DailyAvgs<-Day328_Tiles_DailyAvgs[!is.na(Day328_Tiles_DailyAvgs)]
Day328_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day328_Tiles_DailyAvgs,sep="")
Day329_Tiles_DailyAvgs<-Day329_Tiles_DailyAvgs[!is.na(Day329_Tiles_DailyAvgs)]
Day329_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day329_Tiles_DailyAvgs,sep="")
Day330_Tiles_DailyAvgs<-Day330_Tiles_DailyAvgs[!is.na(Day330_Tiles_DailyAvgs)]
Day330_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day330_Tiles_DailyAvgs,sep="")
Day331_Tiles_DailyAvgs<-Day331_Tiles_DailyAvgs[!is.na(Day331_Tiles_DailyAvgs)]
Day331_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day331_Tiles_DailyAvgs,sep="")
Day332_Tiles_DailyAvgs<-Day332_Tiles_DailyAvgs[!is.na(Day332_Tiles_DailyAvgs)]
Day332_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day332_Tiles_DailyAvgs,sep="")
Day333_Tiles_DailyAvgs<-Day333_Tiles_DailyAvgs[!is.na(Day333_Tiles_DailyAvgs)]
Day333_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day333_Tiles_DailyAvgs,sep="")
Day334_Tiles_DailyAvgs<-Day334_Tiles_DailyAvgs[!is.na(Day334_Tiles_DailyAvgs)]
Day334_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day334_Tiles_DailyAvgs,sep="")
Day335_Tiles_DailyAvgs<-Day335_Tiles_DailyAvgs[!is.na(Day335_Tiles_DailyAvgs)]
Day335_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day335_Tiles_DailyAvgs,sep="")
Day336_Tiles_DailyAvgs<-Day336_Tiles_DailyAvgs[!is.na(Day336_Tiles_DailyAvgs)]
Day336_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day336_Tiles_DailyAvgs,sep="")
Day337_Tiles_DailyAvgs<-Day337_Tiles_DailyAvgs[!is.na(Day337_Tiles_DailyAvgs)]
Day337_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day337_Tiles_DailyAvgs,sep="")
Day338_Tiles_DailyAvgs<-Day338_Tiles_DailyAvgs[!is.na(Day338_Tiles_DailyAvgs)]
Day338_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day338_Tiles_DailyAvgs,sep="")
Day339_Tiles_DailyAvgs<-Day339_Tiles_DailyAvgs[!is.na(Day339_Tiles_DailyAvgs)]
Day339_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day339_Tiles_DailyAvgs,sep="")
Day340_Tiles_DailyAvgs<-Day340_Tiles_DailyAvgs[!is.na(Day340_Tiles_DailyAvgs)]
Day340_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day340_Tiles_DailyAvgs,sep="")
Day341_Tiles_DailyAvgs<-Day341_Tiles_DailyAvgs[!is.na(Day341_Tiles_DailyAvgs)]
Day341_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day341_Tiles_DailyAvgs,sep="")
Day342_Tiles_DailyAvgs<-Day342_Tiles_DailyAvgs[!is.na(Day342_Tiles_DailyAvgs)]
Day342_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day342_Tiles_DailyAvgs,sep="")
Day343_Tiles_DailyAvgs<-Day343_Tiles_DailyAvgs[!is.na(Day343_Tiles_DailyAvgs)]
Day343_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day343_Tiles_DailyAvgs,sep="")
Day344_Tiles_DailyAvgs<-Day344_Tiles_DailyAvgs[!is.na(Day344_Tiles_DailyAvgs)]
Day344_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day344_Tiles_DailyAvgs,sep="")
Day345_Tiles_DailyAvgs<-Day345_Tiles_DailyAvgs[!is.na(Day345_Tiles_DailyAvgs)]
Day345_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day345_Tiles_DailyAvgs,sep="")
Day346_Tiles_DailyAvgs<-Day346_Tiles_DailyAvgs[!is.na(Day346_Tiles_DailyAvgs)]
Day346_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day346_Tiles_DailyAvgs,sep="")
Day347_Tiles_DailyAvgs<-Day347_Tiles_DailyAvgs[!is.na(Day347_Tiles_DailyAvgs)]
Day347_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day347_Tiles_DailyAvgs,sep="")
Day348_Tiles_DailyAvgs<-Day348_Tiles_DailyAvgs[!is.na(Day348_Tiles_DailyAvgs)]
Day348_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day348_Tiles_DailyAvgs,sep="")
Day349_Tiles_DailyAvgs<-Day349_Tiles_DailyAvgs[!is.na(Day349_Tiles_DailyAvgs)]
Day349_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day349_Tiles_DailyAvgs,sep="")
Day350_Tiles_DailyAvgs<-Day350_Tiles_DailyAvgs[!is.na(Day350_Tiles_DailyAvgs)]
Day350_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day350_Tiles_DailyAvgs,sep="")
Day351_Tiles_DailyAvgs<-Day351_Tiles_DailyAvgs[!is.na(Day351_Tiles_DailyAvgs)]
Day351_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day351_Tiles_DailyAvgs,sep="")
Day352_Tiles_DailyAvgs<-Day352_Tiles_DailyAvgs[!is.na(Day352_Tiles_DailyAvgs)]
Day352_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day352_Tiles_DailyAvgs,sep="")
Day353_Tiles_DailyAvgs<-Day353_Tiles_DailyAvgs[!is.na(Day353_Tiles_DailyAvgs)]
Day353_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day353_Tiles_DailyAvgs,sep="")
Day354_Tiles_DailyAvgs<-Day354_Tiles_DailyAvgs[!is.na(Day354_Tiles_DailyAvgs)]
Day354_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day354_Tiles_DailyAvgs,sep="")
Day355_Tiles_DailyAvgs<-Day355_Tiles_DailyAvgs[!is.na(Day355_Tiles_DailyAvgs)]
Day355_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day355_Tiles_DailyAvgs,sep="")
Day356_Tiles_DailyAvgs<-Day356_Tiles_DailyAvgs[!is.na(Day356_Tiles_DailyAvgs)]
Day356_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day356_Tiles_DailyAvgs,sep="")
Day357_Tiles_DailyAvgs<-Day357_Tiles_DailyAvgs[!is.na(Day357_Tiles_DailyAvgs)]
Day357_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day357_Tiles_DailyAvgs,sep="")
Day358_Tiles_DailyAvgs<-Day358_Tiles_DailyAvgs[!is.na(Day358_Tiles_DailyAvgs)]
Day358_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day358_Tiles_DailyAvgs,sep="")
Day359_Tiles_DailyAvgs<-Day359_Tiles_DailyAvgs[!is.na(Day359_Tiles_DailyAvgs)]
Day359_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day359_Tiles_DailyAvgs,sep="")
Day360_Tiles_DailyAvgs<-Day360_Tiles_DailyAvgs[!is.na(Day360_Tiles_DailyAvgs)]
Day360_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day360_Tiles_DailyAvgs,sep="")
Day361_Tiles_DailyAvgs<-Day361_Tiles_DailyAvgs[!is.na(Day361_Tiles_DailyAvgs)]
Day361_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day361_Tiles_DailyAvgs,sep="")
Day362_Tiles_DailyAvgs<-Day362_Tiles_DailyAvgs[!is.na(Day362_Tiles_DailyAvgs)]
Day362_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day362_Tiles_DailyAvgs,sep="")
Day363_Tiles_DailyAvgs<-Day363_Tiles_DailyAvgs[!is.na(Day363_Tiles_DailyAvgs)]
Day363_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day363_Tiles_DailyAvgs,sep="")
Day364_Tiles_DailyAvgs<-Day364_Tiles_DailyAvgs[!is.na(Day364_Tiles_DailyAvgs)]
Day364_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day364_Tiles_DailyAvgs,sep="")
Day365_Tiles_DailyAvgs<-Day365_Tiles_DailyAvgs[!is.na(Day365_Tiles_DailyAvgs)]
Day365_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day365_Tiles_DailyAvgs,sep="")
Day366_Tiles_DailyAvgs<-Day366_Tiles_DailyAvgs[!is.na(Day366_Tiles_DailyAvgs)]
Day366_Tiles_DailyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/",Day366_Tiles_DailyAvgs,sep="")


#Create mosaics
Day1_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day1_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day1_Tiles_DailyAvgs[1]),raster(Day1_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day1_name, datatype="HFA")
Day2_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day2_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day2_Tiles_DailyAvgs[1]),raster(Day2_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day2_name, datatype="HFA")
Day3_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day3_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day3_Tiles_DailyAvgs[1]),raster(Day3_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day3_name, datatype="HFA")
Day4_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day4_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day4_Tiles_DailyAvgs[1]),raster(Day4_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day4_name, datatype="HFA")
Day5_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day5_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day5_Tiles_DailyAvgs[1]),raster(Day5_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day5_name, datatype="HFA")
Day6_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day6_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day6_Tiles_DailyAvgs[1]),raster(Day6_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day6_name, datatype="HFA")
Day7_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day7_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day7_Tiles_DailyAvgs[1]),raster(Day7_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day7_name, datatype="HFA")
Day8_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day8_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day8_Tiles_DailyAvgs[1]),raster(Day8_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day8_name, datatype="HFA")
Day9_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day9_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day9_Tiles_DailyAvgs[1]),raster(Day9_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day9_name, datatype="HFA")
Day10_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day10_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day10_Tiles_DailyAvgs[1]),raster(Day10_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day10_name, datatype="HFA")
Day11_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day11_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day11_Tiles_DailyAvgs[1]),raster(Day11_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day11_name, datatype="HFA")
Day12_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day12_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day12_Tiles_DailyAvgs[1]),raster(Day12_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day12_name, datatype="HFA")
Day13_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day13_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day13_Tiles_DailyAvgs[1]),raster(Day13_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day13_name, datatype="HFA")
Day14_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day14_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day14_Tiles_DailyAvgs[1]),raster(Day14_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day14_name, datatype="HFA")
Day15_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day15_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day15_Tiles_DailyAvgs[1]),raster(Day15_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day15_name, datatype="HFA")
Day16_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day16_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day16_Tiles_DailyAvgs[1]),raster(Day16_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day16_name, datatype="HFA")
Day17_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day17_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day17_Tiles_DailyAvgs[1]),raster(Day17_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day17_name, datatype="HFA")
Day18_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day18_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day18_Tiles_DailyAvgs[1]),raster(Day18_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day18_name, datatype="HFA")
Day19_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day19_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day19_Tiles_DailyAvgs[1]),raster(Day19_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day19_name, datatype="HFA")
Day20_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day20_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day20_Tiles_DailyAvgs[1]),raster(Day20_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day20_name, datatype="HFA")
Day21_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day21_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day21_Tiles_DailyAvgs[1]),raster(Day21_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day21_name, datatype="HFA")
Day22_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day22_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day22_Tiles_DailyAvgs[1]),raster(Day22_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day22_name, datatype="HFA")
Day23_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day23_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day23_Tiles_DailyAvgs[1]),raster(Day23_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day23_name, datatype="HFA")
Day24_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day24_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day24_Tiles_DailyAvgs[1]),raster(Day24_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day24_name, datatype="HFA")
Day25_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day25_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day25_Tiles_DailyAvgs[1]),raster(Day25_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day25_name, datatype="HFA")
Day26_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day26_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day26_Tiles_DailyAvgs[1]),raster(Day26_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day26_name, datatype="HFA")
Day27_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day27_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day27_Tiles_DailyAvgs[1]),raster(Day27_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day27_name, datatype="HFA")
Day28_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day28_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day28_Tiles_DailyAvgs[1]),raster(Day28_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day28_name, datatype="HFA")
Day29_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day29_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day29_Tiles_DailyAvgs[1]),raster(Day29_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day29_name, datatype="HFA")
Day30_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day30_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day30_Tiles_DailyAvgs[1]),raster(Day30_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day30_name, datatype="HFA")
Day31_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day31_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day31_Tiles_DailyAvgs[1]),raster(Day31_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day31_name, datatype="HFA")
Day32_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day32_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day32_Tiles_DailyAvgs[1]),raster(Day32_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day32_name, datatype="HFA")
Day33_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day33_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day33_Tiles_DailyAvgs[1]),raster(Day33_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day33_name, datatype="HFA")
Day34_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day34_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day34_Tiles_DailyAvgs[1]),raster(Day34_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day34_name, datatype="HFA")
Day35_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day35_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day35_Tiles_DailyAvgs[1]),raster(Day35_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day35_name, datatype="HFA")
Day36_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day36_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day36_Tiles_DailyAvgs[1]),raster(Day36_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day36_name, datatype="HFA")
Day37_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day37_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day37_Tiles_DailyAvgs[1]),raster(Day37_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day37_name, datatype="HFA")
Day38_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day38_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day38_Tiles_DailyAvgs[1]),raster(Day38_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day38_name, datatype="HFA")
Day39_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day39_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day39_Tiles_DailyAvgs[1]),raster(Day39_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day39_name, datatype="HFA")
Day40_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day40_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day40_Tiles_DailyAvgs[1]),raster(Day40_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day40_name, datatype="HFA")
Day41_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day41_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day41_Tiles_DailyAvgs[1]),raster(Day41_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day41_name, datatype="HFA")
Day42_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day42_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day42_Tiles_DailyAvgs[1]),raster(Day42_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day42_name, datatype="HFA")
Day43_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day43_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day43_Tiles_DailyAvgs[1]),raster(Day43_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day43_name, datatype="HFA")
Day44_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day44_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day44_Tiles_DailyAvgs[1]),raster(Day44_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day44_name, datatype="HFA")
Day45_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day45_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day45_Tiles_DailyAvgs[1]),raster(Day45_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day45_name, datatype="HFA")
Day46_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day46_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day46_Tiles_DailyAvgs[1]),raster(Day46_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day46_name, datatype="HFA")
Day47_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day47_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day47_Tiles_DailyAvgs[1]),raster(Day47_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day47_name, datatype="HFA")
Day48_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day48_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day48_Tiles_DailyAvgs[1]),raster(Day48_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day48_name, datatype="HFA")
Day49_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day49_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day49_Tiles_DailyAvgs[1]),raster(Day49_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day49_name, datatype="HFA")
Day50_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day50_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day50_Tiles_DailyAvgs[1]),raster(Day50_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day50_name, datatype="HFA")
Day51_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day51_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day51_Tiles_DailyAvgs[1]),raster(Day51_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day51_name, datatype="HFA")
Day52_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day52_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day52_Tiles_DailyAvgs[1]),raster(Day52_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day52_name, datatype="HFA")
Day53_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day53_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day53_Tiles_DailyAvgs[1]),raster(Day53_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day53_name, datatype="HFA")
Day54_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day54_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day54_Tiles_DailyAvgs[1]),raster(Day54_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day54_name, datatype="HFA")
Day55_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day55_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day55_Tiles_DailyAvgs[1]),raster(Day55_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day55_name, datatype="HFA")
Day56_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day56_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day56_Tiles_DailyAvgs[1]),raster(Day56_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day56_name, datatype="HFA")
Day57_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day57_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day57_Tiles_DailyAvgs[1]),raster(Day57_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day57_name, datatype="HFA")
Day58_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day58_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day58_Tiles_DailyAvgs[1]),raster(Day58_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day58_name, datatype="HFA")
Day59_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day59_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day59_Tiles_DailyAvgs[1]),raster(Day59_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day59_name, datatype="HFA")
Day60_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day60_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day60_Tiles_DailyAvgs[1]),raster(Day60_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day60_name, datatype="HFA")
Day61_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day61_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day61_Tiles_DailyAvgs[1]),raster(Day61_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day61_name, datatype="HFA")
Day62_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day62_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day62_Tiles_DailyAvgs[1]),raster(Day62_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day62_name, datatype="HFA")
Day63_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day63_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day63_Tiles_DailyAvgs[1]),raster(Day63_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day63_name, datatype="HFA")
Day64_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day64_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day64_Tiles_DailyAvgs[1]),raster(Day64_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day64_name, datatype="HFA")
Day65_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day65_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day65_Tiles_DailyAvgs[1]),raster(Day65_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day65_name, datatype="HFA")
Day66_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day66_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day66_Tiles_DailyAvgs[1]),raster(Day66_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day66_name, datatype="HFA")
Day67_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day67_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day67_Tiles_DailyAvgs[1]),raster(Day67_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day67_name, datatype="HFA")
Day68_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day68_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day68_Tiles_DailyAvgs[1]),raster(Day68_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day68_name, datatype="HFA")
Day69_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day69_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day69_Tiles_DailyAvgs[1]),raster(Day69_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day69_name, datatype="HFA")
Day70_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day70_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day70_Tiles_DailyAvgs[1]),raster(Day70_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day70_name, datatype="HFA")
Day71_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day71_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day71_Tiles_DailyAvgs[1]),raster(Day71_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day71_name, datatype="HFA")
Day72_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day72_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day72_Tiles_DailyAvgs[1]),raster(Day72_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day72_name, datatype="HFA")
Day73_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day73_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day73_Tiles_DailyAvgs[1]),raster(Day73_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day73_name, datatype="HFA")
Day74_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day74_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day74_Tiles_DailyAvgs[1]),raster(Day74_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day74_name, datatype="HFA")
Day75_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day75_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day75_Tiles_DailyAvgs[1]),raster(Day75_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day75_name, datatype="HFA")
Day76_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day76_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day76_Tiles_DailyAvgs[1]),raster(Day76_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day76_name, datatype="HFA")
Day77_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day77_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day77_Tiles_DailyAvgs[1]),raster(Day77_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day77_name, datatype="HFA")
Day78_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day78_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day78_Tiles_DailyAvgs[1]),raster(Day78_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day78_name, datatype="HFA")
Day79_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day79_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day79_Tiles_DailyAvgs[1]),raster(Day79_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day79_name, datatype="HFA")
Day80_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day80_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day80_Tiles_DailyAvgs[1]),raster(Day80_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day80_name, datatype="HFA")
Day81_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day81_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day81_Tiles_DailyAvgs[1]),raster(Day81_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day81_name, datatype="HFA")
Day82_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day82_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day82_Tiles_DailyAvgs[1]),raster(Day82_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day82_name, datatype="HFA")
Day83_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day83_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day83_Tiles_DailyAvgs[1]),raster(Day83_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day83_name, datatype="HFA")
Day84_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day84_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day84_Tiles_DailyAvgs[1]),raster(Day84_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day84_name, datatype="HFA")
Day85_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day85_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day85_Tiles_DailyAvgs[1]),raster(Day85_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day85_name, datatype="HFA")
Day86_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day86_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day86_Tiles_DailyAvgs[1]),raster(Day86_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day86_name, datatype="HFA")
Day87_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day87_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day87_Tiles_DailyAvgs[1]),raster(Day87_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day87_name, datatype="HFA")
Day88_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day88_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day88_Tiles_DailyAvgs[1]),raster(Day88_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day88_name, datatype="HFA")
Day89_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day89_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day89_Tiles_DailyAvgs[1]),raster(Day89_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day89_name, datatype="HFA")
Day90_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day90_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day90_Tiles_DailyAvgs[1]),raster(Day90_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day90_name, datatype="HFA")
Day91_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day91_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day91_Tiles_DailyAvgs[1]),raster(Day91_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day91_name, datatype="HFA")
Day92_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day92_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day92_Tiles_DailyAvgs[1]),raster(Day92_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day92_name, datatype="HFA")
Day93_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day93_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day93_Tiles_DailyAvgs[1]),raster(Day93_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day93_name, datatype="HFA")
Day94_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day94_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day94_Tiles_DailyAvgs[1]),raster(Day94_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day94_name, datatype="HFA")
Day95_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day95_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day95_Tiles_DailyAvgs[1]),raster(Day95_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day95_name, datatype="HFA")
Day96_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day96_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day96_Tiles_DailyAvgs[1]),raster(Day96_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day96_name, datatype="HFA")
Day97_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day97_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day97_Tiles_DailyAvgs[1]),raster(Day97_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day97_name, datatype="HFA")
Day98_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day98_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day98_Tiles_DailyAvgs[1]),raster(Day98_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day98_name, datatype="HFA")
Day99_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day99_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day99_Tiles_DailyAvgs[1]),raster(Day99_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day99_name, datatype="HFA")
Day100_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day100_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day100_Tiles_DailyAvgs[1]),raster(Day100_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day100_name, datatype="HFA")
Day101_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day101_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day101_Tiles_DailyAvgs[1]),raster(Day101_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day101_name, datatype="HFA")
Day102_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day102_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day102_Tiles_DailyAvgs[1]),raster(Day102_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day102_name, datatype="HFA")
Day103_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day103_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day103_Tiles_DailyAvgs[1]),raster(Day103_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day103_name, datatype="HFA")
Day104_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day104_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day104_Tiles_DailyAvgs[1]),raster(Day104_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day104_name, datatype="HFA")
Day105_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day105_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day105_Tiles_DailyAvgs[1]),raster(Day105_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day105_name, datatype="HFA")
Day106_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day106_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day106_Tiles_DailyAvgs[1]),raster(Day106_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day106_name, datatype="HFA")
Day107_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day107_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day107_Tiles_DailyAvgs[1]),raster(Day107_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day107_name, datatype="HFA")
Day108_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day108_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day108_Tiles_DailyAvgs[1]),raster(Day108_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day108_name, datatype="HFA")
Day109_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day109_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day109_Tiles_DailyAvgs[1]),raster(Day109_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day109_name, datatype="HFA")
Day110_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day110_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day110_Tiles_DailyAvgs[1]),raster(Day110_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day110_name, datatype="HFA")
Day111_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day111_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day111_Tiles_DailyAvgs[1]),raster(Day111_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day111_name, datatype="HFA")
Day112_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day112_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day112_Tiles_DailyAvgs[1]),raster(Day112_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day112_name, datatype="HFA")
Day113_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day113_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day113_Tiles_DailyAvgs[1]),raster(Day113_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day113_name, datatype="HFA")
Day114_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day114_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day114_Tiles_DailyAvgs[1]),raster(Day114_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day114_name, datatype="HFA")
Day115_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day115_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day115_Tiles_DailyAvgs[1]),raster(Day115_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day115_name, datatype="HFA")
Day116_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day116_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day116_Tiles_DailyAvgs[1]),raster(Day116_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day116_name, datatype="HFA")
Day117_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day117_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day117_Tiles_DailyAvgs[1]),raster(Day117_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day117_name, datatype="HFA")
Day118_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day118_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day118_Tiles_DailyAvgs[1]),raster(Day118_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day118_name, datatype="HFA")
Day119_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day119_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day119_Tiles_DailyAvgs[1]),raster(Day119_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day119_name, datatype="HFA")
Day120_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day120_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day120_Tiles_DailyAvgs[1]),raster(Day120_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day120_name, datatype="HFA")
Day121_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day121_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day121_Tiles_DailyAvgs[1]),raster(Day121_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day121_name, datatype="HFA")
Day122_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day122_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day122_Tiles_DailyAvgs[1]),raster(Day122_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day122_name, datatype="HFA")
Day123_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day123_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day123_Tiles_DailyAvgs[1]),raster(Day123_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day123_name, datatype="HFA")
Day124_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day124_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day124_Tiles_DailyAvgs[1]),raster(Day124_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day124_name, datatype="HFA")
Day125_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day125_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day125_Tiles_DailyAvgs[1]),raster(Day125_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day125_name, datatype="HFA")
Day126_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day126_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day126_Tiles_DailyAvgs[1]),raster(Day126_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day126_name, datatype="HFA")
Day127_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day127_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day127_Tiles_DailyAvgs[1]),raster(Day127_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day127_name, datatype="HFA")
Day128_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day128_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day128_Tiles_DailyAvgs[1]),raster(Day128_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day128_name, datatype="HFA")
Day129_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day129_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day129_Tiles_DailyAvgs[1]),raster(Day129_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day129_name, datatype="HFA")
Day130_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day130_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day130_Tiles_DailyAvgs[1]),raster(Day130_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day130_name, datatype="HFA")
Day131_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day131_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day131_Tiles_DailyAvgs[1]),raster(Day131_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day131_name, datatype="HFA")
Day132_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day132_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day132_Tiles_DailyAvgs[1]),raster(Day132_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day132_name, datatype="HFA")
Day133_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day133_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day133_Tiles_DailyAvgs[1]),raster(Day133_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day133_name, datatype="HFA")
Day134_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day134_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day134_Tiles_DailyAvgs[1]),raster(Day134_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day134_name, datatype="HFA")
Day135_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day135_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day135_Tiles_DailyAvgs[1]),raster(Day135_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day135_name, datatype="HFA")
Day136_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day136_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day136_Tiles_DailyAvgs[1]),raster(Day136_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day136_name, datatype="HFA")
Day137_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day137_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day137_Tiles_DailyAvgs[1]),raster(Day137_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day137_name, datatype="HFA")
Day138_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day138_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day138_Tiles_DailyAvgs[1]),raster(Day138_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day138_name, datatype="HFA")
Day139_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day139_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day139_Tiles_DailyAvgs[1]),raster(Day139_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day139_name, datatype="HFA")
Day140_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day140_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day140_Tiles_DailyAvgs[1]),raster(Day140_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day140_name, datatype="HFA")
Day141_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day141_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day141_Tiles_DailyAvgs[1]),raster(Day141_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day141_name, datatype="HFA")
Day142_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day142_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day142_Tiles_DailyAvgs[1]),raster(Day142_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day142_name, datatype="HFA")
Day143_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day143_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day143_Tiles_DailyAvgs[1]),raster(Day143_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day143_name, datatype="HFA")
Day144_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day144_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day144_Tiles_DailyAvgs[1]),raster(Day144_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day144_name, datatype="HFA")
Day145_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day145_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day145_Tiles_DailyAvgs[1]),raster(Day145_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day145_name, datatype="HFA")
Day146_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day146_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day146_Tiles_DailyAvgs[1]),raster(Day146_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day146_name, datatype="HFA")
Day147_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day147_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day147_Tiles_DailyAvgs[1]),raster(Day147_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day147_name, datatype="HFA")
Day148_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day148_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day148_Tiles_DailyAvgs[1]),raster(Day148_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day148_name, datatype="HFA")
Day149_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day149_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day149_Tiles_DailyAvgs[1]),raster(Day149_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day149_name, datatype="HFA")
Day150_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day150_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day150_Tiles_DailyAvgs[1]),raster(Day150_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day150_name, datatype="HFA")
Day151_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day151_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day151_Tiles_DailyAvgs[1]),raster(Day151_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day151_name, datatype="HFA")
Day152_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day152_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day152_Tiles_DailyAvgs[1]),raster(Day152_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day152_name, datatype="HFA")
Day153_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day153_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day153_Tiles_DailyAvgs[1]),raster(Day153_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day153_name, datatype="HFA")
Day154_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day154_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day154_Tiles_DailyAvgs[1]),raster(Day154_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day154_name, datatype="HFA")
Day155_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day155_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day155_Tiles_DailyAvgs[1]),raster(Day155_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day155_name, datatype="HFA")
Day156_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day156_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day156_Tiles_DailyAvgs[1]),raster(Day156_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day156_name, datatype="HFA")
Day157_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day157_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day157_Tiles_DailyAvgs[1]),raster(Day157_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day157_name, datatype="HFA")
Day158_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day158_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day158_Tiles_DailyAvgs[1]),raster(Day158_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day158_name, datatype="HFA")
Day159_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day159_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day159_Tiles_DailyAvgs[1]),raster(Day159_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day159_name, datatype="HFA")
Day160_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day160_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day160_Tiles_DailyAvgs[1]),raster(Day160_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day160_name, datatype="HFA")
Day161_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day161_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day161_Tiles_DailyAvgs[1]),raster(Day161_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day161_name, datatype="HFA")
Day162_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day162_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day162_Tiles_DailyAvgs[1]),raster(Day162_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day162_name, datatype="HFA")
Day163_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day163_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day163_Tiles_DailyAvgs[1]),raster(Day163_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day163_name, datatype="HFA")
Day164_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day164_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day164_Tiles_DailyAvgs[1]),raster(Day164_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day164_name, datatype="HFA")
Day165_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day165_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day165_Tiles_DailyAvgs[1]),raster(Day165_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day165_name, datatype="HFA")
Day166_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day166_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day166_Tiles_DailyAvgs[1]),raster(Day166_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day166_name, datatype="HFA")
Day167_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day167_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day167_Tiles_DailyAvgs[1]),raster(Day167_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day167_name, datatype="HFA")
Day168_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day168_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day168_Tiles_DailyAvgs[1]),raster(Day168_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day168_name, datatype="HFA")
Day169_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day169_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day169_Tiles_DailyAvgs[1]),raster(Day169_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day169_name, datatype="HFA")
Day170_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day170_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day170_Tiles_DailyAvgs[1]),raster(Day170_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day170_name, datatype="HFA")
Day171_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day171_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day171_Tiles_DailyAvgs[1]),raster(Day171_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day171_name, datatype="HFA")
Day172_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day172_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day172_Tiles_DailyAvgs[1]),raster(Day172_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day172_name, datatype="HFA")
Day173_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day173_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day173_Tiles_DailyAvgs[1]),raster(Day173_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day173_name, datatype="HFA")
Day174_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day174_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day174_Tiles_DailyAvgs[1]),raster(Day174_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day174_name, datatype="HFA")
Day175_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day175_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day175_Tiles_DailyAvgs[1]),raster(Day175_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day175_name, datatype="HFA")
Day176_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day176_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day176_Tiles_DailyAvgs[1]),raster(Day176_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day176_name, datatype="HFA")
Day177_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day177_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day177_Tiles_DailyAvgs[1]),raster(Day177_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day177_name, datatype="HFA")
Day178_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day178_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day178_Tiles_DailyAvgs[1]),raster(Day178_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day178_name, datatype="HFA")
Day179_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day179_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day179_Tiles_DailyAvgs[1]),raster(Day179_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day179_name, datatype="HFA")
Day180_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day180_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day180_Tiles_DailyAvgs[1]),raster(Day180_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day180_name, datatype="HFA")
Day181_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day181_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day181_Tiles_DailyAvgs[1]),raster(Day181_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day181_name, datatype="HFA")
Day182_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day182_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day182_Tiles_DailyAvgs[1]),raster(Day182_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day182_name, datatype="HFA")
Day183_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day183_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day183_Tiles_DailyAvgs[1]),raster(Day183_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day183_name, datatype="HFA")
Day184_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day184_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day184_Tiles_DailyAvgs[1]),raster(Day184_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day184_name, datatype="HFA")
Day185_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day185_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day185_Tiles_DailyAvgs[1]),raster(Day185_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day185_name, datatype="HFA")
Day186_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day186_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day186_Tiles_DailyAvgs[1]),raster(Day186_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day186_name, datatype="HFA")
Day187_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day187_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day187_Tiles_DailyAvgs[1]),raster(Day187_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day187_name, datatype="HFA")
Day188_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day188_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day188_Tiles_DailyAvgs[1]),raster(Day188_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day188_name, datatype="HFA")
Day189_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day189_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day189_Tiles_DailyAvgs[1]),raster(Day189_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day189_name, datatype="HFA")
Day190_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day190_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day190_Tiles_DailyAvgs[1]),raster(Day190_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day190_name, datatype="HFA")
Day191_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day191_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day191_Tiles_DailyAvgs[1]),raster(Day191_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day191_name, datatype="HFA")
Day192_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day192_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day192_Tiles_DailyAvgs[1]),raster(Day192_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day192_name, datatype="HFA")
Day193_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day193_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day193_Tiles_DailyAvgs[1]),raster(Day193_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day193_name, datatype="HFA")
Day194_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day194_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day194_Tiles_DailyAvgs[1]),raster(Day194_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day194_name, datatype="HFA")
Day195_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day195_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day195_Tiles_DailyAvgs[1]),raster(Day195_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day195_name, datatype="HFA")
Day196_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day196_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day196_Tiles_DailyAvgs[1]),raster(Day196_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day196_name, datatype="HFA")
Day197_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day197_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day197_Tiles_DailyAvgs[1]),raster(Day197_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day197_name, datatype="HFA")
Day198_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day198_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day198_Tiles_DailyAvgs[1]),raster(Day198_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day198_name, datatype="HFA")
Day199_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day199_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day199_Tiles_DailyAvgs[1]),raster(Day199_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day199_name, datatype="HFA")
Day200_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day200_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day200_Tiles_DailyAvgs[1]),raster(Day200_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day200_name, datatype="HFA")
Day201_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day201_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day201_Tiles_DailyAvgs[1]),raster(Day201_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day201_name, datatype="HFA")
Day202_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day202_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day202_Tiles_DailyAvgs[1]),raster(Day202_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day202_name, datatype="HFA")
Day203_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day203_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day203_Tiles_DailyAvgs[1]),raster(Day203_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day203_name, datatype="HFA")
Day204_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day204_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day204_Tiles_DailyAvgs[1]),raster(Day204_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day204_name, datatype="HFA")
Day205_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day205_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day205_Tiles_DailyAvgs[1]),raster(Day205_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day205_name, datatype="HFA")
Day206_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day206_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day206_Tiles_DailyAvgs[1]),raster(Day206_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day206_name, datatype="HFA")
Day207_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day207_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day207_Tiles_DailyAvgs[1]),raster(Day207_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day207_name, datatype="HFA")
Day208_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day208_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day208_Tiles_DailyAvgs[1]),raster(Day208_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day208_name, datatype="HFA")
Day209_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day209_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day209_Tiles_DailyAvgs[1]),raster(Day209_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day209_name, datatype="HFA")
Day210_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day210_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day210_Tiles_DailyAvgs[1]),raster(Day210_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day210_name, datatype="HFA")
Day211_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day211_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day211_Tiles_DailyAvgs[1]),raster(Day211_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day211_name, datatype="HFA")
Day212_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day212_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day212_Tiles_DailyAvgs[1]),raster(Day212_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day212_name, datatype="HFA")
Day213_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day213_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day213_Tiles_DailyAvgs[1]),raster(Day213_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day213_name, datatype="HFA")
Day214_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day214_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day214_Tiles_DailyAvgs[1]),raster(Day214_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day214_name, datatype="HFA")
Day215_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day215_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day215_Tiles_DailyAvgs[1]),raster(Day215_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day215_name, datatype="HFA")
Day216_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day216_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day216_Tiles_DailyAvgs[1]),raster(Day216_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day216_name, datatype="HFA")
Day217_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day217_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day217_Tiles_DailyAvgs[1]),raster(Day217_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day217_name, datatype="HFA")
Day218_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day218_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day218_Tiles_DailyAvgs[1]),raster(Day218_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day218_name, datatype="HFA")
Day219_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day219_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day219_Tiles_DailyAvgs[1]),raster(Day219_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day219_name, datatype="HFA")
Day220_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day220_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day220_Tiles_DailyAvgs[1]),raster(Day220_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day220_name, datatype="HFA")
Day221_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day221_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day221_Tiles_DailyAvgs[1]),raster(Day221_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day221_name, datatype="HFA")
Day222_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day222_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day222_Tiles_DailyAvgs[1]),raster(Day222_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day222_name, datatype="HFA")
Day223_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day223_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day223_Tiles_DailyAvgs[1]),raster(Day223_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day223_name, datatype="HFA")
Day224_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day224_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day224_Tiles_DailyAvgs[1]),raster(Day224_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day224_name, datatype="HFA")
Day225_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day225_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day225_Tiles_DailyAvgs[1]),raster(Day225_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day225_name, datatype="HFA")
Day226_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day226_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day226_Tiles_DailyAvgs[1]),raster(Day226_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day226_name, datatype="HFA")
Day227_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day227_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day227_Tiles_DailyAvgs[1]),raster(Day227_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day227_name, datatype="HFA")
Day228_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day228_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day228_Tiles_DailyAvgs[1]),raster(Day228_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day228_name, datatype="HFA")
Day229_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day229_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day229_Tiles_DailyAvgs[1]),raster(Day229_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day229_name, datatype="HFA")
Day230_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day230_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day230_Tiles_DailyAvgs[1]),raster(Day230_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day230_name, datatype="HFA")
Day231_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day231_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day231_Tiles_DailyAvgs[1]),raster(Day231_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day231_name, datatype="HFA")
Day232_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day232_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day232_Tiles_DailyAvgs[1]),raster(Day232_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day232_name, datatype="HFA")
Day233_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day233_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day233_Tiles_DailyAvgs[1]),raster(Day233_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day233_name, datatype="HFA")
Day234_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day234_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day234_Tiles_DailyAvgs[1]),raster(Day234_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day234_name, datatype="HFA")
Day235_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day235_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day235_Tiles_DailyAvgs[1]),raster(Day235_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day235_name, datatype="HFA")
Day236_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day236_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day236_Tiles_DailyAvgs[1]),raster(Day236_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day236_name, datatype="HFA")
Day237_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day237_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day237_Tiles_DailyAvgs[1]),raster(Day237_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day237_name, datatype="HFA")
Day238_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day238_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day238_Tiles_DailyAvgs[1]),raster(Day238_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day238_name, datatype="HFA")
Day239_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day239_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day239_Tiles_DailyAvgs[1]),raster(Day239_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day239_name, datatype="HFA")
Day240_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day240_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day240_Tiles_DailyAvgs[1]),raster(Day240_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day240_name, datatype="HFA")
Day241_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day241_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day241_Tiles_DailyAvgs[1]),raster(Day241_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day241_name, datatype="HFA")
Day242_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day242_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day242_Tiles_DailyAvgs[1]),raster(Day242_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day242_name, datatype="HFA")
Day243_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day243_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day243_Tiles_DailyAvgs[1]),raster(Day243_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day243_name, datatype="HFA")
Day244_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day244_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day244_Tiles_DailyAvgs[1]),raster(Day244_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day244_name, datatype="HFA")
Day245_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day245_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day245_Tiles_DailyAvgs[1]),raster(Day245_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day245_name, datatype="HFA")
Day246_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day246_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day246_Tiles_DailyAvgs[1]),raster(Day246_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day246_name, datatype="HFA")
Day247_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day247_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day247_Tiles_DailyAvgs[1]),raster(Day247_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day247_name, datatype="HFA")
Day248_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day248_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day248_Tiles_DailyAvgs[1]),raster(Day248_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day248_name, datatype="HFA")
Day249_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day249_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day249_Tiles_DailyAvgs[1]),raster(Day249_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day249_name, datatype="HFA")
Day250_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day250_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day250_Tiles_DailyAvgs[1]),raster(Day250_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day250_name, datatype="HFA")
Day251_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day251_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day251_Tiles_DailyAvgs[1]),raster(Day251_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day251_name, datatype="HFA")
Day252_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day252_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day252_Tiles_DailyAvgs[1]),raster(Day252_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day252_name, datatype="HFA")
Day253_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day253_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day253_Tiles_DailyAvgs[1]),raster(Day253_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day253_name, datatype="HFA")
Day254_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day254_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day254_Tiles_DailyAvgs[1]),raster(Day254_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day254_name, datatype="HFA")
Day255_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day255_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day255_Tiles_DailyAvgs[1]),raster(Day255_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day255_name, datatype="HFA")
Day256_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day256_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day256_Tiles_DailyAvgs[1]),raster(Day256_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day256_name, datatype="HFA")
Day257_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day257_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day257_Tiles_DailyAvgs[1]),raster(Day257_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day257_name, datatype="HFA")
Day258_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day258_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day258_Tiles_DailyAvgs[1]),raster(Day258_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day258_name, datatype="HFA")
Day259_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day259_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day259_Tiles_DailyAvgs[1]),raster(Day259_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day259_name, datatype="HFA")
Day260_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day260_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day260_Tiles_DailyAvgs[1]),raster(Day260_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day260_name, datatype="HFA")
Day261_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day261_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day261_Tiles_DailyAvgs[1]),raster(Day261_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day261_name, datatype="HFA")
Day262_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day262_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day262_Tiles_DailyAvgs[1]),raster(Day262_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day262_name, datatype="HFA")
Day263_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day263_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day263_Tiles_DailyAvgs[1]),raster(Day263_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day263_name, datatype="HFA")
Day264_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day264_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day264_Tiles_DailyAvgs[1]),raster(Day264_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day264_name, datatype="HFA")
Day265_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day265_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day265_Tiles_DailyAvgs[1]),raster(Day265_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day265_name, datatype="HFA")
Day266_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day266_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day266_Tiles_DailyAvgs[1]),raster(Day266_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day266_name, datatype="HFA")
Day267_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day267_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day267_Tiles_DailyAvgs[1]),raster(Day267_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day267_name, datatype="HFA")
Day268_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day268_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day268_Tiles_DailyAvgs[1]),raster(Day268_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day268_name, datatype="HFA")
Day269_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day269_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day269_Tiles_DailyAvgs[1]),raster(Day269_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day269_name, datatype="HFA")
Day270_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day270_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day270_Tiles_DailyAvgs[1]),raster(Day270_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day270_name, datatype="HFA")
Day271_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day271_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day271_Tiles_DailyAvgs[1]),raster(Day271_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day271_name, datatype="HFA")
Day272_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day272_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day272_Tiles_DailyAvgs[1]),raster(Day272_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day272_name, datatype="HFA")
Day273_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day273_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day273_Tiles_DailyAvgs[1]),raster(Day273_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day273_name, datatype="HFA")
Day274_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day274_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day274_Tiles_DailyAvgs[1]),raster(Day274_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day274_name, datatype="HFA")
Day275_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day275_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day275_Tiles_DailyAvgs[1]),raster(Day275_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day275_name, datatype="HFA")
Day276_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day276_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day276_Tiles_DailyAvgs[1]),raster(Day276_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day276_name, datatype="HFA")
Day277_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day277_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day277_Tiles_DailyAvgs[1]),raster(Day277_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day277_name, datatype="HFA")
Day278_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day278_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day278_Tiles_DailyAvgs[1]),raster(Day278_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day278_name, datatype="HFA")
Day279_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day279_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day279_Tiles_DailyAvgs[1]),raster(Day279_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day279_name, datatype="HFA")
Day280_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day280_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day280_Tiles_DailyAvgs[1]),raster(Day280_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day280_name, datatype="HFA")
Day281_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day281_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day281_Tiles_DailyAvgs[1]),raster(Day281_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day281_name, datatype="HFA")
Day282_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day282_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day282_Tiles_DailyAvgs[1]),raster(Day282_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day282_name, datatype="HFA")
Day283_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day283_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day283_Tiles_DailyAvgs[1]),raster(Day283_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day283_name, datatype="HFA")
Day284_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day284_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day284_Tiles_DailyAvgs[1]),raster(Day284_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day284_name, datatype="HFA")
Day285_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day285_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day285_Tiles_DailyAvgs[1]),raster(Day285_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day285_name, datatype="HFA")
Day286_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day286_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day286_Tiles_DailyAvgs[1]),raster(Day286_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day286_name, datatype="HFA")
Day287_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day287_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day287_Tiles_DailyAvgs[1]),raster(Day287_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day287_name, datatype="HFA")
Day288_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day288_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day288_Tiles_DailyAvgs[1]),raster(Day288_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day288_name, datatype="HFA")
Day289_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day289_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day289_Tiles_DailyAvgs[1]),raster(Day289_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day289_name, datatype="HFA")
Day290_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day290_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day290_Tiles_DailyAvgs[1]),raster(Day290_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day290_name, datatype="HFA")
Day291_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day291_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day291_Tiles_DailyAvgs[1]),raster(Day291_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day291_name, datatype="HFA")
Day292_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day292_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day292_Tiles_DailyAvgs[1]),raster(Day292_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day292_name, datatype="HFA")
Day293_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day293_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day293_Tiles_DailyAvgs[1]),raster(Day293_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day293_name, datatype="HFA")
Day294_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day294_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day294_Tiles_DailyAvgs[1]),raster(Day294_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day294_name, datatype="HFA")
Day295_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day295_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day295_Tiles_DailyAvgs[1]),raster(Day295_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day295_name, datatype="HFA")
Day296_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day296_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day296_Tiles_DailyAvgs[1]),raster(Day296_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day296_name, datatype="HFA")
Day297_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day297_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day297_Tiles_DailyAvgs[1]),raster(Day297_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day297_name, datatype="HFA")
Day298_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day298_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day298_Tiles_DailyAvgs[1]),raster(Day298_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day298_name, datatype="HFA")
Day299_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day299_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day299_Tiles_DailyAvgs[1]),raster(Day299_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day299_name, datatype="HFA")
Day300_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day300_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day300_Tiles_DailyAvgs[1]),raster(Day300_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day300_name, datatype="HFA")
Day301_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day301_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day301_Tiles_DailyAvgs[1]),raster(Day301_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day301_name, datatype="HFA")
Day302_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day302_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day302_Tiles_DailyAvgs[1]),raster(Day302_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day302_name, datatype="HFA")
Day303_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day303_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day303_Tiles_DailyAvgs[1]),raster(Day303_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day303_name, datatype="HFA")
Day304_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day304_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day304_Tiles_DailyAvgs[1]),raster(Day304_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day304_name, datatype="HFA")
Day305_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day305_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day305_Tiles_DailyAvgs[1]),raster(Day305_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day305_name, datatype="HFA")
Day306_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day306_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day306_Tiles_DailyAvgs[1]),raster(Day306_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day306_name, datatype="HFA")
Day307_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day307_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day307_Tiles_DailyAvgs[1]),raster(Day307_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day307_name, datatype="HFA")
Day308_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day308_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day308_Tiles_DailyAvgs[1]),raster(Day308_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day308_name, datatype="HFA")
Day309_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day309_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day309_Tiles_DailyAvgs[1]),raster(Day309_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day309_name, datatype="HFA")
Day310_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day310_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day310_Tiles_DailyAvgs[1]),raster(Day310_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day310_name, datatype="HFA")
Day311_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day311_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day311_Tiles_DailyAvgs[1]),raster(Day311_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day311_name, datatype="HFA")
Day312_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day312_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day312_Tiles_DailyAvgs[1]),raster(Day312_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day312_name, datatype="HFA")
Day313_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day313_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day313_Tiles_DailyAvgs[1]),raster(Day313_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day313_name, datatype="HFA")
Day314_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day314_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day314_Tiles_DailyAvgs[1]),raster(Day314_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day314_name, datatype="HFA")
Day315_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day315_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day315_Tiles_DailyAvgs[1]),raster(Day315_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day315_name, datatype="HFA")
Day316_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day316_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day316_Tiles_DailyAvgs[1]),raster(Day316_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day316_name, datatype="HFA")
Day317_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day317_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day317_Tiles_DailyAvgs[1]),raster(Day317_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day317_name, datatype="HFA")
Day318_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day318_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day318_Tiles_DailyAvgs[1]),raster(Day318_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day318_name, datatype="HFA")
Day319_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day319_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day319_Tiles_DailyAvgs[1]),raster(Day319_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day319_name, datatype="HFA")
Day320_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day320_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day320_Tiles_DailyAvgs[1]),raster(Day320_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day320_name, datatype="HFA")
Day321_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day321_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day321_Tiles_DailyAvgs[1]),raster(Day321_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day321_name, datatype="HFA")
Day322_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day322_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day322_Tiles_DailyAvgs[1]),raster(Day322_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day322_name, datatype="HFA")
Day323_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day323_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day323_Tiles_DailyAvgs[1]),raster(Day323_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day323_name, datatype="HFA")
Day324_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day324_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day324_Tiles_DailyAvgs[1]),raster(Day324_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day324_name, datatype="HFA")
Day325_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day325_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day325_Tiles_DailyAvgs[1]),raster(Day325_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day325_name, datatype="HFA")
Day326_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day326_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day326_Tiles_DailyAvgs[1]),raster(Day326_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day326_name, datatype="HFA")
Day327_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day327_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day327_Tiles_DailyAvgs[1]),raster(Day327_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day327_name, datatype="HFA")
Day328_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day328_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day328_Tiles_DailyAvgs[1]),raster(Day328_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day328_name, datatype="HFA")
Day329_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day329_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day329_Tiles_DailyAvgs[1]),raster(Day329_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day329_name, datatype="HFA")
Day330_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day330_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day330_Tiles_DailyAvgs[1]),raster(Day330_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day330_name, datatype="HFA")
Day331_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day331_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day331_Tiles_DailyAvgs[1]),raster(Day331_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day331_name, datatype="HFA")
Day332_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day332_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day332_Tiles_DailyAvgs[1]),raster(Day332_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day332_name, datatype="HFA")
Day333_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day333_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day333_Tiles_DailyAvgs[1]),raster(Day333_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day333_name, datatype="HFA")
Day334_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day334_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day334_Tiles_DailyAvgs[1]),raster(Day334_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day334_name, datatype="HFA")
Day335_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day335_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day335_Tiles_DailyAvgs[1]),raster(Day335_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day335_name, datatype="HFA")
Day336_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day336_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day336_Tiles_DailyAvgs[1]),raster(Day336_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day336_name, datatype="HFA")
Day337_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day337_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day337_Tiles_DailyAvgs[1]),raster(Day337_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day337_name, datatype="HFA")
Day338_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day338_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day338_Tiles_DailyAvgs[1]),raster(Day338_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day338_name, datatype="HFA")
Day339_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day339_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day339_Tiles_DailyAvgs[1]),raster(Day339_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day339_name, datatype="HFA")
Day340_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day340_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day340_Tiles_DailyAvgs[1]),raster(Day340_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day340_name, datatype="HFA")
Day341_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day341_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day341_Tiles_DailyAvgs[1]),raster(Day341_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day341_name, datatype="HFA")
Day342_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day342_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day342_Tiles_DailyAvgs[1]),raster(Day342_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day342_name, datatype="HFA")
Day343_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day343_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day343_Tiles_DailyAvgs[1]),raster(Day343_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day343_name, datatype="HFA")
Day344_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day344_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day344_Tiles_DailyAvgs[1]),raster(Day344_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day344_name, datatype="HFA")
Day345_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day345_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day345_Tiles_DailyAvgs[1]),raster(Day345_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day345_name, datatype="HFA")
Day346_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day346_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day346_Tiles_DailyAvgs[1]),raster(Day346_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day346_name, datatype="HFA")
Day347_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day347_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day347_Tiles_DailyAvgs[1]),raster(Day347_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day347_name, datatype="HFA")
Day348_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day348_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day348_Tiles_DailyAvgs[1]),raster(Day348_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day348_name, datatype="HFA")
Day349_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day349_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day349_Tiles_DailyAvgs[1]),raster(Day349_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day349_name, datatype="HFA")
Day350_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day350_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day350_Tiles_DailyAvgs[1]),raster(Day350_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day350_name, datatype="HFA")
Day351_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day351_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day351_Tiles_DailyAvgs[1]),raster(Day351_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day351_name, datatype="HFA")
Day352_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day352_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day352_Tiles_DailyAvgs[1]),raster(Day352_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day352_name, datatype="HFA")
Day353_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day353_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day353_Tiles_DailyAvgs[1]),raster(Day353_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day353_name, datatype="HFA")
Day354_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day354_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day354_Tiles_DailyAvgs[1]),raster(Day354_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day354_name, datatype="HFA")
Day355_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day355_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day355_Tiles_DailyAvgs[1]),raster(Day355_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day355_name, datatype="HFA")
Day356_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day356_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day356_Tiles_DailyAvgs[1]),raster(Day356_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day356_name, datatype="HFA")
Day357_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day357_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day357_Tiles_DailyAvgs[1]),raster(Day357_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day357_name, datatype="HFA")
Day358_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day358_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day358_Tiles_DailyAvgs[1]),raster(Day358_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day358_name, datatype="HFA")
Day359_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day359_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day359_Tiles_DailyAvgs[1]),raster(Day359_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day359_name, datatype="HFA")
Day360_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day360_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day360_Tiles_DailyAvgs[1]),raster(Day360_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day360_name, datatype="HFA")
Day361_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day361_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day361_Tiles_DailyAvgs[1]),raster(Day361_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day361_name, datatype="HFA")
Day362_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day362_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day362_Tiles_DailyAvgs[1]),raster(Day362_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day362_name, datatype="HFA")
Day363_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day363_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day363_Tiles_DailyAvgs[1]),raster(Day363_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day363_name, datatype="HFA")
Day364_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day364_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day364_Tiles_DailyAvgs[1]),raster(Day364_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day364_name, datatype="HFA")
Day365_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day365_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day365_Tiles_DailyAvgs[1]),raster(Day365_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day365_name, datatype="HFA")
Day366_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day366_Mosaic_DailyAvg_Scaled.img"
mosaic(raster(Day366_Tiles_DailyAvgs[1]),raster(Day366_Tiles_DailyAvgs[2]), fun=mean,na.rm=TRUE, filename=Day366_name, datatype="HFA")