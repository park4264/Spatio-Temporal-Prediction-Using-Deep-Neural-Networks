################################################################################
################################################################################
################################################################################

################################################################################
##########################        최종1_220109       ###########################
################################################################################

################################################################################
################################################################################
################################################################################


library(geoR)
library(spatstat)
library(caret)

library(splitTools)
library(scoringRules)
library(verification)

library(sp)
library(spacetime)
library(ggplot2)
library(dplyr)
library(gstat)
library(RColorBrewer)
library(STRbook)
library(tidyr)



# 원본 Covariate dataframe
#covariate_df[,'tmp'] = na.approx(covariate_df[,'tmp'])
#covariate_df[,'uwind'] = na.approx(covariate_df[,'uwind'])
#covariate_df[,'vwind'] = na.approx(covariate_df[,'vwind'])
#covariate_df[,'prec'] = na.approx(covariate_df[,'prec'])
#covariate_df[,'hum'] = na.approx(covariate_df[,'hum'])
#covariate_df = covariate_df[ order( covariate_df[,'t'], covariate_df[,'code']), ]
covariate_df = read.csv("AWS_220109_R_ver2.csv")
sum(is.na(covariate_df))

hist(covariate_df[,'wind'])

covariate_df[,'wind'] = log(covariate_df[,'wind'] + 0.2) ##220109
hist(covariate_df[,'wind'])



hist(covariate_df[,'uwind'])
hist(covariate_df[,'vwind'])
hist(covariate_df[,'prec'])



covariate_sp = covariate_df[,2:3] %>% unique() %>% SpatialPoints
time = as.POSIXct("2022-02-01") + 3600*(1:24)
covariate_data = covariate_df[c(  "tmp","uwind","vwind" ,"wind", "prec", "hum")]

covariate_stfdf = STFDF(covariate_sp, time, endTime = delta(time), data = covariate_data) #=> training data
plot(covariate_stfdf[,1])
stplot(covariate_stfdf, cex = 0.5)

st_data = read.csv("PM25_220109_R.csv")
sp = st_data[,2:3] %>% unique() %>% SpatialPoints
time = as.POSIXct("2022-02-01") + 3600*(1:24)



length(sp)


# kriging할 위치
covariate_spat_pred_grid1 = sp 
covariate_temp_pred_grid1 = time

covariate_DE_pred1 <- STF(sp = covariate_spat_pred_grid1, 
                          time = covariate_temp_pred_grid1)

#covariate_DE_pred2 <- STF(sp = sp, 
#                          time = time)



# 1. tmp
covariate_tmp_vv = variogramST(formula = tmp ~ 1 , data = covariate_stfdf )
plot(covariate_tmp_vv, map = F)
covariate_tmp_sepVgm = vgmST(stModel = "separable",
                             space = vgm(2, "Exp", 5, nugget = 0.1),
                             time = vgm(10, "Gau", 30,  nugget = 0.1),
                             sill = 1)
covariate_tmp_sepVgm = fit.StVariogram(covariate_tmp_vv, covariate_tmp_sepVgm)
plot(covariate_tmp_vv, covariate_tmp_sepVgm, map = F)
covariate_tmp_pred_kriged <- krigeST(tmp ~ 1 , 
                                     data = covariate_stfdf, 
                                     newdata = covariate_DE_pred1,
                                     modelList = covariate_tmp_sepVgm,
                                     computeVar = FALSE) 


# 2. uwind
covariate_uwind_vv = variogramST(formula = uwind ~ 1 , data = covariate_stfdf )
plot(covariate_uwind_vv, map = F)
covariate_uwind_sepVgm = vgmST(stModel = "separable",
                               space = vgm(0.5, "Exp", 5, nugget = 0.1),
                               time = vgm(2, "Exp", 5,  nugget = 0.1),
                               sill = 1)
covariate_uwind_sepVgm = fit.StVariogram(covariate_uwind_vv, covariate_uwind_sepVgm)
plot(covariate_uwind_vv, covariate_uwind_sepVgm, map = F)
covariate_uwind_pred_kriged <- krigeST(uwind ~ 1 , 
                                       data = covariate_stfdf, 
                                       newdata = covariate_DE_pred1,
                                       modelList = covariate_uwind_sepVgm,
                                       computeVar = FALSE) 

# 3. vwind
covariate_vwind_vv = variogramST(formula = vwind ~ 1 , data = covariate_stfdf )
plot(covariate_vwind_vv, map = F)
covariate_vwind_sepVgm = vgmST(stModel = "separable",
                               space = vgm(1, "Exp", 5, nugget = 0.1),
                               time = vgm(2, "Exp", 10,  nugget = 0.1),
                               sill = 1)
covariate_vwind_sepVgm = fit.StVariogram(covariate_vwind_vv, covariate_vwind_sepVgm)
plot(covariate_vwind_vv, covariate_vwind_sepVgm, map = F)
covariate_vwind_pred_kriged <- krigeST(vwind ~ 1 , 
                                       data = covariate_stfdf, 
                                       newdata = covariate_DE_pred1,
                                       modelList = covariate_vwind_sepVgm,
                                       computeVar = FALSE) 




# 4. wind
covariate_wind_vv = variogramST(formula = wind ~ 1 , data = covariate_stfdf )
plot(covariate_wind_vv, map = F)
covariate_wind_sepVgm = vgmST(stModel = "separable",
                              space = vgm(0.5, "Exp", 1, nugget = 0.1),
                              time = vgm(2, "Exp", 5,  nugget = 0.1),
                              sill = 1)
covariate_wind_sepVgm = fit.StVariogram(covariate_wind_vv, covariate_wind_sepVgm)
plot(covariate_wind_vv, covariate_wind_sepVgm, map = F)
covariate_wind_pred_kriged <- krigeST(wind ~ 1 , 
                                      data = covariate_stfdf, 
                                      newdata = covariate_DE_pred1,
                                      modelList = covariate_wind_sepVgm,
                                      computeVar = FALSE) 


# 5. hum
covariate_hum_vv = variogramST(formula = hum ~ 1 , data = covariate_stfdf )
plot(covariate_hum_vv, map = F)
covariate_hum_sepVgm = vgmST(stModel = "separable",
                             space = vgm(100, "Exp", 200, nugget = 0.1),
                             time = vgm(1, "Exp", 10,  nugget = 1),
                             sill = 1)
covariate_hum_sepVgm = fit.StVariogram(covariate_hum_vv, covariate_hum_sepVgm)
plot(covariate_hum_vv, covariate_hum_sepVgm, map = F)
covariate_hum_pred_kriged <- krigeST(hum ~ 1 , 
                                     data = covariate_stfdf, 
                                     newdata = covariate_DE_pred1,
                                     modelList = covariate_hum_sepVgm,
                                     computeVar = FALSE) 


# 6. prec
covariate_prec_vv = variogramST(formula = prec ~ 1 , data = covariate_stfdf )
plot(covariate_prec_vv, map = F)
covariate_prec_sepVgm = vgmST(stModel = "separable",
                              space = vgm(1, "Exp", 5, nugget = 0.1),
                              time = vgm(1, "Exp", 0.05,  nugget = 1),
                              sill = 100000000)
covariate_prec_sepVgm = fit.StVariogram(covariate_prec_vv, covariate_prec_sepVgm)
plot(covariate_prec_vv, covariate_prec_sepVgm, map = F)
covariate_prec_pred_kriged <- krigeST(prec ~ 1 , 
                                      data = covariate_stfdf, 
                                      newdata = covariate_DE_pred1,
                                      modelList = covariate_prec_sepVgm,
                                      computeVar = FALSE) 



#stplot(covariate_tmp_pred_kriged)
#stplot(covariate_uwind_pred_kriged)
#stplot(covariate_vind_pred_kriged)
#stplot(covariate_wind_pred_kriged)
#stplot(covariate_hum_pred_kriged)
#stplot(covariate_prec_pred_kriged)

kriged_tmp = covariate_tmp_pred_kriged$var1.pred
kriged_uwind = covariate_uwind_pred_kriged$var1.pred
kriged_vwind = covariate_vwind_pred_kriged$var1.pred
kriged_wind = covariate_wind_pred_kriged$var1.pred
kriged_hum = covariate_hum_pred_kriged$var1.pred
kriged_prec = covariate_prec_pred_kriged$var1.pred



dim(covariate_wind_pred_kriged)

dim(st_data)
length(kriged_hum)

# 이제 kriged Covariate로 STkriging

# 이것은 원본 전체 STFDF 데이터 => [stfdf]
st_data = read.csv("PM25_220109_R.csv")

st_data['tmp'] = kriged_tmp
st_data['uwind'] = kriged_uwind
st_data['vwind'] = kriged_vwind
st_data['wind'] = kriged_wind
st_data['hum'] = kriged_hum
st_data['prec'] = kriged_prec

#write.csv(st_data,"PM25_with_covariate_220109_R_ver2.csv") ################save
st_data = read.csv("PM25_with_covariate_220109_R_ver2.csv") ################open

hist(st_data[,'z'])


sp = st_data[,2:3] %>% unique() %>% SpatialPoints
time = as.POSIXct("2022-02-01") + 3600*(1:24)
mydata = st_data[c("z", "tmp", "uwind", "vwind", "wind", "hum", "prec")]


stfdf = STFDF(sp, time, endTime = delta(time), mydata)
str(stfdf)
plot(stfdf[,1])

dim(stfdf)
length(stfdf)
mean(mydata[,'z'])

#stplot(stfdf, cex = 0.5)


date = c("01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00",
         "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00",
         "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00")


time1 = c(14, 17)
time2 = c(4, 7)
time3 = c(5, 18)
time4 = c(13, 16)
time5 = c(2, 6)

#time1 = c(time[time0[1]] , time[time0[2]], time[time0[3]])
#time2 = c("22-02-01", "22-02-02", "22-02-03")
#time3 = c("22-03-01", "22-03-02", "22-03-03")
time_list = list(time1, time2, time3, time4, time5)
time_str_list = list(date[time1], date[time2], date[time3], date[time4], date[time5])

# 검정할 위치
loc1 = c(21, 46, 48, 72, 94, 95, 97, 155, 202, 228, 231, 238, 243, 260, 262, 275, 300, 304, 314, 322, 324, 355, 399, 406, 411, 414, 437, 438, 455, 473)
loc2 = c(12, 24, 25, 40, 61, 91, 94, 118, 137, 163, 176, 220, 228, 282, 295, 307, 313, 333, 350, 359, 362, 375, 378, 405, 449, 451, 452, 455, 473, 477)
loc3 = c(36, 95, 119, 141, 144, 154, 158, 164, 211, 227, 245, 246, 254, 257, 265, 270, 272, 315, 323, 325, 333, 344, 351, 418, 430, 450, 456, 460, 463, 465)
loc4 = c(4, 14, 37, 51, 59, 80, 90, 115, 148, 155, 166, 173, 183, 193, 194, 207, 263, 277, 291, 302, 324, 343, 352, 353, 355, 359, 392, 396, 430, 441)
loc5 = c(20, 21, 40, 45, 76, 94, 118, 131, 154, 162, 180, 197, 198, 207, 219, 241, 257, 273, 276, 323, 339, 368, 369, 378, 382, 395, 430, 443, 468, 472)



loc_list = list(loc1, loc2, loc3, loc4, loc5)
















acc = c()

for (i in 1:length(time_list))
{
  print(cat("i (time) = ", i))
  
  for (j in 1:length(loc_list))
  {
    
    
    print(cat("j (location) =", j))
    
    
    # Step: Training and Validation Data #
    
    # Training Data => [stfdf2]
    st_data2 = read.csv("PM25_220109_R.csv")
    sp2 = st_data2[,2:3] %>% unique() %>% SpatialPoints
    sp_tmp = sp2[ - loc_list[[j]], ] ### 건드리는 부분 [뺄 위치] ☆
    stfdf2 = stfdf[ - loc_list[[j]], ] ### 건드리는 부분 [뺄 위치] ☆
    
    
    stfdf2$timeHM = format(time(stfdf2), "%H:%M")
    valblock_idx = which( stfdf2$timeHM %in% time_str_list[[i]] ) ### 건드리는 부분[뺼 시간] ☆
    time2 = as.POSIXct("2022-02-01") + 3600*(1:24)
    time_tmp = time2[ -valblock_idx[1: length(time_list[[i]]) ] ] ### 건드리는 부분 [뺄 시간 개수] ☆☜
    
    
    mtot = length(stfdf2)
    obs_idx = setdiff(1:mtot, valblock_idx)
    
    mydata_tmp = data.frame(  "tmp" =  stfdf2$tmp[obs_idx], 
                              "uwind" = stfdf2$uwind[obs_idx],
                              "vwind" = stfdf2$vwind[obs_idx],
                              "wind" = stfdf2$wind[obs_idx],
                              "prec" = stfdf2$prec[obs_idx], 
                              "hum" = stfdf2$hum[obs_idx], 
                              "z" = stfdf2$z[obs_idx])
    
    stfdf2 = STFDF(sp_tmp, time_tmp, endTime = delta(time_tmp), mydata_tmp) # => training data 완성!
    
    
    # Validation Data (Location + Time) => [DE_pred1]
    
    # kriging할 위치 + 원본의 PM25 준비해야함.
    spat_pred_grid1 = sp2[ loc_list[[j]], ] ### 건드리는 부분 [뺐던 위치] ☆ 
    #temp_pred_grid1 = as.POSIXct( time_str[i] ) + 3600*c(0,10,15) ### 건드리는 부분 [뺐던 시간] ☆
    temp_pred_grid1 = c( as.POSIXct(  time[ sort(time_list[[i]]) ] ) )
    
    
    
    target_data = data.frame( "tmp"   = stfdf[loc_list[[j]], time_list[[i]] ]$tmp,
                              "uwind"  = stfdf[loc_list[[j]], time_list[[i]] ]$uwind,
                              "vwind"  = stfdf[loc_list[[j]], time_list[[i]] ]$vwind,
                              "wind"  = stfdf[loc_list[[j]], time_list[[i]] ]$wind,
                              "prec"  = stfdf[loc_list[[j]], time_list[[i]] ]$prec,
                              "hum"   = stfdf[loc_list[[j]], time_list[[i]] ]$hum)
    
    
    DE_pred1 <- STFDF(sp = spat_pred_grid1, # spatial part
                      time = temp_pred_grid1,
                      data = target_data) # temporal part
    DE_pred1 #=> kriging할 부분
    
    
    
    
    # kriging 부분1의 원본 데이터
    #stfdf[DE_pred1, ]$z
    
    stfdf[ loc_list[[j]], sort(time_list[[i]]) ]$z ### ☆ ☜
    
    ########################## 적합 ##########################
    
    vv = variogramST(formula = z ~ 1 + x + y #+ tmp + uwind + vwind  + prec + hum 
                     , data = stfdf2 )
    
    plot(vv, map = F)
    
    #sepVgm = vgmST(stModel = "separable",
    #               space = vgm(10, "Exp", 5, nugget = 1),
    #               time = vgm(10, "Exp", 700,  nugget = 1),
    #               sill = 1)
    
    sepVgm = vgmST(stModel = "metric",
                   joint =  vgm(100, "Gau", 700, nugget = 1),
                   stAni = 150,
                   sill = 1)
    
    
    sepVgm = fit.StVariogram(vv, sepVgm)
    plot(vv, sepVgm, map = F)
    #plot(vv, sepVgm, map = F)
    
    ########################## krige ##########################
    
    
    
    
    pred_kriged <- krigeST(z ~ 1 + x + y + tmp + uwind + vwind + wind + prec + hum, # latitude trend
                           data = stfdf2, 
                           newdata = DE_pred1, # prediction grid
                           modelList = sepVgm, # semivariogram
                           computeVar = FALSE) # compute variances
    
    stplot(pred_kriged)
    
    # PLOTTING
    
    #stplot(stfdf[loc1, time0]) ### ☆
    #stplot(pred_kriged)
    
    #pred_kriged[2,][,1]
    
    
    #print(pred_kriged$var1.pred)
    
    print(cat(i, j))
    
    print( sum((pred_kriged$var1.pred - stfdf[ loc_list[[j]], sort(time_list[[i]]) ]$z )^2) / length(pred_kriged$var1.pred))
    
    acc = c(acc, sum((pred_kriged$var1.pred - stfdf[ loc_list[[j]], sort(time_list[[i]]) ]$z )^2) / length(pred_kriged$var1.pred) )
    
  }
  
}
#acc=c()

acc
mean(acc)
sd(acc)


mean(sqrt(acc))
sd(sqrt(acc))


################################################################################
################################################################################
################################################################################

################################################################################
##########################        최종2_220201       ###########################
################################################################################

################################################################################
################################################################################
################################################################################


library(geoR)
library(spatstat)
library(caret)

library(splitTools)
library(scoringRules)
library(verification)

library(sp)
library(spacetime)
library(ggplot2)
library(dplyr)
library(gstat)
library(RColorBrewer)
library(STRbook)
library(tidyr)



# 원본 Covariate dataframe
#covariate_df[,'tmp'] = na.approx(covariate_df[,'tmp'])
#covariate_df[,'uwind'] = na.approx(covariate_df[,'uwind'])
#covariate_df[,'vwind'] = na.approx(covariate_df[,'vwind'])
#covariate_df[,'prec'] = na.approx(covariate_df[,'prec'])
#covariate_df[,'hum'] = na.approx(covariate_df[,'hum'])
#covariate_df = covariate_df[ order( covariate_df[,'t'], covariate_df[,'code']), ]
covariate_df = read.csv("AWS_220201_R_ver2.csv")
sum(is.na(covariate_df))

hist(covariate_df[,'wind'])

covariate_df[,'wind'] = log(covariate_df[,'wind'] + 0.2) ##220201
hist(covariate_df[,'wind'])



hist(covariate_df[,'uwind'])
hist(covariate_df[,'vwind'])
hist(covariate_df[,'prec'])



covariate_sp = covariate_df[,2:3] %>% unique() %>% SpatialPoints
time = as.POSIXct("2022-02-01") + 3600*(1:24)
covariate_data = covariate_df[c(  "tmp","uwind","vwind" ,"wind", "prec", "hum")]

covariate_stfdf = STFDF(covariate_sp, time, endTime = delta(time), data = covariate_data) #=> training data
plot(covariate_stfdf[,1])
stplot(covariate_stfdf, cex = 0.5)

st_data = read.csv("PM25_220201_R.csv")
sp = st_data[,2:3] %>% unique() %>% SpatialPoints
time = as.POSIXct("2022-02-01") + 3600*(1:24)

length(sp)


# kriging할 위치
covariate_spat_pred_grid1 = sp 
covariate_temp_pred_grid1 = time

covariate_DE_pred1 <- STF(sp = covariate_spat_pred_grid1, 
                          time = covariate_temp_pred_grid1)

#covariate_DE_pred2 <- STF(sp = sp, 
#                          time = time)



# 1. tmp
covariate_tmp_vv = variogramST(formula = tmp ~ 1 , data = covariate_stfdf )
plot(covariate_tmp_vv, map = F)
covariate_tmp_sepVgm = vgmST(stModel = "separable",
                             space = vgm(20, "Exp", 50, nugget = 0.1),
                             time = vgm(1, "Exp", 10,  nugget = 0.1),
                             sill = 1)
covariate_tmp_sepVgm = fit.StVariogram(covariate_tmp_vv, covariate_tmp_sepVgm)
plot(covariate_tmp_vv, covariate_tmp_sepVgm, map = F)
covariate_tmp_pred_kriged <- krigeST(tmp ~ 1 , 
                                     data = covariate_stfdf, 
                                     newdata = covariate_DE_pred1,
                                     modelList = covariate_tmp_sepVgm,
                                     computeVar = FALSE) 


# 2. uwind
covariate_uwind_vv = variogramST(formula = uwind ~ 1 , data = covariate_stfdf )
plot(covariate_uwind_vv, map = F)
covariate_uwind_sepVgm = vgmST(stModel = "separable",
                               space = vgm(0.5, "Exp", 5, nugget = 0.1),
                               time = vgm(2, "Exp", 5,  nugget = 0.1),
                               sill = 1)
covariate_uwind_sepVgm = fit.StVariogram(covariate_uwind_vv, covariate_uwind_sepVgm)
plot(covariate_uwind_vv, covariate_uwind_sepVgm, map = F)
covariate_uwind_pred_kriged <- krigeST(uwind ~ 1 , 
                                       data = covariate_stfdf, 
                                       newdata = covariate_DE_pred1,
                                       modelList = covariate_uwind_sepVgm,
                                       computeVar = FALSE) 

# 3. vwind
covariate_vwind_vv = variogramST(formula = vwind ~ 1 , data = covariate_stfdf )
plot(covariate_vwind_vv, map = F)
covariate_vwind_sepVgm = vgmST(stModel = "separable",
                               space = vgm(1, "Exp", 5, nugget = 0.1),
                               time = vgm(2, "Exp", 10,  nugget = 0.1),
                               sill = 1)
covariate_vwind_sepVgm = fit.StVariogram(covariate_vwind_vv, covariate_vwind_sepVgm)
plot(covariate_vwind_vv, covariate_vwind_sepVgm, map = F)
covariate_vwind_pred_kriged <- krigeST(vwind ~ 1 , 
                                       data = covariate_stfdf, 
                                       newdata = covariate_DE_pred1,
                                       modelList = covariate_vwind_sepVgm,
                                       computeVar = FALSE) 




# 4. wind
covariate_wind_vv = variogramST(formula = wind ~ 1 , data = covariate_stfdf )
plot(covariate_wind_vv, map = F)
covariate_wind_sepVgm = vgmST(stModel = "separable",
                              space = vgm(0.5, "Exp", 1, nugget = 0.1),
                              time = vgm(2, "Exp", 5,  nugget = 0.1),
                              sill = 1)
covariate_wind_sepVgm = fit.StVariogram(covariate_wind_vv, covariate_wind_sepVgm)
plot(covariate_wind_vv, covariate_wind_sepVgm, map = F)
covariate_wind_pred_kriged <- krigeST(wind ~ 1 , 
                                      data = covariate_stfdf, 
                                      newdata = covariate_DE_pred1,
                                      modelList = covariate_wind_sepVgm,
                                      computeVar = FALSE) 


# 5. hum
covariate_hum_vv = variogramST(formula = hum ~ 1 , data = covariate_stfdf )
plot(covariate_hum_vv, map = F)
covariate_hum_sepVgm = vgmST(stModel = "separable",
                             space = vgm(100, "Exp", 200, nugget = 0.1),
                             time = vgm(1, "Exp", 10,  nugget = 1),
                             sill = 1)
covariate_hum_sepVgm = fit.StVariogram(covariate_hum_vv, covariate_hum_sepVgm)
plot(covariate_hum_vv, covariate_hum_sepVgm, map = F)
covariate_hum_pred_kriged <- krigeST(hum ~ 1 , 
                                     data = covariate_stfdf, 
                                     newdata = covariate_DE_pred1,
                                     modelList = covariate_hum_sepVgm,
                                     computeVar = FALSE) 


# 6. prec
covariate_prec_vv = variogramST(formula = prec ~ 1 , data = covariate_stfdf )
plot(covariate_prec_vv, map = F)
covariate_prec_sepVgm = vgmST(stModel = "separable",
                              space = vgm(1, "Exp", 5, nugget = 0.1),
                              time = vgm(1, "Exp", 0.05,  nugget = 1),
                              sill = 10)
covariate_prec_sepVgm = fit.StVariogram(covariate_prec_vv, covariate_prec_sepVgm)
plot(covariate_prec_vv, covariate_prec_sepVgm, map = F)
covariate_prec_pred_kriged <- krigeST(prec ~ 1 , 
                                      data = covariate_stfdf, 
                                      newdata = covariate_DE_pred1,
                                      modelList = covariate_prec_sepVgm,
                                      computeVar = FALSE) 



#stplot(covariate_tmp_pred_kriged)
#stplot(covariate_uwind_pred_kriged)
#stplot(covariate_vind_pred_kriged)
#stplot(covariate_wind_pred_kriged)
#stplot(covariate_hum_pred_kriged)
#stplot(covariate_prec_pred_kriged)

kriged_tmp = covariate_tmp_pred_kriged$var1.pred
kriged_uwind = covariate_uwind_pred_kriged$var1.pred
kriged_vwind = covariate_vwind_pred_kriged$var1.pred
kriged_wind = covariate_wind_pred_kriged$var1.pred
kriged_hum = covariate_hum_pred_kriged$var1.pred
kriged_prec = covariate_prec_pred_kriged$var1.pred



dim(covariate_wind_pred_kriged)

dim(st_data)
length(kriged_tmp)

# 이제 kriged Covariate로 STkriging

# 이것은 원본 전체 STFDF 데이터 => [stfdf]
st_data = read.csv("PM25_220201_R.csv")

st_data['tmp'] = kriged_tmp
st_data['uwind'] = kriged_uwind
st_data['vwind'] = kriged_vwind
st_data['wind'] = kriged_wind
st_data['hum'] = kriged_hum
st_data['prec'] = kriged_prec

#write.csv(st_data,"PM25_with_covariate_220201_R_ver2.csv") ################save
st_data = read.csv("PM25_with_covariate_220201_R_ver2.csv")







sp = st_data[,2:3] %>% unique() %>% SpatialPoints
time = as.POSIXct("2022-02-01") + 3600*(1:24)
mydata = st_data[c("z", "tmp", "uwind", "vwind", "wind", "hum", "prec")]


stfdf = STFDF(sp, time, endTime = delta(time), mydata)
str(stfdf)
plot(stfdf[,1])







dim(stfdf)
length(stfdf)
mean(mydata[,'z'])
hist(mydata[,'z'])





#stplot(stfdf, cex = 0.5)


date = c("01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00",
         "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00",
         "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00")



time1 = c(1, 18)
time2 = c(21, 23)
time3 = c(9, 17)
time4 = c(14, 18)
time5 = c(8, 15)

#time1 = c(time[time0[1]] , time[time0[2]], time[time0[3]])
#time2 = c("22-02-01", "22-02-02", "22-02-03")
#time3 = c("22-03-01", "22-03-02", "22-03-03")
time_list = list(time1, time2, time3, time4, time5)
time_str_list = list(date[time1], date[time2], date[time3], date[time4], date[time5])

# 검정할 위치
loc1 = c(7, 17, 22, 38, 71, 82, 105, 127, 142, 145, 167, 181, 184, 195, 215, 219, 236, 247, 250, 251, 266, 295, 308, 334, 381, 415, 416, 421, 423, 440)
loc2 = c(13, 38, 47, 51, 52, 64, 77, 86, 102, 106, 133, 162, 163, 167, 168, 171, 208, 210, 218, 230, 242, 293, 297, 320, 325, 345, 348, 392, 402, 434)
loc3 = c(3, 13, 15, 24, 34, 37, 41, 68, 81, 107, 124, 131, 178, 192, 203, 205, 236, 267, 271, 274, 306, 312, 318, 328, 334, 335, 417, 423, 425, 431)
loc4 = c(14, 16, 27, 30, 65, 66, 90, 103, 105, 125, 141, 144, 160, 178, 225, 235, 268, 270, 309, 317, 326, 328, 340, 362, 377, 380, 390, 405, 419, 435)
loc5 = c(32, 35, 43, 45, 50, 78, 96, 113, 114, 124, 162, 163, 168, 171, 177, 186, 219, 242, 275, 285, 309, 311, 325, 326, 346, 354, 392, 406, 422, 442)





loc_list = list(loc1, loc2, loc3, loc4, loc5)
















acc = c()

for (i in 1:length(time_list))
{
  print(cat("i (time) = ", i))
  
  for (j in 1:length(loc_list))
  {
    
    
    print(cat("j (location) =", j))
    
    
    # Step: Training and Validation Data #
    
    # Training Data => [stfdf2]
    st_data2 = read.csv("PM25_220201_R.csv")
    sp2 = st_data2[,2:3] %>% unique() %>% SpatialPoints
    sp_tmp = sp2[ - loc_list[[j]], ] ### 건드리는 부분 [뺄 위치] ☆
    stfdf2 = stfdf[ - loc_list[[j]], ] ### 건드리는 부분 [뺄 위치] ☆
    
    
    stfdf2$timeHM = format(time(stfdf2), "%H:%M")
    valblock_idx = which( stfdf2$timeHM %in% time_str_list[[i]] ) ### 건드리는 부분[뺼 시간] ☆
    time2 = as.POSIXct("2022-02-01") + 3600*(1:24)
    time_tmp = time2[ -valblock_idx[1: length(time_list[[i]]) ] ] ### 건드리는 부분 [뺄 시간 개수] ☆☜
    
    
    mtot = length(stfdf2)
    obs_idx = setdiff(1:mtot, valblock_idx)
    
    mydata_tmp = data.frame(  "tmp" =  stfdf2$tmp[obs_idx], 
                              "uwind" = stfdf2$uwind[obs_idx],
                              "vwind" = stfdf2$vwind[obs_idx],
                              "wind" = stfdf2$wind[obs_idx],
                              "prec" = stfdf2$prec[obs_idx], 
                              "hum" = stfdf2$hum[obs_idx], 
                              "z" = stfdf2$z[obs_idx])
    
    stfdf2 = STFDF(sp_tmp, time_tmp, endTime = delta(time_tmp), mydata_tmp) # => training data 완성!
    
    
    # Validation Data (Location + Time) => [DE_pred1]
    
    # kriging할 위치 + 원본의 PM25 준비해야함.
    spat_pred_grid1 = sp2[ loc_list[[j]], ] ### 건드리는 부분 [뺐던 위치] ☆ 
    #temp_pred_grid1 = as.POSIXct( time_str[i] ) + 3600*c(0,10,15) ### 건드리는 부분 [뺐던 시간] ☆
    temp_pred_grid1 = c( as.POSIXct(  time[ sort(time_list[[i]]) ] ) )
    
    
    
    target_data = data.frame( "tmp"   = stfdf[loc_list[[j]], time_list[[i]] ]$tmp,
                              "uwind"  = stfdf[loc_list[[j]], time_list[[i]] ]$uwind,
                              "vwind"  = stfdf[loc_list[[j]], time_list[[i]] ]$vwind,
                              "wind"  = stfdf[loc_list[[j]], time_list[[i]] ]$wind,
                              "prec"  = stfdf[loc_list[[j]], time_list[[i]] ]$prec,
                              "hum"   = stfdf[loc_list[[j]], time_list[[i]] ]$hum)
    
    
    DE_pred1 <- STFDF(sp = spat_pred_grid1, # spatial part
                      time = temp_pred_grid1,
                      data = target_data) # temporal part
    DE_pred1 #=> kriging할 부분
    
    
    
    
    # kriging 부분1의 원본 데이터
    #stfdf[DE_pred1, ]$z
    
    stfdf[ loc_list[[j]], sort(time_list[[i]]) ]$z ### ☆ ☜
    
    ########################## 적합 ##########################
    
    vv = variogramST(formula = z ~ 1 + x + y + tmp + uwind + vwind  + prec + hum 
                     , data = stfdf2 )
    
    plot(vv, map = F)
    
    sepVgm = vgmST(stModel = "separable",
                   space = vgm(10, "Gau", 5, nugget = 0.1),
                   time = vgm(10, "Gau", 500,  nugget = 1),
                   sill = 1)
    
    
    sepVgm = fit.StVariogram(vv, sepVgm)
    
    #plot(vv, sepVgm, map = F)
    
    ########################## krige ##########################
    
    
    
    
    pred_kriged <- krigeST(z ~ 1 + x + y + tmp + uwind + vwind + wind + prec + hum, # latitude trend
                           data = stfdf2, 
                           newdata = DE_pred1, # prediction grid
                           modelList = sepVgm, # semivariogram
                           computeVar = FALSE) # compute variances
    
    stplot(pred_kriged)
    
    # PLOTTING
    
    #stplot(stfdf[loc1, time0]) ### ☆
    #stplot(pred_kriged)
    
    #pred_kriged[2,][,1]
    
    
    #print(pred_kriged$var1.pred)
    
    print(cat(i, j))
    
    print( sum((pred_kriged$var1.pred - stfdf[ loc_list[[j]], sort(time_list[[i]]) ]$z )^2) / length(pred_kriged$var1.pred))
    
    acc = c(acc, sum((pred_kriged$var1.pred - stfdf[ loc_list[[j]], sort(time_list[[i]]) ]$z )^2) / length(pred_kriged$var1.pred) )
    
  }
  
}
#acc=c()

acc
mean(acc)
sd(acc)


mean(sqrt(acc))
sd(sqrt(acc))



################################################################################
################################################################################
################################################################################

################################################################################
##########################        최종3_220629       ###########################
################################################################################

################################################################################
################################################################################
################################################################################


library(geoR)
library(spatstat)
library(caret)

library(splitTools)
library(scoringRules)
library(verification)

library(sp)
library(spacetime)
library(ggplot2)
library(dplyr)
library(gstat)
library(RColorBrewer)
library(STRbook)
library(tidyr)



# 원본 Covariate dataframe
#covariate_df[,'tmp'] = na.approx(covariate_df[,'tmp'])
#covariate_df[,'uwind'] = na.approx(covariate_df[,'uwind'])
#covariate_df[,'vwind'] = na.approx(covariate_df[,'vwind'])
#covariate_df[,'prec'] = na.approx(covariate_df[,'prec'])
#covariate_df[,'hum'] = na.approx(covariate_df[,'hum'])
#covariate_df = covariate_df[ order( covariate_df[,'t'], covariate_df[,'code']), ]
covariate_df = read.csv("AWS_220629_R_ver2.csv")
sum(is.na(covariate_df))

hist(covariate_df[,'wind'])

covariate_df[,'wind'] = log(covariate_df[,'wind'] + 0.2) ##220629
hist(covariate_df[,'wind'])



hist(covariate_df[,'uwind'])
hist(covariate_df[,'vwind'])
hist(covariate_df[,'prec'])



covariate_sp = covariate_df[,2:3] %>% unique() %>% SpatialPoints
time = as.POSIXct("2022-02-01") + 3600*(1:24)
covariate_data = covariate_df[c(  "tmp","uwind","vwind" ,"wind", "prec", "hum")]

covariate_stfdf = STFDF(covariate_sp, time, endTime = delta(time), data = covariate_data) #=> training data
plot(covariate_stfdf[,1])
stplot(covariate_stfdf, cex = 0.5)

st_data = read.csv("PM25_220629_R.csv")
sp = st_data[,2:3] %>% unique() %>% SpatialPoints
time = as.POSIXct("2022-02-01") + 3600*(1:24)

length(sp)


# kriging할 위치
covariate_spat_pred_grid1 = sp 
covariate_temp_pred_grid1 = time

covariate_DE_pred1 <- STF(sp = covariate_spat_pred_grid1, 
                          time = covariate_temp_pred_grid1)

#covariate_DE_pred2 <- STF(sp = sp, 
#                          time = time)



# 1. tmp
covariate_tmp_vv = variogramST(formula = tmp ~ 1 , data = covariate_stfdf )
plot(covariate_tmp_vv, map = F)
covariate_tmp_sepVgm = vgmST(stModel = "separable",
                             space = vgm(2, "Exp", 5, nugget = 0.1),
                             time = vgm(1, "Exp", 10,  nugget = 0.1),
                             sill = 1)
covariate_tmp_sepVgm = fit.StVariogram(covariate_tmp_vv, covariate_tmp_sepVgm)
plot(covariate_tmp_vv, covariate_tmp_sepVgm, map = F)
covariate_tmp_pred_kriged <- krigeST(tmp ~ 1 , 
                                     data = covariate_stfdf, 
                                     newdata = covariate_DE_pred1,
                                     modelList = covariate_tmp_sepVgm,
                                     computeVar = FALSE) 


# 2. uwind
covariate_uwind_vv = variogramST(formula = uwind ~ 1 , data = covariate_stfdf )
plot(covariate_uwind_vv, map = F)
covariate_uwind_sepVgm = vgmST(stModel = "separable",
                               space = vgm(0.5, "Exp", 5, nugget = 0.1),
                               time = vgm(2, "Exp", 10,  nugget = 0.1),
                               sill = 1)
covariate_uwind_sepVgm = fit.StVariogram(covariate_uwind_vv, covariate_uwind_sepVgm)
plot(covariate_uwind_vv, covariate_uwind_sepVgm, map = F)
covariate_uwind_pred_kriged <- krigeST(uwind ~ 1 , 
                                       data = covariate_stfdf, 
                                       newdata = covariate_DE_pred1,
                                       modelList = covariate_uwind_sepVgm,
                                       computeVar = FALSE) 

# 3. vwind
covariate_vwind_vv = variogramST(formula = vwind ~ 1 , data = covariate_stfdf )
plot(covariate_vwind_vv, map = F)
covariate_vwind_sepVgm = vgmST(stModel = "separable",
                               space = vgm(1, "Exp", 5, nugget = 0.1),
                               time = vgm(2, "Exp", 10,  nugget = 0.1),
                               sill = 1)
covariate_vwind_sepVgm = fit.StVariogram(covariate_vwind_vv, covariate_vwind_sepVgm)
plot(covariate_vwind_vv, covariate_vwind_sepVgm, map = F)
covariate_vwind_pred_kriged <- krigeST(vwind ~ 1 , 
                                       data = covariate_stfdf, 
                                       newdata = covariate_DE_pred1,
                                       modelList = covariate_vwind_sepVgm,
                                       computeVar = FALSE) 




# 4. wind
covariate_wind_vv = variogramST(formula = wind ~ 1 , data = covariate_stfdf )
plot(covariate_wind_vv, map = F)
covariate_wind_sepVgm = vgmST(stModel = "separable",
                              space = vgm(0.5, "Exp", 1, nugget = 0.1),
                              time = vgm(2, "Exp", 5,  nugget = 0.1),
                              sill = 1)
covariate_wind_sepVgm = fit.StVariogram(covariate_wind_vv, covariate_wind_sepVgm)
plot(covariate_wind_vv, covariate_wind_sepVgm, map = F)
covariate_wind_pred_kriged <- krigeST(wind ~ 1 , 
                                      data = covariate_stfdf, 
                                      newdata = covariate_DE_pred1,
                                      modelList = covariate_wind_sepVgm,
                                      computeVar = FALSE) 


# 5. hum
covariate_hum_vv = variogramST(formula = hum ~ 1 , data = covariate_stfdf )
plot(covariate_hum_vv, map = F)
covariate_hum_sepVgm = vgmST(stModel = "separable",
                             space = vgm(100, "Exp", 200, nugget = 0.1),
                             time = vgm(1, "Exp", 10,  nugget = 1),
                             sill = 1)
covariate_hum_sepVgm = fit.StVariogram(covariate_hum_vv, covariate_hum_sepVgm)
plot(covariate_hum_vv, covariate_hum_sepVgm, map = F)
covariate_hum_pred_kriged <- krigeST(hum ~ 1 , 
                                     data = covariate_stfdf, 
                                     newdata = covariate_DE_pred1,
                                     modelList = covariate_hum_sepVgm,
                                     computeVar = FALSE) 


# 6. prec
covariate_prec_vv = variogramST(formula = prec ~ 1 , data = covariate_stfdf )
plot(covariate_prec_vv, map = F)
covariate_prec_sepVgm = vgmST(stModel = "separable",
                              space = vgm(1, "Exp", 5, nugget = 0.1),
                              time = vgm(10, "Exp", 30,  nugget = 1),
                              sill = 10)
covariate_prec_sepVgm = fit.StVariogram(covariate_prec_vv, covariate_prec_sepVgm)
plot(covariate_prec_vv, covariate_prec_sepVgm, map = F)
covariate_prec_pred_kriged <- krigeST(prec ~ 1 , 
                                      data = covariate_stfdf, 
                                      newdata = covariate_DE_pred1,
                                      modelList = covariate_prec_sepVgm,
                                      computeVar = FALSE) 



#stplot(covariate_tmp_pred_kriged)
#stplot(covariate_uwind_pred_kriged)
#stplot(covariate_vind_pred_kriged)
#stplot(covariate_wind_pred_kriged)
#stplot(covariate_hum_pred_kriged)
#stplot(covariate_prec_pred_kriged)

kriged_tmp = covariate_tmp_pred_kriged$var1.pred
kriged_uwind = covariate_uwind_pred_kriged$var1.pred
kriged_vwind = covariate_vwind_pred_kriged$var1.pred
kriged_wind = covariate_wind_pred_kriged$var1.pred
kriged_hum = covariate_hum_pred_kriged$var1.pred
kriged_prec = covariate_prec_pred_kriged$var1.pred



dim(covariate_wind_pred_kriged)

dim(st_data)
length(kriged_tmp)

# 이제 kriged Covariate로 STkriging

# 이것은 원본 전체 STFDF 데이터 => [stfdf]
st_data = read.csv("PM25_220629_R.csv")



st_data['tmp'] = kriged_tmp
st_data['uwind'] = kriged_uwind
st_data['vwind'] = kriged_vwind
st_data['wind'] = kriged_wind
st_data['hum'] = kriged_hum
st_data['prec'] = kriged_prec

#write.csv(st_data,"PM25_with_covariate_220629_R_ver2.csv") ################save
st_data = read.csv("PM25_with_covariate_220629_R_ver2.csv")

sp = st_data[,2:3] %>% unique() %>% SpatialPoints
time = as.POSIXct("2022-02-01") + 3600*(1:24)
mydata = st_data[c("z", "tmp", "uwind", "vwind", "wind", "hum", "prec")]


stfdf = STFDF(sp, time, endTime = delta(time), mydata)
str(stfdf)
plot(stfdf[,1])

#stplot(stfdf, cex = 0.5)






dim(stfdf)
length(stfdf)
mean(mydata[,'z'])
hist(mydata[,'z'])

hist(log(mydata[,'z'] + 0.5))




date = c("01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00",
         "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00",
         "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00", "00:00")


time1 = c(9, 16)
time2 = c(13, 23)
time3 = c(2, 11)
time4 = c(11, 18)
time5 = c(10, 19)

#time1 = c(time[time0[1]] , time[time0[2]], time[time0[3]])
#time2 = c("22-02-01", "22-02-02", "22-02-03")
#time3 = c("22-03-01", "22-03-02", "22-03-03")
time_list = list(time1, time2, time3, time4, time5)
time_str_list = list(date[time1], date[time2], date[time3], date[time4], date[time5])

# 검정할 위치
loc1 = c(1, 6, 31, 74, 75, 84, 103, 108, 117, 141, 174, 180, 189, 192, 196, 226, 236, 248, 271, 286, 296, 308, 320, 330, 337, 339, 342, 354, 415, 420)
loc2 = c(7, 15, 44, 47, 60, 78, 86, 87, 94, 100, 112, 146, 156, 226, 228, 240, 252, 256, 265, 299, 326, 344, 350, 362, 370, 371, 379, 388, 393, 413)
loc3 = c(1, 11, 40, 41, 86, 91, 125, 127, 138, 146, 177, 187, 200, 201, 203, 209, 222, 251, 258, 278, 287, 295, 322, 335, 374, 388, 405, 411, 413, 424)
loc4 = c(15, 33, 38, 56, 68, 80, 85, 99, 112, 113, 155, 217, 224, 242, 253, 256, 259, 261, 262, 274, 285, 286, 333, 337, 345, 365, 366, 391, 407, 414)
loc5 = c(5, 22, 24, 48, 67, 77, 79, 86, 95, 118, 123, 136, 157, 171, 200, 212, 240, 248, 263, 320, 322, 328, 341, 346, 372, 405, 406, 407, 408, 414)



loc_list = list(loc1, loc2, loc3, loc4, loc5)
















acc = c()

for (i in 1:length(time_list))
{
  print(cat("i (time) = ", i))
  
  for (j in 1:length(loc_list))
  {
    
    
    print(cat("j (location) =", j))
    
    
    # Step: Training and Validation Data #
    
    # Training Data => [stfdf2]
    st_data2 = read.csv("PM25_220629_R.csv")
    sp2 = st_data2[,2:3] %>% unique() %>% SpatialPoints
    sp_tmp = sp2[ - loc_list[[j]], ] ### 건드리는 부분 [뺄 위치] ☆
    stfdf2 = stfdf[ - loc_list[[j]], ] ### 건드리는 부분 [뺄 위치] ☆
    
    
    stfdf2$timeHM = format(time(stfdf2), "%H:%M")
    valblock_idx = which( stfdf2$timeHM %in% time_str_list[[i]] ) ### 건드리는 부분[뺼 시간] ☆
    time2 = as.POSIXct("2022-02-01") + 3600*(1:24)
    time_tmp = time2[ -valblock_idx[1: length(time_list[[i]]) ] ] ### 건드리는 부분 [뺄 시간 개수] ☆☜
    
    
    mtot = length(stfdf2)
    obs_idx = setdiff(1:mtot, valblock_idx)
    
    mydata_tmp = data.frame(  "tmp" =  stfdf2$tmp[obs_idx], 
                              "uwind" = stfdf2$uwind[obs_idx],
                              "vwind" = stfdf2$vwind[obs_idx],
                              "wind" = stfdf2$wind[obs_idx],
                              "prec" = stfdf2$prec[obs_idx], 
                              "hum" = stfdf2$hum[obs_idx], 
                              "z" = stfdf2$z[obs_idx])
    
    stfdf2 = STFDF(sp_tmp, time_tmp, endTime = delta(time_tmp), mydata_tmp) # => training data 완성!
    
    
    # Validation Data (Location + Time) => [DE_pred1]
    
    # kriging할 위치 + 원본의 PM25 준비해야함.
    spat_pred_grid1 = sp2[ loc_list[[j]], ] ### 건드리는 부분 [뺐던 위치] ☆ 
    #temp_pred_grid1 = as.POSIXct( time_str[i] ) + 3600*c(0,10,15) ### 건드리는 부분 [뺐던 시간] ☆
    temp_pred_grid1 = c( as.POSIXct(  time[ sort(time_list[[i]]) ] ) )
    
    
    
    target_data = data.frame( "tmp"   = stfdf[loc_list[[j]], time_list[[i]] ]$tmp,
                              "uwind"  = stfdf[loc_list[[j]], time_list[[i]] ]$uwind,
                              "vwind"  = stfdf[loc_list[[j]], time_list[[i]] ]$vwind,
                              "wind"  = stfdf[loc_list[[j]], time_list[[i]] ]$wind,
                              "prec"  = stfdf[loc_list[[j]], time_list[[i]] ]$prec,
                              "hum"   = stfdf[loc_list[[j]], time_list[[i]] ]$hum)
    
    
    DE_pred1 <- STFDF(sp = spat_pred_grid1, # spatial part
                      time = temp_pred_grid1,
                      data = target_data) # temporal part
    DE_pred1 #=> kriging할 부분
    
    
    
    
    # kriging 부분1의 원본 데이터
    #stfdf[DE_pred1, ]$z
    
    stfdf[ loc_list[[j]], sort(time_list[[i]]) ]$z ### ☆ ☜
    
    ########################## 적합 ##########################
    
    vv = variogramST(formula = z ~ 1 + x + y #+ tmp + uwind + vwind  + prec + hum 
                     , data = stfdf2 )
    
    plot(vv, map = F)
    
    sepVgm = vgmST(stModel = "separable",
                   space = vgm(1, "Gau", 5, nugget = 1),
                   time = vgm(10, "Gau", 50,  nugget = 1),
                   sill = 1)
    
    
    sepVgm = fit.StVariogram(vv, sepVgm)
    
    #plot(vv, sepVgm, map = F)
    
    ########################## krige ##########################
    
    
    
    
    pred_kriged <- krigeST(z ~ 1 + x + y + tmp + uwind + vwind + wind + prec + hum, # latitude trend
                           data = stfdf2, 
                           newdata = DE_pred1, # prediction grid
                           modelList = sepVgm, # semivariogram
                           computeVar = FALSE) # compute variances
    
    stplot(pred_kriged)
    
    # PLOTTING
    
    #stplot(stfdf[loc1, time0]) ### ☆
    #stplot(pred_kriged)
    
    #pred_kriged[2,][,1]
    
    
    #print(pred_kriged$var1.pred)
    
    print(cat(i, j))
    
    print( sum((pred_kriged$var1.pred - stfdf[ loc_list[[j]], sort(time_list[[i]]) ]$z )^2) / length(pred_kriged$var1.pred))
    
    acc = c(acc, sum((pred_kriged$var1.pred - stfdf[ loc_list[[j]], sort(time_list[[i]]) ]$z )^2) / length(pred_kriged$var1.pred) )
    
  }
  
}
#acc=c()

acc
mean(acc)
sd(acc)


mean(sqrt(acc))
sd(sqrt(acc))

