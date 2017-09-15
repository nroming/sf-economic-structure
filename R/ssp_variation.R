
if(force_sector_match_gdp){
  # compute sum of sectoral value added ----
  result <- mutate(result, va_sum_pc = va_agr_pc + va_ind_pc + va_ser_pc,
                   # compute sectoral shares of sum of sectoral value added
                   va_agr_pc_share = va_agr_pc/va_sum_pc,
                   va_ind_pc_share = va_ind_pc/va_sum_pc,
                   va_ser_pc_share = va_ser_pc/va_sum_pc,

                   # apply these shares to gdp to get adjusted sectoral value added
                   va_agr_pc = va_agr_pc_share * gdp_pc,
                   va_ind_pc = va_ind_pc_share * gdp_pc,
                   va_ind_pc = va_ind_pc_share * gdp_pc,

                   # compute total sector levels
                   va_sum = va_agr + va_ind + va_ser,

                   va_agr_share = va_agr/va_sum,
                   va_ind_share = va_ind/va_sum,
                   va_ser_share = va_ser/va_sum,

                   # apply these shares to gdp to get adjusted sectoral value added
                   va_agr = va_agr_share * gdp,
                   va_ind = va_ind_share * gdp,
                   va_ind = va_ind_share * gdp
)
}


# calculate growth rates
result <- group_by(result, scenario, spatial) %>%
  mutate(va_agr_pc_gr = lag(va_agr_pc, n = 0, order_by = temporal) /
           lag(va_agr_pc, n = 1, order_by = temporal) - 1,
         va_ind_pc_gr = lag(va_ind_pc, n = 0, order_by = temporal) /
           lag(va_ind_pc, n = 1, order_by = temporal) - 1,
         va_ser_pc_gr = lag(va_ser_pc, n = 0, order_by = temporal) /
           lag(va_ser_pc, n = 1, order_by = temporal) - 1) %>%
  ungroup()

# SSP variation 
ssp_var = read_excel("data/SSP_variation.xlsx")
ssp_var$variation = ssp_var$level + ssp_var$spread/2
ssp_var = select(ssp_var,scenario,sector,variation)
# ssp_var is translated into agr,ind,ser
ssp_var = spread(ssp_var,sector,variation)

res = filter(result, temporal >"2014")
#res = filter(res, temporal <"2051")
res = select(res, scenario,spatial,temporal,pop,va_agr_pc,va_ind_pc,va_ser_pc,va_agr_pc_gr,va_ind_pc_gr,va_ser_pc_gr)
res = inner_join(res,ssp_var, by = "scenario")

res[res$temporal == "2015", "new_agr"] = res[res$temporal == "2015", "va_agr_pc"]
res[res$temporal == "2015", "new_ind"] = res[res$temporal == "2015", "va_ind_pc"]
res[res$temporal == "2015", "new_ser"] = res[res$temporal == "2015", "va_ser_pc"]

#shift of growth rate reference year
for (t in unique(res$temporal)){  
  res[res$temporal == t-5, "va_agr_pc_gr"] =  res[res$temporal == t, "va_agr_pc_gr"] 
  res[res$temporal == t-5, "va_ind_pc_gr"] =  res[res$temporal == t, "va_ind_pc_gr"] 
  res[res$temporal == t-5, "va_ser_pc_gr"] =  res[res$temporal == t, "va_ser_pc_gr"] 
} 
#projection beyond 2050 with vanishing long-term change rate
for (t in (2051:2100)){ 
  res[res$temporal == t, "va_agr_pc_gr"] =  (2100-t) * (res[res$temporal == "2050", "va_agr_pc_gr"]/50)
  res[res$temporal == t, "va_ind_pc_gr"] =  (2100-t) * (res[res$temporal == "2050", "va_ind_pc_gr"]/50)
  res[res$temporal == t, "va_ser_pc_gr"] =  (2100-t) * (res[res$temporal == "2050", "va_ser_pc_gr"]/50)
}
#computation of va_pc with growth rates adjusted
for (t in unique(res$temporal)){  
  res[res$temporal == t+5, "new_agr"] = res[res$temporal == t, "new_agr"] * (1 + (res[res$temporal == t, "va_agr_pc_gr"] *
                        (1 + res[res$temporal == t, "agr"]/2)))
  res[res$temporal == t+5, "new_ind"] = res[res$temporal == t, "new_ind"] * (1 + (res[res$temporal == t, "va_ind_pc_gr"] *
                        (1 + res[res$temporal == t, "ind"]/2)))
  res[res$temporal == t+5, "new_ser"] = res[res$temporal == t, "new_ser"] * (1 + (res[res$temporal == t, "va_ser_pc_gr"] *
                        (1 + res[res$temporal == t, "ser"]/2)))
}  


#diagnostics
resu=filter(res,spatial=="USA")
ggplot(resu, aes(x=temporal, y=va_ind_pc_gr)) + geom_line(aes(color=scenario)) + facet_wrap(~scenario)
ggplot(resu, aes(x=temporal, y=new_ind)) +geom_line(aes(color=scenario))+facet_wrap(~scenario) + xlim(2015,2100) 
ggplot(resu, aes(x=temporal, y=va_ind_pc)) +geom_line(aes(color=scenario))+facet_wrap(~scenario) + xlim(2015,2100) 


#send back updated growth rates
# result$va_agr_pc_gr <- resu$va_agr_pc_gr
# result$va_agr_pc_gr <- resu$va_agr_pc_gr
# result$va_agr_pc_gr <- resu$va_agr_pc_gr
