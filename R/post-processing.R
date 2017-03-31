
if(settings$force_sector_match_gdp){
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
  mutate(va_agr_pc_gr = (lag(va_agr_pc, n = 0, order_by = temporal) /
           lag(va_agr_pc, n = 1, order_by = temporal))^(1/(lag(temporal, n = 0, order_by = temporal) -
           lag(temporal, n = 1, order_by = temporal))) - 1,
         va_ind_pc_gr = (lag(va_ind_pc, n = 0, order_by = temporal) /
           lag(va_ind_pc, n = 1, order_by = temporal))^(1/(lag(temporal, n = 0, order_by = temporal) -
           lag(temporal, n = 1, order_by = temporal))) - 1,
         va_ser_pc_gr = (lag(va_ser_pc, n = 0, order_by = temporal) /
           lag(va_ser_pc, n = 1, order_by = temporal))^(1/(lag(temporal, n = 0, order_by = temporal) -
           lag(temporal, n = 1, order_by = temporal))) - 1) %>%
  ungroup()



