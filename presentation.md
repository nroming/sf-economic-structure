SSPs and economic structure
========================================================
author: Niklas Roming, Marian Leimbach
date: 28 November 2016
width: 1920
height: 1080



Status quo
========================================================
<img src="presentation-figure/unnamed-chunk-1-1.png" title="plot of chunk unnamed-chunk-1" alt="plot of chunk unnamed-chunk-1" width="1920px" />

An observation I
========================================================

<img src="presentation-figure/unnamed-chunk-2-1.png" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" width="1920px" />

An observation II
========================================================

<img src="presentation-figure/unnamed-chunk-3-1.png" title="plot of chunk unnamed-chunk-3" alt="plot of chunk unnamed-chunk-3" width="1920px" />

Econometric estimation and projection
========================================================
incremental: true

-  $\text{Agriculture:} \quad VApC_{it}^{agr} = \beta_0 + \beta_1 GDPpC_{it} + \beta_2 popdens_{it} + \beta_3 recession_{it} + \beta_4 spatial_i + u_{it}$
- $\text{Industry:} \quad VApC_{it}^{ind} = \beta_0 + \beta_1 GDPpC_{it} + \beta_2 GDPpC_{it}^2 + \beta_3 GDPpC_{it}^3 + \beta_4 popdens_{it} + \beta_5 recession_{it} + \beta_6 spatial_i + u_{it}$
- Prediction using the SSP scenarios
- Service sector is a residual: $VApC_{it}^{ser} = GDPpC_{it} - VApC_{it}^{agr} - VApC_{it}^{ind}$

Results - G20
========================================================
<img src="presentation-figure/unnamed-chunk-4-1.png" title="plot of chunk unnamed-chunk-4" alt="plot of chunk unnamed-chunk-4" width="1920px" />

Results - USA
========================================================
<img src="presentation-figure/unnamed-chunk-5-1.png" title="plot of chunk unnamed-chunk-5" alt="plot of chunk unnamed-chunk-5" width="1920px" />

Results - China
========================================================
<img src="presentation-figure/unnamed-chunk-6-1.png" title="plot of chunk unnamed-chunk-6" alt="plot of chunk unnamed-chunk-6" width="1920px" />

Results - India
========================================================
<img src="presentation-figure/unnamed-chunk-7-1.png" title="plot of chunk unnamed-chunk-7" alt="plot of chunk unnamed-chunk-7" width="1920px" />

Next steps
========================================================
incremental: true

- Close the gaps in the data: combine GGDC with WDI data
- Proper model selection
- Integrate further explanatory variables: Current account, resource exporting dummy
- Different model structures: lagged variables (dependent and independent)
