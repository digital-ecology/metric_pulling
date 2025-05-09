
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Overview

<!-- badges: start -->
<!-- badges: end -->

metricpulling reads data from a DEFRA Biodiversity Net Gain metric xlsx
file and does the following actions:

- runs a series of checks for errors or missing data
- creates summaries of baseline and proposed data for all habitat
  modules (habitat, hedge, watercourse)
- creates sumamries of net units for each habitat module
- creates summaries of the trading rules

These summaries can be used in creating reports on BNG projects.

## Installation

The package can be installed from Digital Ecology’s Codeberg repository:

``` r
remotes::install_git("https://codeberg.org/Digital-Ecology/metric_pulling.git")
```

## Examples

To run the metric checks:

``` r
library(metricpulling)

metric <- system.file("extdata", "OnSiteBoth.xlsx", package = "metricpulling")

metric_check(metric)
#> $checkresult
#> [1] "\n\n - All sheets filled out correctly. No issues detected."
#> 
#> $metricdata
#> $metricdata$`Headline Results`
#>                                                                                                           X1
#> 1                                                                                  Snarton Farm (Land south)
#> 2                                                                            Scroll down for final results ⚠
#> 3                                                                                           On-site baseline
#> 4                                                                                                       <NA>
#> 5                                                                                                       <NA>
#> 6                        On-site post-intervention&#10;(Including habitat retention, creation & enhancement)
#> 7                                                                                                       <NA>
#> 8                                                                                                       <NA>
#> 9                                                               On-site net change &#10;(units & percentage)
#> 10                                                                                                      <NA>
#> 11                                                                                                      <NA>
#> 12                                                                                                          
#> 13                                                                                         Off-site baseline
#> 14                                                                                                      <NA>
#> 15                                                                                                      <NA>
#> 16                      Off-site post-intervention&#10;(Including habitat retention, creation & enhancement)
#> 17                                                                                                      <NA>
#> 18                                                                                                      <NA>
#> 19                                                              Off-site net change&#10;(units & percentage)
#> 20                                                                                                      <NA>
#> 21                                                                                                      <NA>
#> 22                                           Off-site unit change&#10;(with Spatial Risk Multiplier applied)
#> 23                                                                                                      <NA>
#> 24                                                                                                      <NA>
#> 25 Combined net unit change&#10;(Including all on-site & off-site habitat retention, creation & enhancement)
#> 26                                                                                                      <NA>
#> 27                                                                                                      <NA>
#> 28                                                                  Spatial risk multiplier (SRM) deductions
#> 29                                                                                                      <NA>
#> 30                                                                                                      <NA>
#> 31                                                                                                          
#> 32                                                                                             FINAL RESULTS
#> 33    Total net unit change&#10;(Including all on-site & off-site habitat retention, creation & enhancement)
#> 34                                                                                                      <NA>
#> 35                                                                                                      <NA>
#> 36       Total net % change&#10;(Including all on-site & off-site habitat retention, creation & enhancement)
#> 37                                                                                                      <NA>
#> 38                                                                                                      <NA>
#> 39                                                                                  Trading rules satisfied?
#> 40                                                                                                          
#> 41                                                                                                          
#> 42                                                                                                          
#> 43                                                                                                          
#> 44                                                                                                 Unit Type
#> 45                                                                                             Habitat units
#> 46                                                                                            Hedgerow units
#> 47                                                                                         Watercourse units
#> 48                                                                                                          
#>        X2             X3                X4                X5
#> 1    <NA>           <NA>                                <NA>
#> 2    <NA>           <NA>              <NA>              <NA>
#> 3    <NA>           <NA>     Habitat units              40.5
#> 4    <NA>           <NA>    Hedgerow units            23.442
#> 5    <NA>           <NA> Watercourse units           1.46421
#> 6    <NA>           <NA>     Habitat units  52.3313796232431
#> 7    <NA>           <NA>    Hedgerow units  25.8416927979978
#> 8    <NA>           <NA> Watercourse units  2.31493153121461
#> 9    <NA>           <NA>     Habitat units  11.8313796232431
#> 10   <NA>           <NA>    Hedgerow units  2.39969279799779
#> 11   <NA>           <NA> Watercourse units 0.850721531214613
#> 12   <NA>           <NA>              <NA>              <NA>
#> 13   <NA>           <NA>     Habitat units                 0
#> 14   <NA>           <NA>    Hedgerow units                 0
#> 15   <NA>           <NA> Watercourse units                 0
#> 16   <NA>           <NA>     Habitat units                 0
#> 17   <NA>           <NA>    Hedgerow units                 0
#> 18   <NA>           <NA> Watercourse units                 0
#> 19   <NA>           <NA>     Habitat units                 0
#> 20   <NA>           <NA>    Hedgerow units                 0
#> 21   <NA>           <NA> Watercourse units                 0
#> 22   <NA>           <NA>     Habitat units               N/A
#> 23   <NA>           <NA>    Hedgerow units               N/A
#> 24   <NA>           <NA> Watercourse units               N/A
#> 25   <NA>           <NA>     Habitat units  11.8313796232431
#> 26   <NA>           <NA>    Hedgerow units  2.39969279799779
#> 27   <NA>           <NA> Watercourse units 0.850721531214613
#> 28   <NA>           <NA>     Habitat units              0.00
#> 29   <NA>           <NA>    Hedgerow units              0.00
#> 30   <NA>           <NA> Watercourse units              0.00
#> 31   <NA>           <NA>              <NA>              <NA>
#> 32   <NA>           <NA>              <NA>              <NA>
#> 33   <NA>           <NA>     Habitat units  11.8313796232431
#> 34   <NA>           <NA>    Hedgerow units  2.39969279799779
#> 35   <NA>           <NA> Watercourse units 0.850721531214613
#> 36   <NA>           <NA>     Habitat units 0.292132830203534
#> 37   <NA>           <NA>    Hedgerow units 0.102367238204837
#> 38   <NA>           <NA> Watercourse units    0.581010600402
#> 39   <NA>           <NA>             Yes ✓              <NA>
#> 40   <NA>           <NA>              <NA>              <NA>
#> 41   <NA>           <NA>              <NA>              <NA>
#> 42   <NA>           <NA>              <NA>              <NA>
#> 43   <NA>           <NA>              <NA>              <NA>
#> 44 Target Baseline Units    Units Required      Unit Deficit
#> 45    0.1           40.5             44.55                 0
#> 46    0.1         23.442           25.7862                 0
#> 47    0.1        1.46421          1.610631                 0
#> 48   <NA>           <NA>              <NA>              <NA>
#>                                                             X6   X7 X8
#> 1                                                         <NA> <NA> NA
#> 2                                                         <NA> <NA> NA
#> 3                                                         <NA> <NA> NA
#> 4                                                         <NA> <NA> NA
#> 5                                                         <NA> <NA> NA
#> 6                                                         <NA> <NA> NA
#> 7                                                         <NA> <NA> NA
#> 8                                                         <NA> <NA> NA
#> 9                                            0.292132830203534      NA
#> 10                                           0.102367238204837      NA
#> 11                                              0.581010600402      NA
#> 12                                                        <NA> <NA> NA
#> 13                                                        <NA> <NA>  0
#> 14                                                        <NA> <NA>  0
#> 15                                                        <NA> <NA>  0
#> 16                                                        <NA> <NA>  0
#> 17                                                        <NA> <NA>  0
#> 18                                                        <NA> <NA>  0
#> 19                                                           0      NA
#> 20                                                           0      NA
#> 21                                                           0      NA
#> 22     SRM only applies where unit change is positive (uplift) <NA> NA
#> 23              SRM only applies where unit change is positive <NA> NA
#> 24              SRM only applies where unit change is positive <NA> NA
#> 25                                                        <NA> <NA> NA
#> 26                                                        <NA> <NA> NA
#> 27                                                        <NA> <NA> NA
#> 28                                                        <NA> <NA> NA
#> 29                                                        <NA> <NA> NA
#> 30                                                        <NA> <NA> NA
#> 31                                                        <NA> <NA> NA
#> 32                                                        <NA> <NA> NA
#> 33                                                        <NA> <NA> NA
#> 34                                                        <NA> <NA> NA
#> 35                                                        <NA> <NA> NA
#> 36                                                             <NA> NA
#> 37                                                             <NA> NA
#> 38                                                             <NA> NA
#> 39                                                        <NA> <NA> NA
#> 40                                                        <NA> <NA> NA
#> 41                                                        <NA> <NA> NA
#> 42                                                        <NA> <NA> NA
#> 43                                                        <NA> <NA> NA
#> 44                                                        <NA> <NA> NA
#> 45 No additional area habitat units required to meet target  ✓ <NA> NA
#> 46     No additional hedgerow units required to meet target  ✓ <NA> NA
#> 47  No additional watercourse units required to meet target  ✓ <NA> NA
#> 48                                                        <NA> <NA> NA
#> 
#> $metricdata$`A-1 On-Site Habitat Baseline`
#>           Broad Habitat                             Habitat Type Area (Ha)
#> 1              Cropland                             Cereal crops    17.030
#> 2             Grassland                       Modified grassland     1.610
#> 3                 Urban Artificial unvegetated, unsealed surface     0.110
#> 4                 Urban           Developed land; sealed surface     0.003
#> 5 Watercourse footprint                    Watercourse footprint     0.080
#>   Distinctiveness                Condition Total Habitat Units
#> 1             Low Condition Assessment N/A               34.06
#> 2             Low                 Moderate                6.44
#> 3           V.Low              N/A - Other                0.00
#> 4           V.Low              N/A - Other                0.00
#> 5           V.low              N/A - Other                0.00
#>   Area Retained (Ha) Area Enhanced  (Ha) Baseline Units Retained
#> 1               0.00                0.00                       0
#> 2               0.00                1.51                       0
#> 3               0.00                0.00                       0
#> 4               0.00                0.00                       0
#> 5               0.08                0.00                       0
#>   Baseline Units Enhanced Area Lost Baseline Units Lost
#> 1                    0.00    17.030               34.06
#> 2                    6.04     0.100                0.40
#> 3                    0.00     0.110                0.00
#> 4                    0.00     0.003                0.00
#> 5                    0.00     0.000                0.00
#> 
#> $metricdata$`A-2 On-Site Habitat Creation`
#>   Proposed Broad Habitat          Proposed Habitat Type Area (Ha)
#> 1                  Urban                     Allotments     0.140
#> 2                  Urban Developed land; sealed surface     6.490
#> 3                  Urban               Vegetated garden     2.300
#> 4    Heathland and shrub                    Mixed scrub     1.680
#> 5              Grassland             Modified grassland     1.060
#> 6                  Urban    Sustainable drainage system     0.400
#> 8              Grassland        Other neutral grassland     5.170
#> 9       Individual trees                     Urban tree     0.851
#>   Distinctiveness                Condition Habitat Units Delivered
#> 1             Low                 Moderate                   0.540
#> 2           V.Low              N/A - Other                   0.000
#> 3             Low Condition Assessment N/A                   4.439
#> 4          Medium                 Moderate                  11.247
#> 5             Low                 Moderate                   3.677
#> 6             Low                     Poor                   0.517
#> 8          Medium                     Poor                  19.258
#> 9          Medium                     Poor                   2.383
#> 
#> $metricdata$`A-3 On-Site Habitat Enhancement`
#>                 Existing Habitat Proposed Broad Habitat   Proposed Habitat Type
#> 2 Grassland - Modified grassland              Grassland Other neutral grassland
#>   Area (Ha) Distinctiveness Condition Habitat Units Delivered
#> 2      1.51          Medium  Moderate                   10.27
#> 
#> $metricdata$`B-1 On-Site Hedge Baseline`
#>    Hedge Number                                                 Habitat Type
#> 1            H1                                              Native hedgerow
#> 2            H2                                              Native hedgerow
#> 3            H3                                              Native hedgerow
#> 4            H4                      Species-rich native hedgerow with trees
#> 5            H5              Native hedgerow - associated with bank or ditch
#> 6            H6                                   Native hedgerow with trees
#> 7            H7 Species-rich native hedgerow - associated with bank or ditch
#> 8            H8                      Species-rich native hedgerow with trees
#> 9            H9              Native hedgerow - associated with bank or ditch
#> 10          H10                                   Native hedgerow with trees
#>    Length (Km) Condition Baseline Units Length Retained Length Enhanced
#> 1        0.343      Good          2.058           0.268               0
#> 2        0.273      Good          1.638           0.273               0
#> 3        0.207  Moderate          0.828           0.195               0
#> 4        0.331      Good          5.958           0.331               0
#> 5        0.238  Moderate          1.904           0.238               0
#> 6        0.194  Moderate          1.552           0.194               0
#> 7        0.198      Good          3.564           0.198               0
#> 8        0.181  Moderate          2.172           0.181               0
#> 9        0.186  Moderate          1.488           0.174               0
#> 10       0.190      Good          2.280           0.190               0
#>    Units Retained Units Enhanced Length Lost Units Lost
#> 1           1.608              0       0.075      0.450
#> 2           1.638              0       0.000      0.000
#> 3           0.780              0       0.012      0.048
#> 4           5.958              0       0.000      0.000
#> 5           1.904              0       0.000      0.000
#> 6           1.552              0       0.000      0.000
#> 7           3.564              0       0.000      0.000
#> 8           2.172              0       0.000      0.000
#> 9           1.392              0       0.012      0.096
#> 10          2.280              0       0.000      0.000
#> 
#> $metricdata$`B-2 On-Site Hedge Creation`
#>   New Hedge Number               Created Habitat Type Length (Km)
#> 1              H11         Native hedgerow with trees       0.118
#> 2              H12                    Native hedgerow       0.609
#> 3              H13 Non-native and ornamental hedgerow       0.161
#>   Distinctiveness Condition
#> 1          Medium      Poor
#> 2             Low      Good
#> 3           V.Low      Poor
#>                                       Strategic Significance Units Created
#> 1 Area/compensation not in local strategy/ no local strategy         0.455
#> 2 Area/compensation not in local strategy/ no local strategy         2.383
#> 3 Area/compensation not in local strategy/ no local strategy         0.155
#> 
#> $metricdata$`B-3 On-Site Hedge Enhancement`
#>     Baseline.habitat Length.(km) Condition Strategic.significance X5
#> 1                                       NA                     NA   
#> 2                                       NA                     NA   
#> 3                                       NA                     NA   
#> 4                                       NA                     NA   
#> 5                                       NA                     NA   
#> 6                                       NA                     NA   
#> 7                                       NA                     NA   
#> 8                                       NA                     NA   
#> 9                                       NA                     NA   
#> 10                                      NA                     NA   
#> 11                                      NA                     NA   
#> 12                                      NA                     NA   
#> 13                                      NA                     NA   
#> 14                                      NA                     NA   
#> 15                                      NA                     NA   
#> 16                                      NA                     NA   
#> 17                                      NA                     NA   
#> 18                                      NA                     NA   
#> 19                                      NA                     NA   
#> 20                                      NA                     NA   
#> 21                                      NA                     NA   
#> 22                                      NA                     NA   
#> 23                                      NA                     NA   
#> 24                                      NA                     NA   
#> 25                                      NA                     NA   
#> 26                                      NA                     NA   
#> 27                                      NA                     NA   
#> 28                                      NA                     NA   
#> 29                                      NA                     NA   
#> 30                                      NA                     NA   
#> 31                                      NA                     NA   
#> 32                                      NA                     NA   
#> 33                                      NA                     NA   
#> 34                                      NA                     NA   
#> 35                                      NA                     NA   
#> 36                                      NA                     NA   
#> 37                                      NA                     NA   
#> 38                                      NA                     NA   
#> 39                                      NA                     NA   
#> 40                                      NA                     NA   
#> 41                                      NA                     NA   
#> 42                                      NA                     NA   
#> 43                                      NA                     NA   
#> 44                                      NA                     NA   
#> 45                                      NA                     NA   
#> 46                                      NA                     NA   
#> 47                                      NA                     NA   
#> 48                                      NA                     NA   
#> 49                                      NA                     NA   
#> 50                                      NA                     NA   
#> 51                                      NA                     NA   
#> 52                                      NA                     NA   
#> 53                                      NA                     NA   
#> 54                                      NA                     NA   
#> 55                                      NA                     NA   
#> 56                                      NA                     NA   
#> 57                                      NA                     NA   
#> 58                                      NA                     NA   
#> 59                                      NA                     NA   
#> 60                                      NA                     NA   
#> 61                                      NA                     NA   
#> 62                                      NA                     NA   
#> 63                                      NA                     NA   
#> 64                                      NA                     NA   
#> 65                                      NA                     NA   
#> 66                                      NA                     NA   
#> 67                                      NA                     NA   
#> 68                                      NA                     NA   
#> 69                                      NA                     NA   
#> 70                                      NA                     NA   
#> 71                                      NA                     NA   
#> 72                                      NA                     NA   
#> 73                                      NA                     NA   
#> 74                                      NA                     NA   
#> 75                                      NA                     NA   
#> 76                                      NA                     NA   
#> 77                                      NA                     NA   
#> 78                                      NA                     NA   
#> 79                                      NA                     NA   
#> 80                                      NA                     NA   
#> 81                                      NA                     NA   
#> 82                                      NA                     NA   
#> 83                                      NA                     NA   
#> 84                                      NA                     NA   
#> 85                                      NA                     NA   
#> 86                                      NA                     NA   
#> 87                                      NA                     NA   
#> 88                                      NA                     NA   
#> 89                                      NA                     NA   
#> 90                                      NA                     NA   
#> 91                                      NA                     NA   
#> 92                                      NA                     NA   
#> 93                                      NA                     NA   
#> 94                                      NA                     NA   
#> 95                                      NA                     NA   
#> 96                                      NA                     NA   
#> 97                                      NA                     NA   
#> 98                                      NA                     NA   
#> 99                                      NA                     NA   
#> 100                                     NA                     NA   
#> 101                                     NA                     NA   
#> 102                                     NA                     NA   
#> 103                                     NA                     NA   
#> 104                                     NA                     NA   
#> 105                                     NA                     NA   
#> 106                                     NA                     NA   
#> 107                                     NA                     NA   
#> 108                                     NA                     NA   
#> 109                                     NA                     NA   
#> 110                                     NA                     NA   
#> 111                                     NA                     NA   
#> 112                                     NA                     NA   
#> 113                                     NA                     NA   
#> 114                                     NA                     NA   
#> 115                                     NA                     NA   
#> 116                                     NA                     NA   
#> 117                                     NA                     NA   
#> 118                                     NA                     NA   
#> 119                                     NA                     NA   
#> 120                                     NA                     NA   
#> 121                                     NA                     NA   
#> 122                                     NA                     NA   
#> 123                                     NA                     NA   
#> 124                                     NA                     NA   
#> 125                                     NA                     NA   
#> 126                                     NA                     NA   
#> 127                                     NA                     NA   
#> 128                                     NA                     NA   
#> 129                                     NA                     NA   
#> 130                                     NA                     NA   
#> 131                                     NA                     NA   
#> 132                                     NA                     NA   
#> 133                                     NA                     NA   
#> 134                                     NA                     NA   
#> 135                                     NA                     NA   
#> 136                                     NA                     NA   
#> 137                                     NA                     NA   
#> 138                                     NA                     NA   
#> 139                                     NA                     NA   
#> 140                                     NA                     NA   
#> 141                                     NA                     NA   
#> 142                                     NA                     NA   
#> 143                                     NA                     NA   
#> 144                                     NA                     NA   
#> 145                                     NA                     NA   
#> 146                                     NA                     NA   
#> 147                                     NA                     NA   
#> 148                                     NA                     NA   
#> 149                                     NA                     NA   
#> 150                                     NA                     NA   
#> 151                                     NA                     NA   
#> 152                                     NA                     NA   
#> 153                                     NA                     NA   
#> 154                                     NA                     NA   
#> 155                                     NA                     NA   
#> 156                                     NA                     NA   
#> 157                                     NA                     NA   
#> 158                                     NA                     NA   
#> 159                                     NA                     NA   
#> 160                                     NA                     NA   
#> 161                                     NA                     NA   
#> 162                                     NA                     NA   
#> 163                                     NA                     NA   
#> 164                                     NA                     NA   
#> 165                                     NA                     NA   
#> 166                                     NA                     NA   
#> 167                                     NA                     NA   
#> 168                                     NA                     NA   
#> 169                                     NA                     NA   
#> 170                                     NA                     NA   
#> 171                                     NA                     NA   
#> 172                                     NA                     NA   
#> 173                                     NA                     NA   
#> 174                                     NA                     NA   
#> 175                                     NA                     NA   
#> 176                                     NA                     NA   
#> 177                                     NA                     NA   
#> 178                                     NA                     NA   
#> 179                                     NA                     NA   
#> 180                                     NA                     NA   
#> 181                                     NA                     NA   
#> 182                                     NA                     NA   
#> 183                                     NA                     NA   
#> 184                                     NA                     NA   
#> 185                                     NA                     NA   
#> 186                                     NA                     NA   
#> 187                                     NA                     NA   
#> 188                                     NA                     NA   
#> 189                                     NA                     NA   
#> 190                                     NA                     NA   
#> 191                                     NA                     NA   
#> 192                                     NA                     NA   
#> 193                                     NA                     NA   
#> 194                                     NA                     NA   
#> 195                                     NA                     NA   
#> 196                                     NA                     NA   
#> 197                                     NA                     NA   
#> 198                                     NA                     NA   
#> 199                                     NA                     NA   
#> 200                                     NA                     NA   
#> 201                                     NA                     NA   
#> 202                                     NA                     NA   
#> 203                                     NA                     NA   
#> 204                                     NA                     NA   
#> 205                                     NA                     NA   
#> 206                                     NA                     NA   
#> 207                                     NA                     NA   
#> 208                                     NA                     NA   
#> 209                                     NA                     NA   
#> 210                                     NA                     NA   
#> 211                                     NA                     NA   
#> 212                                     NA                     NA   
#> 213                                     NA                     NA   
#> 214                                     NA                     NA   
#> 215                                     NA                     NA   
#> 216                                     NA                     NA   
#> 217                                     NA                     NA   
#> 218                                     NA                     NA   
#> 219                                     NA                     NA   
#> 220                                     NA                     NA   
#> 221                                     NA                     NA   
#> 222                                     NA                     NA   
#> 223                                     NA                     NA   
#> 224                                     NA                     NA   
#> 225                                     NA                     NA   
#> 226                                     NA                     NA   
#> 227                                     NA                     NA   
#> 228                                     NA                     NA   
#> 229                                     NA                     NA   
#> 230                                     NA                     NA   
#> 231                                     NA                     NA   
#> 232                                     NA                     NA   
#> 233                                     NA                     NA   
#> 234                                     NA                     NA   
#> 235                                     NA                     NA   
#> 236                                     NA                     NA   
#> 237                                     NA                     NA   
#> 238                                     NA                     NA   
#> 239                                     NA                     NA   
#> 240                                     NA                     NA   
#> 241                                     NA                     NA   
#> 242                                     NA                     NA   
#> 243                                     NA                     NA   
#> 244                                     NA                     NA   
#> 245                                     NA                     NA   
#> 246                                     NA                     NA   
#> 247             <NA>        <NA>        NA                     NA  0
#> 
#> $metricdata$`C-1 On-Site WaterC' Baseline`
#>           Watercourse Type Length (Km) Condition
#> 1 Other rivers and streams       0.522      Poor
#>                                      Strategic Significance Existing Units
#> 1 Location ecologically desirable but not in local strategy          1.464
#>   Length Retained (Km) Units Retained Length Lost (Km) Units Lost
#> 1                    0              0                0          0
#> 
#> $metricdata$`C-2 On-Site WaterC' Creation`
#>     Watercourse.type Length.(km) Condition Strategic.significance X5
#> 1                 NA          NA        NA                     NA   
#> 2                 NA          NA        NA                     NA   
#> 3                 NA          NA        NA                     NA   
#> 4                 NA          NA        NA                     NA   
#> 5                 NA          NA        NA                     NA   
#> 6                 NA          NA        NA                     NA   
#> 7                 NA          NA        NA                     NA   
#> 8                 NA          NA        NA                     NA   
#> 9                 NA          NA        NA                     NA   
#> 10                NA          NA        NA                     NA   
#> 11                NA          NA        NA                     NA   
#> 12                NA          NA        NA                     NA   
#> 13                NA          NA        NA                     NA   
#> 14                NA          NA        NA                     NA   
#> 15                NA          NA        NA                     NA   
#> 16                NA          NA        NA                     NA   
#> 17                NA          NA        NA                     NA   
#> 18                NA          NA        NA                     NA   
#> 19                NA          NA        NA                     NA   
#> 20                NA          NA        NA                     NA   
#> 21                NA          NA        NA                     NA   
#> 22                NA          NA        NA                     NA   
#> 23                NA          NA        NA                     NA   
#> 24                NA          NA        NA                     NA   
#> 25                NA          NA        NA                     NA   
#> 26                NA          NA        NA                     NA   
#> 27                NA          NA        NA                     NA   
#> 28                NA          NA        NA                     NA   
#> 29                NA          NA        NA                     NA   
#> 30                NA          NA        NA                     NA   
#> 31                NA          NA        NA                     NA   
#> 32                NA          NA        NA                     NA   
#> 33                NA          NA        NA                     NA   
#> 34                NA          NA        NA                     NA   
#> 35                NA          NA        NA                     NA   
#> 36                NA          NA        NA                     NA   
#> 37                NA          NA        NA                     NA   
#> 38                NA          NA        NA                     NA   
#> 39                NA          NA        NA                     NA   
#> 40                NA          NA        NA                     NA   
#> 41                NA          NA        NA                     NA   
#> 42                NA          NA        NA                     NA   
#> 43                NA          NA        NA                     NA   
#> 44                NA          NA        NA                     NA   
#> 45                NA          NA        NA                     NA   
#> 46                NA          NA        NA                     NA   
#> 47                NA          NA        NA                     NA   
#> 48                NA          NA        NA                     NA   
#> 49                NA          NA        NA                     NA   
#> 50                NA          NA        NA                     NA   
#> 51                NA          NA        NA                     NA   
#> 52                NA          NA        NA                     NA   
#> 53                NA          NA        NA                     NA   
#> 54                NA          NA        NA                     NA   
#> 55                NA          NA        NA                     NA   
#> 56                NA          NA        NA                     NA   
#> 57                NA          NA        NA                     NA   
#> 58                NA          NA        NA                     NA   
#> 59                NA          NA        NA                     NA   
#> 60                NA          NA        NA                     NA   
#> 61                NA          NA        NA                     NA   
#> 62                NA          NA        NA                     NA   
#> 63                NA          NA        NA                     NA   
#> 64                NA          NA        NA                     NA   
#> 65                NA          NA        NA                     NA   
#> 66                NA          NA        NA                     NA   
#> 67                NA          NA        NA                     NA   
#> 68                NA          NA        NA                     NA   
#> 69                NA          NA        NA                     NA   
#> 70                NA          NA        NA                     NA   
#> 71                NA          NA        NA                     NA   
#> 72                NA          NA        NA                     NA   
#> 73                NA          NA        NA                     NA   
#> 74                NA          NA        NA                     NA   
#> 75                NA          NA        NA                     NA   
#> 76                NA          NA        NA                     NA   
#> 77                NA          NA        NA                     NA   
#> 78                NA          NA        NA                     NA   
#> 79                NA          NA        NA                     NA   
#> 80                NA          NA        NA                     NA   
#> 81                NA          NA        NA                     NA   
#> 82                NA          NA        NA                     NA   
#> 83                NA          NA        NA                     NA   
#> 84                NA          NA        NA                     NA   
#> 85                NA          NA        NA                     NA   
#> 86                NA          NA        NA                     NA   
#> 87                NA          NA        NA                     NA   
#> 88                NA          NA        NA                     NA   
#> 89                NA          NA        NA                     NA   
#> 90                NA          NA        NA                     NA   
#> 91                NA          NA        NA                     NA   
#> 92                NA          NA        NA                     NA   
#> 93                NA          NA        NA                     NA   
#> 94                NA          NA        NA                     NA   
#> 95                NA          NA        NA                     NA   
#> 96                NA          NA        NA                     NA   
#> 97                NA          NA        NA                     NA   
#> 98                NA          NA        NA                     NA   
#> 99                NA          NA        NA                     NA   
#> 100               NA          NA        NA                     NA   
#> 101               NA          NA        NA                     NA   
#> 102               NA          NA        NA                     NA   
#> 103               NA          NA        NA                     NA   
#> 104               NA          NA        NA                     NA   
#> 105               NA          NA        NA                     NA   
#> 106               NA          NA        NA                     NA   
#> 107               NA          NA        NA                     NA   
#> 108               NA          NA        NA                     NA   
#> 109               NA          NA        NA                     NA   
#> 110               NA          NA        NA                     NA   
#> 111               NA          NA        NA                     NA   
#> 112               NA          NA        NA                     NA   
#> 113               NA          NA        NA                     NA   
#> 114               NA          NA        NA                     NA   
#> 115               NA          NA        NA                     NA   
#> 116               NA          NA        NA                     NA   
#> 117               NA          NA        NA                     NA   
#> 118               NA          NA        NA                     NA   
#> 119               NA          NA        NA                     NA   
#> 120               NA          NA        NA                     NA   
#> 121               NA          NA        NA                     NA   
#> 122               NA          NA        NA                     NA   
#> 123               NA          NA        NA                     NA   
#> 124               NA          NA        NA                     NA   
#> 125               NA          NA        NA                     NA   
#> 126               NA          NA        NA                     NA   
#> 127               NA          NA        NA                     NA   
#> 128               NA          NA        NA                     NA   
#> 129               NA          NA        NA                     NA   
#> 130               NA          NA        NA                     NA   
#> 131               NA          NA        NA                     NA   
#> 132               NA          NA        NA                     NA   
#> 133               NA          NA        NA                     NA   
#> 134               NA          NA        NA                     NA   
#> 135               NA          NA        NA                     NA   
#> 136               NA          NA        NA                     NA   
#> 137               NA          NA        NA                     NA   
#> 138               NA          NA        NA                     NA   
#> 139               NA          NA        NA                     NA   
#> 140               NA          NA        NA                     NA   
#> 141               NA          NA        NA                     NA   
#> 142               NA          NA        NA                     NA   
#> 143               NA          NA        NA                     NA   
#> 144               NA          NA        NA                     NA   
#> 145               NA          NA        NA                     NA   
#> 146               NA          NA        NA                     NA   
#> 147               NA          NA        NA                     NA   
#> 148               NA          NA        NA                     NA   
#> 149               NA          NA        NA                     NA   
#> 150               NA          NA        NA                     NA   
#> 151               NA          NA        NA                     NA   
#> 152               NA          NA        NA                     NA   
#> 153               NA          NA        NA                     NA   
#> 154               NA          NA        NA                     NA   
#> 155               NA          NA        NA                     NA   
#> 156               NA          NA        NA                     NA   
#> 157               NA          NA        NA                     NA   
#> 158               NA          NA        NA                     NA   
#> 159               NA          NA        NA                     NA   
#> 160               NA          NA        NA                     NA   
#> 161               NA          NA        NA                     NA   
#> 162               NA          NA        NA                     NA   
#> 163               NA          NA        NA                     NA   
#> 164               NA          NA        NA                     NA   
#> 165               NA          NA        NA                     NA   
#> 166               NA          NA        NA                     NA   
#> 167               NA          NA        NA                     NA   
#> 168               NA          NA        NA                     NA   
#> 169               NA          NA        NA                     NA   
#> 170               NA          NA        NA                     NA   
#> 171               NA          NA        NA                     NA   
#> 172               NA          NA        NA                     NA   
#> 173               NA          NA        NA                     NA   
#> 174               NA          NA        NA                     NA   
#> 175               NA          NA        NA                     NA   
#> 176               NA          NA        NA                     NA   
#> 177               NA          NA        NA                     NA   
#> 178               NA          NA        NA                     NA   
#> 179               NA          NA        NA                     NA   
#> 180               NA          NA        NA                     NA   
#> 181               NA          NA        NA                     NA   
#> 182               NA          NA        NA                     NA   
#> 183               NA          NA        NA                     NA   
#> 184               NA          NA        NA                     NA   
#> 185               NA          NA        NA                     NA   
#> 186               NA          NA        NA                     NA   
#> 187               NA          NA        NA                     NA   
#> 188               NA          NA        NA                     NA   
#> 189               NA          NA        NA                     NA   
#> 190               NA          NA        NA                     NA   
#> 191               NA          NA        NA                     NA   
#> 192               NA          NA        NA                     NA   
#> 193               NA          NA        NA                     NA   
#> 194               NA          NA        NA                     NA   
#> 195               NA          NA        NA                     NA   
#> 196               NA          NA        NA                     NA   
#> 197               NA          NA        NA                     NA   
#> 198               NA          NA        NA                     NA   
#> 199               NA          NA        NA                     NA   
#> 200               NA          NA        NA                     NA   
#> 201               NA          NA        NA                     NA   
#> 202               NA          NA        NA                     NA   
#> 203               NA          NA        NA                     NA   
#> 204               NA          NA        NA                     NA   
#> 205               NA          NA        NA                     NA   
#> 206               NA          NA        NA                     NA   
#> 207               NA          NA        NA                     NA   
#> 208               NA          NA        NA                     NA   
#> 209               NA          NA        NA                     NA   
#> 210               NA          NA        NA                     NA   
#> 211               NA          NA        NA                     NA   
#> 212               NA          NA        NA                     NA   
#> 213               NA          NA        NA                     NA   
#> 214               NA          NA        NA                     NA   
#> 215               NA          NA        NA                     NA   
#> 216               NA          NA        NA                     NA   
#> 217               NA          NA        NA                     NA   
#> 218               NA          NA        NA                     NA   
#> 219               NA          NA        NA                     NA   
#> 220               NA          NA        NA                     NA   
#> 221               NA          NA        NA                     NA   
#> 222               NA          NA        NA                     NA   
#> 223               NA          NA        NA                     NA   
#> 224               NA          NA        NA                     NA   
#> 225               NA          NA        NA                     NA   
#> 226               NA          NA        NA                     NA   
#> 227               NA          NA        NA                     NA   
#> 228               NA          NA        NA                     NA   
#> 229               NA          NA        NA                     NA   
#> 230               NA          NA        NA                     NA   
#> 231               NA          NA        NA                     NA   
#> 232               NA          NA        NA                     NA   
#> 233               NA          NA        NA                     NA   
#> 234               NA          NA        NA                     NA   
#> 235               NA          NA        NA                     NA   
#> 236               NA          NA        NA                     NA   
#> 237               NA          NA        NA                     NA   
#> 238               NA          NA        NA                     NA   
#> 239               NA          NA        NA                     NA   
#> 240               NA          NA        NA                     NA   
#> 241               NA          NA        NA                     NA   
#> 242               NA          NA        NA                     NA   
#> 243               NA          NA        NA                     NA   
#> 244               NA          NA        NA                     NA   
#> 245               NA          NA        NA                     NA   
#> 246               NA          NA        NA                     NA   
#> 247               NA          NA        NA                     NA   
#> 248               NA          NA        NA                     NA   
#> 249               NA           0        NA                     NA  0
#> 
#> $metricdata$`C-3 On-Site WaterC' Enhancement`
#>   Existing Watercourse Type Existing Condition Proposed Watercourse Type
#> 1  Other rivers and streams               Poor  Other rivers and streams
#>   Length (Km) Target Condition
#> 1       0.522         Moderate
#>                                      Strategic Significance Units Enhanced
#> 1 Location ecologically desirable but not in local strategy          2.315
```

To return a data summary for baseline habitats:

``` r

pullonsitehabitatbaseline(metric)
#> $habitatbaselinedata
#>            broadhabitat                              habitattype baselinearea
#> 1              Cropland                             Cereal crops      17.0300
#> 2             Grassland                       Modified grassland       1.6100
#> 3                 Urban Artificial unvegetated, unsealed surface       0.1100
#> 4                 Urban           Developed land; sealed surface       0.0026
#> 5 Watercourse footprint                    Watercourse footprint       0.0800
#>   distinctiveness        baselinecondition baseliness baselineabu
#> 1             Low Condition Assessment N/A        Low       34.06
#> 2             Low                 Moderate        Low        6.44
#> 3        Very Low              N/A - Other        Low        0.00
#> 4        Very Low              N/A - Other        Low        0.00
#> 5        Very Low              N/A - Other        Low        0.00
#> 
#> $totalarea
#>   totalarea
#> 1   18.8326
#> 
#> $totalunits
#>   totalbaselineabu
#> 1             40.5
```

## Bugs

If you encounter a bug, please file an issue with a minimal reproducible
example on
[Codeberg](https://codeberg.org/Digital-Ecology/metric_pulling/issues).

## Code of Conduct

This project is released subject to a [Contributor Code of
Conduct](https://www.contributor-covenant.org/version/2/1/code_of_conduct/).
By participating in the project you agree to bide by its terms.
