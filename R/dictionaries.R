## These dictionaries define the front facing station names and variable names
## and links these strings to the original name from the database.

stnNameDict <- list(
  "Upper Skeena" = "upperskeena",
  "Datlamen Pass" = "datlamen",
  "Rennell Pass" = "rennellpass",
  "Mount Maya" = "mountmaya",
  "Clayton Falls" = "claytonfalls",
  "Ape Lake" = "apelake",
  "Machmell Kliniklini" = "machmellkliniklini",
  "Machmell" = "machmell",
  "Buxton East Ridge" = "eastbuxton",
  "Klinaklini" = "klinaklini",
  "Plummer Hut" = "plummerhut",
  "Homathko" = "homathko",
  "Cain Ridge Run" = "cainridgerun",
  "Cain Lower" = "lowercain",
  "Stephanie 1" = "steph1",
  "Stephanie 2" = "steph2",
  "Stephanie 3" = "steph3",
  "Stephanie 4" = "steph4",
  "Stephanie 6" = "steph6",
  "Stephanie 7" = "steph7",
  "Stephanie 8" = "steph8",
  "Upper Russell" = "upperrussell",
  "Russell Main" = "russellmain",
  "Upper Cruickshank" = "uppercruickshank",
  "Perseverance" = "perseverance",
  "Mount Arrowsmith" = "mountarrowsmith",
  "Mount Cayley" = "mountcayley",
  "Tetrahedron" = "tetrahedron"
)

varsDict <- list("DateTime" = "DateTime",
                 "Air Temperature (\u00b0C)" = "Air_Temp",
                 "Relative Humidity (%)" = "RH",
                 "Air Pressure (kPa)" = "BP",
                 "Wind Speed (km/h)" = "Wind_Speed",
                 "Wind Direction (deg)" = "Wind_Dir",
                 "Peak Wind Speed (km/h)" = "Pk_Wind_Speed",
                 "Peak Wind Direction (deg)" = "Pk_Wind_Dir",
                 "Tipping Bucket Increment (mm)" = "PP_Tipper",
                 "Tipping Bucket Cumulative (mm)" = "PC_Tipper",
                 "Stand Pipe Raw (mm)" = "PC_Raw_Pipe",
                 "Stand Pipe Increment (mm)" = "PP_Pipe",
                 "Snow Depth (cm)" =  "Snow_Depth",
                 "Snow Water Equivalent (mm)" = "SWE",
                 "Solar Radiation (W/m2)" = "Solar_Rad",
                 "Short Wave Radiation Upper (W/m2)" = "SWU",
                 "Short Wave Radiation Lower (W/m2)" = "SWL",
                 "Long Wave Radiation Upper (W/m2)" = "LWU",
                 "Long Wave Radiation Lower (W/m2)" = "LWL",
                 "Lysimeter (mm)" = "Lysimeter",
                 "Soil Moisture (%)" = "Soil_Moisture",
                 "Soil Temperature (\u00b0C)" = "Soil_Temperature",
                 "Battery (V)" = "Batt")

monthlyVarsDict <- list(
                 "Air Temperature (\u00b0C)" = "Air_Temp",
                 "Snow Depth (cm)" =  "Snow_Depth",
                 "SWE (mm)" =  "SWE",
                 "Accumulated Precip. (mm)" =  "PC_accumulated_wtr_yr",
                 "Precip. Totals (mm)" = 'PC_monthly_total'
                 )

hourlyVarsDict <- list(
  "Air Temperature (\u00b0C)" = "Air_Temp",
  "Snow Depth (cm)" =  "Snow_Depth",
  "Accumulated SWE (mm)" =  "SWE",
  "Accumulated Precip. (mm)" =  "PC_accumulated_wtr_yr")

pretty_stats_var_names_monthly <- list(
  "Air Temperature" = "Air_Temp",
  "Snow Depth" =  "Snow_Depth",
  "SWE" =  "SWE",
  "Stand Pipe Accumulated" =  "PC_accumulated_wtr_yr",
  "Stand Pipe Monthly Totals" =  "PC_monthly_total")

pretty_stats_var_names_hourly <- list(
  "Air Temperature" = "Air_Temp",
  "Snow Depth" =  "Snow_Depth",
  "SWE" =  "SWE",
  "Stand Pipe Accumulated" =  "PC_accumulated_wtr_yr")