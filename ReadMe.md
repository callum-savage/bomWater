## bomWater

This `R` package grabs data from the Australian Bureau of Meteorology Water Data online (http://bom.gov.au/waterdata/) via the WISKI API.

Several functions have been written to retrieve quality checked timeseries. These are:

- `get_as_stored()`
- `get_hourly()`
- `get_daily()`
- `get_monthly()`
- `get_yearly()`

The following Water Data Online variables can be accessed using these functions:

| Parameter                      | Units  |
| ------------------------------ | ------ |
| Water Course Discharge         | m3/s   |
| Water Course Level             | m      |
| Electrical conductivity at 25C | µS/cm  |
| Turbidity                      | NTU    |
| pH                             | pH     |
| Water Temperature              | ºC     |
| Storage Volume                 | ML     |
| Storage Level                  | m      |
| Ground Water Level             | m      |
| Rainfall                       | mm     |
| Evaporation                    | mm     |
| Dry Air Temperature            | ºC     |
| Relative Humidity              | %      |
| Wind Speed                     | m/s    |

Make sure formatting of the parameter types is as in the table above when making requests. The function `parameters()` can be used to retrieve this from within R.

Station information can be queried using `get_station_list()`, as in the example below.

The SOS2 manual can be consulted for the units of the different timeseries, as well as the meanings of the different quality codes ([BoM WISKI manual](http://www.bom.gov.au/waterdata/wiski-web-public/Guide\%20to\%20Sensor\%20Observation\%20Services\%20(SOS2)\%20for\%20Water\%20Data\%20\%20Online\%20v1.0.1.pdf)).

### Installation

```r
# Install the development version from GitHub
devtools::install_github('buzacott/bomWater')
```

### Copyright

All code is licensed MIT.

The license and copyright for the data can be viewed under the copyright tab at http://www.bom.gov.au/waterdata/. More information about the Bureau of Meteorology copyright can be found at http://www.bom.gov.au/other/copyright.shtml.

### Examples

```r
library(bomWater)

# Daily streamflow from Cotter River at Gingera (in m3/s)
cotter_river <- get_daily(parameter = 'Water Course Discharge',
                          station_number = '410730',
                          start_date     = '2020-01-01',
                          end_date       = '2020-01-31')

cotter_river
# A tibble: 31 x 3
   Timestamp           Value `Quality Code`
   <dttm>              <dbl>          <int>
 1 2020-01-01 00:00:00 0.013             10
 2 2020-01-02 00:00:00 0.013             10
 3 2020-01-03 00:00:00 0.011             10
 4 2020-01-04 00:00:00 0.009             10
 5 2020-01-05 00:00:00 0.01              10
 6 2020-01-06 00:00:00 0.015             10
 7 2020-01-07 00:00:00 0.023             10
 8 2020-01-08 00:00:00 0.019             10
 9 2020-01-09 00:00:00 0.017             10
10 2020-01-10 00:00:00 0.014             10
# … with 21 more rows

# Monthly total rainfall in mm at Cotter Hut
cotter_hut <- get_monthly(parameter = 'Rainfall',
                          station_number = '570946',
                          start_date     = '2019-01-01',
                          end_date       = '2019-12-31')
cotter_hut
# A tibble: 12 x 3
   Timestamp           Value `Quality Code`
   <dttm>              <dbl>          <int>
 1 2019-01-01 00:00:00  57.2             10
 2 2019-02-01 00:00:00  23.2             10
 3 2019-03-01 00:00:00  89.2             10
 4 2019-04-01 00:00:00  11.2             10
 5 2019-05-01 00:00:00 111.              10
 6 2019-06-01 00:00:00  44.8             10
 7 2019-07-01 00:00:00  38               10
 8 2019-08-01 00:00:00  50.8             10
 9 2019-09-01 00:00:00  50.8             10
10 2019-10-01 00:00:00  53.6             10
11 2019-11-01 00:00:00  41.2             10
12 2019-12-01 00:00:00   8               10

# Get a list of groundwater bore data available from water data online
get_station_list(parameter = 'Ground Water Level')
# A tibble: 4,439 x 5
   station_name station_no station_id station_latitude station_longitude
   <chr>        <chr>           <int>            <dbl>             <dbl>
 1 01/DD01 D    60930131       387998            -33.2              118.
 2 01/DD01 OB   60930132       388003            -33.2              118.
 3 01/DD04 S    60930135       388008            -33.2              118.
 4 02/DD25 OB   60930141       388013            -33.2              118.
 5 02/DD26 OB   60930142       388018            -33.2              118.
 6 02/DD27 OB   60930143       388023            -33.2              118.
 7 02/DD28 OB   60930144       388028            -33.2              118.
 8 02/DD29 OB   60930145       388033            -33.2              118.
 9 02/DD30 OB   60930146       388038            -33.2              118.
10 02/DD31 OB   60930147       388043            -33.2              118.
# … with 4,429 more rows
```


## Design philosphy
- Intentionally designed to simplify requests and output. For fine control use make_Bom_request to construct your own calls
- All 'get' functions always return a tibble (including single column response)
   - if there's no result, a zero-row tibble is returned
   - the columns will correspond to the values (and order) provided by 'return_fields'
      - the exception is 'custom_attributes', which I need to figure out how to deal with
   - request fields can also be included in the return fields???????
   - if multiple stations are requested and not all stations have returned a result a warning is raised
- metadata functions return a tibble by default, can return a vector with specific arguments
   - these functions need a naming convention:
      - bw_parameters
      - bw_interpolation_types
      - bw_return_fields
      - bw_quality_codes
- helper functions which extract a specific column are named with the prefix extract_ and return a vector
   - Not user facing
   - extract_data_owner
   - extract_timezone
   - extract_timeseries_id
- results and calls are always in snake_case (syntactic)
   - requests may also use title case? actually no, this just adds complexity
   - interaction with parameters: e.g. station_number and parameter are uniform, and converted to bom names before request
   - in general, outputs are based on what you see on bom water data online
   - show how to convert to sentene case in vignette
