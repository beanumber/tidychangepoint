# Rainfall in Medellín, Colombia

Rainfall in Medellín, Colombia

## Usage

``` r
mde_rain

mde_rain_monthly
```

## Format

An object of class `spec_tbl_df` (inherits from `tbl_df`, `tbl`,
`data.frame`) with 185705 rows and 8 columns.

An object of class `xts` (inherits from `zoo`) with 444 rows and 1
columns.

## Details

Daily rainfall measurements for 13 different weather stations positioned
around Medellín, Colombia. Variables:

- `station_id`:

- `lat`, `long`: latitude and longitude for the weather station

- `date`, `year`, `month`, `day`: date variables

- `rainfall`: daily rainfall (in cubic centimeters) as measured by the
  weather station

&nbsp;

- `mean_rainfall`: average rainfall across all weather stations

## References

[OpenStreetMap](https://www.openstreetmap.org/?mlat=6.244747&mlon=-75.574828&zoom=12)
