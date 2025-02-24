Use this tool to easily download hourly weather data for any point in the United States and Canada (below 60°N latitude). Weather data is provided by a subscription to IBM's Environmental Intelligence Suite API. From this hourly weather data, we compute daily values, moving averages, growing degree days, and selected plant disease risk values.

### Units

Most values can be shown in either imperial or metric units. Temperature and dew point: °C or °F, precipitation (rain/melted snow): mm or in, snow accumulation: cm or in, relative humidity: %, pressure: mbar or inHg, wind speed: km/h or mph, wind direction: compass degrees (N=0°, E=90°, etc.). Growing degree day base/upper thresholds and accumulations always in Fahrenheit.

### Hourly data

Hourly data includes the timestamp in UTC and an adjustment to local time based on the timezone associated with the GPS coordinates. This is accomplished using the `tz_lookup_coords` function from the `lutz` package. This method may not correctly assign the time zone at timezone borders. Hourly weather parameters include air temperature, dew point, dew point depression (difference between air temperature and dew point), relative humidity, precipitation, snow accumulation, wind speed, wind gusts, wind direction, barometric pressure (mean sea level), and pressure change since the previous hour.

Note: A wind gust is defined as a brief increase in wind speed that is at least 10 mph (16 km/h, 4.5 m/s) faster than the average wind speed and peaks above 18 mph (30 km/h, 8 m/s). Due to these definitions not every hour or day will have recorded wind gusts.

### Daily data

For each hourly weather parameter, the minimum, mean, and maximum value are generated. In addition, the total daily value is generated when appropriate (precipitation and snow accumulation).

### Moving averages

7, 14, 21, and 30-day moving averages are calculated for each daily value using the `roll_apply` function from the `zoo` package. Either centered or right-aligned (trailing) moving average types are available. These moving averages will use *up to* the window size, so if for example the centered 30-day moving average is desired for a particular date, weather should be downloaded for at least 15 days on either side of the desired date or date range.

### Growing degree days

The single sine method is used to calculate growing degree days from daily minimum and maximum air temperature values. For each base temperature, a model is provided with and without the common 86°F upper threshold temperature (horizontal cutoff). The single sine method differs from the simple average method only when the minimum temperature is below the lower threshold temperature, or the maximum temperature is above the upper threshold temperature. In such cases, the single sine method will more accurately reflect the amount of heat energy available, relative to the simple average method.

## Plant disease models

Selected field crops and vegetable disease model outputs are provided. These models are subject to change. The calculations used to generate each model prediction can be viewed in the source code.

-   White mold (aka Sporecaster) - dry, irrigated 15-inch row spacing, irrigated 30-inch row spacing - probability of apothecial presence. More information: <https://cropprotectionnetwork.org/news/smartphone-application-to-forecast-white-mold-in-soybean-now-available-to-growers>
-   Frogeye Leaf Spot of soybean - probability of presence. More information: <https://cropprotectionnetwork.org/encyclopedia/frogeye-leaf-spot-of-soybean>
-   Gray Leaf Spot of corn - probability of presence. More information: <https://cropprotectionnetwork.org/encyclopedia/gray-leaf-spot-of-corn>
-   Tar Spot of corn (aka Tarspotter) - probability of presence. More information: <https://cropprotectionnetwork.org/encyclopedia/tar-spot-of-corn>
-   Potato physiological days - risk of Early blight when cumulative p-days exceed 300 since crop emergence. More information: <https://vegpath.plantpath.wisc.edu/diseases/potato-early-blight/>
-   Late blight disease severity values - risk of disease increased with accumulated severity values since last fungicide application. Uses the Wallin BLITECAST algorithm. More information: <https://vegpath.plantpath.wisc.edu/diseases/potato-late-blight/>
-   Carrot foliar disease severity values - risk of disease increases with accumulated values. More information: <https://vegpath.plantpath.wisc.edu/diseases/carrot-alternaria-and-cercospora-leaf-blights/>
-   Cercospora leaf spot daily infection values - risk of disease increases with accumulated values. Based on the model outlined in [A Cerospora Leaf Spot Model for Sugar Beet: In Practice by an Industry](https://apsjournals.apsnet.org/doi/abs/10.1094/PDIS.1998.82.7.716). More information about Cercospora leaf spot: <https://vegpath.plantpath.wisc.edu/diseases/carrot-alternaria-and-cercospora-leaf-blights/>
