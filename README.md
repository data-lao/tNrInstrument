# tNrInstrument
![entire setup](https://user-images.githubusercontent.com/80794322/166525419-27ea44cc-c435-49a8-8bdc-d8df3889e2a6.PNG)

The total reactive nitrogen (tNr) instrument was developed by the [VandenBoer research group](https://www.tcvandenboer.ca/) at York University, Toronto and funded by the Sloan foundation.  
This instrument is designed to measure the total gas-phase reactive nitrogen (tNr) budget, defined as all N-containing species execpt N<sub>2</sub> and N<sub>2</sub>O. The instrument can also determine contributions from NOx, HONO, and NH<sub>3</sub>, which are key drivers of indoor air quality. The tNr instrument has a small footprint, safe (no dangerous chemicals required), insulated oven, quiet and automated operation making it suitable for measurements in both occupied and unoccupied indoor spaces. The tNr instrument can be set up to measure without any operator input for several days and so can be used continuously in occupied indoor areas. Thus, it is ideal for surveying the indoor air quality contributions of a broad range Nr species across different environments, such as domestic residences, offices, and public buildings, without the need for multiple instruments targeting individual Nr species. 

A custom SCADA system written in LabVIEW (National Instruments) was used to control the instrument via this [LabJack VI](https://github.com/data-lao/tNrInstrument/tree/main/labview).

A custom R-script is provided to speed up the data processing and finalization for visualizations and interpretation (sample data provided in tNr sample data.zip file).

For full details and optimal operating conditions, please see our published instrument description paper (https://doi.org/10.1039/D2EM00446A). 

If you use this software, please review the citation file: https://github.com/data-lao/tNrInstrument/blob/main/citation


