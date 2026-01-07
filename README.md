# shime-monitor-r

## Version
1.0.0

## Overview
SHIME Monitor (`shime_monitor.R`) visualizes data from the Simulator of the Human Intestinal Microbial Ecosystem (SHIME®), a dynamic in vitro gut model system. The script processes and displays both pump activity and pH measurements, providing researchers with clear visualizations of system behavior during gut microbiome experiments.

## Background
During SHIME® experiments, microbial fermentation of non-digestible carbohydrates into short-chain fatty acids (SCFAs) influences pH levels in the reactors. To maintain target pH conditions, the system automatically adjusts acidity using acid and base pumps. While other methods are needed to directly measure SCFA production, the amount of base added provides a rough indicator of this ermentation activity. Closely tracking pH and pump activity is thus crucial to:
- Ensure proper system operation and stability
- Track microbial fermentation dynamics
- Identify potential issues or unexpected trends

## Features
- Visualizes acid and base pump activity over time
- Displays pH measurements from probes
- Supports multiple treatment conditions
- Handles data from both PC (proximal colon) and DC (distal colon) reactors
- Allows flexible date range selection
- Configurable through YAML file

## Requirements
### Software
- R (latest version recommended)
- Required R packages:
	- yaml
	- dplyr
	- ggplot2
	- scales

### Input Files
It is recommended that the user directly extract Logs.csv files from the .accdbt SHIME® file (using Microsoft Access or similar), WITH default headings.

1. PumpsLogs.csv file containing:
	- "ID"
	- "LogDate" (format: DD-MM-YYYY HH:MM:SS)
	- "PumpNr"
	- "Volume"

2. pHLogs.csv file containing:
	- "ID"
	- "LogDate" (format: DD-MM-YYYY HH:MM:SS)
	- "ProbeNr"
	- "Ph"

3. config.yaml file with user-specified settings
	- ALL fields must be inputted by user

## Setup
1. Place `shime_monitor.R` in your working directory
2. Create/modify `config.yaml` with your settings
3. Ensure your pump and pH log files are in the specified location
4. Run the script

## Output
The script generates plots showing:
- Acid pump activity over time for each reactor  
- Base pump activity over time for each reactor  
- pH measurements over time for each reactor  
- Data grouped by treatment conditions

## Troubleshooting
Common issues and solutions:
* **Missing data**: Verify date range in config matches your data files
* **File not found**: Check file paths and working directory
* **Parsing errors**: Ensure correct date format and separators
* **Empty plots**: Verify pump/probe numbers match your data

## Contributing
Feel free to submit issues and enhancement requests!

## License
License information TBD.

## Contact
For questions or feedback, you can reach me here:  
[GitHub Profile](https://github.com/lvsvendsen)  

## Acknowledgments
* SHIME® is a registered trademark of ProDigest
* Script developed for Arumugam Lab
