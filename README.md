# Wilson_Summary_Reports

Dynamic report generating tool to view activities in the houses on a daily/weekly basis.

## Instalation / Preparation

  git clone `https://github.com/CreateHealth/Wilson_Summary_Reports.git` <br>
  Request zproc credential information from admin and enter them in  `.my.zproc` file <br>
  Install all the libraries in `runRmd.R` file <br>
  Run `runRmd.R` file
 
## Usage

  Input variables (end_date, location_id, threshold values) can be defined in `config.yaml` file under config directory.
  Each house can get different `end_date` and `threshold values`. The reports are generated for the 28 days prior to `end_time` value.<br>
  In the current setup, crontab runs every monday at 01:33 am in the villages server. The reports get generated for the past 28 days/4 weeks and saved under 'reports' directory. Then the reports get sent to the e-mail address(es) defined in `config.yaml` file. 


  
  
