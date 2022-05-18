
# This project is for Data Quality Assessement of ORBIT Events data


After you have acquired the data, you should do the Data Quality Assessement (DQA) and Exploratory Data Anlysis (EDA) before to move forward to Advanced Analytics.

# Version and development
This is a test version (v.0.1.0);
Depending on the need and feedback of the team, we might refine the scripts and expand to an R package. 
To report bugs or give feedback, please reach out to [Benediekt Muylaert-Gelein](mailto:benediekt.muylaert-gelein@ucb.com) and [Neil](mailto:neil.niu@ucb.com). 

# Getting started
1) firstly, you set the directory and get the ORBIT Events data (0_Set_directory_get_data.R);
2) next, run the 1_DQA_EVENTS.R 
The structure is as follows
* **Data quality assessment using metrics: DQA1-DQA5**
* **Each DQA is visualized in figs**
### Purpose and structure of this kernel
* Give a systematic assessment of the quality of the raw data on ORBIT about events and Events ** [link](https://orbit-ucb.my.salesforce.com)

    + **1.cleaning and removing duplicates**
    *Check if data conforms to the syntax (format, type, range) of its definition;
    Nothing will be recorded more than once based upon how that thing is identified*
    + **2.Completeness**
    *The overall proportion of stored data against the potential of "100% complete"*
    + **3.Events'frequency and seasonality_wday**
    **3.1. events' frequency**
    
    **3.2. day_of_week_effect and seasonality_wday**
    + **4.attendee distribution and outlier detection**
    *An outlier is a value that lies in the tail of the statistical distribution of a set of data values(usually +-2 SD)*
    + **5.Timeliness/availability**
 # Directory structure
    


  

