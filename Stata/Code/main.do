/*******************************************************************************
							Template Main do-file							   
*******************************************************************************/

	* Set version
	version 18.5

	* Set project global(s)	
	// User: Gaston Bernheim 
	display "`c(username)'" 	//Check username and copy to set project globals by user
	
	* Add file paths to DataWork folder and the Github folder for RRF2024
	if "`c(username)'" == "wb639770" {
        global onedrive "C:/Users/wb639770/OneDrive - WBG/RRF 2024/DataWork"
		global github 	"C:/Users/wb639770/OneDrive - WBG/RRF 2024/GitHub-rrf24-gaston"
    }
	
	
	* Set globals for sub-folders 
	global data 	"${onedrive}/Data"
	global code 	"${github}/Stata/Code"
	global outputs 	"${github}/Stata/Outputs"
	
	* Ado path
	sysdir set PLUS "$code/ado"
	mata: mata mlib index

	* Install packages 
	local user_commands	ietoolkit iefieldkit winsor sumstats estout keeporder grc1leg2 //Add required user-written commands

	foreach command of local user_commands {
	   capture which `command'
	   if _rc == 111 {
		   ssc install `command'
	   }
	}

	* Run do files 
	* Switch to 0/1 to not-run/run do-files 
	if (0) do "${code}/01-processing-data.do"


* End of do-file!	