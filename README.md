## ReadMe

This repository stores all the scripts needed to perform the EU-requested spatial trade-off analysis between reducing the extent of mobile bottom-contacting gear (MBCG) disturbance to sea-floor habitats and potential costs to fisheries.
The official ICES Advice comprises a main advice document (PDF) and interactive documents (4 HTMLs), and can be found here: https://doi.org/10.17895/ices.advice.25601121.v1.

# Input data
The assessment requires several input datasets, from various sources:
 * Fisheries data: Annual estimates of international fishing activity using mobile bottom-contacting gears (MBCG) per c-square, in terms of fishing footprint (swept area ratio), landings weight (kg), and landings value (euro). This data is provided by ICES, based on the ICES 2022 VMS and logbook data call and quality checked by WGSFD and covers 2017-2022.
 * Seabed sensitivity data: Estimates of the median longevity of the benthic community at the seabed per c-square. Obtained from WGFBIT.
 * Small-scale fisheries information: Estimates of the contribution of Small-scale fisheries to the overall fishing activity per FAO region. Obtained from WKTRADE4.
 * Spatial shapefiles delineating the assessment areas and their subdivisions:
	- EU marine waters: obtained from EEA, specifically for this assessment.
	- OSPAR assessment areas: obtained from OSPAR, provide the basis for the delineation of subdivisions in the Greater North Sea, Celtic Seas, and the Bay of Biscay and the Iberian Coast areas.
	- HELCOM subbasins: obtained from HELCOM, provide the basis for the delineation of subdivisions in the Baltic Sea.
	- FAO regions: obtained from the spatial facility of ICES, used to show the overlap of assessment areas and FAO regions.

# Workflow of assessment
The assessment is runned in three steps:
 1. Pre-assessment script ("Ecoregion_Assessment_preprocessing.R") to prepare the fisheries data and the gear modifications scenarios. 
 2. Assessment script ("Ecoregion_Assessment.R") that performs the assessment in several steps: 
	a. per assessment area (or subdivision):
		i. Load data
		ii. Produce overview plot of assessment area (not for subdivisions)
		iii. Perform calculations
		iv. Create figures and tables
		v. Perform core fishing analysis
		vi. Perform footprint reduction scenario
		vii. Perform gear modification scenarios
	b. Correct for data limitations/errors
	c. Produce summary tables
 3. HTML-production script ("Script to create all HTMLs.R") that configures the final HTMLs. 

