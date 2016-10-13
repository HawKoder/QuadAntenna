#################################################################################
## File			: QuadAntenna.r                                        ##
## Title		      : Quad Antenna Calculator                              ##
## Author      	      : Vinícius Jean Ferreira                               ##
## Purpose	   		: Calculate Quad Antenna Parameters                    ##
## Notes        		: Graduation Project                                   ##
#################################################################################

## ALWAYS REMEMBER TO CALL THE SCRIPT IN THE CONSOLE WITH THE FUNCTION
##   source("C:/Users/Vinny/Documents/R/QuadAntenna/QuadAntenna.R")

# Calculate the Inductance of the Antenna

# Input parameters: 
# f, frequency of the radio signal in kHz
# n, wavelenght divided by n
# L, side of the quad in centimeters
# P, depth of the quad in centimeters

#Return the inductance in micro Henry, the number of loops and the total length of the wire
Antenna_L <- function(f,n,L,P)
{
	# Calculating wavelength
	lambda = 300000000/(1000*f)
	
	# Calculating max lenght of the wire
	lfm = 0.1*lambda

	# Calculating length of the wire
	lf = lambda/n

	# If the wire length is bigger than the maximum possible length, ends the function
	if( lf > lfm )
	{ return('error') }

	#Calculating the number of loops in the coil
	ne = as.integer((100*lf)/(4*L))

	#Calculating the inductance of the coil
	Lc = 0.008*(ne^2)*L*(log(1.4142*L*ne/(P*(ne + 1))) + 0.37942 + ((0.3333*(ne + 1)*P)/(L*ne)))
	return(list(Inductance = Lc, Loops = ne, Wirelength = lf))
}