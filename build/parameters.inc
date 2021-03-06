! Initial parameters
!Input and output datasets

!The Aboveground and Belowground biomass in the past (t C ha-1)
input_dir = "/home/fernando/Documents/workspace/wbcm/build/database/input/"
output_dir = "/home/fernando/Documents/workspace/wbcm/build/database/output/"

!1 - Forest formation
!2 - Savanna formation
!3 - Grassland
!4 - Agriculture/Pasture
!5 - Rainfed agriculture
!6 - Irrigated agriculture
!7 - Pasture
!8 - Water bodies
!9 - Urban areas/Farm buildings

!Calculates soil disturbance with (yes/no)?
withDisturb = "yes"

!Agriculture disturbance rate
disturb = 0.02

!ton ha-1
avgAGB = (/36.69,26.36,6.57,5.49,7.04,8.50,1.28,0.00,0.00/)
avgBGB = (/11.13,10.81,13.53,4.11,3.52,3.40,5.41,0.00,0.00/)

!ton C ha-1
avgSOC = (/79.46,67.10,67.10,62.40,57.45,78.06,51.69,0.00,0.00/)

!NPP - Net Primary Production
NPP = 0.6 !NPP (kgCm-2year-1

!PRE - Percentual of Roots Emission
PRE = 0.7

!ESL - Emission Soil Loss (fraction)
ESL = 0.8

!PRA - Percentual of root allocation
PRA = 0.2

!Percentual Aboveground biomass loss to atmosphere
percATMloss = 0.575

!Percentual soil loss to atmosphere
percSOILloss = 0.425 

!Decay rate of elemental carbon
decayREC = 0.001

!Decay rate of slash
decayRS = 0.40

!Decay rate of Bellowground biomass
decayRBGB = 0.70
