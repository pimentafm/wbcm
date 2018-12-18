! Initial parameters
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

avgAGB = (/36.69,26.36,6.57,5.49,7.04,8.50,1.28,0.00,0.00/)
avgBGB = (/11.13,10.81,13.53,4.11,3.52,3.40,5.41,0.00,0.00/)
avgSOC = (/79.46,67.10,67.10,62.40,57.45,78.06,51.69,0.00,0.00/)

!Carbon stock changes from one year of vegetation growth (t C ha-1)
!AGB
cGrow = (/2.6,2.6,2.6,2.6,2.6,2.6,2.6,0.00,0.00/)

!BGB
cbGrow = (/0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00/)

!NPP - Net Primary Production
NPP = 0.6 !NPP (kgCm-2year-1
