! Constants
character(len=200) :: input_dir, output_dir

real(kind=float), dimension(9) :: avgAGB, avgBGB, avgSOC
real(kind=float) :: NPP, PRE, ESL, PRA, percATMloss, percSOILloss, decayREC, decayRS, decayRBGB, disturb

! Calculates Soil Carbon Emission with or not disturbance
character(len=3) :: withDisturb


integer(kind=4), dimension(:,:), allocatable :: cropCount 
