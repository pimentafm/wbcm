! Constants

character(len=200) :: input_dir, output_dir

real(kind=float), dimension(9) :: avgAGB, avgBGB, avgSOC
real(kind=float) :: NPP, PRE, ESL, PRA

real(kind=double), parameter :: factor_bio2carbon = 0.42
integer(kind=byte), parameter :: isDisturb = 1

integer(kind=byte), dimension(:,:), allocatable :: agrcont 
