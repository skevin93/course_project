module chemistry

   use kinds

   implicit none

   ! type :: atomic
   !    character(len=2) :: symbol
   !    real(dp)         :: mass
   ! end type atomic

   real(dp) :: atomic_masses(9) = (/ &
       1.00790_dp, & ! H
       4.00260_dp, & ! He
       6.94000_dp, & ! Li
       9.01218_dp, & ! Be
      10.81000_dp, & ! B
      12.01100_dp, & ! C
      14.00670_dp, & ! N
      15.99940_dp, & ! O
      18.99840_dp/)  ! F




end module chemistry