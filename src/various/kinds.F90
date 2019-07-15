module kinds
!!
!!    Kinds
!!    Written by Marco Scavino, June 2019
!!
!!    Define the kinds usable (single and double precision reals).
!!
   implicit none

   integer, parameter :: sp = selected_real_kind(6, 300)
   integer, parameter :: dp = selected_real_kind(15, 300)

end module