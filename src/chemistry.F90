module chemistry

   use kinds
   use parameters
   use file_info, only: output_error_msg

   implicit none

   type :: atomic
      integer          :: Z
      character(len=2) :: symbol
   end type atomic

   integer, parameter, private :: num_atomic = 9
   type(atomic) :: atomic_list(num_atomic)

   data atomic_list(1) / atomic(1,  'H ') /
   data atomic_list(2) / atomic(2,  'He') /
   data atomic_list(3) / atomic(3,  'Li') /
   data atomic_list(4) / atomic(4,  'Be') /
   data atomic_list(5) / atomic(5,  'B ') /
   data atomic_list(6) / atomic(6,  'C ') /
   data atomic_list(7) / atomic(7,  'N ') /
   data atomic_list(8) / atomic(8,  'O ') /
   data atomic_list(9) / atomic(9,  'F ') /

contains

   integer function symbol_to_Z(symbol)

      implicit none

      character(len=2), intent(in) :: symbol

      integer :: i

      symbol_to_Z = 1

      do i=1, num_atomic 
         if(trim(symbol) == trim(atomic_list(i)%symbol)) then
            symbol_to_Z = atomic_list(i)%Z
            return
         end if
      end do

      call output_error_msg("Unknown atomic symbol!")

   end function

   character(len=2) function Z_to_symbol(Z)

      implicit none

      integer, intent(in) :: Z

      Z_to_symbol = "H"

      if(Z <= num_atomic) then

         Z_to_symbol = atomic_list(Z)%symbol
      
      else

         call output_error_msg("Unknown atomic number!")

      end if

   end function Z_to_symbol

   subroutine center_of_mass(masses, coord, n_atoms)

      implicit none

      integer, intent(in) :: n_atoms
      real(dp), intent(in) :: masses(n_atoms)
      real(dp), intent(inout) :: coord(3, n_atoms)

      real(dp) :: R_0(3), sum_masses
      integer :: i

      R_0        = zero
      sum_masses = zero

      do i=1, n_atoms
         sum_masses = sum_masses + masses(i)
         R_0 = R_0 + masses(i)*coord(:,i)
      end do

      R_0 = R_0/sum_masses

      do i=1, n_atoms
         coord(:,i) = coord(:,i) - R_0
      end do

   end subroutine

end module chemistry