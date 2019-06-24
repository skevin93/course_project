module eckart_rotation
!!
!!    Eckart rotation
!!    Written by Marco Scavino, June 2019
!!
!!    Calculate the inertia tensor, mandatory for the Eckart rotation
!!    Then diagonalize it, usign the DSEV subroutine, and construct the
!!    rotation matrix using the 
!!

   use kinds

   implicit none


contains

   subroutine inertia_mom(n_atom, coord, atomic_mass, eigen_value, eigen_vec)

      implicit none
      ! Input
      integer, intent(in) :: n_atom
      real(dp), intent(in) :: coord(:,:), atomic_mass(:)
      ! Output
      real(dp), intent(out) :: eigen_vec(3,3), eigen_value(3)
      ! Local
      integer :: i, info, j
      real :: T(3,3), x_2, y_2, z_2

      eigen_value = zero
      eigen_vec   = zero

      T = zero

      ! Definition of the tensor of moments of inertia

      do i = 1, n_atom

         x_2 = coord(1,i)*coord(1,i)
         y_2 = coord(2,i)*coord(2,i)
         z_2 = coord(3,i)*coord(3,i)

         T(1,1) = T(1,1) + atomic_mass(i)*(y_2 + z_2)
         T(2,2) = T(2,2) + atomic_mass(i)*(x_2 + z_2)
         T(3,3) = T(3,3) + atomic_mass(i)*(x_2 + y_2)
         T(1,2) = T(1,2) - atomic_mass(i)*(coord(1,i)*coord(2,i))
         T(1,3) = T(1,3) - atomic_mass(i)*(coord(1,i)*coord(3,i))
         T(2,3) = T(2,3) - atomic_mass(i)*(coord(2,i)*coord(3,i))
      
      end do

      T(2,1) = T(1,2)
      T(3,1) = T(1,3)
      T(3,2) = T(2,3)

      !Diagonalization

      if (abs(T(1,2)) + abs(T(1,3)) + abs(T(2,3)) < 1.0e-6E0) then
         forall (i=1:3)
            eigen_value(i) = T(i,i)
            eigen_vec(i,i) = 1.0E0
         end forall
      else
      
         call dsyev('V', 'L', 3, T, 3, eigen_value, eigen_vec, 9, info)

         if (info /= 0) then
         
            call output_error_msg('Diagonalizing tensor failed')
         
         end if
      
         eigen_vec = T
      end if
      return
   end subroutine inertia_mom

end module