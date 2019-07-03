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
   use parameters
   use input_file
   use file_info, only: file
   use output_module, only: output_xyz_file
   use chemistry, only: atomic_masses

   implicit none

   real(dp), parameter :: tolerance = 1.0E-6_dp

contains

   subroutine first_rotation(atoms, coord, n)

      implicit none

      real(dp), intent(inout) :: coord(:,:)
      integer, intent(in)     :: atoms(:), n

      real(dp) :: T(3,3)
      real(dp), allocatable :: atomic_mass(:), tmp_coord(:,:)

      integer :: i

      type(file) :: f1, f2

      character(len=30) :: orientation

      orientation = "eckart"

      call read_var("orientation", orientation, &
         description="Orientation of the first atom", &
         expected=(/"eckart  ",&
                    "original"/))

      f1 = file(20, "old_1.xyz", "xyz")
      f2 = file(21, "new_1.xyz", "xyz")

      call open_File(f1, "write")

      call output_xyz_file(f1, atoms, coord, n, comment="r_0")

      call close_file(f1)

      if(trim(orientation) == "eckart") then

         allocate(atomic_mass(n))

         do i=1, n
            atomic_mass(i) = atomic_masses(atoms(i))
         end do

         call eckart(n, coord, atomic_mass, T)

         deallocate(atomic_mass)

         allocate(tmp_coord(3,n))

         call dgemm("N", "N", 3,n,3, one, T, 3, coord, 3, zero, tmp_coord, 3)

         coord = tmp_coord

         deallocate(tmp_coord)

      end if


      call open_File(f2, "write")

      call output_xyz_file(f2, atoms, coord, n, comment="T*r_0")

      call close_file(f2)

   end subroutine

   subroutine eckart(n_atom, coord, atomic_mass, eigen_vec, eigen_val)

      implicit none
      ! Input
      integer, intent(in) :: n_atom
      real(dp), intent(in) :: coord(:,:), atomic_mass(:)
      ! Output
      real(dp), intent(out) :: eigen_vec(3,3)
      real(dp), intent(out), optional :: eigen_val(3)
      ! Local
      integer :: i, info
      real(dp) :: T(3,3), x_2, y_2, z_2
      real(dp) :: eigen_value(3)

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

      if (abs(T(1,2)) < tolerance .and. &
          abs(T(1,3)) < tolerance .and. &
          abs(T(2,3)) < tolerance) then

         do i=1, 3
            eigen_value(i) = T(i,i)
            eigen_vec(i,i) = one
         end do

      else
      
         call dsyev('V', 'L', 3, T, 3, eigen_value, eigen_vec, 9, info)

         if (info /= 0) then
         
            call output_error_msg('Diagonalizing tensor failed')
         
         end if
      
         eigen_vec = T

      end if

      if(present(eigen_val))then
         eigen_val = eigen_value
      end if

      return
   end subroutine eckart

end module