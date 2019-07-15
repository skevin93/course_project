module array_tools
!!
!!    Array tools
!!    Written by Marco Scavino, June 2019
!!
!!    Tools to calc some matrix properties
!!
   use kinds
   use parameters
   use file_info, only: output_error_msg

contains

   pure function deter(mat)
!!
!!    Determinant of matrix
!!    Written by Marco Scavino, June 2019
!!
!!    Determinant of a matrix 3×3
!!
      implicit none

      real(dp), intent(in) :: mat(3,3)

      real(dp) :: deter
      
      deter = mat(1,1)*mat(2,2)*mat(3,3)
      deter = deter + mat(1,2)*mat(2,3)*mat(3,1)
      deter = deter + mat(1,3)*mat(3,2)*mat(2,1)
      deter = deter - mat(1,3)*mat(2,2)*mat(3,1)
      deter = deter - mat(1,2)*mat(2,1)*mat(3,3)
      deter = deter - mat(2,3)*mat(3,2)*mat(1,1)

   end function deter

   subroutine inverse(mat, mat_res)
!!
!!    Inverse of a matrix
!!    Written by Marco Scavino, June 2019
!!
!!    Inverse of a matrix 3×3.
!!
      implicit none

      real(dp), intent(in) :: mat(3,3)
      real(dp), intent(out) :: mat_res(3,3)

      real(dp) :: deter_mat

      deter_mat = deter(mat)

      mat_res = zero

      if(deter_mat == zero) then
         call output_error_msg("Not invertible matrix!")
      end if
!  C_A^T
      mat_res(1,1) =   mat(2,2)*mat(3,3) - mat(2,3)*mat(3,2)
      mat_res(1,2) = -(mat(1,2)*mat(3,3) - mat(1,3)*mat(3,2))
      mat_res(1,3) =   mat(1,2)*mat(2,3) - mat(1,3)*mat(2,2)
      mat_res(2,1) = -(mat(2,1)*mat(3,3) - mat(2,3)*mat(3,1))
      mat_res(2,2) =   mat(1,1)*mat(3,3) - mat(1,3)*mat(3,1)
      mat_res(2,3) = -(mat(1,1)*mat(2,3) - mat(1,3)*mat(2,1))
      mat_res(3,1) =   mat(2,1)*mat(3,2) - mat(2,2)*mat(3,1)
      mat_res(3,2) = -(mat(1,1)*mat(3,2) - mat(1,2)*mat(3,1))
      mat_res(3,3) =   mat(1,1)*mat(2,2) - mat(1,2)*mat(2,1)

      mat_res = mat_res/deter_mat
   end subroutine inverse

end module array_tools