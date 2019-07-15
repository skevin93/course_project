module duschinsky_tools

   use kinds
   use parameters
   use file_info
   use input_file, only: read_var
   use output_module
   implicit none

   real(dp) :: tolerance = 1.0E-6

contains

   subroutine duschinsky_matrix(weight, coord_1, coord_2, L_matr_1, L_matr_2, n_atoms)
!!
!!    Calc duschinsky matrix
!!    Written by Marco Scavino, July 2019
!!
      implicit none
!
      integer, intent(in) :: n_atoms
      ! integer, intent(in) :: atoms(:)
      real(dp), intent(in), dimension(:) :: weight
      real(dp), intent(in), dimension(:,:) :: coord_1, coord_2
      real(dp), intent(inout), dimension(:,:) :: L_matr_1, L_matr_2

      integer :: n, i, j
      real(dp), allocatable :: J_matr(:,:), K_shift(:), tmp(:)
      real(dp), allocatable :: sqrt_weight(:)

      character(len=30) :: result

      result       = "both"

      n = size(L_matr_1, 1)

      call build_L(L_matr_1)
      call build_L(L_matr_2)

      call read_var("result", result, &
         description="Result to print", &
         expected=(/"matrix      ", &
                    "shift vector", &
                    "both        "/))

      if(trim(result) == "matrix" .or. trim(result) == "both") then

         ! J = L'^T (L''^T)^-1 = L'^T L''

         allocate(J_matr(n,n))
         call dgemm("T", "N", n, n, n, one, L_matr_1, n, L_matr_2, n, zero, J_matr, n)

         call check_orthogonal(J_matr, n)

         call output_J_matrix(J_matr, n)
      
      end if

      if(trim(result) == "shift vector" .or. trim(result) == "both") then
         ! K = L'^T √(M) (X''^eq - X'^eq)

         allocate(K_shift(n))
         allocate(tmp(3*n_atoms))
         allocate(sqrt_weight(n_atoms))

         sqrt_weight = sqrt(weight)

         !L'^T √(M)
         do j=1, n_atoms
            do i=1, 3
               tmp(i+(j-1)*3) = sqrt_weight(i)*(coord_2(i,j)-coord_1(i,j))
            end do
         end do

         call dgemv("T", n,n, one, L_matr_1, n, tmp, n, zero, K_shift, n)

         call output_K_shift(K_shift, n_atoms)

      end if

   end subroutine

   subroutine output_J_matrix(J_matr, n)
!!
!!    Output duschinsky matrix
!!    Written by Marco Scavino, July 2019
!!
      implicit none

      real(dp), intent(inout) :: J_matr(:,:)
      integer, intent(in) :: n
      
      character(len=30) :: info, info_2
      real(dp), allocatable :: max_element(:)

      info   = "full"
      info_2 = "row"

      call read_var("matrix", info, &
         description="Matrix info", &
         expected=(/"full          ", &
                    "squared       ", &
                    "highest square"/))

      select case(trim(info))
      case("full")
         call write_output("duschinsky matrix", J_matr)
      case("squared")

         J_matr = J_matr*J_matr
         call write_output("duschinsky matrix squared", J_matr)

      case("highest square")

         allocate(max_element(n))
         call read_var("max square", info_2, &
         description="Matrix info", &
         expected=(/"row   ", &
                    "column"/))

         if(trim(info_2) == "row")then
            max_element = maxval(J_matr,1)
         else if(trim(info_2)  == "column") then
            max_element = maxval(J_matr,2)
         end if

         call write_output("Max squared", max_element, extra=info_2)
      end select

   end subroutine

   subroutine output_K_shift(K_shift, n_atoms)
!!
!!    Output shift vector
!!    Written by Marco Scavino, July 2019
!!
      implicit none

      real(dp), intent(in) :: K_shift(:)
      integer, intent(in) :: n_atoms
      
      character(len=30) :: info
      integer :: i, n
      real(dp) :: max_displ, average_displ, sigma, tmp

      info  = "full"

      n = 3*n_atoms

      call read_var("shift vector", info, &
         description="Matrix info", &
         expected=(/"full            ", &
                    "max displacement"/))

      select case(trim(info))
      case("full")
         call write_output("shift vector", K_shift)
      case("max displacement")
         
         max_displ     = maxval(K_shift)
         average_displ = sum(K_shift)/n

         sigma = zero
         do i=1, n
            tmp = (K_shift(i)-average_displ)
            sigma = sigma + tmp*tmp
         end do

         sigma = sqrt(sigma/n)

         call write_output("Maximum displacement", max_displ)
         call write_output("Average displacement", abs(average_displ), extra="unsigned")
         call write_output("Standard deviation", sigma)
      end select

   end subroutine

   subroutine build_L(matrix)
!!
!!    Diagonalize force constant matrix
!!    Written by Marco Scavino, July 2019
!!
      implicit none

      real(dp), intent(inout) :: matrix(:,:)

      integer :: n, info, i, j
      real(dp) :: diff
      real(dp), allocatable :: tmp_1(:), tmp_2(:,:)

      info = 0
      n = size(matrix,1)

      allocate(tmp_1(n))
      allocate(tmp_2(n,n))

      do j=1, n
         do i=1, n
            diff = abs(matrix(i,j) - matrix(j,i))

            if(diff > tolerance) stop "Non symmetric!"
         end do
      end do

      call dsyev("V", "L", n, matrix, n, tmp_1, tmp_2, n*n, info)

      if(info /= 0) then
         call output_error_msg("Diagonalization of force constant failed!")
      end if

      deallocate(tmp_1)
      deallocate(tmp_2)

      call check_orthogonal(matrix, n)

   end subroutine

   subroutine check_orthogonal(matrix, n, msg)
      
      implicit none

      integer, intent(in)  :: n
      real(dp), intent(in) :: matrix(:,:)
      character(len=*), intent(in), optional :: msg

      real(dp) :: tmp_matrix(n,n)
      integer :: i, j

      logical :: orthogonal

      orthogonal = .true.

      call dgemm("T", "N", n, n, n, one, matrix, n, matrix, n, zero, tmp_matrix, n)
   l1:do j = 1, n
         do i = 1, n
            if(i==j) then
               if(abs(tmp_matrix(i,j)-one) > tolerance) orthogonal = .false.
            else
               if(abs(tmp_matrix(i,j)) > tolerance) orthogonal = .false.
            end if

            if(.not. orthogonal) exit l1
         end do
      end do l1

      if(.not. orthogonal) then
         if(present(msg))then
            call output_error_msg(msg)
         else
            call output_error_msg("Matrix not orthogonal!")
         end if
      end if
   end subroutine
end module