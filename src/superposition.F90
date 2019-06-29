module superposition

   use kinds
   use parameters
   use input_file
   ! use system_info

   real(dp), parameter :: tolerance = 1.0E-6_dp

contains

   subroutine superposition_second(masses, coord_1, coord_2, n_atoms)

      implicit none

      real(dp), intent(in), dimension(:) :: masses
      real(dp), intent(in), target, dimension(:,:) :: coord_1, coord_2
      integer, intent(in) :: n_atoms

      integer, allocatable :: atoms_indexes(:)
      real(dp), pointer    :: r_0(:,:), r_1(:,:)
      character(len=30) :: superposition, maximize
      real(dp), allocatable :: weight(:)
      real(dp) :: total_weight
      integer :: i, num_points

      superposition = "masses"
      maximize      = "all"

      call read_var("superposition", superposition, &
      description="Superposition of the second molecule", &
      expected=(/"none  ", &
                 "masses"/))

      call read_var("maximize", maximize, &
         description="Maximize overlap over the following atoms", &
         expected=(/"all  ", &
                  "three"/))
      
      if(maximize == "three")then

         num_points = 3

         allocate(atoms_indexes(3))
         allocate(r_0(3,3))
         allocate(r_1(3,3))

         call read_var("atoms", atoms_indexes, &
            description="Insert the three atom index to superpose", &
            required=.true.)
      
         do i=1, 3
            r_0(:,i) = coord_1(:,atoms_indexes(i))
            r_1(:,i) = coord_2(:,atoms_indexes(i))
         end do

         deallocate(atoms_indexes)

      else

         num_points = n_atoms

         r_0 => coord_1
         r_1 => coord_2

      end if


      allocate(weight(num_points))

      if(trim(superposition) == "masses") then

         weight = masses
         total_weight = sum(weight)
         weight = weight/total_weight

      else

         weight = one/n_atoms

      end if

   end subroutine

   subroutine calc_q(w, r_0, r_1, q, n)

      implicit none

      integer, intent(in) :: n
      real(dp), intent(in) :: w(:)
      real(dp), intent(in) :: r_0(:,:), r_1(:,:)
      real(dp), intent(out), dimension(4) :: q

      real(dp) :: M(4,4)
      integer :: i, info
      real(dp) :: coord_sq, xx, yy, zz
      real(dp), dimension(4) :: tmp_1, tmp_2

      M = zero

      do i=1, n

         coord_sq = r_0(1,i)*r_0(1,i) + r_0(2,i)*r_0(2,i) + r_0(3,i)*r_0(3,i)
         coord_sq = coord_sq + r_1(1,i)*r_1(1,i) + r_1(2,i)*r_1(2,i) + r_1(3,i)*r_1(3,i)

         xx = two*r_0(1,i)*r_1(1,i)
         yy = two*r_0(2,i)*r_1(2,i)
         zz = two*r_0(3,i)*r_1(3,i)

         M(1,1) = M(1,1) + w(i)*(coord_sq-xx-yy-zz)
         M(2,2) = M(2,2) + w(i)*(coord_sq-xx+yy+zz)
         M(3,3) = M(3,3) + w(i)*(coord_sq+xx-yy+zz)
         M(4,4) = M(4,4) + w(i)*(coord_sq-xx+yy-zz)

         M(1,2) = M(1,2) + w(i)*two*(r_0(2,i)*r_1(3,i) + r_0(3,i)*r_1(2,i))
         M(1,3) = M(1,3) + w(i)*two*(r_0(3,i)*r_1(1,i) - r_0(1,i)*r_1(3,i))
         M(1,4) = M(1,4) + w(i)*two*(r_0(1,i)*r_1(2,i) + r_0(2,i)*r_1(1,i))
         M(2,3) = M(2,3) - w(i)*two*(r_0(1,i)*r_1(1,i) + r_0(1,i)*r_1(1,i))
         M(2,4) = M(2,4) - w(i)*two*(r_0(1,i)*r_1(3,i) + r_0(3,i)*r_1(1,i))
         M(3,4) = M(3,4) - w(i)*two*(r_0(2,i)*r_1(3,i) + r_0(3,i)*r_1(2,i))

      end do

      M(2,1) = M(1,2)
      M(3,1) = M(1,3)
      M(4,1) = M(1,4)
      M(3,2) = M(2,3)
      M(4,2) = M(2,4)
      M(4,3) = M(3,4)

      if (abs(M(1,2)) < tolerance .and. &
          abs(M(1,3)) < tolerance .and. &
          abs(M(2,3)) < tolerance .and. &
          abs(M(2,4)) < tolerance .and. &
          abs(M(3,4)) < tolerance) then

         q = one

      else
      
         call dsyev('V', 'L', 4, M, 4, tmp_1, tmp_2, 16, info)

         if (info /= 0) then
         
            call output_error_msg('Diagonalizing M matrix for superposition failed')
         
         end if

         q = M(:,1)

      end if

   end subroutine

   subroutine build_rotation_matrix(q, D)

      implicit none

      real(dp), intent(in)  :: q(0:3)
      real(dp), intent(out) :: D(3,3)

      real(dp) :: q_sq(0:3)

      q_sq(0) = q(0)*q(0)
      q_sq(1) = q(1)*q(1)
      q_sq(2) = q(2)*q(2)
      q_sq(3) = q(3)*q(3)

      D(1,1) = q_sq(0) + q_sq(1) - q_sq(2) - q_sq(3)
      D(2,2) = q_sq(0) + q_sq(2) - q_sq(1) - q_sq(3)
      D(3,3) = q_sq(0) + q_sq(3) - q_sq(1) - q_sq(2)

      D(1,2) = two*(q(1)*q(2) - q(0)*q(3))
      D(1,3) = two*(q(0)*q(2) + q(1)*q(3))
      D(2,1) = two*(q(0)*q(3) + q(1)*q(2))
      D(2,3) = two*(q(2)*q(3) - q(0)*q(1))
      D(3,1) = two*(q(1)*q(3) - q(0)*q(2))
      D(3,2) = two*(q(0)*q(1) + q(2)*q(3))

   end subroutine
end module superposition