module superposition
!!
!!    Superposition module
!!    Written by Marco Scavino, June 2019
!!
!!    Superposition is made usign the quaternions.
!!
   use kinds
   use parameters
   use input_file
   use output_module
   use chemistry
   ! use system_info

   real(dp), parameter :: tolerance = 1.0E-6_dp

contains

   subroutine superposition_second(atoms, weight, coord_1, coord_2, n_atoms)
!!
!!    Superposition of second system
!!    Written by Marco Scavino, June 2019
!!
!!    Superpose the second system over the first one usign quaternions
!!
      implicit none

      integer, intent(in) :: n_atoms
      integer, intent(in), dimension(n_atoms) :: atoms
      real(dp), intent(in), target, dimension(n_atoms) :: weight
      real(dp), intent(in), target, dimension(3,n_atoms) :: coord_1
      real(dp), intent(inout), target, dimension(3,n_atoms) :: coord_2

      integer, allocatable :: atoms_indexes(:)
      real(dp), pointer    :: r_0(:,:), r_1(:,:)
      character(len=30) :: superposition, maximize
      real(dp), allocatable :: curr_weight(:), result(:,:)
      real(dp) :: total_weight, q(4), D(3,3)
      integer :: num_atoms
      
      type(file) :: f1, f2

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

         num_atoms = 3

         allocate(curr_weight(num_atoms))
         allocate(atoms_indexes(num_atoms))

         allocate(r_0(3,num_atoms))
         allocate(r_1(3,num_atoms))

         call read_var("atoms", atoms_indexes, &
            description="Insert the three atom index to superpose", &
            required=.true.)

         
         r_0 = coord_1(:,atoms_indexes)
         r_1 = coord_1(:,atoms_indexes)
         curr_weight = curr_weight(atoms_indexes)

         deallocate(atoms_indexes)

      else

         allocate(curr_weight(n_atoms))

         curr_weight = weight
         r_0 => coord_1
         r_1 => coord_2

      end if

      ! If "masses" is setup for superposition, use atomic masses, else use the same weights for
      !  all the atoms. The weights are normalized to assure that Σ_α w_α = 1
      if(trim(superposition) == "masses") then

         total_weight = sum(curr_weight)
         curr_weight = curr_weight/total_weight

      else

         curr_weight = one/n_atoms

      end if

      call calc_q(curr_weight, r_0, r_1, q, num_atoms)

      call build_rotation_matrix(q, D)

      allocate(result(3,n_atoms))
      call dgemm("N", "N", 3, n_atoms, 3, one, D, 3, coord_1, 3, zero, result, 3)

      f1 = file(21, "old_2.xyz", "xyz")
      f2 = file(22, "new_2.xyz", "xyz")

      call open_file(f1, "write")
      call open_file(f2, "write")

      call output_xyz_file(f1, atoms, coord_2, n_atoms, comment="r_1")
      call output_xyz_file(f2, atoms, result, n_atoms, comment="D*r_0")

      call close_file(f1)
      call close_file(f2)

      coord_2 = result

      deallocate(result)

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
      real(dp) :: tmp_1(4), tmp_2(4,4)

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
         M(4,4) = M(4,4) + w(i)*(coord_sq+xx+yy-zz)

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
         
            
            call output_error_msg('Diagonalizing M matrix for superposition failed', error=info)
         
         end if

         q = M(:,1)

      end if

   end subroutine

   subroutine build_rotation_matrix(q, D_check)

      implicit none

      real(dp), intent(in)  :: q(0:3)
      real(dp), intent(out) :: D_check(3,3)

      real(dp) :: q_sq(0:3), atan30, atan12, alpha, beta, gamma
      real(dp), dimension(3,3) :: D, Rz1, Ry, Rz2

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

      !print*, "q·q = ", q(0)*q(0) + q(1)*q(1) + q(2)*q(2) + q(3)*q(3)

      atan30 = atan(q(3)/q(0))
      atan12 = atan(q(1)/q(2))

      alpha = atan30-atan12
      gamma = atan30+atan12
      beta  = q(0)/(cos((gamma+alpha)*half))
      !print*, "α = ", alpha/pi*180
      !print*, "β = ", beta/pi*180
      !print*, "γ = ", gamma/pi*180

      Rz2(1,:) = (/cos(gamma), -sin(gamma), zero/)
      Rz2(2,:) = (/sin(gamma),  cos(gamma), zero/)
      Rz2(3,:) = (/      zero,        zero,  one/)

      Ry(1,:) = (/ cos(beta),  zero, sin(beta)/)
      Ry(2,:) = (/      zero,   one,      zero/)
      Ry(3,:) = (/-sin(beta),  zero, cos(beta)/)

      Rz1(1,:) = (/cos(alpha), -sin(alpha), zero/)
      Rz1(2,:) = (/sin(alpha),  cos(alpha), zero/)
      Rz1(3,:) = (/      zero,        zero,  one/)


      D_check = matmul(Ry, Rz1)

      D_check = matmul(Rz2, D_check)

      ! print'(/1x, "det(",a,"): ", f0.6)', "D", deter(D)
      !print*, "D matrix:"
      ! print'(3F12.6)', (D(i,:), i=1, 3)

      call inverse(D, Ry)
      Ry = Ry - transpose(D)
      !print*, "D-1 matrix:"
      ! print'(3F12.6)', (Ry(i,:), i=1, 3)


      ! print'(/1x, "det(",a,"): ", f0.6)', "D_check", deter(D_check)
      !print*, "D matrix:"
      ! print'(3F12.6)', (D_check(i,:), i=1, 3)

      call inverse(D_check, Ry)
      ! print'(/1x, a)', "D-1 matrix:"
      ! print'(3F12.6)', (Ry(i,:), i=1, 3

      ! print'(/,2a36)', "r_0", "r_1"
      ! print'(6F12.6)', (vec(:,i), r_1(:,i), i=1, size(vec,2))
   end subroutine

   pure function deter(mat)

      implicit none

      real(dp), intent(in) :: mat(3,3)

      real(dp) :: deter
      
      deter = mat(1,1)*mat(2,2)*mat(3,3)
      deter = deter + mat(1,2)*mat(2,3)*mat(3,1)
      deter = deter + mat(1,3)*mat(3,2)*mat(2,1)
      deter = deter - mat(1,3)*mat(2,2)*mat(3,1)
      deter = deter - mat(1,2)*mat(2,1)*mat(3,3)
      deter = deter - mat(2,3)*mat(3,2)*mat(1,1)

   end function

   pure subroutine inverse(mat, mat_res)

      implicit none

      real(dp), intent(in) :: mat(3,3)
      real(dp), intent(out) :: mat_res(3,3)

      real(dp) :: deter_mat

      deter_mat = deter(mat)

      if(deter_mat == zero) return
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
   end subroutine
end module superposition