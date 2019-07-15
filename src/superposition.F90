module superposition
!!
!!    Superposition module
!!    Written by Marco Scavino, June 2019
!!
!!    Superposition subroutines thanks to the quaternions, based on article
!!    Kneller GR. “Superposition of molecular structures using quaternions”.
!!    Molecular Simulation. 1991 May 1;7(1-2):113-9.
!!
   use kinds
   use parameters
   use input_file
   use output_module
   use chemistry
   use array_tools
   ! use system_info

   real(dp), parameter :: tolerance = 1.0E-6_dp

contains

   subroutine superposition_second(weight, coord_1, coord_2, n_atoms)
!!
!!    Superposition of second system
!!    Written by Marco Scavino, June 2019
!!
!!    Superpose the second system over the first one usign quaternions
!!    To achieve this task is necessary to construct the q array, then from it
!!    build the rotation matrix D.
!!
!!    D matrix is applied to the second system "coord_2" and superposes it to the
!!    first system "coord_1".
!!
      implicit none

      integer, intent(in) :: n_atoms
      real(dp), intent(in), target, dimension(n_atoms) :: weight
      real(dp), intent(in), target, dimension(3,n_atoms) :: coord_1
      real(dp), intent(inout), target, dimension(3,n_atoms) :: coord_2

      integer, allocatable :: atoms_indexes(:)
      real(dp), pointer    :: r_0(:,:), r_1(:,:)
      character(len=30) :: superposition, maximize
      real(dp), allocatable :: curr_weight(:), result(:,:)
      real(dp) :: total_weight, q(4), D(3,3)
      integer :: num_atoms

      ! default value for superposition and number of atom to maximize
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

      ! if selected three atoms to maximize, then it requests the
      !  atom index in input
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
         r_1 = coord_2(:,atoms_indexes)
         curr_weight = weight(atoms_indexes)

         deallocate(atoms_indexes)

      else

         ! set all atoms if "all" keyword used
         num_atoms = n_atoms

         allocate(curr_weight(num_atoms))
         curr_weight = weight

         r_0 => coord_1
         r_1 => coord_2

      end if

      ! If "masses" is setup for superposition, use atomic masses, else use the same weights for
      !  all the atoms. The weights are normalized in both cases to assure that Σ_α w_α = 1
      if(trim(superposition) == "masses") then

         total_weight = sum(curr_weight)
         curr_weight(:) = curr_weight(:)/total_weight

      else

         curr_weight(:) = one/num_atoms

      end if

      call calc_q(curr_weight, r_1, r_0, q, num_atoms)

      call build_rotation_matrix(q, D)

      ! apply the rotation matrix to 
      allocate(result(3,n_atoms))
      call dgemm("N", "N", 3, n_atoms, 3, one, D, 3, coord_2, 3, zero, result, 3)

      coord_2 = result

      deallocate(result)

   end subroutine

   subroutine calc_q(w, r_0, r_1, q, n)
!!
!!    Calc quaternions array q
!!    Written by Marco Scavino, June 2019
!!
!!    First of all, this subroutine builds the matrix M_α, for each atom, using the two set of coordinates r_0 and r_1,
!!    All the matrices are summed up in the final M matrix, multiplied for the corresponding weight:
!!
!!       M = Σ_α w_α*Μ_α
!!
!!    This matrix is diagolize: the eigenvector corresponding to the lowest eigenvalue is the quaternions array q.
!!
      implicit none

      integer, intent(in) :: n
      real(dp), intent(in) :: w(:)
      real(dp), intent(in) :: r_0(:,:), r_1(:,:)
      real(dp), intent(out), dimension(4) :: q

      real(dp) :: M(4,4)
      integer :: i, info
      real(dp) :: coord_sq, xx, yy, zz, sum_q
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

         M(1,2) = M(1,2) + w(i)*two*(r_1(2,i)*r_0(3,i) - r_1(3,i)*r_0(2,i))
         M(1,3) = M(1,3) + w(i)*two*(r_1(3,i)*r_0(1,i) - r_1(1,i)*r_0(3,i))
         M(1,4) = M(1,4) + w(i)*two*(r_1(1,i)*r_0(2,i) - r_1(2,i)*r_0(1,i))
         M(2,3) = M(2,3) - w(i)*two*(r_1(1,i)*r_0(2,i) + r_1(2,i)*r_0(1,i))
         M(2,4) = M(2,4) - w(i)*two*(r_1(1,i)*r_0(3,i) + r_1(3,i)*r_0(1,i))
         M(3,4) = M(3,4) - w(i)*two*(r_1(2,i)*r_0(3,i) + r_1(3,i)*r_0(2,i))

      end do

      M(2,1) = M(1,2)
      M(3,1) = M(1,3)
      M(4,1) = M(1,4)
      M(3,2) = M(2,3)
      M(4,2) = M(2,4)
      M(4,3) = M(3,4)

      !diagonalization of M matrix
      call dsyev('V', 'L', 4, M, 4, tmp_1, tmp_2, 16, info)

      if (info /= 0) then

         call output_error_msg('Diagonalizing M matrix for superposition failed', error=info)

      end if

      q = M(:,1)

      ! check that quaternions array is normalized
      sum_q = q(1)*q(1) + q(2)*q(2) + q(3)*q(3) + q(4)*q(4)

      if(abs(sum_q-one) > tolerance) then
         call output_error_msg("q array is not normal!")
      end if

   end subroutine

   subroutine build_rotation_matrix(q, D)
!!
!!    Build rotation matrix D
!!    Written by Marco Scavino, June 2019
!!
!!    Using the quaternions array q, build the rotation matrix D
!!
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