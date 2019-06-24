module array_info

   use kinds

   type :: info
      character(len=30) :: method
      character(len=30) :: basis
   end type

   integer, allocatable :: atoms(:)
   type(info) :: system_1, system_2

   real(dp), allocatable :: coord_1(:,:)
   real(dp), allocatable :: coord_2(:,:)

   real(dp), allocatable, dimension(:) :: force_constant_1
   real(dp), allocatable, dimension(:) :: force_constant_2

   real(dp), allocatable, dimension(:) :: shift_vector
   real(dp), allocatable, dimension(:) :: matrix_duschinsky

end module