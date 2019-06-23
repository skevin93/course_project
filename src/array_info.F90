module array_info


   character(len=30) :: method_1, basis_1
   real, allocatable, dimension(:) :: force_constant_1

   character(len=30) :: method_2, basis_2
   real, allocatable, dimension(:) :: force_constant_2

   real, allocatable, dimension(:) :: shift_vector
   real, allocatable, dimension(:) :: matrix_duschinsky

end module