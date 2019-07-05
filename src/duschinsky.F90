program duschinsky

   use input_file,     only: read_argument, read_var
   ! use file_info,      only: open_file, input, output
   use external_files, only: get_external
   use eckart_rotation,  only: first_rotation
   use superposition,    only: superposition_second
   use duschinsky_tools, only: duschinsky_matrix

   use system_info

   implicit none

   character(len=30) :: program_name

   call read_argument()

   program_name = "gaussian"

   call read_var("program", program_name, &
      description="Select the program used for input file", &
      expected=(/"gaussian", &
                 "dalton  "/))

   call get_external()

   call first_rotation(atoms, coord_1, n_atoms)

   call superposition_second(atoms, weight, coord_1, coord_2, n_atoms)

   call duschinsky_matrix(weight, coord_1, coord_2, force_constant_1, force_constant_2, n_atoms)

end program duschinsky