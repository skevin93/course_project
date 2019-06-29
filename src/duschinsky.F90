program duschinsky

   use input_file,     only: read_input, read_var
   use file_info,      only: open_file, input, output
   use external_files
   use eckart_rotation, only: first_rotation
   use superposition,   only: superposition_second

   implicit none

   character(len=30) :: program_name

   call open_file(output, "write")
   call open_file(input,  "read")

   call read_input()

   program_name = "gaussian"

   call read_var("program", program_name, &
      description="Select the program used for input file", &
      expected=(/"gaussian", &
                 "dalton  "/))

   call get_external()

   call first_rotation(atoms, coord_1, n_atoms)

   call superposition_second()

end program duschinsky