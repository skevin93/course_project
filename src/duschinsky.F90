program duschinsky

   use input_file,       only: read_argument, read_var
   use external_files,   only: get_external
   use file_info
   use output_module
   use eckart,           only: eckart_rotation
   use superposition,    only: superposition_second
   use duschinsky_tools, only: duschinsky_matrix
   use chemistry,        only: center_of_mass

   use system_info

   implicit none

   character(len=30) :: program_name, orientation
   type(file) :: f1, f2

   ! Read from command line info. Depending on argument passed,
   ! set input and output files
   call read_argument()

   program_name = "gaussian"

   ! Read program used to generate external files
   call read_var("program", program_name, &
      description="Select the program used for input file", &
      expected=(/"gaussian", &
                 "dalton  "/))
   ! Read external files informations
   call get_external()

   f1 = file(20, "old_1.xyz", "xyz")
   f2 = file(21, "old_2.xyz", "xyz")

   call open_File(f1, "write")
   call open_File(f2, "write")

   call output_xyz_file(f1, atoms, coord_1, n_atoms, comment="r_0")
   call output_xyz_file(f1, atoms, coord_2, n_atoms, comment="r_1")

   call close_file(f1)
   call close_file(f2)


   !Move the two system to center of mass
   call center_of_mass(atomic_masses, coord_1, n_atoms)
   call center_of_mass(atomic_masses, coord_2, n_atoms)

   f1 = file(20, "com_1.xyz", "xyz")
   f2 = file(21, "com_2.xyz", "xyz")

   call open_File(f1, "write")
   call open_File(f2, "write")

   call output_xyz_file(f1, atoms, coord_1, n_atoms, comment="r_0-R_0")
   call output_xyz_file(f2, atoms, coord_2, n_atoms, comment="r_1-R_0")

   call close_file(f1)
   call close_file(f2)
   !Apply rotation to 1-st system
   orientation = "eckart"

   call read_var("orientation", orientation, &
      description="Orientation of the first atom", &
      expected=(/"eckart  ",&
                 "original"/))

   if(trim(orientation) == "eckart") then
      
      print*, orientation
      call eckart_rotation(atoms, atomic_masses, coord_1, n_atoms)

   end if
   f1 = file(20, "new_1.xyz", "xyz")
   call open_File(f1, "write")

   call output_xyz_file(f1, atoms, coord_1, n_atoms, comment="T*r_0")

   call close_file(f1)

   !Superpose the second system on the first
   call superposition_second(atomic_masses, coord_1, coord_2, n_atoms)

   !Compute the duschinsky matrix and print to output
   call duschinsky_matrix(atomic_masses, coord_1, coord_2, force_constant_1, force_constant_2, n_atoms)


   f1 = file(20, "new_2.xyz", "xyz")
   call open_File(f1, "write")

   call output_xyz_file(f1, atoms, coord_1, n_atoms, comment="D*T*r_0")

   call close_file(f1)
end program duschinsky