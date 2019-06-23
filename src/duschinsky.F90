program duschinsky

   use input_file,     only: read_input
   use file_info,      only: open_file, input, output
   use external_files

   use array_info,   only: force_constant_1

   implicit none

   call open_file(output, "output.out", "write")
   call open_file(input, "input.inp", "read")

   call read_input()

   call open_external()

   call sanity_check_external()

   call read_external()
   call close_external()

   print*, force_constant_1

end program