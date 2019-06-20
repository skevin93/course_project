program duschinsky

   use input_file
   use file_info

   implicit none

   call init_file(output, "output.out", "write")
   call init_file(input, "input.inp", "read")

   call read_input()

end program