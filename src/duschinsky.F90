program duschinsky

   use input_file,     only: read_input
   use file_info,      only: open_file, input, output
   use external_files, only: read_external

   implicit none

   call open_file(output, "output.out", "write")
   call open_file(input, "input.inp", "read")

   call read_input()

   call read_external()

end program