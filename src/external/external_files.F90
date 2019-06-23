module external_files

   use input_file
   use gaussian_input, only: read_gaussian_var

   implicit none

contains


   subroutine read_external()

      implicit none

      character(len=10) :: file_type
      character(len=30) :: file_1, file_2, program_name


      program_name = "gaussian"
      file_type = "fchk"
      file_1    = ""
      file_2    = ""

      call read_var("program", program_name, &
         description="Select the program used for input file", &
         expected=(/"gaussian", & !possible keys for this keyword
                    "dalton  "/))

      call read_var("type file", file_type, &
         description="Select the file type", &
         expected=(/"fchk"/))

      call check_file("file1", file_1, file_type)

      call check_file("file2", file_2, file_type)

      ! call open_file(f1, unit)

   end subroutine

   subroutine check_file(var_title, file_name, file_type)

      implicit none

      character(len=*), intent(in) :: var_title
      character(len=*), intent(inout) :: file_name, file_type

      character(len=100) :: tmp_name
      character(len=10)  :: tmp_ext
      logical :: found

      call read_var(var_title, file_name, &
         description= "Insert first file name", &
         required=.true.)

      inquire(file=file_name, exist=found)
      if(.not. found) then
         call output_error_msg("File """ // trim(file_name) // """ not found!")
      end if

      call get_filename(file_name, tmp_name, tmp_ext)
      if(trim(tmp_ext) /= trim(file_type)) then
         call output_error_msg("File format """ // trim(tmp_ext) // &
            """ different from expected type """ // trim(file_type) // """!")
      end if
   end subroutine

end module external_files