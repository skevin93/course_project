module external_files

   use input_file
   use gaussian_input, only: gaussian_var, gaussian_section, gaussian_array_num
   use array_info

   implicit none

   integer :: external_1 = 14
   integer :: external_2 = 15

contains

   subroutine open_external()

      implicit none

      character(len=10) :: file_type
      character(len=30) :: file_1, file_2, program_name

      integer ::  n


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

      call open_file(external_1, file_1, "read")
      call open_file(external_2, file_2, "read")

   end subroutine

   subroutine sanity_check_external()

      implicit none

      real, allocatable, dimension(:) :: atomic_1, atomic_2

      integer :: n1, n2

      call gaussian_array_num(external_1, "Atomic numbers", "I", n1)

      allocate(atomic_1(n1))
      call gaussian_var(external_1, atomic_1)

      call gaussian_array_num(external_2, "Atomic numbers", "I", n2)

      if(n1 /= n2) then
         call output_error_msg("Different number of atoms!")
      end if

      allocate(atomic_2(n2))
      call gaussian_var(external_2, atomic_2)

      if( .not. all(atomic_1 == atomic_2)) then

      end if

   end subroutine sanity_check_external

   subroutine read_external()

      implicit none

      integer :: n

      call gaussian_array_num(external_1, "Cartesian force constants", "R", n)

      allocate(force_constant_1(n))

      call gaussian_var(external_1, force_constant_1)

   end subroutine read_external

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
   end subroutine check_file

   subroutine close_external()

      implicit none

      close(external_1)
      close(external_2)

   end subroutine close_external

end module external_files