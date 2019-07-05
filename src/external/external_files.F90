module external_files

   use file_info
   use input_file
   use gaussian_input, only: gaussian_var, gaussian_section, gaussian_array_num
   use system_info

   implicit none

   type(file), save :: external_1 = file(14, "", "")
   type(file), save :: external_2 = file(15, "", "")

contains

   subroutine get_external()

      implicit none

      character(len=10) :: file_type

      ! call read_var("program", program_name, &
      !    description="Select the program used for input file", &
      !    expected=(/"gaussian", &
      !               "dalton  "/))

      file_type = "fchk"

      call read_var("file_type", file_type, &
         description="Insert fyle type", &
         expected=(/"fchk"/))

      external_1%type_ = file_type
      external_2%type_ = file_type


      call read_var("file1", external_1%name_,  &
         description= "Insert first file name", &
         required=.true.)

      call read_var("file2", external_2%name_,  &
         description= "Insert second file name", &
         required=.true.)

      call open_external(external_1)
      call open_external(external_2)

      call sanity_check_external()

      call read_external()

      call close_file(external_1)
      call close_file(external_2)

   end subroutine

   subroutine sanity_check_external()
!!
!!    Sanity check external files
!!    Written by Marco Scavino, June 2019
!!
!!    Read the atomic number list from each input files and compare them.
!!    If the to system are different, throw an error and suspend the program.
!!
      use system_info, only: atoms, weight

      implicit none

      integer, allocatable :: tmp_atoms(:)
      real(dp), dimension(:), allocatable :: tmp_weight
      integer :: n2
      real(dp), parameter :: tolerance = 1.0E-6

      !Read array from gaussian input
      call gaussian_array_num(external_1, "Atomic numbers", "I", n_atoms)

      allocate(atoms(n_atoms))
      call gaussian_var(external_1, atoms)

      call gaussian_array_num(external_2, "Atomic numbers", "I", n2)

      if(n_atoms /= n2) then
         call output_error_msg("Different number of atoms!")
      end if

      allocate(tmp_atoms(n2))
      call gaussian_var(external_2, tmp_atoms)

      if( .not. all(atoms == tmp_atoms)) then

         call output_error_msg("Two input have different number of atoms!")

      end if
      deallocate(tmp_atoms)

      !Read array from gaussian input
      call gaussian_array_num(external_1, "Real atomic weights", "R", n_atoms)

      allocate(weight(n_atoms))
      call gaussian_var(external_1, weight)

      call gaussian_array_num(external_2, "Real atomic weights", "R", n2)

      if(n_atoms /= n2) then
         call output_error_msg("Different number of atoms!")
      end if

      allocate(tmp_weight(n2))
      call gaussian_var(external_2, tmp_weight)

      if(.not. all(abs(weight-tmp_weight)<tolerance)) then

         call output_error_msg("Two input have different weight!")

      end if

      deallocate(tmp_weight)

   end subroutine sanity_check_external

   subroutine read_external()

      implicit none

      real(dp), allocatable :: tmp_coord(:)
      integer  :: n

      call gaussian_array_num(external_1, "Cartesian force constants", "R", n)

      allocate(force_constant_1(3*n_atoms, 3*n_atoms))

      call gaussian_var(external_1, force_constant_1, n)

      call gaussian_array_num(external_1, "Current cartesian coordinates", "R", n)

      allocate(tmp_coord(n))
      allocate(coord_1(3, n_atoms))

      call gaussian_var(external_1, tmp_coord)

      coord_1 = reshape(tmp_coord, (/3, n_atoms/))


      call gaussian_array_num(external_2, "Cartesian force constants", "R", n)

      allocate(force_constant_2(3*n_atoms, 3*n_atoms))

      call gaussian_var(external_2, force_constant_2, n)

      call gaussian_array_num(external_2, "Current cartesian coordinates", "R", n)

      allocate(coord_2(3, n_atoms))

      call gaussian_var(external_2, tmp_coord)

      coord_2 = reshape(tmp_coord, (/3, n_atoms/))

      deallocate(tmp_coord)

   end subroutine read_external

   subroutine open_external(the_file)

      implicit none

      type(file), intent(inout) :: the_file

      character(len=100) :: tmp_name
      character(len=10)  :: tmp_ext
      logical :: found

      inquire(file=the_file%name_, exist=found)
      if(.not. found) then
         call output_error_msg("File """ // trim(the_file%name_) // """ not found!")
      end if

      call get_filename(the_file%name_, tmp_name, tmp_ext)
      if(trim(tmp_ext) /= trim(the_file%type_)) then
         call output_error_msg("File format """ // trim(tmp_ext) // &
            """ different from expected type """ // trim(the_file%type_) // """!")
      end if

      call open_file(the_file, "read")

   end subroutine open_external

end module external_files