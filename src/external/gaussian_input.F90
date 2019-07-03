module gaussian_input
!!
!!    Read Gaussian input
!!    Written by Marco Scavino, June 2019
!!
!!    Module for read input file information from Gaussian formatted file.
!!    Current procedures:
!!
!!       *`gaussian_var`
!!       *`gaussian_array_num`
!!       *`gaussian_section`
!!
   use kinds
   use file_info
   use string_tools

   implicit none

   private

   interface gaussian_var

      module procedure read_var_real, read_var_int, read_var_char, read_var_logical, &
                       read_array_real, read_array_int, read_array_char, read_array_logical, &
                       read_array_2_real

   end interface

   character(len=*), parameter ::  &
      int_format     = "(I12)",    &
      real_format    = "(E22.15)", &
      char_format    = "(A12)",    &
      logical_format = "(L1)",     &
      int_array_format     = "(6I12)",   &
      real_array_format    = "(5E16.8)", &
      char_array_format    = "(5A12)",   &
      logical_array_format = "(72L1)"

   character(len=1), parameter :: allowed_type(5) = (/"R", "I", "C", "H", "L"/)

   public :: gaussian_var, gaussian_array_num, gaussian_section

contains

   subroutine read_var_real(the_file, var_title, var)
!!
!!
!!    Read real variable
!!    Written by Marco Scavino, June 2019
!!
!!    Search for "var_title" in file "the_file" and
!!    return it as a real "var"
!!
      implicit none

      type(file), intent(in) :: the_file
      character(len=*), intent(in) :: var_title

      real(dp), intent(out) :: var

      character(len=30) :: line

      line =  find_title_variable(the_file, var_title, "R", .false.)

      read(line, real_format) var

   end subroutine read_var_real

   subroutine read_var_int(the_file, var_title, var)
!!
!!
!!    Read int variable
!!    Written by Marco Scavino, June 2019
!!
!!    Search for "var_title" in file "the_file" and
!!    return it as a integer "var"
!!
      implicit none

      type(file), intent(in) :: the_file
      character(len=*), intent(in) :: var_title

      integer, intent(out) :: var

      character(len=30) :: line

      line =  find_title_variable(the_file, var_title, "I", .false.)

      read(line, int_format) var

   end subroutine read_var_int

   subroutine read_var_char(the_file, var_title, var)
!!
!!    Read char variable
!!    Written by Marco Scavino, June 2019
!!
!!    Search for "var_title" in file "the_file"
!!    and return it as a char "var
!!
      implicit none

      type(file), intent(in) :: the_file
      character(len=*), intent(in) :: var_title
      character(len=12), intent(out) :: var

      character(len=30) :: line

      line =  find_title_variable(the_file, var_title, "C", .false.)

      read(line, char_format) var

   end subroutine read_var_char
   
   subroutine read_var_logical(the_file, var_title, var)
!!
!!    Read logical variable
!!    Written by Marco Scavino, June 2019
!!
!!    Search for "var_title" in file "the_file"
!!    and return it as a logical "var
!!
      implicit none

      type(file), intent(in) :: the_file
      character(len=*), intent(in) :: var_title
      logical, intent(out) :: var

      character(len=30) :: line

      line =  find_title_variable(the_file, var_title, "C", .false.)

      read(line, logical_format) var

   end subroutine read_var_logical

   subroutine gaussian_array_num(the_file, var_title, var_type, var)
!!
!!    Read array dimension
!!    Written by Marco Scavino, June 2019
!!
!!    Search for "var_title" in file "the_file" and return the array
!!    dimension in "var". "var_type" is necessary for 
!!
      implicit none

      type(file), intent(in) :: the_file
      character(len=*), intent(in) :: var_title
      character(len=1), intent(in) :: var_type

      integer, intent(out) :: var

      character(len=30) :: line

      call check_type(var_type)

      line =  find_title_variable(the_file, var_title, var_type, .true.)

      read(line, int_format) var

   end subroutine gaussian_array_num

   subroutine read_array_real(the_file, var)
!!
!!    Read real array
!!    Written by Marco Scavino, June 2019
!!
!!    Read "var" of reals from "the_file"
!!
      implicit none

      type(file), intent(in) :: the_file
      real(dp), intent(inout) :: var(:)

      read(the_file%unit_, real_array_format) var

   end subroutine read_array_real

   subroutine read_array_int(the_file, var)
!!
!!    Read int array
!!    Written by Marco Scavino, June 2019
!!
!!    Read "var" of integers from "the_file"
!!
      implicit none

      type(file), intent(in) :: the_file
      integer, intent(inout) :: var(:)

      read(the_file%unit_, int_array_format) var

   end subroutine read_array_int

   subroutine read_array_char(the_file, var)
!!
!!    Read char array
!!    Written by Marco Scavino, June 2019
!!
!!    Read "var" of characters from "the_file"
!!
      implicit none

      type(file), intent(in) :: the_file
      character(len=12), intent(inout) :: var(:)

      read(the_file%unit_, char_array_format) var

   end subroutine read_array_char

   subroutine read_array_logical(the_file, var)
!!
!!    Read logical array
!!    Written by Marco Scavino, June 2019
!!
!!    Read "var" of logicals from "the_file"
!!
      implicit none

      type(file), intent(in) :: the_file
      logical, intent(inout) :: var(:)

      read(the_file%unit_, logical_array_format) var

   end subroutine read_array_logical

   subroutine read_array_2_real(the_file, var, n_array)
      implicit none

      type(file), intent(in) :: the_file

      real(dp), intent(inout) :: var(:,:)
      integer, intent(in) :: n_array

      integer :: i, j, n, offset
      real(dp) :: tmp_array(n_array)

      read(the_file%unit_, real_array_format) tmp_array

      i = 0
      j = 0
      offset = 0

      n = size(var, 1)

      do while(offset < n_array)

         i = i+1
         do j=1, i
            offset = int(i*(i-1)/2) + j
            if(i == j) then
               var(i,i) = tmp_array(offset)
            else
               var(i,j) = tmp_array(offset)
               var(j,i) = tmp_array(offset)
            end if
         end do
      end do

   end subroutine

   subroutine gaussian_section(the_file, section_title, section_method, section_basis)
!!
!!    Find section
!!    Written by Marco Scavino, June 2019
!!
!!    Search for "section_title" in file "the_file"
!!    and return the method and the basis of that section
!!
      implicit none

      type(file), intent(in) :: the_file
      character(len=*), intent(in) :: section_title
      character(len=30), intent(out) :: section_method, section_basis
      character(len=50) :: tmp_basis

      character(len=10) :: curr_title

      integer :: f_error

      rewind(the_file%unit_)
      do while (.true.)

         read(the_file%unit_, '(a10,a30,a)', iostat=f_error) curr_title, section_method, tmp_basis

         if(trim(uppercase(section_title)) == trim(uppercase(curr_title))) then
            
            tmp_basis = adjustl(tmp_basis)
            read(tmp_basis, '(a30)') section_basis
            return

         end if

         if(f_error /= 0) exit
      
      end do
   
      call output_error_msg("""" // trim(section_title) // """ not found!")

   end subroutine gaussian_section

   function find_title_variable(the_file, var_title, var_type, is_array) result(var)
!!
!!    Find variable
!!    Written by Marco Scavino, June 2019
!!
!!    Search for "var_title" in file "the_file" and return it as string "var".
!!    This string will be converted in the proper type in the upper subroutine.
!!
!!    Two sanity check are performed:
!!
!!       * "var_type" is checked, it have to match the read value
!!       * "is_array" check if current line is the array header
!!
      implicit none

      type(file), intent(in) :: the_file
      character(len=*), intent(in) :: var_title, var_type
      logical, intent(in) :: is_array
      
      character(len=30) :: var

      integer :: f_error
      character(len=40) :: curr_title
      character(len=1)  :: curr_type
      character(len=30) :: line, array_num
      character(len=2)  :: check_array

      rewind(the_file%unit_)
      do while (.true.)

         line = ""
         read(the_file%unit_, '(a40,3x,a1,3x,a)', iostat=f_error) curr_title, curr_type, line

         if(trim(uppercase(var_title)) == trim(uppercase(curr_title))) goto 100
          
         if(f_error /= 0) exit
      
      end do


      call output_error_msg("""" // trim(var_title) // """ not found!")

      !Sanity check type is correct
100   if(uppercase(var_type) /= uppercase(curr_type)) then
         call output_error_msg("Variable """ // trim(var_title) // &
            """ of type """ // trim(curr_type) // &
            """, while expected of type """ // trim(var_type) // """!")

      end if

      read(line, '(a2,a)') check_array, array_num

      if(is_array) then

         if(check_array == "N=") then

            var = array_num

         else

            call output_error_msg("Expected array in gaussian input, but not found!")

         end if

      else

         var = line

      end if


      return

   end function find_title_variable

   subroutine check_type(var_type)
!!
!!    Check type
!!    Written by Marco Scavino, June 2019
!!
!!    Check if type is one of the allowed.
!!
      implicit none

      character(len=1), intent(in) :: var_type

      integer :: i

      do i=1, size(allowed_type)

         if(var_type == allowed_type(i)) return

      end do

      call output_error_msg("Used type not allowed!", required=allowed_type)

   end subroutine check_type

end module gaussian_input