module file_info
!!
!!    Read input
!!    Written by Marco Scavino, June 2019
!!
!!    Module for read input file information
!!

   use kinds

   implicit none

   type file
      integer           :: unit_ = -1
      character(len=30) :: name_ = ""
      character(len=30) :: type_ = ""
   end type file

   type(file) :: input  = file(11, "input.inp", "inp")
   type(file) :: output = file(12, "output.out", "out")

!
! detect if windows system and in case, set the path
! separator
!
#if _WIN32
   character(len=1), parameter :: path_sep='\'
#else
   character(len=1), parameter :: path_sep='/'
#endif

contains

   subroutine open_file(the_file, f_action, f_form)

      implicit none

      type(file), intent(inout) :: the_file
      character(len=*), intent(in) :: f_action
      character(len=*), intent(in), optional :: f_form

      character(len=30) :: form
      logical :: check_file = .false.
      integer :: f_error

      f_error = 0

      form =  "formatted"
      if(present(f_form)) form = f_form

      inquire(unit=the_file%unit_, opened=check_file)

      if (check_file) then
         call output_error_msg("Unit for file " // trim(the_file%name_) // "already opened!")
      end if

      if("read" == trim(f_action)) then

         inquire(file=the_file%name_, exist=check_file)

         if(.not. check_file) then
            call output_error_msg("Input file " // trim(the_file%name_) // " not found!")
         end if
   
      end if
   
      open(unit=the_file%unit_, file=the_file%name_, &
         access="sequential", form=trim(form), &
         action=f_action, iostat=f_error)

      if(f_error /= 0) then
         call output_error_msg("Impossible to open file """ // trim(the_file%name_) // """")
      end if
      
      rewind(the_file%unit_)
      
   end subroutine open_file

   subroutine close_file(the_file)

      implicit none

      type(file), intent(in) :: the_file
      logical :: opened

      inquire(unit=the_file%unit_, opened=opened)

      if(.not. opened) then

         call output_error_msg("File """ // trim(the_file%name_) // """ not opened!")

      end if

      close(the_file%unit_)

   end subroutine close_file

   subroutine get_filename(file_path, file_name, file_ext)

      implicit none

      character(len=*), intent(in) :: file_path
      character(len=20), intent(out) :: file_name
      character(len=10), intent(out), optional :: file_ext

      integer :: i, name_pos, ext_pos

      name_pos = 0
      ext_pos  = 0

      do i=1, len_trim(file_path)
         if(file_path(i:i) == path_sep) then
            name_pos = i
         else if(file_path(i:i) == ".") then
            ext_pos = i
         end if
      end do

      if(present(file_ext)) then

         file_ext = ""
         if(ext_pos == 0) then
            ext_pos = len_trim(file_path)
         end if

         file_ext = trim(file_path(ext_pos+1:))

      end if

      file_name = file_path(name_pos+1:ext_pos)

   end subroutine

   subroutine output_error_msg(message, required, line)

      implicit none

      character(len=*), intent(in) :: message
      character(len=*), intent(in), optional :: required(:)
      integer, intent(in), optional :: line

      integer :: i

      write(*, '(1x, "Error:",1x,a)', advance='no') trim(message)

      if(present(line)) then
         write(*, '(1x,"on line",1x,i0)', advance='no') line
      end if

      write(*,*)

      if(present(required)) then
         
         write(*, '(1x, a, a)', advance='no') "Accepted values => [", trim(required(1))
         
         do i=2, size(required)
            write(*, '(",",1x,a)', advance="no") trim(required(i))
         end do

         write(*,'(a)') "]"

      end if
      stop 1

   end subroutine

   subroutine find_variable(the_file, var_title, required, var, found, line_num)

      implicit none

      type(file), intent(in) :: the_file
      character(len=*), intent(in) :: var_title
      logical, intent(in) :: required

      character(len=*), intent(inout), optional :: var
      logical, intent(out), optional :: found
      integer, intent(out), optional :: line_num

      logical :: var_found
      integer :: i, f_error, num

      character(len=225) :: line

      rewind(the_file%unit_)

      var_found = .false.
      f_error = 0
      num = 0

      ! loop over the file until "var_title" is found

      do while (.not. var_found)

         num = num + 1

         line = ""
         read(the_file%unit_, '(a)', iostat=f_error) line

         if(f_error /= 0) exit
         if(trim(line) == "") cycle

         line = adjustl(line)

         do i=1, len_trim(line)

            if(line(i:i) == "") then

               !check for variable title
               if(trim(line(1:i-1)) == trim(var_title)) then

                  var_found = .true.
               
               end if

               if(var_found) then

                  ! If requested value, then 
                  if(present(var)) then
                  
                     ! Finally, return the variable
                     if(trim(line(i:)) /= "") then
                        
                        var = trim(adjustl(line(i:)))
                     end if

                  end if
                  
                  exit

               end if

            end if

         end do
      
      end do

      if(required .and. .not. var_found) then
         call output_error_msg("Required variable """ // trim(var_title) // &
            """ not found!")
      end if

      if(present(var)) then
         if(trim(var) == "") then
            call output_error_msg("Variable """ // trim(var_title) // """ empty", line=num)
         end if
      end if

      if(present(found)) found = var_found
      if(present(line_num)) line_num = num

      return

   end subroutine find_variable

end module