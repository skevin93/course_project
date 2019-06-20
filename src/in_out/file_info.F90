module file_info
!!
!!    Read input
!!    Written by Marco Scavino, June 2019
!!
!!    Module for read input file information
!!

   use kinds

   implicit none

   integer :: input = 10, output = 11

   interface init_file

      module procedure init_formatted_file

   end interface

contains

   subroutine init_formatted_file(f_unit, f_name, f_action)

      implicit none

      integer, intent(out) :: f_unit
      character(len=*), intent(in) :: f_name, f_action

      logical :: check_file = .false.
      integer :: f_error

      f_error = 0

      inquire(unit=f_unit, opened=check_file)

      if (check_file) then
         call output_error_msg("Unit for file " // trim(f_name) // "alrteady opened!")
      end if

      if("read" == trim(f_action)) then

         inquire(file=f_name, exist=check_file)

         if(.not. check_file) then
            call output_error_msg("Input file not found!")
         end if
   
      end if
   
      open(unit=f_unit, file=f_name, access="sequential", form="formatted", action=f_action, iostat=f_error)
      
      if(f_error /= 0) then
         call output_error_msg("Impossible to open file """ // trim(f_name) // """")
      end if
      
      rewind(f_unit)
      
   end subroutine init_formatted_file

   subroutine write_to_output()

   end subroutine

   subroutine output_error_msg(message, required)

      implicit none

      character(len=*), intent(in) :: message
      character(len=*), intent(in), optional :: required(:)

      integer :: i

      write(output, '(1x, "Error: ",1x,a)') message

      if(present(required)) then
         
         write(output, '(a)', advance='no') "Accepted value => [", trim(required(1))
         
         do i=2, size(required)
            write(output, '(",",1x,a)', advance="no") trim(required(i))
         end do
         write(output,*) "]"
      end if
      stop 1

   end subroutine

end module