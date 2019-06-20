module input_file

   use kinds
   use file_info

   implicit none

   interface read_var

      module procedure read_var_flag, read_var_char

   end interface

contains

   subroutine read_input()

      implicit none

      logical :: interactive
      character(len=30) :: program_name

      call read_var("interactive", interactive)

      call read_var("program", program_name, (/"gaussian", "dalton  "/))

   end subroutine

   subroutine read_var_flag(var_title, var)

      implicit none

      character(len=*), intent(in) :: var_title
      logical, intent(inout) :: var

      call find_variable(input, var_title, found=var)

      return

   end subroutine

   subroutine read_var_char(var_title, var, expected)

      implicit none


      character(len=*), intent(in) :: var_title, expected(:)
      character(len=*), intent(out) :: var

      integer :: i

      
      call find_variable(input, var_title, var=var)

      do i=1, size(expected)
         if(.not. var == trim(expected(i))) then

            call output_error_msg("Variable value not correct!", expected)
         end if
      end do
   
   end subroutine

   subroutine find_variable(f_unit, var_title, var, found)

      implicit none

      integer, intent(in) :: f_unit
      character(len=*), intent(in) :: var_title
      character(len=*), intent(inout), optional :: var
      logical, intent(out), optional :: found

      logical :: var_found
      integer :: i, f_error

      character(len=225) :: line

      rewind(f_unit)

      var_found = .false.
      f_error = 0

      do while (.true.)

         line = ""
         read(f_unit, '(a)', iostat=f_error) line

         if(f_error /= 0) exit

         line = adjustl(line)

         i = 0

      l1:do while(i <= len_trim(line))

            i = i+1

            if(line(i:i) == "") then

               !check for variable title
               if(line(1:i) == var_title) then

                  line = adjustl(line(i:))
                  i = 0
                  cycle l1

               else

                  ! If requested value, then 
                  if(present(var)) then
                  
                     ! Finally, return the variable
                     var = adjustl(line(i:))
   
                  end if

                  var_found = .true.

                  goto 100

               end if

            end if

         end do l1
      
      end do


      call output_error_msg("""" // trim(var_title) // """ not found!")
      return


      ! Check if variable is empty after all the search
100   if(var == "") then

         call output_error_msg("Empty value for variable """ // trim(var_title) // """")

      end if

      if(present(found)) found = var_found

      return

   end subroutine find_variable

end module input_file