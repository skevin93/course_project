module gaussian_fchk
!!
!!    Read Gaussian input
!!    Written by Marco Scavino, June 2019
!!
!!    Module for read input file information
!!    from Gaussian fchk file
!!

   use kinds
   use file_info

   implicit none

   interface read_var

      module procedure read_var_real, read_var_int, read_var_char, &
                read_array_real, read_array_int

   end interface

contains

   subroutine read_var_real(f_unit, var_title, var)

      implicit none

      integer, intent(in) :: f_unit
      character(len=*), intent(in) :: var_title

      real, intent(out) :: var

      character(len=225) :: line

      rewind(f_unit)

      line =  find_title_variable(f_unit, var_title, "R", .false.)

      read(line, *) var

   end subroutine

   subroutine read_array_real(f_unit, var_title, var)

      implicit none

      integer, intent(in) :: f_unit
      character(len=*), intent(in) :: var_title

      real, intent(out) :: var(:)
      real, allocatable :: tmp_var(:)

      character(len=225) :: line

      integer :: n, i

      rewind(f_unit)

      line =  find_title_variable(f_unit, var_title, "R", .true.)

      read(line, *) n

      allocate(tmp_var(n))

      do i=1, n, 5

         if(n-i > 5) then
            read(f_unit, *) tmp_var(i:i+4)
         else
            read(f_unit, *) tmp_var(i:n)
         end if
      end do

      var = tmp_var

      deallocate(tmp_var)

   end subroutine

   subroutine read_var_int(f_unit, var_title, var)

      implicit none

      integer, intent(in) :: f_unit
      character(len=*), intent(in) :: var_title

      integer, intent(out) :: var

      character(len=225) :: line

      rewind(f_unit)

      line =  find_title_variable(f_unit, var_title, "I", .false.)

      read(line, *) var

   end subroutine

   subroutine read_array_int(f_unit, var_title, var)

      implicit none

      integer, intent(in) :: f_unit
      character(len=*), intent(in) :: var_title

      integer :: var(:)

      integer, allocatable :: tmp_var(:)

      character(len=225) :: line

      integer :: n, i

      rewind(f_unit)

      line =  find_title_variable(f_unit, var_title, "I", .true.)

      read(line, *) n

      allocate(tmp_var(n))

      do i=1, n, 6

         if(n-i > 6) then
            read(f_unit, *) tmp_var(i:i+5)
         else
            read(f_unit, *) tmp_var(i:n)
         end if
      end do

      var = tmp_var

      deallocate(tmp_var)

   end subroutine

   subroutine read_var_char(f_unit, var_title, var)

      implicit none

      integer, intent(in) :: f_unit
      character(len=*), intent(in) :: var_title
      character(len=*), intent(out) :: var

      rewind(f_unit)

      var =  find_title_variable(f_unit, var_title, "C", .false.)

   end subroutine

   function find_title_variable(f_unit, var_title, var_type, is_array) result(var)

      implicit none

      integer, intent(in) :: f_unit
      character(len=*), intent(in) :: var_title, var_type
      logical, intent(in) :: is_array
      
      character(len=225) :: var

      integer :: i, f_error

      character(len=225) :: line

      do while (.true.)

         line = ""
         read(f_unit, '(a)', iostat=f_error) line

         if(f_error /= 0) exit

         line = adjustl(line)

         i = 0
      l1:do while(i <= len_trim(line))

            i = i + 1

            if(line(i:i) == "") then

               !check for variable title
               if(line(1:i) == var_title) then

                  line = adjustl(line(i:))
                  i = 0
                  cycle l1

               else

                  ! Sanity check for variable type
                  if(line(1:i) == var_type) then

                     line = adjustl(line(i:))
                     i = 0
                     cycle l1
                  
                  else

                     ! if requested array, check for "N="
                     if(is_array .and. line(1:i) == "N=") then
                           
                        i = i+2

                     else

                        goto 102

                     end if
                  
                     ! Finally, return the variable
                     var = adjustl(line(i:))

                     goto 100
   
                  end if

                  goto 101

               end if

            end if
         end do l1
      
      end do


      call output_error_msg("""" // trim(var_title) // """ not found!")
      ! Check if variable is empty after all the search
100   if(var == "") then

         call output_error_msg("Empty value for variable """ // trim(var_title) // """")

      end if

      return

101   call output_error_msg("""" // trim(var_type)  // """ expected variable type different!")
102   call output_error_msg("""" // trim(var_title) // """ requested array not found!")

   end function

end module