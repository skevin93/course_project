module input_file
!!
!!    Input file
!!    Written by Marco Scavino, June 2019
!!
!!    Read the input file opened in "input" unit. Core of this module is the "read_var" subroutine,
!!    that take the following arguments:
!!
!!       * `var_title`, the keyword it will search in the input file
!!       * `var`, returned variable. Can be one of the following type:
!!          + character (variable)
!!          + integer (variable or array)
!!          + real (variable)
!!          + logical (variable)
!!       * `description`, a description of the input, printed in interactive mode
!!       * `expected` (optional, only char version), a list of expected values from the variable. If user enters unexpected
!!          values, it will print an error, followed by the list
!!       * `required` (optional), a boolean value to set if a variable is required. If `.true.`, and the
!!           variable is not found, program will stop and print an error.
!!
   use kinds
   use file_info

   implicit none

   interface read_var

      module procedure read_var_real, read_var_int, read_var_char, read_var_flag, &
                       read_array_int

   end interface

   logical, private :: interactive = .false.

contains

   subroutine check_interactive()
!!
!!    Check interactive mode
!!    Writte by Marco Scavino, June 2019
!!
!!    Search the flag "interactive" in the input file. In case, switch to the
!!    interactive mode.
!!
      implicit none

      call read_var("interactive", interactive)

      if(interactive) write(*,*) "INTERACTIVE MODE"

   end subroutine check_interactive

   subroutine read_var_real(var_title, var, description, required)
!!
!!    Read real
!!    Writte by Marco Scavino, June 2019
!!
!!    Search for `var_title` in input. If found, `var` will be set
!!    to real value
!!
      implicit none

      character(len=*), intent(in) :: var_title
      real(dp), intent(inout) :: var

      character(len=*), intent(in), optional :: description
      logical, intent(in), optional :: required

      character(len=30) :: var_answer
      logical :: var_required

      var_required = .false.

      if(interactive) then

         call get_stdin_var(description, var_answer)


      else

         if(present(required))  var_required = required

         call find_variable(input, var_title, var_required, var=var_answer)

      end if

      read(var_answer,*) var

   end subroutine read_var_real

   subroutine read_var_int(var_title, var, description, required)
!!
!!    Read int
!!    Writte by Marco Scavino, June 2019
!!
!!    Search for `var_title` in input. If found, `var` will be set
!!    to int value
!!
      implicit none

      character(len=*), intent(in) :: var_title
      integer, intent(inout) :: var

      character(len=*), intent(in), optional :: description
      logical, intent(in), optional :: required

      character(len=30) :: var_answer
      logical :: var_required

      var_required = .false.

      if(interactive) then

         call get_stdin_var(description, var_answer)


      else

         if(present(required))  var_required = required

         call find_variable(input, var_title, var_required, var=var_answer)

      end if

      read(var_answer,*) var

   end subroutine read_var_int

   subroutine read_var_char(var_title, var, description, required, expected)
!!
!!    Read char
!!    Writte by Marco Scavino, June 2019
!!
!!    Search for `var_title` in input. If found, `var` will be set
!!    to char value
!!
      implicit none

      character(len=*), intent(in) :: var_title
      character(len=*), intent(inout) :: var

      character(len=*), intent(in), optional :: expected(:), description
      logical, intent(in), optional :: required

      character(len=30) :: var_answer
      logical :: var_required
      integer :: i, line

      var_required = .false.

      if(interactive) then

         call get_stdin_var(description, var_answer, expected)

         var = trim(var_answer)

      else

         if(present(required))  var_required = required

         call find_variable(input, var_title, var_required, var=var, line_num=line)

      end if


      if(.not. present(expected)) return

      do i=1, size(expected)
         if(trim(var) == trim(expected(i))) return
      end do

      call output_error_msg("Variable value """ // trim(var) // """ not correct!", expected, line)

   end subroutine read_var_char

   subroutine read_var_flag(var_title, var, description, required)
!!
!!    Read flag
!!    Writte by Marco Scavino, June 2019
!!
!!    Search for `var_title` in input. If found, `var` will be set
!!    to .true.
!!
      implicit none

      character(len=*), intent(in) :: var_title
      logical, intent(inout) :: var

      character(len=*), intent(in), optional :: description
      logical, intent(in), optional :: required

      character(len=30) :: var_answer
      logical :: var_required

      var_required = .false.

      if(interactive) then

         call get_stdin_var(description, var_answer, (/"yes", "no "/))

         if(trim(var_answer) == "yes") var = .true.

      else

         if(present(required))  var_required = required

         call find_variable(input, var_title, var_required, found=var)

      end if

      return

   end subroutine read_var_flag

   subroutine read_array_int(var_title, var, description, required)
!!
!!    Read array int
!!    Writte by Marco Scavino, June 2019
!!
!!    Search for `var_title` in input. If found, the remaing line will be read
!!    as an array of int and saved in `var`. If the user 
!!
      implicit none

      character(len=*), intent(in) :: var_title
      integer, intent(inout) :: var(:)

      character(len=*), intent(in), optional :: description
      logical, intent(in), optional :: required

      character(len=30) :: var_answer
      integer :: i, j, f_error
      logical :: var_required

      var_required = .false.
      var = 0

      if(interactive) then

         call get_stdin_var(description, var_answer)

      else

         if(present(required))  var_required = required

         call find_variable(input, var_title, var_required, var=var_answer)

      end if

      if(trim(var_answer) == "") return

      j = 0
      i = 0
      do while(i<=len_trim(var_answer))
         i = i+1

         if(var_answer(i:i) == " ") then
            j = j+1
            read(var_answer(1:i),*, iostat=f_error) var(j)
            if(f_error /= 0) exit
            var_answer = adjustl(var_answer(i:))
            i = 1
         end if
      end do

      if(j < size(var))then
         write(*,'(1x,a,i0,a, i0)') "Error: """ // var_title // """ expects ", size(var), " entries, but get only ", j
         stop
      end if
   end subroutine

   subroutine get_stdin_var(description, answer, choices)
!!
!!    Read standard input 
!!    Writte by Marco Scavino, June 2019
!!
!!    Read from STDIN the required variable and return in "answer". "description" is printed to the STDOUT.
!!    If the "choices" is present, they will be printed for the user.
!!
      implicit none

      character(len=*), intent(in) :: description
      character(len=*), intent(in), optional :: choices(:)
      character(len=*), intent(out) :: answer

      integer :: i

      write(*,'(1x,a)', advance='no') description

      if(present(choices)) then
         if(len(choices) > 0) then

            write(*,'(1x,a, a)', advance='no') "(", trim(choices(1))
            do i=2, size(choices)
               write(*,'("/",a)', advance='no') trim(choices(i))
            end do
            write(*,'(")")', advance="no")

         end if
      
      end if

      write(*, '(":",1x)', advance='no')
      read(*,*)  answer

   end subroutine

end module input_file