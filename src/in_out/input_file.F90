module input_file
!!
!!    Input file
!!    Written by Marco Scavino, June 2019
!!
!!    Read the input file opened in "input" unit. Core of this module is the "read_var" subroutine,
!!    that take the following arguments:
!!
!!       * "var_title", the keyword it will search in the input file
!!       * "var", returned variable
!!       * "description", a description of the input, printed in interactive mode
!!       * "expected" (optional), a list of expected values from the variable. If user enters unexpected
!!          values, it will print an error, followed by the list
!!       * "required" (optional), a boolean value to set if a variable is required. If .true., and the
!!           variable is not found, program will stop and pritn the proper error.
!!
   use kinds
   use file_info

   implicit none

   interface read_var

      module procedure read_var_real, read_var_int, read_var_char, read_var_flag

   end interface

   logical :: interactive = .false.

contains

   subroutine read_input()
!!
!!    Read input
!!    Writte by Marco Scavino, June 2019
!!
!!    Read some settings from the "input" file
!!
      implicit none

      character(len=30) :: orientation, superposition, maximize
      character(len=30) ::result

      orientation   = "eckart"
      superposition = "masses"
      maximize      = "full"
      result        = "matrix"

      call read_var("interactive", interactive)

      if(interactive) write(*,*) "INTERACTIVE MODE"

      call read_var("orientation", orientation, &
         description="Orientation of the first atom", &
         expected=(/"original",&
                    "eckart  "/))

      call read_var("superposition", superposition, &
         description="Superposition of the second atom", &
         expected=(/"none  ", &
                    "masses"/))

      call read_var("maximize", maximize, &
         description="Maximize overlap", &
         expected=(/"full ", &
                    "three"/))

      call read_var("result", result, &
         description="Result to print", &
         expected=(/"matrix", &
                    "shift ", &
                    "both  "/))

   end subroutine

   subroutine read_var_real(var_title, var, description, required)
!!
!!    Read real
!!    Writte by Marco Scavino, June 2019
!!
!!    Search for "var_title" in input. If found, "var"" will be set
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

   end subroutine

   subroutine read_var_int(var_title, var, description, required)
!!
!!    Read int
!!    Writte by Marco Scavino, June 2019
!!
!!    Search for "var_title" in input. If found, "var"" will be set
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

   end subroutine

   subroutine read_var_char(var_title, var, description, required, expected)
!!
!!    Read char
!!    Writte by Marco Scavino, June 2019
!!
!!    Search for "var_title" in input. If found, "var"" will be set
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

   end subroutine

   subroutine read_var_flag(var_title, var, description, required)
!!
!!    Read flag
!!    Writte by Marco Scavino, June 2019
!!
!!    Search for "var_title" in input. If found, "var"" will be set
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