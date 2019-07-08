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

   subroutine read_argument()
!!
!!    Check interactive mode
!!    Writte by Marco Scavino, June 2019
!!
!!    Search the flag "interactive" in the input file. In case, switch to the
!!    interactive mode.
!!
      implicit none

      integer :: n_args
      character(len=30) :: buffer, command

#ifdef __INTEL_COMPILER
      interface
         integer function iargc()
         end function
      end interface
      external :: getarg
#endif
      call getarg(0, command)

      n_args = iargc()

      if(n_args == 0) then

         interactive = .true.

      else

         call getarg(1,buffer)

         select case(trim(buffer))
         case("-h")
            call print_help(command,    &
               options=(/ "-h   ", "-I   "/), &
               descr=(/                 &
                  "Print this help ",   &
                  "Interactive mode" /) )
            stop

         case("-I")
            interactive = .true.
         case default
            input%name_ = adjustl(buffer)
         end select

         if(n_args == 2) then
            call getarg(2, buffer)
            output%name_ = adjustl(buffer)
         end if
      end if

      ! Open input and output file after get the information form command line
      call open_file(output, "write")
      call open_file(input, "read")

      if(interactive) write(*,*) "INTERACTIVE MODE"

   end subroutine read_argument

   subroutine print_help(command, options, descr)
      implicit none

      character(len=*), intent(in) :: command
      character(len=*), intent(in), dimension(:) :: options, descr

      character(len=30) :: name
      integer :: i

      call get_filename(command, name)

      write(*,'(a,a)', advance="no") "Usage: ", trim(name)

      if (size(options) > 0) then
         write(*,'(1x,"[",a)', advance="no") trim(options(1))
         do i=2, size(options)
            write(*,'("|",a)', advance="no") trim(options(i))
         end do
         write(*,'("]")', advance="no")
      end if
      write(*,'(2(1x,a))') "[input_file]", "[output_file]"

      write(*,'(/1x,a)') "If no file is provided, it starts the interactive mode"
      write(*,*)

      do i=1, size(options)
         write(*,'(t3,a5,1x,a)') adjustl(options(i)), trim(descr(i))
      end do
   end subroutine

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

      call output_error_msg("Variable value """ // trim(var) // """ not correct!", &
         required=expected, &
         filename=input%name_, &
         line=line)

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

      character(len=50) :: var_answer
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

         write(var_answer, '("""",a,""" expects ", i0, " entries, but get only ",i0)') var_title, size(var), j
         call output_error_msg(var_answer)
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