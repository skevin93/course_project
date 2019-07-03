module output_module

   use file_info
   use input_file
   use kinds

   implicit none

   character(len=*), parameter :: real_format = "F16.8"

   interface write_output

      module procedure output_real, output_int, output_char, output_real_array, output_real_array_2
   
   end interface

contains

   subroutine output_real(var_title, var, extra)

      implicit none

      character(len=*), intent(in) :: var_title
      real(dp), intent(in) :: var
      character(len=*), intent(in), optional :: extra

      write(output%unit_,'(a)', advance='no') var_title

      if(present(extra)) write(output%unit_,'("(",a,")")', advance='no') extra

      write(output%unit_, '(":",1x,' // real_format // ')') var

   end subroutine
   
   subroutine output_int(var_title, var, extra)

      implicit none

      character(len=*), intent(in) :: var_title
      integer, intent(in) :: var
      character(len=*), intent(in), optional :: extra
      
      write(output%unit_,'(a)', advance='no') var_title

      if(present(extra)) write(output%unit_,'("(",a,")")', advance='no') extra

      write(output%unit_, '(":",1x,i0)') var

   end subroutine

   subroutine output_char(var_title, var, extra)

      implicit none

      character(len=*), intent(in) :: var_title
      character(len=*), intent(in) :: var
      character(len=*), intent(in), optional :: extra

      write(output%unit_,'(a)', advance='no') var_title

      if(present(extra)) write(output%unit_,'("(",a,")")', advance='no') extra

      write(output%unit_, '(":",1x,a)') var

   end subroutine

   subroutine output_real_array(var_title, var, extra)

      implicit none

      character(len=*), intent(in) :: var_title
      real(dp), intent(in) :: var(:)
      character(len=*), intent(in), optional :: extra
      character(len=20) :: curr_format

      write(output%unit_,'(a)', advance='no') var_title

      if(present(extra)) write(output%unit_,'("(",a,")")', advance='no') extra
      
      write(output%unit_, '(":",1x,i0)') size(var)

      write(curr_format, '("(",i0,a,")")') size(var), real_format
      write(output%unit_, curr_format) var

   end subroutine

   subroutine output_real_array_2(var_title, var, extra)

      implicit none

      character(len=*), intent(in) :: var_title
      real(dp), intent(in) :: var(:,:)
      character(len=*), intent(in), optional :: extra

      character(len=20) :: curr_format

      write(output%unit_,'(a)', advance='no') var_title

      if(present(extra)) write(output%unit_,'("(",a,")")', advance='no') extra
      
      write(output%unit_, '(":",2(1x,i0))') size(var, 1), size(var, 2)

      write(curr_format, '("(",i0,a,")")') size(var,1), real_format

      write(output%unit_, curr_format) var

   end subroutine

   subroutine output_xyz_file(the_file, atoms, coord, n_atoms, comment)

      implicit none

      type(file), intent(in) :: the_file
      integer, intent(in)  :: n_atoms
      integer, intent(in) :: atoms(n_atoms)
      real(dp), intent(in) :: coord(3,n_atoms)
      character(len=*), intent(in), optional :: comment
      character(len=255) :: comment_line
      integer :: i

      comment_line = ""
      if(present(comment)) comment_line = adjustl(comment)

      if(trim(the_file%type_) /= "xyz") then
         call output_error_msg("Trying to write ""xyz"" file in different type!")
      end if

      write(the_file%unit_,'(i0)') n_atoms

      write(the_file%unit_,'(a)') comment_line
      do i=1, n_atoms
         write(the_file%unit_,'(i4,3F10.6)') atoms(i), coord(:,i)
      end do

   end subroutine

end module output_module