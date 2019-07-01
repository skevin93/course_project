module output_module

   use file_info
   use kinds

   implicit none

contains

   subroutine output_xyz_file(the_file, atoms, coord, n_atoms)

      implicit none

      type(file), intent(in) :: the_file
      integer, intent(in)  :: n_atoms
      integer, intent(in) :: atoms(n_atoms)
      real(dp), intent(in) :: coord(3,n_atoms)

      integer :: i

      if(trim(the_file%type_) /= "xyz") then
         call output_error_msg("Trying to write ""xyz"" file in different type!")
      end if

      write(the_file%unit_,'(i0,/)') n_atoms

      do i=1, n_atoms
         write(the_file%unit_,*) atoms(i), coord(:,i)
      end do

   end subroutine

end module output_module