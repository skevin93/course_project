module string_tools
!!
!!    String tools
!!    Written by Marco Scavino, June 2019
!!
!!    Some tools to manipulate strings
!!

   character(len=*), private, parameter :: lower_alpha = "abcdefghijklmnopqrstuvwxyz"
   character(len=*), private, parameter :: upper_alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

contains

   function uppercase(str) result(out_str)
!!
!!    Uppercase
!!    Written by Marco Scavino, June 2019
!!
!!    Convert `str` to upper case. Adapted from http://rosettacode.org/wiki/String_case#Fortran
!!    Assumes ASCII table for representing characters as integers, 
!!    where the lowercase letter is +32 relative to the uppercase letters.
!!
!!    Note: uppercase (65-90) and lowercase (97-122).
!!
      implicit none
!
      character(len=*), intent(in) :: str
      character(len=len(str)) :: out_str
!
      integer :: i

      out_str = adjustl(str)

      do i = 1, len_trim(out_str)

         if(out_str(i:i) >= "a" .and. out_str(i:i) <= "z") then

            out_str(i:i) = achar(iachar(out_str(i:i))-32)

         end if

      end do

   end function uppercase

function lowercase(str) result(out_str)
!!
!!    Lowercase
!!    Written by Marco Scavino, June 2019
!!
!!    Convert `str` to lower case. Adapted from http://rosettacode.org/wiki/String_case#Fortran
!!    Assumes ASCII table for representing characters as integers, 
!!    where the lowercase letter is +32 relative to the uppercase letters.
!!
!!    Note: uppercase (65-90) and lowercase (97-122).
!!
         implicit none
   !
         character(len=*), intent(in) :: str
         character(len=len(str)) :: out_str
   !
         integer :: i
   
         out_str = adjustl(str)
   
         do i = 1, len_trim(out_str)
   
            if(out_str(i:i) >= "a" .and. out_str(i:i) <= "z") then
   
               out_str(i:i) = achar(iachar(out_str(i:i))+32)
   
            end if
   
         end do
   
      end function lowercase

end module