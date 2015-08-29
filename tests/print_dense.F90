module print_dense
  implicit none
contains

  !> Print a dense matrix.
  !!
  !! @param tag A string that is printed before the matrix.
  !! @param a The matrix.
  subroutine print_dense_matrix(tag, a)

    character(len=*), intent(in) :: tag
    double precision, intent(in) :: a(:, :)

    character(len=100) :: format_string
    integer :: i, j

    write(format_string, *) size(a, 2)
    write(format_string, "(A)") "("//trim(adjustl(format_string))//"F6.2)"
    write(*, "(A)") trim(tag)
    do i = 1, size(a, 1)
       write(*, format_string) (a(i, j), j = 1, size(a, 2))
    end do
  end subroutine print_dense_matrix

end module print_dense
