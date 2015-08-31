  !> Conversions to and from spamm for 2d.
module spamm_convert_2d

  implicit none

  !> Interface to conversion procedures.
  interface convert
     module procedure convert_dense_to_2d
     module procedure convert_2d_to_dense
  end interface convert

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

  !> Convert dense matrix to 2d.
  !!
  !! @param a_dense The dense matrix.
  !! @return A newly allocated chunk.
  function convert_dense_to_2d(a_dense) result(chunk)

    use spamm_chunk_2d
    use spamm_decorate_2d

    double precision, intent(in) :: a_dense(:, :)
    type(chunk_2d_t), pointer :: chunk

    integer :: i, j

    if(size(a_dense, 1) > SPAMM_CHUNK_SIZE .or. size(a_dense, 2) > SPAMM_CHUNK_SIZE) then
       print *, "[FIXME] matrix too large"
       error stop
    end if

    allocate(chunk)

    chunk%lower = [1, 1]
    chunk%upper = shape(a_dense)

    do i = chunk%lower(1), chunk%upper(1), SPAMM_BLOCK_SIZE
       do j = chunk%lower(2), chunk%upper(2), SPAMM_BLOCK_SIZE
          associate(matrix => chunk%blocks(chunk_block_index(i), chunk_block_index(j))%matrix, &
               i_max => min(i+SPAMM_BLOCK_SIZE-1, size(a_dense, 1)), &
               j_max => min(j+SPAMM_BLOCK_SIZE-1, size(a_dense, 2)))
            matrix(1:1+i_max-i, 1:1+j_max-j) = a_dense(i:i_max, j:j_max)
          end associate
       end do
    end do

    call redecorate(chunk)

  end function convert_dense_to_2d

  !> Convert 2d to dense matrix.
  !!
  !! @param a The matrix chunk.
  !! @return The allocated dense matrix.
  function convert_2d_to_dense(a) result(a_dense)

    use spamm_chunk_2d

    type(chunk_2d_t), pointer, intent(in) :: a
    double precision, pointer :: a_dense(:, :)

    integer :: i, j

    allocate(a_dense(a%upper(1), a%upper(2)))
    a_dense = 0

    do i = 1, a%upper(1), SPAMM_BLOCK_SIZE
       do j = 1, a%upper(2), SPAMM_BLOCK_SIZE
          associate(matrix => a%blocks(chunk_block_index(i), chunk_block_index(j))%matrix, &
               i_max => min(i+SPAMM_BLOCK_SIZE-1, a%upper(1)), &
               j_max => min(j+SPAMM_BLOCK_SIZE-1, a%upper(2)))
            a_dense(i:i_max, j:j_max) = matrix(1:1+i_max-i, 1:1+j_max-j)
          end associate
       end do
    end do

  end function convert_2d_to_dense

end module spamm_convert_2d
