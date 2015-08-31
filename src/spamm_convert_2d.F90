  !> Conversions to and from spamm for 2d.
module spamm_convert_2d

  implicit none

  !> Interface to conversion procedures.
  interface convert
     module procedure convert_dense_to_2d
     module procedure convert_2d_to_dense
  end interface convert

contains

  !> Convert dense matrix to 2d.
  !!
  !! @param a The dense matrix.
  !! @return A newly allocated chunk.
  function convert_dense_to_2d(a) result(chunk)

    use spamm_types_chunk_2d
    use spamm_constructor_2d
    use spamm_decorate_2d
    use spamm_strings

    double precision, intent(in) :: a(:, :)
    type(chunk_2d_t), pointer :: chunk

    integer :: i

    if(size(a, 1) > SPAMM_CHUNK_SIZE .or. size(a, 2) > SPAMM_CHUNK_SIZE) then
       print *, "[FIXME] matrix too large"
       error stop
    end if

    chunk => new_chunk([1, 1], shape(a))

    do i = 1, SPAMM_CHUNK_BLOCKS**2
       associate(matrix => chunk%blocks(i)%matrix, node => chunk%nodes(get_node(i)))
         if(node%lower(1) >= chunk%lower(1) .and. node%lower(2) >= chunk%lower(2)) then
            associate(i_min => node%lower(1), &
                 i_max => min(chunk%upper(1), node%upper(1)), &
                 j_min => node%lower(2), &
                 j_max => min(chunk%upper(2), node%upper(2)))
              !> print *, trim(to_string(i))//": " &
              !>      //"matrix(1:"//trim(to_string(i_max-i_min+1))//"," &
              !>      //"1:"//trim(to_string(j_max-j_min+1))//")" &
              !>      //" <- " &
              !>      //"a("//trim(to_string(i_min))//":"//trim(to_string(i_max))//"," &
              !>      //trim(to_string(j_min))//":"//trim(to_string(j_max))//")"
              matrix(1:1+i_max-i_min, 1:1+j_max-j_min) = a(i_min:i_max, j_min:j_max)
            end associate
         end if
       end associate
    end do

  end function convert_dense_to_2d

  !> Convert 2d to dense matrix.
  !!
  !! @param chunk The matrix chunk.
  !! @return The allocated dense matrix.
  function convert_2d_to_dense(chunk) result(a)

    use spamm_types_chunk_2d

    type(chunk_2d_t), pointer, intent(in) :: chunk
    double precision, pointer :: a(:, :)

    integer :: i

    allocate(a(chunk%upper(1), chunk%upper(2)))
    a = 0

    do i = 1, SPAMM_CHUNK_BLOCKS**2
       associate(matrix => chunk%blocks(i)%matrix, node => chunk%nodes(get_node(i)))
         if(node%lower(1) >= chunk%lower(1) .and. node%lower(2) >= chunk%lower(2)) then
            associate(i_min => node%lower(1), &
                 i_max => min(chunk%upper(1), node%upper(1)), &
                 j_min => node%lower(2), &
                 j_max => min(chunk%upper(2), node%upper(2)))
              a(i_min:i_max, j_min:j_max) = matrix(1:1+i_max-i_min, 1:1+j_max-j_min)
            end associate
         end if
       end associate
    end do

  end function convert_2d_to_dense

end module spamm_convert_2d
