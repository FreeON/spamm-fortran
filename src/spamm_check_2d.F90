!> Verification procedures for 2D chunks.
module spamm_check_2d

  implicit none

  !> Interface for check procedures.
  interface check
     module procedure check_2d
  end interface check

contains

  !> Check a chunk.
  !!
  !! @param chunk The chunk to check.
  subroutine check_2d(chunk)

    use spamm_types_chunk_2d

    type(chunk_2d_t), pointer, intent(in) :: chunk

    write(*, "(A)") "checking chunk"
    write(*, "(A)") trim(chunk_2d_to_string(chunk))

    if(abs(check_node_2d(chunk, 1)-chunk%nodes(1)%norm2) > 1e-12) then
       print *, "incorrect norms"
       error stop
    end if

  end subroutine check_2d

  !> Check the validity of node decoration, i.e. the norms and some
  !! other things.
  !!
  !! @param chunk The chunk.
  !! @param parent The parent index.
  !! @return The square of the Frobenius norm of that parent node.
  recursive function check_node_2d(chunk, parent) result(norm2)

    use spamm_types_chunk_2d

    type(chunk_2d_t), pointer, intent(in) :: chunk
    integer, intent(in) :: parent
    double precision :: norm2

    integer :: i, j

    norm2 = 0
    if(get_child(parent) > SPAMM_CHUNK_NODES) then
       !norm2 = get_matrix_norm(blocks(get_block_2d(parent)))
    else
       do i = 1, 2
          do j = 1, 2
          end do
       end do
    end if

  end function check_node_2d

end module spamm_check_2d
