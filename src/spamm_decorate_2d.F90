  !> Decorate procedures for 2d.
module spamm_decorate_2d

  implicit none

  !> Interface for redecoration procedures.
  interface redecorate
     module procedure redecorate_chunk_2d
  end interface redecorate

contains

  !> Redecorate a 2d chunk.
  !!
  !! @param chunk The chunk.
  subroutine redecorate_chunk_2d(chunk)

    use spamm_types_chunk_2d

    type(chunk_2d_t), pointer, intent(inout) :: chunk

    if(.not. associated(chunk)) return
    chunk%nodes(1)%lower = chunk%lower
    chunk%nodes(1)%upper = chunk%upper
    call redecorate_node_2d(chunk, 1)

  end subroutine redecorate_chunk_2d

  !> Redecorate a 2d node.
  !!
  !! @param chunk The chunk.
  !! @param node The node index.
  recursive subroutine redecorate_node_2d(chunk, node)

    use spamm_types_chunk_2d

    type(chunk_2d_t), pointer, intent(inout) :: chunk
    integer, intent(in) :: node

    integer :: i, j

    if(is_leaf(node)) then
       chunk%nodes(node)%norm2 = get_matrix_norm(chunk%blocks(get_block(node)))
       chunk%nodes(node)%number_nonzeros = get_matrix_nonzeros(chunk%blocks(get_block(node)))
    else
       chunk%nodes(node)%norm2 = 0
       do i = 1, 2
          do j = 1, 2
             call redecorate_node_2d(chunk, get_child(node, i, j))
             chunk%nodes(node)%norm2 = chunk%nodes(node)%norm2 &
                  +chunk%nodes(get_child(node, i, j))%norm2
             chunk%nodes(node)%number_nonzeros = chunk%nodes(node)%number_nonzeros &
                  +chunk%nodes(get_child(node, i, j))%number_nonzeros
          end do
       end do
    end if

  end subroutine redecorate_node_2d

end module spamm_decorate_2d
