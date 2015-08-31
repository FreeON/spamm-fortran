!> Allocation procedures for 2D chunks.
module spamm_constructor_2d

  implicit none

  private

  !> Interface for allocation procedures.
  interface new_chunk
     module procedure new_chunk_2d
  end interface new_chunk

  public :: new_chunk

contains

  !> Allocate a new chunk.
  !!
  !! @param lower The unpadded lower indices.
  !! @param upper The unpadded upper indices.
  !! @return chunk The new chunk.
  function new_chunk_2d(lower, upper) result(chunk)

    use spamm_types_chunk_2d

    integer, intent(in) :: lower(2)
    integer, intent(in) :: upper(2)
    type(chunk_2d_t), pointer :: chunk

    allocate(chunk)

    chunk%lower = lower
    chunk%upper = upper
    chunk%nodes(1)%lower = lower
    chunk%nodes(1)%upper = upper
    chunk%nodes(1)%width = chunk%n_padded
    call initialize_node_2d(chunk, 1)

  end function new_chunk_2d

  !> Initialize the decoration on a node.
  !!
  !! @param chunk The chunk.
  !! @param node The node index.
  recursive subroutine initialize_node_2d(chunk, node)

    use spamm_types_chunk_2d

    type(chunk_2d_t), pointer, intent(inout) :: chunk
    integer, intent(in) :: node

    integer :: i, j
    integer :: i_min, j_min, i_max, j_max

    if(is_leaf(node)) then
       return
    end if
    associate(width => chunk%nodes(node)%width)
      do i = 1, 2
         do j = 1, 2
            associate(child => get_child(node, i, j))
              associate(lower => chunk%nodes(node)%lower, upper => chunk%nodes(node)%upper)
                i_min = lower(1)+width(1)/2*(i-1)
                j_min = lower(2)+width(2)/2*(j-1)
                if(i_min < upper(1) .and. j_min < upper(2)) then
                   i_max = min(lower(1)+width(1)/2*i-1, upper(1))
                   j_max = min(lower(2)+width(2)/2*j-1, upper(2))
                   chunk%nodes(child)%lower = [i_min, j_min]
                   chunk%nodes(child)%upper = [i_max, j_max]
                   chunk%nodes(child)%width = chunk%nodes(node)%width/2
                   call initialize_node_2d(chunk, child)
                end if
              end associate
            end associate
         end do
      end do
    end associate

  end subroutine initialize_node_2d

end module spamm_constructor_2d
