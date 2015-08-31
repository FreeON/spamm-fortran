!> 2D spamm types.
!! @ingroup chunk_2d
module spamm_types_chunk_2d

  implicit none

  !> The maximum returned string length for internal functions.
  integer, private, parameter :: MAX_STRING_LENGTH = 1000

  !> The tree-node type.
  type :: chunk_node_2d_t
     sequence
     !> Lower unpadded index bound.
     integer :: lower(2) = [-1, -1]
     !> Upper unpadded index bound.
     integer :: upper(2) = [-1, -1]
     !> The index width.
     integer :: width(2) = [-1, -1]
     !> The number of non-zero elements.
     double precision :: number_nonzeros = 0
     !> The square of the Frobenius norm.
     double precision :: norm2 = 0
  end type chunk_node_2d_t

  !> The leaf-node matrix type.
  type :: chunk_matrix_2d_t
     sequence
     !> The matrix.
     double precision :: matrix(SPAMM_BLOCK_SIZE, SPAMM_BLOCK_SIZE) = 0
  end type chunk_matrix_2d_t

  !> The 2D chunk type.
  !!
  !! The components are ordered such that the matrix data comes
  !! first. The `sequence` attribute guarantees that the storage order
  !! of the components is the same as layed out in the type
  !! definition.  All components are guaranteed to be allocated
  !! contiguously (Section 5.3.7 F08 Standard).
  !!
  !! The tree is stored in a complete quadtree. Pointer linking is
  !! therefore not necessary since the indices can be computed.  In
  !! more detail, the nodes can be indexed from a starting index \f$ i
  !! \f$ as follows (this assumes that the root is at index 1):
  !!   - Children of node \f$ i \f$: \f$ \rightarrow 4 (i-1) + \{ 2,
  !!     3, 4, 5 \} \f$.
  !!   - Parent of node \f$ i \f$: \f$ \lfloor \frac{ i-2 }{4} \rfloor
  !!     + 1 \f$.
  !!
  !! The correspondence between a node and its associated submatrix
  !! block is given by the nodes index shifted to 1.
  !!
  !! @f$ i \leftarrow i - 4^{d} @f$
  !!
  !! where @f$ d @f$ is the the depth of the tree.
  !!
  !! <table>
  !! <tr><td>1</td></tr>
  !! </table>
  !! @f$ \downarrow @f$
  !! <table>
  !! <tr><td>2</td><td>4</td></tr>
  !! <tr><td>3</td><td>5</td></tr>
  !! </table>
  !! @f$ \downarrow @f$
  !! <table>
  !! <tr><td> 6</td><td> 8</td><td>14</td><td>16</td></tr>
  !! <tr><td> 7</td><td> 9</td><td>15</td><td>17</td></tr>
  !! <tr><td>10</td><td>12</td><td>18</td><td>20</td></tr>
  !! <tr><td>11</td><td>13</td><td>19</td><td>21</td></tr>
  !! </table>
  !!
  !! @dotfile chunk_tree_2d "A simple example of the memory layout of a full quadtree inside a chunk."
  type :: chunk_2d_t
     sequence
     !> Lower unpadded index bound.
     integer :: lower(2) = [-1, -1]
     !> Upper unpadded index bound.
     integer :: upper(2) = [-1, -1]
     !> The padded dimensions (the padded width of the chunk).
     integer :: n_padded(2) = [SPAMM_CHUNK_SIZE, SPAMM_CHUNK_SIZE]
     !> The tree nodes.
     type(chunk_node_2d_t) :: nodes(SPAMM_CHUNK_NODES)
     !> The matrices at the leaves.
     type(chunk_matrix_2d_t) :: blocks(SPAMM_CHUNK_BLOCKS**2)
  end type chunk_2d_t

  !> Interface for is_leaf procedures.
  interface is_leaf
     module procedure is_leaf_2d
  end interface is_leaf

  !> Interface for get_block procedures.
  interface get_block
     module procedure get_block_2d
  end interface get_block

  !> Interface for get_node procedures.
  interface get_node
     module procedure get_node_2d
  end interface get_node

  !> Interface for get_parent procedures.
  interface get_parent
     module procedure get_parent_2d
  end interface get_parent

  !> Interface for get_child procedures.
  interface get_child
     module procedure get_child_2d
  end interface get_child

  !> Interface for print_chunk procedures.
  interface print_chunk
     module procedure print_chunk_2d
  end interface print_chunk

contains

  !> Check whether a node is a leaf node.
  !!
  !! @param node The node index.
  !! @return Whether the node is a leaf node.
  function is_leaf_2d(node) result(is_leaf)

    integer, intent(in) :: node
    logical :: is_leaf

    is_leaf = .false.
    if(get_child_2d(node) == node) then
       is_leaf = .true.
    end if

  end function is_leaf_2d

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

  !> Calculate the Frobenius norm of a dense matrix.
  !!
  !! @param a The dense matrix (of type chunk_matrix_2d_t).
  !! @return The square of the Frobenius norm.
  function get_matrix_norm(a) result(norm2)

    type(chunk_matrix_2d_t), intent(in) :: a
    double precision :: norm2

    norm2 = sum(a%matrix*a%matrix)

  end function get_matrix_norm

  !> Calculate the number of non-zero elements of a dense matrix.
  !!
  !! @param a The dense matrix (of type chunk_matrix_2d_t).
  !! @return The number of non-zeros.
  function get_matrix_nonzeros(a) result(number_nonzeros)

    type(chunk_matrix_2d_t), intent(in) :: a
    double precision :: number_nonzeros

    integer :: i, j

    number_nonzeros = 0
    do i = 1, SPAMM_BLOCK_SIZE
       do j = 1, SPAMM_BLOCK_SIZE
          if(a%matrix(i, j) /= 0) then
             number_nonzeros = number_nonzeros+1
          end if
       end do
    end do

  end function get_matrix_nonzeros

  !> Return the index of a submatrix block associated with a leaf
  !! node.
  !!
  !! @param node The leaf node index.
  !! @return The submatrix block index.
  function get_block_2d(node) result(submatrix)

    use spamm_strings

    integer, intent(in) :: node
    integer :: submatrix

    submatrix = node-SPAMM_CHUNK_NODES+SPAMM_CHUNK_BLOCKS**2
    if(submatrix < 1) then
       print *, "node "//trim(to_string(node))//" is not a leaf node"
       error stop
    end if

  end function get_block_2d

  !> Return the index of the leaf node corresponding to the block ID.
  !!
  !! @param block_id The index of the submatrix.
  !! @return The index of the leaf node.
  function get_node_2d(block_id) result(node)

    integer, intent(in) :: block_id
    integer :: node

    if(block_id < 1 .or. block_id > SPAMM_CHUNK_BLOCKS**2) then
       print *, "block_id out of range"
       error stop
    end if
    node = block_id+SPAMM_CHUNK_NODES-SPAMM_CHUNK_BLOCKS**2

  end function get_node_2d

  !> Get the parent index.
  !!
  !! @param node The node index.
  !! @return The index of the parent.
  function get_parent_2d(node) result(parent)

    integer, intent(in) :: node
    integer :: parent

    parent = (node-2)/4+1
    if(parent < 1) then
       parent = 1
    end if

  end function get_parent_2d

  !> The a child index.
  !!
  !! In a quadtree, the children are organized in a @f$ 2 \times 2 @f$
  !! matrix.
  !!
  !! @param node The node index.
  !! @param i_child The row index of the child (defaults to 1)
  !! @param j_child The column index of the child (defaults to 1)
  !! @return The child index.
  function get_child_2d(node, i_child, j_child) result(child)

    integer, intent(in) :: node
    integer, intent(in), optional :: i_child, j_child
    integer :: child

    integer :: i_child_, j_child_

    if(present(i_child)) then
       i_child_ = i_child
    else
       i_child_ = 1
    end if

    if(present(j_child)) then
       j_child_ = j_child
    else
       j_child_ = 1
    end if

    child = 4*(node-1)+(i_child_-1)+2*j_child_
    if(child > SPAMM_CHUNK_NODES) then
       child = node
    end if

  end function get_child_2d

  !> Convert the meta-data of a chunk to a string.
  !!
  !! @param chunk The chunk.
  !! @return The string representation.
  function chunk_2d_to_string(chunk) result(string)

    use spamm_strings

    type(chunk_2d_t), pointer, intent(in) :: chunk
    character(len=MAX_STRING_LENGTH) :: string

    string = "chunk:"
    write(string, "(A)") trim(string)//" N_c: "//trim(to_string(SPAMM_CHUNK_SIZE))
    write(string, "(A)") trim(string)//", box: [[" &
         //trim(to_string(chunk%lower(1)))//", " &
         //trim(to_string(chunk%upper(1)))//"]"
    write(string, "(A)") trim(string)//", [" &
         //trim(to_string(chunk%lower(2)))//", " &
         //trim(to_string(chunk%upper(2)))//"]]"
    write(string, "(A)") trim(string)//"; N_b: " &
         //trim(to_string(SPAMM_BLOCK_SIZE))
    write(string, "(A)") trim(string)//"; " &
         //trim(to_string(SPAMM_CHUNK_NODES))//" nodes"
    write(string, "(A)") trim(string)//"; " &
         //trim(to_string(SPAMM_CHUNK_NODES**2))//" leaves"
    write(string, "(A)") trim(string)//"; total: " &
         //trim(to_string(storage_size(chunk)/8))//" B"
    write(string, "(A)") trim(string)//"; node: " &
         //trim(to_string(storage_size(chunk%nodes)/8))//" B"
    write(string, "(A)") trim(string)//"; basic: " &
         //trim(to_string(storage_size(chunk%blocks)/8))//" B"

  end function chunk_2d_to_string

  !> Write memory layout of chunk to string.
  !!
  !! @param chunk The chunk.
  !! @return The string representation.
  function chunk_2d_memory_layout(chunk) result (string)

    use, intrinsic :: iso_C_binding
    use spamm_strings

    type(chunk_2d_t), pointer, intent(in) :: chunk
    character(len=MAX_STRING_LENGTH) :: string

    character(len=100) :: temp
    integer(c_intptr_t) :: ptr

    ptr = transfer(c_loc(chunk), ptr)+storage_size(chunk)/8
    write(temp, "(Z32)") ptr

    write(string, "(A)") "chunk layout:"//C_NEW_LINE &
         //trim(to_string(c_loc(chunk)))//": chunk start"//C_NEW_LINE &
         //trim(to_string(c_loc(chunk%lower(1)))) &
         //": lower(1)"//C_NEW_LINE &
         //trim(to_string(c_loc(chunk%upper(1)))) &
         //": upper(1)"//C_NEW_LINE &
         //trim(to_string(c_loc(chunk%n_padded(1)))) &
         //": n_padded(1)"//C_NEW_LINE &
         //trim(to_string(c_loc(chunk%blocks(1)))) &
         //": blocks(1)"//C_NEW_LINE &
         //trim(to_string(c_loc(chunk%blocks(SPAMM_CHUNK_BLOCKS)))) &
         //": blocks("//trim(to_string(SPAMM_CHUNK_BLOCKS))//")"//C_NEW_LINE &
         //trim(to_string(c_loc(chunk%blocks(SPAMM_CHUNK_BLOCKS**2)))) &
         //": blocks("//trim(to_string(SPAMM_CHUNK_BLOCKS**2))//", 1)"//C_NEW_LINE &
         //trim(to_string(c_loc(chunk%nodes(1)))) &
         //": nodes(1)"//C_NEW_LINE &
         //trim(to_string(c_loc(chunk%nodes(SPAMM_CHUNK_NODES)))) &
         //": nodes("//trim(to_string(SPAMM_CHUNK_NODES))//")"//C_NEW_LINE &
         //"0x"//trim(adjustl(temp))//": chunk end"

  end function chunk_2d_memory_layout

  !> Print a tree.
  !!
  !! @param chunk The chunk.
  subroutine print_chunk_2d(chunk)

    use spamm_strings

    type(chunk_2d_t), pointer, intent(in) :: chunk

    write(*, "(A)") "chunk: N_c = "//trim(to_string(SPAMM_CHUNK_SIZE))//", " &
         //"N_b = "//trim(to_string(SPAMM_BLOCK_SIZE))//", [" &
         //trim(to_string(chunk%lower(1)))//":" &
         //trim(to_string(chunk%upper(1)))//"," &
         //trim(to_string(chunk%lower(2)))//":" &
         //trim(to_string(chunk%upper(2)))//"]"
    call print_node_2d(chunk, 1)

  end subroutine print_chunk_2d

  !> Print a node and its children nodes.
  !!
  !! @param chunk The chunk.
  !! @param node The node ID.
  recursive subroutine print_node_2d(chunk, node)

    use spamm_strings

    type(chunk_2d_t), pointer, intent(in) :: chunk
    integer, intent(in) :: node

    character(len=MAX_STRING_LENGTH) :: buffer

    buffer = trim(to_string(node))//": "
    associate(n => chunk%nodes(node))
      buffer = trim(buffer)//"["//trim(to_string(n%lower(1)))//":"//trim(to_string(n%upper(1)))//","
      buffer = trim(buffer)//trim(to_string(n%lower(2)))//":"//trim(to_string(n%upper(2)))//"]"
      if(is_leaf(node)) then
         buffer = trim(buffer)//"->leaf"
         write(*, "(A)") trim(buffer)
         call print_dense_matrix(trim(to_string(get_block(node)))//":submatrix", &
              chunk%blocks(get_block(node))%matrix)
      else
         buffer = trim(buffer)//"->["//trim(to_string(get_child(node, 1, 1)))
         buffer = trim(buffer)//","//trim(to_string(get_child(node, 2, 1)))
         buffer = trim(buffer)//","//trim(to_string(get_child(node, 1, 2)))
         buffer = trim(buffer)//","//trim(to_string(get_child(node, 2, 2)))//"]"
         write(*, "(A)") trim(buffer)
         call print_node_2d(chunk, get_child(node, 1, 1))
         call print_node_2d(chunk, get_child(node, 2, 1))
         call print_node_2d(chunk, get_child(node, 1, 2))
         call print_node_2d(chunk, get_child(node, 2, 2))
      end if
    end associate

  end subroutine print_node_2d

end module spamm_types_chunk_2d
