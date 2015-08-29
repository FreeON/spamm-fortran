!> 2D spamm types.
module spamm_chunk_2d

  implicit none

  !> The tree-node type.
  type :: chunk_decoration_2d_t
     sequence
     !> The number of non-zero elements.
     double precision :: number_nonzeros = 0
     !> The square of the Frobenius norm.
     double precision :: norm2 = 0
  end type chunk_decoration_2d_t

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
  type :: chunk_2d_t
     sequence
     !> The matrices at the leaves.
     type(chunk_matrix_2d_t) :: blocks(SPAMM_CHUNK_BLOCKS, SPAMM_CHUNK_BLOCKS)
     !> The tree nodes.
     !!
     !! The tree are stored in a complete quadtree. Pointer linking is
     !! therefore not necessary, since the indices can be computed.
     !! In more detail, the nodes can be indexed from a starting index
     !! \f$ i \f$ as follows (this assumes that the root is at index
     !! 1):
     !!   - Children of node \f$ i \f$: \f$ \rightarrow 4 (i-1) + \{
     !!     2, 3, 4, 5 \} \f$.
     !!   - Parent of node \f$ i \f$: \f$ \lfloor \frac{ i-2 }{4}
     !!     \rfloor + 1 \f$.
     type(chunk_decoration_2d_t) :: node(SPAMM_CHUNK_NODES)
     !> Lower index bound.
     integer :: lower(2) = [-1, -1]
     !> Upper index bound.
     integer :: upper(2) = [-1, -1]
     !> The padded dimensions.
     integer :: n_padded(2) = [SPAMM_CHUNK_SIZE, SPAMM_CHUNK_SIZE]
     !> The leaf-node submatrix size.
     integer :: n_block(2) = [SPAMM_BLOCK_SIZE, SPAMM_BLOCK_SIZE]
  end type chunk_2d_t

contains

  !> Return the index in chunk_2d_t::blocks given a matrix index.
  function chunk_block_index(i) result(i_block)

    integer, intent(in) :: i
    integer :: i_block

    i_block = i/SPAMM_BLOCK_SIZE+1

  end function chunk_block_index

  !> Convert the meta-data of a chunk to a string.
  !!
  !! @param a The chunk.
  !! @return The string representation.
  function chunk_2d_to_string(a) result(string)

    use spamm_strings

    character(len=1000) :: string
    type(chunk_2d_t), pointer, intent(in) :: a

    string = "chunk:"
    write(string, "(A)") trim(string)//" N_c: "//trim(to_string(SPAMM_CHUNK_SIZE))
    write(string, "(A)") trim(string)//", box: [["//trim(to_string(a%lower(1)))// &
         ", "//trim(to_string(a%upper(1)))//"]"
    write(string, "(A)") trim(string)//", ["//trim(to_string(a%lower(2)))// &
         ", "//trim(to_string(a%upper(2)))//"]]"
    write(string, "(A)") trim(string)//"; N_b: "//trim(to_string(SPAMM_BLOCK_SIZE))
    write(string, "(A)") trim(string)//"; "//trim(to_string(size(a%node)))//" nodes"
    write(string, "(A)") trim(string)//"; "//trim(to_string(size(a%blocks, 1)**2))//" leaves"
    write(string, "(A)") trim(string)//"; total: "//trim(to_string(storage_size(a)/8))//" B"
    write(string, "(A)") trim(string)//"; node: "//trim(to_string(storage_size(a%node)/8))//" B"
    write(string, "(A)") trim(string)//"; basic: "// &
         trim(to_string(storage_size(a%blocks)/8))//" B"

  end function chunk_2d_to_string

  !> Write memory layout of chunk to string.
  !!
  !! @param A The chunk.
  !! @return The string representation.
  function chunk_2d_memory_layout(a) result (string)

    use, intrinsic :: iso_C_binding
    use spamm_strings

    character(len=1000) :: string
    type(chunk_2d_t), pointer, intent(in) :: a

    type(chunk_matrix_2d_t), pointer :: blocks(:, :)

    character(len=100) :: temp
    integer(c_intptr_t) :: ptr

    ptr = transfer(c_loc(a), ptr)+storage_size(a)
    write(temp, "(Z32)") ptr

    blocks => a%blocks

    write(string, "(A)") "chunk layout:"//C_NEW_LINE// &
         trim(to_string(c_loc(a)))//": chunk start"//C_NEW_LINE// &
         trim(to_string(c_loc(blocks(1, 1))))//": blocks(1, 1)"//C_NEW_LINE// &
         trim(to_string(c_loc(a%blocks(2, 1))))//": blocks(2, 1)"//C_NEW_LINE// &
         trim(to_string(c_loc(a%blocks(1, 2))))//": blocks(1, 2)"//C_NEW_LINE// &
         trim(to_string(c_loc(a%blocks(SPAMM_CHUNK_BLOCKS, 1))))// &
         ": blocks("//trim(to_string(SPAMM_CHUNK_BLOCKS))//", 1)"//C_NEW_LINE// &
         trim(to_string(c_loc(a%blocks(SPAMM_CHUNK_BLOCKS, SPAMM_CHUNK_BLOCKS))))// &
         ": blocks("//trim(to_string(SPAMM_CHUNK_BLOCKS))//", "// &
         trim(to_string(SPAMM_CHUNK_BLOCKS))//")"//C_NEW_LINE// &
         trim(to_string(c_loc(a%node(1))))//": node(1)"//C_NEW_LINE// &
         trim(to_string(c_loc(a%node(SPAMM_CHUNK_NODES))))// &
         ": node("//trim(to_string(SPAMM_CHUNK_NODES))//")"//C_NEW_LINE// &
         trim(to_string(c_loc(a%lower(1))))//": lower(1)"//C_NEW_LINE// &
         trim(to_string(c_loc(a%upper(1))))//": upper(1)"//C_NEW_LINE// &
         trim(to_string(c_loc(a%n_padded(1))))//": n_padded(1)"//C_NEW_LINE// &
         trim(to_string(c_loc(a%n_block(1))))//": n_block(1)"//C_NEW_LINE// &
         "0x"//trim(adjustl(temp))//": chunk end"

  end function chunk_2d_memory_layout

end module spamm_chunk_2d
