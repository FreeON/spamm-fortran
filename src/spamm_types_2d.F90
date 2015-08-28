!> 2D spamm types.
module spamm_types_2d

  !> The tree-node type.
  type :: chunk_node_2d_t
     sequence
     !> The number of non-zero elements.
     double precision :: number_nonzeros = 0
     !> The square of the Frobenius norm.
     double precision :: norm2 = 0
  end type chunk_node_2d_t

  !> The leaf-node matrix type.
  type :: chunk_data_2d_t
     sequence
     !> The matrix.
     double precision :: data(SPAMM_BLOCK_SIZE, SPAMM_BLOCK_SIZE) = 0
  end type chunk_data_2d_t

  !> Bounds.
  type :: chunk_bounds_t
     sequence
     !> The lower and upper bound along a dimension.
     integer :: bounds(0:1) = [ -1, -1 ]
  end type chunk_bounds_t

  !> The 2D chunk type.
  !!
  !! The components are ordered such that the matrix data comes
  !! first. The `sequence`but attribute guarantees that the storage order
  !! of the components is the same as layed out in the type
  !! definition. All components are guaranteed to be allocated
  !! contiguously (Section 5.3.7 F08 Standard).
  type :: chunk_2d_t
     sequence
     !> The matrices at the leaves.
     type(chunk_data_2d_t) :: data(SPAMM_CHUNK_BLOCKS, SPAMM_CHUNK_BLOCKS)
     !> The tree nodes. The tree are stored in a complete quadtree,
     !! i.e.
     !!
     !! The nodes can be indexed from a starting index \f$ i \f$ as
     !! follows (this assumes that the root is at index 1):
     !!   - Children of node \f$ i \f$: \f$ \rightarrow 4 (i-1) + \{
     !!     2, 3, 4, 5 \} \f$.
     !!   - Parent of node \f$ i \f$: \f$ \lfloor \frac{ i-2 }{4}
     !!     \rfloor + 1 \f$.
     type(chunk_node_2d_t) :: node(SPAMM_CHUNK_NODES)
     !> Lower index bound.
     type(chunk_bounds_t) :: lower = chunk_bounds_t([ 1, 1 ])
     !> Upper index bound.
     type(chunk_bounds_t) :: upper = chunk_bounds_t([ SPAMM_CHUNK_SIZE, SPAMM_CHUNK_SIZE ])
  end type chunk_2d_t

end module spamm_types_2d
