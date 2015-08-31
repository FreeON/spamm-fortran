  !> Decorate procedures for 2d.
module spamm_decorate_2d

  implicit none

  !> Interface for redecoration procedures.
  interface redecorate
     module procedure redecorate_2d
  end interface redecorate

contains

  !> Redecorate a 2d chunk.
  !!
  !! @param a The chunk
  subroutine redecorate_2d(a)

    use spamm_chunk_2d

    type(chunk_2d_t), intent(inout) :: a

  end subroutine redecorate_2d

end module spamm_decorate_2d
