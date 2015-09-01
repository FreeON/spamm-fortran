!> Multiply 2d spamm objects.
module spamm_multiply_2d

  implicit none

  !> Multiply interface.
  interface multiply
     module procedure spamm_multiply_2d_2d
  end interface multiply

contains

  !> Multiply two spamm chunks.
  !!
  !! @f$ c \leftarrow \alpha \left( a \otimes_{\tau} b \right) + \beta \, c @f$
  !!
  !! The factors @f$ \alpha @f$ and @f$ \beta @f$ are optional and
  !! default to one. If the matrix @f$ c @f$ is given, then the
  !! multiply is in place, i.e. @f$ c @f$ is updated.
  !!
  !! @param a Chunk a.
  !! @param b Chunk b.
  !! @param c Chunk c.
  !! @param alpha Factor @f$ \alpha @f$.
  !! @param beta Factor @f$ \beta @f$.
  !! @param tau The spamm tolerance @f$ \tau @f$.
  function spamm_multiply_2d_2d(a, b, c, alpha, beta, tau) result(d)

    use spamm_types_chunk_2d

    type(chunk_2d_t), pointer, intent(in) :: a, b
    type(chunk_2d_t), pointer, intent(in), optional :: c
    double precision, intent(in), optional :: alpha, beta, tau
    type(chunk_2d_t), pointer :: d

    if(present(c)) then
       d => c
    else
       d => null()
    end if

  end function spamm_multiply_2d_2d

end module spamm_multiply_2d
