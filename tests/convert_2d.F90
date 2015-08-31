program test

  use spamm

  implicit none

  integer, parameter :: N = 10

  type(chunk_2d_t), pointer :: a
  double precision :: a_dense(N, N)
  double precision, pointer :: b_dense(:, :)

  call random_number(a_dense)
  call print_dense_matrix("A", a_dense)
  a => convert(a_dense)
  !call print_chunk(a)
  call check(a)
  write(*, "(A)") trim(chunk_2d_to_string(a))
  write(*, "(A)") trim(chunk_2d_memory_layout(a))
  b_dense => convert(a)
  call print_dense_matrix("B", b_dense)

  if(maxval(a_dense-b_dense) > 1e-12) then
     print *, "value mismatch"
     error stop
  end if

end program test
