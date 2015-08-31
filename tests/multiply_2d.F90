program test

  use spamm

  implicit none

  integer, parameter :: N = 6
  double precision, parameter :: alpha = 1.2
  double precision, parameter :: beta = 0.8

  type(chunk_2d_t), pointer :: a, b, c

  double precision :: a_dense(N, N)
  double precision :: b_dense(N, N)
  double precision :: c_dense(N, N)

  call random_number(a_dense)
  call random_number(b_dense)
  call print_dense_matrix("A", a_dense)
  call print_dense_matrix("B", b_dense)

  c_dense = alpha*matmul(a_dense, b_dense)

  a => convert(a_dense)
  b => convert(b_dense)
  c => multiply(a, b)

end program test
