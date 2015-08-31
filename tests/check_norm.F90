program test

  use spamm

  type(chunk_matrix_2d_t) :: a
  double precision :: norm2
  integer :: i, j

  call random_number(a%matrix)

  norm2 = 0
  do i = 1, size(a%matrix, 1)
     do j = 1, size(a%matrix, 2)
        norm2 = norm2+a%matrix(i, j)**2
     end do
  end do
  write(*, "(A)") "reference norm2 = "//trim(to_string(norm2))
  write(*, "(A)") "computed norm2  = "//trim(to_string(get_matrix_norm(a)))

  if(abs(norm2-get_matrix_norm(a)) > 1e-12) then
     write(*, *) "norm mismatch"
     error stop
  else
     write(*, *) "norms are matching"
  end if

end program test
