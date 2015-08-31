subroutine print_child_indices(parent)

  use spamm

  implicit none

  integer, intent(in) :: parent

  integer :: i, j, child

  do j = 1, 2
     do i = 1, 2
        child = 4*(parent-1)+(i-1)+2*j
        write(*, "(A)") "parent: "//trim(to_string(parent)) &
             //", child("//trim(to_string(i))//", "//trim(to_string(j))//"): " &
             //trim(to_string(get_child(parent, i, j)))
        if(child /= get_child(parent, i, j)) then
           write(*, *) "incorrect child index"
           error stop
        end if
     end do
  end do

end subroutine print_child_indices

subroutine print_parent_index(child, parent_ref)

  use spamm

  implicit none

  integer, intent(in) :: child, parent_ref

  write(*, "(A)") "child: "//trim(to_string(child)) &
       //", parent: "//trim(to_string(get_parent(child)))
  if(get_parent(child) /= parent_ref) then
     write(*, *) "wrong parent index"
     error stop
  end if

end subroutine print_parent_index

program test

  implicit none

  call print_child_indices(1)
  call print_child_indices(2)
  call print_child_indices(3)

  call print_parent_index(2, 1)
  call print_parent_index(3, 1)
  call print_parent_index(4, 1)
  call print_parent_index(5, 1)
  call print_parent_index(6, 2)
  call print_parent_index(7, 2)
  call print_parent_index(8, 2)
  call print_parent_index(9, 2)
  call print_parent_index(10, 3)
  call print_parent_index(11, 3)
  call print_parent_index(12, 3)
  call print_parent_index(13, 3)

end program test
