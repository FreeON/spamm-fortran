!> Module containing string helper functions.
module spamm_strings

  implicit none

  !> Interface to to_string functions.
  interface to_string
    module procedure c_ptr_to_string
    module procedure double_array_to_string
    module procedure double_to_string
    module procedure int_to_string
    module procedure long_int_to_string
    module procedure single_to_string
  end interface to_string

  interface
     subroutine pointer_to_string(ptr, length, string) bind(C)
       use, intrinsic :: iso_C_binding
       type(c_ptr), intent(in) :: ptr
       integer(c_int), intent(in) :: length
       character(c_char), intent(inout) :: string(*)
     end subroutine pointer_to_string
  end interface

contains

  !> Convert an integer to a string.
  !!
  !! @param i The integer.
  !! @return The string representation.
  function int_to_string(i) result(str_i)

    integer, intent(in) :: i

    character(len=100) :: temp
    character(len=100) :: str_i

    write(temp, *) i
    str_i = trim(adjustl(temp))

  end function int_to_string

  !> Convert an integer to a string.
  !!
  !! @param i The integer.
  !! @return The string representation.
  function long_int_to_string(i) result(str_i)

    integer(selected_int_kind(15)), intent(in) :: i

    character(len=100) :: temp
    character(len=100) :: str_i

    write(temp, *) i
    str_i = trim(adjustl(temp))

  end function long_int_to_string

  !> Convert a single precision real to a string.
  !!
  !! @param x The real
  !!
  !! @return The string representation.
  function single_to_string(x) result(str_x)

    real, intent(in) :: x
    character(len=100) :: temp
    character(len=100), allocatable :: str_x

    write(temp, "(ES16.8E3)") x
    str_x = trim(adjustl(temp))

  end function single_to_string

  !> Convert an array of double precision reals to a string.
  !!
  !! @param x The real array.
  !! @return The string representation.
  function double_array_to_string(x) result(str_x)

    double precision, intent(in) :: x(:)
    character(len=1000) :: temp
    character(len=100) :: format_string
    character(len=100), allocatable :: str_x
    integer :: i

    write(format_string, "(A,I4,A)") "(", size(x), "ES16.8E3)"
    write(temp, format_string) (x(i), i = 1, size(x))
    str_x = trim(adjustl(temp))

  end function double_array_to_string

  !> Convert a double precision real to a string.
  !!
  !! @param x The real.
  !! @return The string representation.
  function double_to_string(x) result(str_x)

    double precision, intent(in) :: x
    character(len=100) :: temp
    character(len=100), allocatable :: str_x

    write(temp, "(ES16.8E3)") x
    str_x = trim(adjustl(temp))

  end function double_to_string

  !> Convert a c_ptr to a string.
  !!
  !! @param ptr The pointer.
  !! @return The string representation.
  function c_ptr_to_string(ptr) result(string)

    use, intrinsic :: iso_C_binding

    type(c_ptr), intent(in) :: ptr

    integer(c_intptr_t) :: ptr_int
    character(len=100) :: string

    ptr_int = transfer(ptr, ptr_int)
    write(string, "(Z32)") ptr_int
    string = "0x"//trim(adjustl(string))

  end function c_ptr_to_string

end module spamm_strings
