! ==============================================================================
! Subroutine    : insort_left_int8_1d_direct_asc (f90)
! ------------------------------------------------------------------------------
! Description   : Sorted insertion, integer(8), 1D array, ascending order
!               : insort_left inserts before existing equal elements
!               : Prints error and returns if arr is full (n >= size(arr))
!
! Author        : Scienttysburg
! Creation Date : 2026/05/10
! Last Modified : 2026/05/10
! Version       : 1.0.0
! ==============================================================================
subroutine insort_left_int8_1d_direct_asc(arr, n, x, lo, hi)
  ! ============================================================================
  implicit none
  ! --- Input Data -------------------------------------------------------------
  integer(8), intent(inout)        :: arr(:)
  integer(8), intent(inout)        :: n
  integer(8), intent(in)           :: x
  integer(8), intent(in), optional :: lo, hi
  ! --- Work Variables / Internal State ----------------------------------------
  integer(8)                       :: lo_used, hi_used
  integer(8)                       :: left, right, mid, pos
  ! ============================================================================

  ! === Boundary Check =========================================================
  if(n >= int(size(arr), 8))then
    write(*,'(A)') '[ERROR] insort_left_int8_1d_direct_asc : array is full (n >= size(arr))'
    stop
  end if

  ! === Resolve Search Range ===================================================
  if(present(lo))then
    lo_used = lo
  else
    lo_used = 1_8
  end if
  if(present(hi))then
    hi_used = hi
  else
    hi_used = n
  end if

  ! === Binary Search (bisect_left) ============================================
  left  = lo_used
  right = hi_used + 1_8

  do while(left < right)
    mid = (left + right) / 2_8
    if(arr(mid) < x)then
      left  = mid + 1_8
    else
      right = mid
    end if
  end do

  pos = left

  ! === Insert =================================================================
  arr(pos+1:n+1) = arr(pos:n)
  arr(pos)       = x
  n              = n + 1_8

end subroutine insort_left_int8_1d_direct_asc

! ==============================================================================
! Subroutine    : insort_right_int8_1d_direct_asc (f90)
! ------------------------------------------------------------------------------
! Description   : Sorted insertion, integer(8), 1D array, ascending order
!               : insort_right inserts after existing equal elements
!               : Prints error and returns if arr is full (n >= size(arr))
!
! Author        : Scienttysburg
! Creation Date : 2026/05/10
! Last Modified : 2026/05/10
! Version       : 1.0.0
! ==============================================================================
subroutine insort_right_int8_1d_direct_asc(arr, n, x, lo, hi)
  ! ============================================================================
  implicit none
  ! --- Input Data -------------------------------------------------------------
  integer(8), intent(inout)        :: arr(:)
  integer(8), intent(inout)        :: n
  integer(8), intent(in)           :: x
  integer(8), intent(in), optional :: lo, hi
  ! --- Work Variables / Internal State ----------------------------------------
  integer(8)                       :: lo_used, hi_used
  integer(8)                       :: left, right, mid, pos
  ! ============================================================================

  ! === Boundary Check =========================================================
  if(n >= int(size(arr), 8))then
    write(*,'(A)') '[ERROR] insort_right_int8_1d_direct_asc : array is full (n >= size(arr))'
    stop
  end if

  ! === Resolve Search Range ===================================================
  if(present(lo))then
    lo_used = lo
  else
    lo_used = 1_8
  end if
  if(present(hi))then
    hi_used = hi
  else
    hi_used = n
  end if

  ! === Binary Search (bisect_right) ===========================================
  left  = lo_used
  right = hi_used + 1_8

  do while(left < right)
    mid = (left + right) / 2_8
    if(arr(mid) <= x)then
      left  = mid + 1_8
    else
      right = mid
    end if
  end do

  pos = left

  ! === Insert =================================================================
  arr(pos+1:n+1) = arr(pos:n)
  arr(pos)       = x
  n              = n + 1_8

end subroutine insort_right_int8_1d_direct_asc