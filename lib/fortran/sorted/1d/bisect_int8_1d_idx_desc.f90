! ==============================================================================
! Function      : bisect_left_int8_1d_idx_desc (f90)
! ------------------------------------------------------------------------------
! Description   : Binary search (bisect left), integer(8), 1D array, descending order
!               : Index-based (indirect) search; arr is unchanged
!               : arr(idx(lo:pos-1)) > x >= arr(idx(pos:hi)), pos in [lo, hi+1]
!
! Author        : Scienttysburg
! Creation Date : 2026/05/10
! Last Modified : 2026/05/10
! Version       : 1.0.0
! ==============================================================================
function bisect_left_int8_1d_idx_desc(arr, idx, x, lo, hi) result(pos)
  ! ============================================================================
  implicit none
  ! --- Input Data -------------------------------------------------------------
  integer(8), intent(in)           :: arr(:)
  integer(8), intent(in)           :: idx(:)
  integer(8), intent(in)           :: x
  integer(8), intent(in), optional :: lo, hi
  ! --- Output Data ------------------------------------------------------------
  integer(8)                       :: pos
  ! --- Work Variables / Internal State ----------------------------------------
  integer(8)                       :: lo_used, hi_used
  integer(8)                       :: left, right, mid
  ! ============================================================================

  ! === Resolve Search Range ===================================================
  if(present(lo))then
    lo_used = lo
  else
    lo_used = 1_8
  end if
  if(present(hi))then
    hi_used = hi
  else
    hi_used = int(size(idx), 8)
  end if

  ! === Binary Search ==========================================================
  left  = lo_used
  right = hi_used + 1_8

  do while(left < right)
    mid = (left + right) / 2_8
    if(arr(idx(mid)) > x)then
      left  = mid + 1_8
    else
      right = mid
    end if
  end do

  ! === Results ================================================================
  pos = left

end function bisect_left_int8_1d_idx_desc

! ==============================================================================
! Function      : bisect_right_int8_1d_idx_desc (f90)
! ------------------------------------------------------------------------------
! Description   : Binary search (bisect right), integer(8), 1D array, descending order
!               : Index-based (indirect) search; arr is unchanged
!               : arr(idx(lo:pos-1)) >= x > arr(idx(pos:hi)), pos in [lo, hi+1]
!
! Author        : Scienttysburg
! Creation Date : 2026/05/10
! Last Modified : 2026/05/10
! Version       : 1.0.0
! ==============================================================================
function bisect_right_int8_1d_idx_desc(arr, idx, x, lo, hi) result(pos)
  ! ============================================================================
  implicit none
  ! --- Input Data -------------------------------------------------------------
  integer(8), intent(in)           :: arr(:)
  integer(8), intent(in)           :: idx(:)
  integer(8), intent(in)           :: x
  integer(8), intent(in), optional :: lo, hi
  ! --- Output Data ------------------------------------------------------------
  integer(8)                       :: pos
  ! --- Work Variables / Internal State ----------------------------------------
  integer(8)                       :: lo_used, hi_used
  integer(8)                       :: left, right, mid
  ! ============================================================================

  ! === Resolve Search Range ===================================================
  if(present(lo))then
    lo_used = lo
  else
    lo_used = 1_8
  end if
  if(present(hi))then
    hi_used = hi
  else
    hi_used = int(size(idx), 8)
  end if

  ! === Binary Search ==========================================================
  left  = lo_used
  right = hi_used + 1_8

  do while(left < right)
    mid = (left + right) / 2_8
    if(arr(idx(mid)) >= x)then
      left  = mid + 1_8
    else
      right = mid
    end if
  end do

  ! === Results ================================================================
  pos = left

end function bisect_right_int8_1d_idx_desc