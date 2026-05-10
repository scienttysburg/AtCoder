! ==============================================================================
! Function      : binary_search_int8_1d_idx_asc (f90)
! ------------------------------------------------------------------------------
! Description   : Binary search, integer(8), 1D array, ascending order
!               : Index-based (indirect) search; arr is unchanged
!               : Returns the index of x in idx, or -1 if not found
!
! Author        : Scienttysburg
! Creation Date : 2026/05/10
! Last Modified : 2026/05/10
! Version       : 1.0.0
! ==============================================================================
function binary_search_int8_1d_idx_asc(arr, idx, x, lo, hi) result(pos)
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
    if(arr(idx(mid)) < x)then
      left  = mid + 1_8
    else
      right = mid
    end if
  end do

  ! === Results ================================================================
  if(left <= hi_used .and. arr(idx(left)) == x)then
    pos = left
  else
    pos = -1_8
  end if

end function binary_search_int8_1d_idx_asc