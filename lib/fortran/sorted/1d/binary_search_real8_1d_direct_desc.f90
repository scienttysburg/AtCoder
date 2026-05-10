! ==============================================================================
! Function      : binary_search_real8_1d_direct_desc (f90)
! ------------------------------------------------------------------------------
! Description   : Binary search, real(8), 1D array, descending order
!               : Returns the index of the closest element to x within tol,
!               : or -1 if no element is found within tol
!
! Author        : Scienttysburg
! Creation Date : 2026/05/10
! Last Modified : 2026/05/10
! Version       : 1.0.0
! ==============================================================================
function binary_search_real8_1d_direct_desc(arr, x, lo, hi, tol) result(pos)
  ! ============================================================================
  implicit none
  ! --- Input Data -------------------------------------------------------------
  real(8),    intent(in)           :: arr(:)
  real(8),    intent(in)           :: x
  integer(8), intent(in), optional :: lo, hi
  real(8),    intent(in), optional :: tol
  ! --- Output Data ------------------------------------------------------------
  integer(8)                       :: pos
  ! --- Work Variables / Internal State ----------------------------------------
  integer(8)                       :: lo_used, hi_used
  real(8)                          :: tol_used
  integer(8)                       :: left, right, mid
  real(8)                          :: best_diff, diff
  integer(8)                       :: cur_pos
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
    hi_used = int(size(arr), 8)
  end if
  if(present(tol))then
    tol_used = tol
  else
    tol_used = 1.0d-12
  end if

  ! === Binary Search ==========================================================
  left  = lo_used
  right = hi_used + 1_8

  do while(left < right)
    mid = (left + right) / 2_8
    if(arr(mid) > x)then
      left  = mid + 1_8
    else
      right = mid
    end if
  end do

  ! === Scan for Closest Match Within Tolerance ================================
  pos       = -1_8
  best_diff = tol_used + 1.0d0

  cur_pos = left
  do while(cur_pos <= hi_used)
    diff = abs(arr(cur_pos) - x)
    if(diff > tol_used)exit
    if(diff < best_diff)then
      best_diff = diff
      pos       = cur_pos
    end if
    cur_pos = cur_pos + 1_8
  end do

  cur_pos = left - 1_8
  do while(cur_pos >= lo_used)
    diff = abs(arr(cur_pos) - x)
    if(diff > tol_used)exit
    if(diff < best_diff)then
      best_diff = diff
      pos       = cur_pos
    end if
    cur_pos = cur_pos - 1_8
  end do

end function binary_search_real8_1d_direct_desc