! ==============================================================================
! Function      : bisect_left_real8_1d_direct_asc (f90)
! ------------------------------------------------------------------------------
! Description   : Binary search (bisect left), real(8), 1D array, ascending order
!               : Returns the leftmost insertion position of x in sorted arr
!               : arr(lo:pos-1) < x <= arr(pos:hi), pos in [lo, hi+1]
!
! Author        : Scienttysburg
! Creation Date : 2026/05/10
! Last Modified : 2026/05/10
! Version       : 1.0.0
! ==============================================================================
function bisect_left_real8_1d_direct_asc(arr, x, lo, hi) result(pos)
  ! ============================================================================
  implicit none
  ! --- Input Data -------------------------------------------------------------
  real(8),    intent(in)           :: arr(:)
  real(8),    intent(in)           :: x
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
    hi_used = int(size(arr), 8)
  end if

  ! === Binary Search ==========================================================
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

  ! === Results ================================================================
  pos = left

end function bisect_left_real8_1d_direct_asc

! ==============================================================================
! Function      : bisect_right_real8_1d_direct_asc (f90)
! ------------------------------------------------------------------------------
! Description   : Binary search (bisect right), real(8), 1D array, ascending order
!               : Returns the rightmost insertion position of x in sorted arr
!               : arr(lo:pos-1) <= x < arr(pos:hi), pos in [lo, hi+1]
!
! Author        : Scienttysburg
! Creation Date : 2026/05/10
! Last Modified : 2026/05/10
! Version       : 1.0.0
! ==============================================================================
function bisect_right_real8_1d_direct_asc(arr, x, lo, hi) result(pos)
  ! ============================================================================
  implicit none
  ! --- Input Data -------------------------------------------------------------
  real(8),    intent(in)           :: arr(:)
  real(8),    intent(in)           :: x
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
    hi_used = int(size(arr), 8)
  end if

  ! === Binary Search ==========================================================
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

  ! === Results ================================================================
  pos = left

end function bisect_right_real8_1d_direct_asc