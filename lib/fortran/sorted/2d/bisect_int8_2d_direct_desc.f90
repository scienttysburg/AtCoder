! ==============================================================================
! Function      : bisect_left_int8_2d_direct_desc (f90)
! ------------------------------------------------------------------------------
! Description   : Binary search (bisect left), integer(8), 2D array, descending order
!               : Row-wise lexicographic search on a sorted 2D array
!               : Returns the leftmost index where row x can be inserted
!
! Author        : Scienttysburg
! Creation Date : 2026/05/11
! Last Modified : 2026/05/11
! Version       : 1.0.0
! ==============================================================================
function bisect_left_int8_2d_direct_desc(arr, x, lo, hi, ord) result(pos)
  ! ============================================================================
  implicit none
  ! --- Input Data -------------------------------------------------------------
  integer(8), intent(in)           :: arr(:, :)
  integer(8), intent(in)           :: x(:)
  integer(8), intent(in), optional :: lo, hi
  integer(8), intent(in), optional :: ord(:)
  ! --- Output Data ------------------------------------------------------------
  integer(8)                       :: pos
  ! --- Work Variables / Internal State ----------------------------------------
  integer(8), allocatable          :: ord_used(:)
  integer(8)                       :: col
  integer(8)                       :: lo_used, hi_used
  integer(8)                       :: left, right, mid
  ! --- Loop Counters ----------------------------------------------------------
  integer(8)                       :: i
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
    hi_used = int(size(arr, 1), 8)
  end if

  ! === Resolve Column Order ===================================================
  if(present(ord))then
    allocate(ord_used(1:int(size(ord), 8)))
    ord_used(:) = ord(:)
  else
    col = int(size(arr, 2), 8)
    allocate(ord_used(1:col))
    do i = 1_8, col
      ord_used(i) = i
    end do
  end if

  ! === Binary Search ==========================================================
  left  = lo_used
  right = hi_used + 1_8

  do while(left < right)
    mid = (left + right) / 2_8
    if(row_cmp_int8_2d(arr(mid, :), x, ord_used) > 0_8)then
      left  = mid + 1_8
    else
      right = mid
    end if
  end do

  ! === Results ================================================================
  pos = left

end function bisect_left_int8_2d_direct_desc

! ==============================================================================
! Function      : bisect_right_int8_2d_direct_desc (f90)
! ------------------------------------------------------------------------------
! Description   : Binary search (bisect right), integer(8), 2D array, descending order
!               : Row-wise lexicographic search on a sorted 2D array
!               : Returns the rightmost index where row x can be inserted
!
! Author        : Scienttysburg
! Creation Date : 2026/05/11
! Last Modified : 2026/05/11
! Version       : 1.0.0
! ==============================================================================
function bisect_right_int8_2d_direct_desc(arr, x, lo, hi, ord) result(pos)
  ! ============================================================================
  implicit none
  ! --- Input Data -------------------------------------------------------------
  integer(8), intent(in)           :: arr(:, :)
  integer(8), intent(in)           :: x(:)
  integer(8), intent(in), optional :: lo, hi
  integer(8), intent(in), optional :: ord(:)
  ! --- Output Data ------------------------------------------------------------
  integer(8)                       :: pos
  ! --- Work Variables / Internal State ----------------------------------------
  integer(8), allocatable          :: ord_used(:)
  integer(8)                       :: col
  integer(8)                       :: lo_used, hi_used
  integer(8)                       :: left, right, mid
  ! --- Loop Counters ----------------------------------------------------------
  integer(8)                       :: i
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
    hi_used = int(size(arr, 1), 8)
  end if

  ! === Resolve Column Order ===================================================
  if(present(ord))then
    allocate(ord_used(1:int(size(ord), 8)))
    ord_used(:) = ord(:)
  else
    col = int(size(arr, 2), 8)
    allocate(ord_used(1:col))
    do i = 1_8, col
      ord_used(i) = i
    end do
  end if

  ! === Binary Search ==========================================================
  left  = lo_used
  right = hi_used + 1_8

  do while(left < right)
    mid = (left + right) / 2_8
    if(row_cmp_int8_2d(arr(mid, :), x, ord_used) >= 0_8)then
      left  = mid + 1_8
    else
      right = mid
    end if
  end do

  ! === Results ================================================================
  pos = left

end function bisect_right_int8_2d_direct_desc

! ==============================================================================
! Function      : row_cmp_int8_2d (f90)
! ------------------------------------------------------------------------------
! Description   : Lexicographic row comparison, integer(8), 2D array
!               : Returns -1 / 0 / 1 (a < b / a == b / a > b)
!               : Columns are compared in the order specified by ord
!
! Author        : Scienttysburg
! Creation Date : 2026/05/07
! Last Modified : 2026/05/10
! Version       : 1.1.0
! ==============================================================================
function row_cmp_int8_2d(a, b, ord) result(res)
  ! ============================================================================
  implicit none
  ! --- Input Data -------------------------------------------------------------
  integer(8), intent(in) :: a(:), b(:), ord(:)
  ! --- Output Data ------------------------------------------------------------
  integer(8)             :: res
  ! --- Work Variables / Internal State ----------------------------------------
  integer(8)             :: o
  ! --- Loop Counters ----------------------------------------------------------
  integer(8)             :: i
  ! ============================================================================

  ! === Calculate ==============================================================
  res = 0_8
  do i = 1_8, int(size(ord), 8)
    o = ord(i)
    if(a(o) < b(o))then
      res = -1_8
      return
    else if(a(o) > b(o))then
      res =  1_8
      return
    end if
  end do

end function row_cmp_int8_2d