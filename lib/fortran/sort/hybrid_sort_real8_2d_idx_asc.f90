! ==============================================================================
! Subroutine    : hybrid_sort_real8_2d_idx_asc (f90)
! ------------------------------------------------------------------------------
! Description   : Hybrid Sort (Iterative Quicksort + Insertion Sort)
!               : Index-based (indirect) sort, real(8), 2D array, ascending order
!               : arr is unchanged; idx(k) gives the k-th lexicographically smallest row
!
! Author        : Scienttysburg
! Creation Date : 2026/05/07
! Last Modified : 2026/05/07
! Version       : 1.0.0
! ==============================================================================
subroutine hybrid_sort_real8_2d_idx_asc(arr, idx)
  ! ============================================================================
  implicit none
  ! --- Constants / Parameters -------------------------------------------------
  integer(8), parameter     :: THRESHOLD = 32_8
  ! --- Input Data -------------------------------------------------------------
  real(8),    intent(in)    :: arr(:, :)
  integer(8), intent(inout) :: idx(:)
  ! --- Work Variables / Internal State ----------------------------------------
  integer(8)                :: stack(0:128, 2)
  integer(8)                :: top, low, high
  integer(8)                :: p0, p1, p2, tmp_p
  real(8)                   :: pivot(int(size(arr, 2), 8))
  integer(8)                :: tmp_idx
  ! --- Loop Counters ----------------------------------------------------------
  integer(8)                :: i, j
  ! ============================================================================

  ! === Initialization =========================================================
  top           = 1_8
  stack(top, 1) = 1_8
  stack(top, 2) = int(size(arr, 1), 8)

  ! === Loop Until Stack Is Empty ==============================================
  do while(top > 0_8)
    low  = stack(top, 1)
    high = stack(top, 2)
    top  = top - 1_8

    ! --- Switch to Insertion Sort for small partitions ------------------------
    if(high - low < THRESHOLD)then
      call insertion_sort_real8_2d_idx_asc(arr, idx, low, high)
      cycle
    end if

    ! --- Select Pivot (Median-of-3) -------------------------------------------
    p0  = low
    p1  = (low + high) / 2_8
    p2  = high

    if(row_cmp_real8_2d(arr(idx(p0), :), arr(idx(p1), :)) > 0_8)then
      tmp_p = p0;  p0 = p1;  p1 = tmp_p
    end if
    if(row_cmp_real8_2d(arr(idx(p0), :), arr(idx(p2), :)) > 0_8)then
      tmp_p = p0;  p0 = p2;  p2 = tmp_p
    end if
    if(row_cmp_real8_2d(arr(idx(p1), :), arr(idx(p2), :)) > 0_8)then
      tmp_p = p1;  p1 = p2;  p2 = tmp_p
    end if
    pivot = arr(idx(p1), :)

    ! --- Partitioning (Hoare Partition) ---------------------------------------
    i = low
    j = high

    do
      do while(row_cmp_real8_2d(arr(idx(i), :), pivot) < 0_8)
        i = i + 1_8
      end do
      do while(row_cmp_real8_2d(arr(idx(j), :), pivot) > 0_8)
        j = j - 1_8
      end do
      if(i >= j)exit

      tmp_idx = idx(i)
      idx(i)  = idx(j)
      idx(j)  = tmp_idx
      i       = i + 1_8
      j       = j - 1_8
    end do

    ! --- Push Next Range To Stack ---------------------------------------------
    ! スタックの深さを O(log N) に抑えるため、要素数の「大きい方」を先に積む
    if((j - low) > (high - i))then
      ! 左側のグループが大きい場合：左を先に積み、右を後に積む（右が先に処理される）
      if(low < j)then
        top           = top + 1_8
        stack(top, 1) = low
        stack(top, 2) = j
      end if
      if(i < high)then
        top           = top + 1_8
        stack(top, 1) = i
        stack(top, 2) = high
      end if
    else
      ! 右側のグループが大きい場合：右を先に積み、左を後に積む（左が先に処理される）
      if(i < high)then
        top           = top + 1_8
        stack(top, 1) = i
        stack(top, 2) = high
      end if
      if(low < j)then
        top           = top + 1_8
        stack(top, 1) = low
        stack(top, 2) = j
      end if
    end if
  end do

end subroutine hybrid_sort_real8_2d_idx_asc

! ==============================================================================
! Function      : row_cmp_real8_2d (f90)
! ------------------------------------------------------------------------------
! Description   : Lexicographic row comparison, real(8), 2D array
!               : Returns -1 / 0 / 1 (a < b / a == b / a > b)
!
! Author        : Scienttysburg
! Creation Date : 2026/05/07
! Last Modified : 2026/05/07
! Version       : 1.0.0
! ==============================================================================
function row_cmp_real8_2d(a, b) result(res)
  ! ============================================================================
  implicit none
  ! --- Input Data -------------------------------------------------------------
  real(8), intent(in) :: a(:), b(:)
  ! --- Output Data ------------------------------------------------------------
  integer(8)          :: res
  ! --- Loop Counters ----------------------------------------------------------
  integer(8)          :: i
  ! ============================================================================

  ! === Calculate ==============================================================
  res = 0_8
  do i = 1_8, int(size(a), 8)
    if(a(i) < b(i))then
      res = -1_8
      return
    else if(a(i) > b(i))then
      res =  1_8
      return
    end if
  end do

end function row_cmp_real8_2d

! ==============================================================================
! Subroutine    : insertion_sort_real8_2d_idx_asc (f90)
! ------------------------------------------------------------------------------
! Description   : Insertion Sort
!               : Index-based (indirect) sort, real(8), 2D array, ascending order
!
! Author        : Scienttysburg
! Creation Date : 2026/05/07
! Last Modified : 2026/05/07
! Version       : 1.0.0
! ==============================================================================
subroutine insertion_sort_real8_2d_idx_asc(arr, idx, low, high)
  ! ============================================================================
  implicit none
  ! --- Input Data -------------------------------------------------------------
  real(8),    intent(in)    :: arr(:, :)
  integer(8), intent(inout) :: idx(:)
  integer(8), intent(in)    :: low, high
  ! --- Work Variables / Internal State ----------------------------------------
  real(8)                   :: key_row(int(size(arr, 2), 8))
  integer(8)                :: key_idx
  ! --- Loop Counters ----------------------------------------------------------
  integer(8)                :: i, j
  ! ============================================================================

  ! === Calculate ==============================================================
  do i = low + 1_8, high
    key_idx = idx(i)
    key_row = arr(key_idx, :)
    if(row_cmp_real8_2d(arr(idx(i - 1_8), :), key_row) > 0_8)then
      idx(i) = idx(i - 1_8)
      j      = i - 1_8
      do while(j > low)
        if(row_cmp_real8_2d(arr(idx(j - 1_8), :), key_row) <= 0_8)exit
        idx(j) = idx(j - 1_8)
        j      = j - 1_8
      end do
      idx(j) = key_idx
    end if
  end do

end subroutine insertion_sort_real8_2d_idx_asc