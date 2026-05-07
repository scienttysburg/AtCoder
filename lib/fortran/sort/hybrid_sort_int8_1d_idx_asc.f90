! ==============================================================================
! Subroutine    : hybrid_sort_int8_1d_idx_asc (f90)
! ------------------------------------------------------------------------------
! Description   : Hybrid Sort (Iterative Quicksort + Insertion Sort)
!               : Index-based (indirect) sort, integer(8), 1D array, ascending order
!               : arr is unchanged; idx(k) gives the k-th smallest position
!
! Author        : Scienttysburg
! Creation Date : 2026/04/09
! Last Modified : 2026/05/07
! Version       : 1.1.0
! ==============================================================================
subroutine hybrid_sort_int8_1d_idx_asc(arr, idx)
  ! ============================================================================
  implicit none
  ! --- Constants / Parameters -------------------------------------------------
  integer(8), parameter     :: THRESHOLD = 32_8
  ! --- Input Data -------------------------------------------------------------
  integer(8), intent(in)    :: arr(:)
  integer(8), intent(inout) :: idx(:)
  ! --- Work Variables / Internal State ----------------------------------------
  integer(8)                :: stack(0:128, 2)
  integer(8)                :: top, low, high
  integer(8)                :: v0, v1, v2, tmp_val
  integer(8)                :: pivot, tmp_idx
  ! --- Loop Counters ----------------------------------------------------------
  integer(8)                :: i, j
  ! ============================================================================

  ! === Initialization =========================================================
  top           = 1_8
  stack(top, 1) = 1_8
  stack(top, 2) = int(size(arr), 8)

  ! === Loop Until Stack Is Empty ==============================================
  do while(top > 0_8)
    low  = stack(top, 1)
    high = stack(top, 2)
    top  = top - 1_8

    ! --- Switch to Insertion Sort for small partitions ------------------------
    if(high - low < THRESHOLD)then
      call insertion_sort_int8_1d_idx_asc(arr, idx, low, high)
      cycle
    end if

    ! --- Select Pivot (Median-of-3) -------------------------------------------
    v0 = arr(idx(low))
    v1 = arr(idx((low + high) / 2_8))
    v2 = arr(idx(high))

    if(v0 > v1)then
      tmp_val = v0;  v0 = v1;  v1 = tmp_val
    end if
    if(v0 > v2)then
      tmp_val = v0;  v0 = v2;  v2 = tmp_val
    end if
    if(v1 > v2)then
      tmp_val = v1;  v1 = v2;  v2 = tmp_val
    end if
    pivot = v1

    ! --- Partitioning (Hoare Partition) ---------------------------------------
    i = low
    j = high

    do
      do while(arr(idx(i)) < pivot)
        i = i + 1_8
      end do
      do while(arr(idx(j)) > pivot)
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

end subroutine hybrid_sort_int8_1d_idx_asc

! ==============================================================================
! Subroutine    : insertion_sort_int8_1d_idx_asc (f90)
! ------------------------------------------------------------------------------
! Description   : Insertion Sort
!               : Index-based (indirect) sort, integer(8), ascending order
!
! Author        : Scienttysburg
! Creation Date : 2026/04/09
! Last Modified : 2026/05/07
! Version       : 1.1.0
! ==============================================================================
subroutine insertion_sort_int8_1d_idx_asc(arr, idx, low, high)
  ! ============================================================================
  implicit none
  ! --- Input Data -------------------------------------------------------------
  integer(8), intent(in)    :: arr(:)
  integer(8), intent(inout) :: idx(:)
  integer(8), intent(in)    :: low, high
  ! --- Work Variables / Internal State ----------------------------------------
  integer(8)                :: key_val, key_idx
  ! --- Loop Counters ----------------------------------------------------------
  integer(8)                :: i, j
  ! ============================================================================

  ! === Calculate ==============================================================
  do i = low + 1_8, high
    key_idx = idx(i)
    key_val = arr(key_idx)
    if(arr(idx(i - 1_8)) > key_val)then
      idx(i) = idx(i - 1_8)
      j      = i - 1_8
      do while(j > low)
        if(arr(idx(j - 1_8)) <= key_val)exit
        idx(j) = idx(j - 1_8)
        j      = j - 1_8
      end do
      idx(j) = key_idx
    end if
  end do

end subroutine insertion_sort_int8_1d_idx_asc