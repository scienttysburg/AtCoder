! ==============================================================================
! Program       : ABC450_D_Fortran_v1 (f90)
! ------------------------------------------------------------------------------
! Problem       : Minimize Range
! Contest       : ABC450 D (2026/03/21)
!
! Author        : Scienttysburg
! Creation Date : 2026/04/09
! Last Modified : 2026/05/07
! Version       : 1.1.0
! ==============================================================================
program ABC450_D_Fortran_v1
  ! ============================================================================
  implicit none
  ! --- Constants / Parameters -------------------------------------------------
  integer(8), parameter :: MAX_N = 200000_8
  integer(8), parameter :: MAX_D = 2_8 * MAX_N
  ! --- Input Data -------------------------------------------------------------
  integer(8)            :: N, K
  integer(8)            :: A(1:MAX_N)
  ! --- Output / Results -------------------------------------------------------
  integer(8)            :: ans
  ! --- Work Variables / Internal State ----------------------------------------
  integer(8)            :: dq(1:MAX_D)
  integer(8)            :: head, tail
  ! --- Loop Counters ----------------------------------------------------------
  integer(8)            :: i
  ! ============================================================================

  ! === Input ==================================================================
  read(*,*) N, K
  read(*,*) (A(i), i = 1_8, N)

  ! === Calculate ==============================================================
  A(1:N) = mod(A(1:N), K)
  call hybrid_sort_int8_direct_asc(A(1:N))

  dq(1:N) = A(1:N)
  head    = 1_8
  tail    = N
  ans     = dq(tail) - dq(head)
  do i = 1_8, N-1_8
    dq(tail+1_8) = dq(head) + K
    head         = head + 1_8
    tail         = tail + 1_8
    ans          = min(ans, dq(tail) - dq(head))
  end do

  ! === Results ================================================================
  write(*,'(I0)') ans

! ==============================================================================
contains
! ==============================================================================
! Subroutine    : hybrid_sort_int8_1d_direct_asc (f90)
! ------------------------------------------------------------------------------
! Description   : Hybrid Sort (Iterative Quicksort + Insertion Sort)
!               : Direct swap, integer(8), 1D array, ascending order
!
! Author        : Scienttysburg
! Creation Date : 2026/04/09
! Last Modified : 2026/05/07
! Version       : 1.1.0
! ==============================================================================
subroutine hybrid_sort_int8_1d_direct_asc(arr)
  ! ============================================================================
  implicit none
  ! --- Constants / Parameters -------------------------------------------------
  integer(8), parameter     :: THRESHOLD = 32_8
  ! --- Input Data -------------------------------------------------------------
  integer(8), intent(inout) :: arr(:)
  ! --- Work Variables / Internal State ----------------------------------------
  integer(8)                :: stack(0:128, 2)
  integer(8)                :: top, low, high
  integer(8)                :: v0, v1, v2, tmp_val
  integer(8)                :: pivot
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
      call insertion_sort_int8_1d_direct_asc(arr, low, high)
      cycle
    end if

    ! --- Select Pivot (Median-of-3) -------------------------------------------
    v0 = arr(low)
    v1 = arr((low + high) / 2_8)
    v2 = arr(high)

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
      do while(arr(i) < pivot)
        i = i + 1_8
      end do
      do while(arr(j) > pivot)
        j = j - 1_8
      end do
      if(i >= j)exit

      tmp_val = arr(i)
      arr(i)  = arr(j)
      arr(j)  = tmp_val
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

end subroutine hybrid_sort_int8_1d_direct_asc

! ==============================================================================
! Subroutine    : insertion_sort_int8_1d_direct_asc (f90)
! ------------------------------------------------------------------------------
! Description   : Insertion Sort
!               : Direct swap, integer(8), ascending order
!
! Author        : Scienttysburg
! Creation Date : 2026/04/09
! Last Modified : 2026/05/07
! Version       : 1.1.0
! ==============================================================================
subroutine insertion_sort_int8_1d_direct_asc(arr, low, high)
  ! ============================================================================
  implicit none
  ! --- Input Data -------------------------------------------------------------
  integer(8), intent(inout) :: arr(:)
  integer(8), intent(in)    :: low, high
  ! --- Work Variables / Internal State ----------------------------------------
  integer(8)                :: key_val
  ! --- Loop Counters ----------------------------------------------------------
  integer(8)                :: i, j
  ! ============================================================================

  ! === Calculate ==============================================================
  do i = low + 1_8, high
    key_val = arr(i)
    if(arr(i - 1_8) > key_val)then
      arr(i) = arr(i - 1_8)
      j      = i - 1_8
      do while(j > low)
        if(arr(j - 1_8) <= key_val)exit
        arr(j) = arr(j - 1_8)
        j      = j - 1_8
      end do
      arr(j) = key_val
    end if
  end do

end subroutine insertion_sort_int8_1d_direct_asc
! ==============================================================================
end program ABC450_D_Fortran_v1