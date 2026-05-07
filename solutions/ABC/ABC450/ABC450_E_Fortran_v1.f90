! ==============================================================================
! Program       : ABC450_E_Fortran_v1 (f90)
! ------------------------------------------------------------------------------
! Problem       : Fibonacci String
! Contest       : ABC450 E (2026/03/21)
!
! Author        : Scienttysburg
! Creation Date : 2026/04/09
! Last Modified : 2026/05/07
! Version       : 1.0.4
! ==============================================================================
program ABC450_E_Fortran_v1
  ! ============================================================================
  implicit none
  ! --- Constants / Parameters -------------------------------------------------
  integer(8), parameter :: MAX_K = 100_8
  integer(8), parameter :: MAX_S = 10000_8
  integer(8), parameter :: INF   = 2000000000000000000_8
  ! --- Input Data -------------------------------------------------------------
  integer(8)            :: Q, L, R
  character(len=1)      :: Cq
  character(len=MAX_S)  :: X, Y
  ! --- Work Variables / Internal State ----------------------------------------
  integer(8)            :: len_x, len_y, K
  integer(8)            :: len_s(1:MAX_K)
  integer(8)            :: cnt_s(1:MAX_K, 0:25)
  integer(8)            :: pre_x(0:MAX_S, 0:25), pre_y(0:MAX_S, 0:25)
  integer(8)            :: ci_idx
  ! --- Loop Counters ----------------------------------------------------------
  integer(8)            :: i
  ! ============================================================================

  ! === Input ==================================================================
  read(*,'(A)') X
  read(*,'(A)') Y
  len_x = int(len_trim(X), 8)
  len_y = int(len_trim(Y), 8)

  ! === Initialization =========================================================
  ! Xの先頭からi文字目までに、各アルファベット(0~25)が何回出現するかを計算する
  pre_x(0, :) = 0_8
  do i = 1_8, len_x
    pre_x(i, :)      = pre_x(i-1, :)
    ci_idx           = ichar(X(i:i)) - ichar('a')
    pre_x(i, ci_idx) = pre_x(i, ci_idx) + 1_8
  end do

  ! Yについても同様に、各アルファベット(0~25)が何回出現するかを計算する
  pre_y(0, :) = 0_8
  do i = 1_8, len_y
    pre_y(i, :)      = pre_y(i-1, :)
    ci_idx           = ichar(Y(i:i)) - ichar('a')
    pre_y(i, ci_idx) = pre_y(i, ci_idx) + 1_8
  end do

  ! S_1 = X, S_2 = Y として、文字列の長さとアルファベット全体の出現回数を記録する
  len_s(1) = len_x;  cnt_s(1, :) = pre_x(len_x, :)
  len_s(2) = len_y;  cnt_s(2, :) = pre_y(len_y, :)

  ! 長さがINF（無限大の代わり）を超えるまで、各S_kの長さと文字数を事前計算する
  K = 2_8
  do while(len_s(K) < INF .and. K < MAX_K)
    K = K + 1_8

    ! オーバーフロー防止: 予測される長さがINFを超える場合はINFで打ち切る
    if(len_s(K-1_8) > INF - len_s(K-2_8))then
      len_s(K) = INF
    else
      len_s(K) = len_s(K-1_8) + len_s(K-2_8)
    end if

    ! 文字の出現回数は単純に足し合わせる
    cnt_s(K, :) = cnt_s(K-1_8, :) + cnt_s(K-2_8, :)
  end do

  ! === Calculate ==============================================================
  read(*,*) Q
  do i = 1_8, Q
    read(*,*) L, R, Cq
    ci_idx = ichar(Cq) - ichar('a')

    ! 区間 [L, R] の個数は、(先頭から R までの個数) - (先頭から L-1 までの個数) で求まる
    write(*,'(I0)') count_prefix(K, R, ci_idx) - count_prefix(K, L-1_8, ci_idx)
  end do

! ==============================================================================
contains
! ==============================================================================
! Function      : count_prefix (f90)
! ------------------------------------------------------------------------------
! Description   : Calculates the prefix sum of character 'c' for
!               : the k-th Fibonacci string.
!
! Author        : Scienttysburg
! Creation Date : 2026/04/09
! Last Modified : 2026/05/07
! Version       : 1.0.1
! ==============================================================================
function count_prefix(k_in, n_in, c) result(res)
  ! ============================================================================
  implicit none
  ! --- Input Data -------------------------------------------------------------
  integer(8), intent(in) :: k_in, n_in, c
  ! --- Output Data ------------------------------------------------------------
  integer(8)             :: res
  ! --- Work Variables / Internal State ----------------------------------------
  integer(8)             :: kk, nn
  ! ============================================================================

  ! === Initialization =========================================================
  kk  = k_in
  nn  = n_in
  res = 0_8

  ! === Calculate ==============================================================
  do
    if(nn <= 0_8)exit

    ! ベースケース到達: k=1(X) または k=2(Y) の場合は、累積和配列から一発で取得
    if(kk == 1_8)then
      res = res + pre_x(nn, c)
      exit
    else if(kk == 2_8)then
      res = res + pre_y(nn, c)
      exit
    else
      ! 探索対象 (nn) が左半分 (S_{k-1}) のみで完結する場合
      if(nn <= len_s(kk-1_8))then
        kk = kk - 1_8

      ! 探索対象が右半分 (S_{k-2}) にまではみ出す場合
      else
        ! 左半分 (S_{k-1}) に含まれる文字数をすべて足し込む
        res = res + cnt_s(kk-1_8, c)

        ! 残りの探索範囲を右半分 (S_{k-2}) のみに絞る
        nn  = nn  - len_s(kk-1_8)
        kk  = kk  - 2_8
      end if
    end if
  end do

end function count_prefix
! ==============================================================================
end program ABC450_E_Fortran_v1