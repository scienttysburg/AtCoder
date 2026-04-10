! ==============================================================================
! Program       : ABC450_C_Fortran_v1 (f90)
! ------------------------------------------------------------------------------
! Problem       : Puddles
! Contest       : ABC450 C (2026/03/21)
!
! Author        : Scienttysburg
! Creation Date : 2026/04/09
! Last Modified : 2026/04/09
! Version       : 1.0.2
! ==============================================================================
program ABC450_C_Fortran_v1
  ! ============================================================================
  implicit none
  ! --- Constants / Parameters -------------------------------------------------
  integer(8), parameter :: MAX_H = 1000_8
  integer(8), parameter :: MAX_W = 1000_8
  integer(8), parameter :: MAX_Q = MAX_H * MAX_W
  integer(8), parameter :: dr(4) = (/ 0_8, 0_8, 1_8, -1_8 /)  ! delta row
  integer(8), parameter :: dc(4) = (/ 1_8, -1_8, 0_8, 0_8 /)  ! delta column
  ! --- Input Data -------------------------------------------------------------
  integer(8)            :: H, W
  character(len=1)      :: grid(1:MAX_H, 1:MAX_W)
  ! --- Output / Results -------------------------------------------------------
  integer(8)            :: ans
  ! --- Work Variables / Internal State ----------------------------------------
  logical               :: visited(1:MAX_H, 1:MAX_W)
  integer(8)            :: qi(1:MAX_Q), qj(1:MAX_Q)
  integer(8)            :: qhead, qtail
  logical               :: on_boundary
  character(len=1000)   :: line
  integer(8)            :: ni, nj
  ! --- Loop Counters ----------------------------------------------------------
  integer(8)            :: i, j, k
  ! ============================================================================

  ! === Input ==================================================================
  read(*,*) H, W
  do i = 1_8, H
    read(*,'(A)') line
    do j = 1_8, W
      grid(i, j) = line(j:j)
    end do
  end do

  ! === Initialization =========================================================
  visited(:, :) = .false.
  ans           = 0_8

  ! === Calculate ==============================================================
  ! グリッド全体をスキャンする
  do i = 1_8, H
    do j = 1_8, W

      ! 未訪問の「.」を発見したら、幅優先探索(BFS)を開始する
      if(grid(i, j) == '.' .and. .not. visited(i, j))then
        ! 探索開始地点をキューに登録し、初期化する
        qhead = 1_8; qtail = 1_8
        qi(1) = i;   qj(1) = j
        visited(i, j) = .true.

        ! 開始地点がグリッドの境界に接しているか判定する
        on_boundary = (i == 1_8 .or. i == H .or. j == 1_8 .or. j == W)

        ! つながっている「.」を全て探索し終えるまでキューを処理する
        do while(qhead <= qtail)

          ! 上下左右の4方向を調べる
          do k = 1_8, 4_8
            ni = qi(qhead) + dr(k)
            nj = qj(qhead) + dc(k)

            ! 隣接するマスがグリッドの範囲内にあることを確認する
            if(ni >= 1_8 .and. ni <= H .and. nj >= 1_8 .and. nj <= W)then

              ! それが未訪問の「.」なら、訪問済みにした上でキューに追加する
              if(grid(ni, nj) == '.' .and. .not. visited(ni, nj))then
                visited(ni, nj) = .true.
                qtail           = qtail + 1_8
                qi(qtail)       = ni
                qj(qtail)       = nj

                ! この「.」の一部でも境界に接していれば、フラグを立てる
                if(ni == 1_8 .or. ni == H .or. nj == 1_8 .or. nj == W)then
                  on_boundary = .true.
                end if
              end if
            end if
          end do

          ! キューの次のマスへ進む
          qhead = qhead + 1_8
        end do

        ! この「.」全体が境界に一切接していなければ、条件を満たすためカウントする
        if(.not. on_boundary) ans = ans + 1_8
      end if
    end do
  end do

  ! === Results ================================================================
  write(*,'(I0)') ans

! ==============================================================================
end program ABC450_C_Fortran_v1