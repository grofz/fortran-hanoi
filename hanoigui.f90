! Hanoi towers puzzle mini-game
!
! =======================================
! | ©2026 Grof Z (grofz@vscht.cz)       |
! | Licensed under the MIT License.     |
! | Provided "as is", without warranty. |
! =======================================
!
! DEPENDENCIES
! raylib library and fortran bindings for raylib
! - https://www.raylib.com/
! - https://github.com/interkosmos/fortran-raylib
!
! TO-DO LIST
! - drop file functionality (could not get it working in Windows)
!
! CONTENT
! module hanoigui_mod
!   subroutine hanoimain()
!   subroutine render_scene(sc)
!   subroutine initialize(sc, n)
!   subroutine move(source, dest, sc)
!   integer function get_focus() result(res)
!   logical function is_valid_move(source, dest, sc)
!   logical function is_win(rods)
!   logical function is_ring_moved(sc)
!   type(rectangle_type) function animate(posb, posf, f, moving_style) result(pos)
!
module hanoigui_mod
  use raylib
  use iso_c_binding, only : cf=>c_float, c_bool, c_null_char, c_int, c_char, c_f_pointer
  use iso_fortran_env, only : iostat_end
  implicit none(type, external)
  private
  public hanoimain

  integer(c_int), parameter :: WINDOW_WIDTH=800, WINDOW_HEIGHT=600, TARGET_FPS=30

  real(cf), parameter :: &
    MARGIN_VERTICAL = 80.0_cf, &
    RODX(3) = [WINDOW_WIDTH/4.0_cf, WINDOW_WIDTH/2.0_cf, 3*WINDOW_WIDTH/4.0_cf], &
    RODY = WINDOW_HEIGHT - MARGIN_VERTICAL, &
    TURNING_POINT = WINDOW_HEIGHT/5.0_cf, &               ! how high rings jump during movement
    MOVING_TIMES(3) = [0.25_cf, 0.12_cf, 0.02_cf], &      ! animation duration in seconds
    SCRIPT_STEP_TIMES(3) = [0.35_cf, 0.02_cf, 0.00_cf], & ! time between moves in the scriptfile
    RING_MAXW = WINDOW_WIDTH/5.0_cf, RING_MINW = 40.0_cf, &
    RING_HEIGHT = 25.0_cf, RING_MAXDX = 35.0_cf, &
    BASE_WIDTH = RING_MAXW+0.7_cf*RING_MAXDX, &
    BASE_HEIGHT = RING_HEIGHT/3.0_cf, &
    FOCUS_LINE_THICK = 2.0_cf

  integer, parameter :: DEFAULT_MOVING_STYLE = 2 ! 1-jumping or 2-straight

  type(rectangle_type), parameter :: FOCUS_RECT(3) = [ &
    rectangle_type(RODX(1)-BASE_WIDTH/2.0_cf, MARGIN_VERTICAL, BASE_WIDTH, WINDOW_HEIGHT-2*MARGIN_VERTICAL+BASE_HEIGHT+FOCUS_LINE_THICK), &
    rectangle_type(RODX(2)-BASE_WIDTH/2.0_cf, MARGIN_VERTICAL, BASE_WIDTH, WINDOW_HEIGHT-2*MARGIN_VERTICAL+BASE_HEIGHT+FOCUS_LINE_THICK), &
    rectangle_type(RODX(3)-BASE_WIDTH/2.0_cf, MARGIN_VERTICAL, BASE_WIDTH, WINDOW_HEIGHT-2*MARGIN_VERTICAL+BASE_HEIGHT+FOCUS_LINE_THICK) ]

  type(color_type), parameter :: RING_COLORS(*) = [ &
    PINK, LIME, BLUE, ORANGE, GOLD, GREEN, MAROON, RED, DARKGREEN, &
    SKYBLUE, DARKPURPLE, DARKBLUE, PURPLE, VIOLET, BEIGE, BROWN, DARKBROWN ]

  integer, parameter :: STATE_NORMAL=0, STATE_SRC_SELECTED=1, &
    STATE_WAITFORFILE=3, STATE_PLAYSCRIPT=4

  type scene_t
    ! all, but selected components initialized/updated by "init" procedure
    type(rectangle_type), allocatable :: rings(:)
    integer, allocatable :: rods(:)       ! 1, 2 or 3
    integer, allocatable :: positions(:)  ! from the bottom to the top
    logical, allocatable :: istop(:)
    integer :: state
    integer :: moved_ring_id
    type(rectangle_type) :: moved_ring_old_rect
    integer :: selected_source
    integer :: top(3) ! index of the top ring on each rod
    integer :: moves_counter
    real(cf) :: timer, script_timer
    integer :: moving_style = DEFAULT_MOVING_STYLE ! not updated by "init"
    integer :: speed_id
    character(len=512) :: filename='' ! not updated by "init"
  end type

contains

  subroutine hanoimain()
    integer :: focus, fid
    type(scene_t) :: sc ! root to keep the state

    call initialize(sc, 4)
    call set_trace_log_level(LOG_WARNING)
    call init_window(WINDOW_WIDTH, WINDOW_HEIGHT, 'Hanoi Towers'//c_null_char)
    call set_target_fps(TARGET_FPS)
    CONTROL_LOOP: do while (.not. window_should_close())
      call begin_drawing()
      call clear_background(RAYWHITE)
      call render_scene(sc)
      call end_drawing()

      ! move rings by mouse clicks
      if (is_mouse_button_pressed(MOUSE_BUTTON_LEFT) .and. .not. is_ring_moved(sc)) then
        focus = get_focus()
        select case(sc%state)
        case(STATE_NORMAL) ! select source?
          if (focus<=3) then
            if (sc%top(focus)/=0) then
              sc%selected_source = focus
              sc%state = STATE_SRC_SELECTED
            end if
          end if
        case(STATE_SRC_SELECTED)
          if (focus>3 .or. focus==sc%selected_source) then
            sc%state = STATE_NORMAL ! un-select
          else if (is_valid_move(sc%selected_source, focus, sc)) then
            call move(sc%selected_source, focus, sc)
            sc%state = STATE_NORMAL
          end if
        end select
      end if

      ! wait for user to enter filename to the console
      WAITFORFILE: if (sc%state==STATE_WAITFORFILE) then
        INIT_SCRIPT: block
          integer :: ios, nscript
          character(len=512) :: iomsg
          write(*,'("HANOI - Enter file name: ")',advance='no')
          read(*,*) sc%filename
          open(newunit=fid,file=sc%filename,iostat=ios,iomsg=iomsg,status='old')
          if (ios/=0) then
            ! error opening the file
            write(*,'(a)') trim(iomsg)
          else
            read(fid,*,iostat=ios,iomsg=iomsg) nscript
            if (ios/=0) then
              ! error reading rhe first line
              write(*,'(a)') trim(iomsg)
            else if (nscript < 1 .or. nscript > 10) then
              write(*,'("Can not play for the number of rings ",i0)') nscript
            else
              ! ok to play script
              call initialize(sc, nscript)
              sc%state = STATE_PLAYSCRIPT
              exit INIT_SCRIPT
            end if
          end if
          ! error occured while processing the file, return to normal 
          print '("Could not play the script.")'
          sc%state = STATE_NORMAL
        end block INIT_SCRIPT
      end if WAITFORFILE

      ! animate ring movement
      if (is_ring_moved(sc)) then
        sc%timer = sc%timer + get_frame_time()
        if (sc%timer >= MOVING_TIMES(sc%speed_id)) then ! move finished
          sc%moved_ring_id = 0
        end if
      end if

      ! re-play from the file
      PLAYSCRIPT: if (sc%state==STATE_PLAYSCRIPT .and. .not. is_ring_moved(sc)) then
        sc%script_timer = sc%script_timer + get_frame_time()
        if (sc%script_timer >= SCRIPT_STEP_TIMES(sc%speed_id)) then
          ! read a next step from the file
          READSTEP: block
            integer :: ios, from, to
            character(len=512) :: iomsg
            read(fid,*,iostat=ios,iomsg=iomsg) from, to
            if (ios == iostat_end) then
              ! normal end of script
              print '("Closing file at the end.")'
            else if (ios /= 0) then
              ! reading error
              write(*,'(a)') trim(iomsg)
              print '("Closing file due to read error")'
            else if (.not. is_valid_move(from, to, sc)) then
              ! invalid move
              write(*,'("The move from ",i0," to ",i0," is not a valid move")') from, to 
              print '("Closing file due to invalid move")'
            else
              ! step ok
              if (sc%speed_id < size(MOVING_TIMES)) &
                print '("Moving ring from ",i0," to ",i0)',from, to
              call move(from, to, sc)
              sc%script_timer = 0.0_cf
              exit READSTEP
            end if
            ! finished reading script
            sc%state = STATE_NORMAL
            close(fid)
          end block READSTEP
        end if
      end if PLAYSCRIPT

      ! restarting the game
      if (is_key_pressed(KEY_ONE)) call initialize(sc, 1)
      if (is_key_pressed(KEY_TWO)) call initialize(sc, 2)
      if (is_key_pressed(KEY_THREE)) call initialize(sc, 3)
      if (is_key_pressed(KEY_FOUR)) call initialize(sc, 4)
      if (is_key_pressed(KEY_FIVE)) call initialize(sc, 5)
      if (is_key_pressed(KEY_SIX)) call initialize(sc, 6)
      if (is_key_pressed(KEY_SEVEN)) call initialize(sc, 7)
      if (is_key_pressed(KEY_EIGHT)) call initialize(sc, 8)
      if (is_key_pressed(KEY_NINE)) call initialize(sc, 9)

      ! modify moving style, play speed, print screen, etc...
      if (is_key_pressed(KEY_Z)) then
        sc%moving_style = mod(sc%moving_style,2)+1
        print '("Moving style is now ",i0)',sc%moving_style
      end if
      if (is_key_pressed(KEY_KP_ADD) .and. sc%speed_id<size(MOVING_TIMES)) then
        sc%speed_id = sc%speed_id+1
        print '("Speed is now increased to ",i0)',sc%speed_id
      end if
      if (is_key_pressed(KEY_KP_SUBTRACT) .and. sc%speed_id>1) then
        sc%speed_id = sc%speed_id-1
        print '("Speed is now decreased to ",i0)',sc%speed_id
      end if
      if (is_key_pressed(KEY_PRINT_SCREEN)) then
        call take_screenshot('hanoi_screenshot.png'//c_null_char)
        print '("Screenshot taken and saved to a PNG file.")'
      end if

      ! activate scripting mode
      if (is_key_pressed(KEY_A)) sc%state = STATE_WAITFORFILE

      ! cancel playing the script
      if (is_key_pressed(KEY_X) .and. sc%state==STATE_PLAYSCRIPT) then
        print '("Canceled by an user")'
        close(fid)
        sc%state = STATE_NORMAL
      end if
    end do CONTROL_LOOP

  end subroutine hanoimain


  subroutine render_scene(sc)
    type(scene_t), intent(in) :: sc
    integer :: i

    ! focus rectangle
    i = get_focus()
    if (sc%state==STATE_SRC_SELECTED) then
      call draw_rectangle_lines_ex(FOCUS_RECT(sc%selected_source), FOCUS_LINE_THICK, BLUE)
      if (i<=3 .and. i/=sc%selected_source) then
        if (is_valid_move(sc%selected_source,i,sc)) call draw_rectangle_lines_ex(FOCUS_RECT(i), FOCUS_LINE_THICK, BLACK)
      end if
    else if (sc%state==STATE_NORMAL) then
      if (i<=3) then
        if (sc%top(i)/=0) call draw_rectangle_lines_ex(FOCUS_RECT(i), FOCUS_LINE_THICK, BLACK)
      end if
    end if

    ! rings
    do i=1, size(sc%rings)
      if (i /= sc%moved_ring_id) then
        call draw_rectangle_pro(sc%rings(i), vector2_type(sc%rings(i)%width/2.0_cf, RING_HEIGHT/2.0_cf), 0.0_cf, RING_COLORS(i))
      else
        block ! animate movement
          type(rectangle_type) :: box
          box = animate(sc%moved_ring_old_rect, sc%rings(i), sc%timer/MOVING_TIMES(sc%speed_id), sc%moving_style)
          call draw_rectangle_pro(box, vector2_type(box%width/2.0_cf, RING_HEIGHT/2.0_cf), 0.0_cf, RING_COLORS(i))
        end block
      end if
      ! outline of the top-most ring
      if (sc%istop(i)) then
        if (sc%state==STATE_SRC_SELECTED .and. sc%selected_source==sc%rods(i)) then
          call draw_rectangle_lines_ex(rectangle_type(sc%rings(i)%x-sc%rings(i)%width/2.0_cf, sc%rings(i)%y-RING_HEIGHT/2.0_cf, sc%rings(i)%width, sc%rings(i)%height), &
            2.0_cf, BLACK)
        end if
      end if
    end do

    ! bases
    call draw_rectangle_pro(rectangle_type(RODX(1),RODY,base_width,BASE_HEIGHT), &
      vector2_type(base_width/2.0_cf,0.0_cf), 0.0_cf, DARKGRAY)
    call draw_rectangle_pro(rectangle_type(RODX(2),RODY,base_width,BASE_HEIGHT), &
      vector2_type(base_width/2.0_cf,0.0_cf), 0.0_cf, DARKGRAY)
    call draw_rectangle_pro(rectangle_type(RODX(3),RODY,base_width,BASE_HEIGHT), &
      vector2_type(base_width/2.0_cf,0.0_cf), 0.0_cf, DARKGRAY)

    ! win banner, starting instructions and move counter
    block
      integer(c_int) :: text_width
      integer(c_int), parameter :: fsize = 50, fsize2 = 20
      character(len=*), parameter :: text = 'Solved!'//c_null_char
      character(len=*), parameter :: text2 = 'Press a number key (1-9) to reset or press A for script mode'//c_null_char
      character(len=*), parameter :: text3 = 'Move all discs to the rightmost base'//c_null_char
      character(len=9) :: buffer
      if (sc%state==STATE_NORMAL) then
        if (is_win(sc%rods)) then
          text_width = measure_text(text, fsize)
          call draw_text(text, (WINDOW_WIDTH-text_width)/2, 150, fsize, BLACK)
          text_width = measure_text(text2, fsize2)
          call draw_text(text2, (WINDOW_WIDTH-text_width)/2, 250, fsize2, BLACK)
        else if (all(sc%rods==1) .and. sc%moves_counter<1 .and. size(sc%rods)/=0) then
          text_width = measure_text(text3, fsize2)
          call draw_text(text3, (WINDOW_WIDTH-text_width)/2, 250, fsize2, BLACK)
        end if
      end if
      write(buffer,'(i0)') sc%moves_counter
      call draw_text(trim(buffer)//c_null_char,10, fsize2, fsize2, BLACK)
    end block

    ! text in scripting mode
    if (sc%state==STATE_PLAYSCRIPT) then
      block
        integer(c_int) :: text_width
        integer(c_int), parameter :: fsize = 20
        character(len=512) :: text
        text = 'Scripting from file "'//trim(sc%filename)//'". Press X to cancel.'//c_null_char
        text_width = measure_text(text, fsize)
        call draw_text(text, (WINDOW_WIDTH-text_width)/2, 150, fsize, DARKBLUE)
      end block
    else if (sc%state==STATE_WAITFORFILE) then
      block
        integer(c_int) :: text_width
        integer(c_int), parameter :: fsize = 35
        character(len=512) :: text
        text = 'Enter file name in the console'//c_null_char
        text_width = measure_text(text, fsize)
        call draw_text(text, (WINDOW_WIDTH-text_width)/2, 150, fsize, DARKBLUE)
      end block
    end if
  end subroutine render_scene


  pure subroutine initialize(sc, n)
    type(scene_t), intent(inout) :: sc
    integer, intent(in) :: n

    if (allocated(sc%rings)) deallocate(sc%rings)
    if (allocated(sc%rods)) deallocate(sc%rods)
    if (allocated(sc%positions)) deallocate(sc%positions)
    if (allocated(sc%istop)) deallocate(sc%istop)
    allocate(sc%rings(n), sc%rods(n), sc%positions(n))
    allocate(sc%istop(n), source=.false.)

    ! place all rings at the leftmost rod
    block
      integer :: i
      real(cf) :: dx
      dx = min((RING_MAXW - RING_MINW) / (n-1), RING_MAXDX)
      do i=1, size(sc%rings)
        sc%rings(i)%x = RODX(1)
        sc%rings(i)%y = RODY - (i-0.5_cf)*RING_HEIGHT
        sc%rings(i)%width = RING_MAXW - (i-1)*dx
        sc%rings(i)%height = RING_HEIGHT
        sc%rods(i) = 1
        sc%positions(i) = i
      end do
    end block
    sc%istop(n) = .true.

    sc%state = STATE_NORMAL
    sc%moved_ring_id = 0
    sc%moved_ring_old_rect = rectangle_type(1.2_cf, 3.4_cf, 56.7_cf, 89.0_cf)
    sc%selected_source = 0
    sc%top = [n, 0, 0]
    sc%moves_counter = 0
    sc%timer = 0.0_cf
    sc%script_timer = 0.0_cf
    sc%speed_id = 1
  end subroutine initialize


  pure subroutine move(from, to, sc)
    integer, intent(in) :: from, to
    type(scene_t), intent(inout) :: sc

    integer :: i

    if (.not. is_valid_move(from,to,sc)) error stop 'ERROR ERROR should not happen'

    ! mark ring to be moved
    sc%moved_ring_id = sc%top(from)
    sc%timer = 0.0_cf

    ! find the next top ring on the originating rod
    sc%top(from) = 0
    do i=size(sc%rods), 1, -1
      if (i==sc%moved_ring_id .or. sc%rods(i)/=from) cycle
      sc%top(from) = i
      exit
    end do

    ! unmark top on the destination rod (if exists)
    ! update position of moved ring
    if (sc%top(to)/=0) then
       sc%istop(sc%top(to)) = .false.
       sc%positions(sc%moved_ring_id) = sc%positions(sc%top(to)) + 1
    else
      sc%positions(sc%moved_ring_id) = 1
    end if
    sc%top(to) = sc%moved_ring_id

    ! update "rods" and the rectangle for moved ring
    sc%rods(sc%moved_ring_id) = to
    sc%moved_ring_old_rect = sc%rings(sc%moved_ring_id)
    sc%rings(sc%moved_ring_id)%x = RODX(to)
    sc%rings(sc%moved_ring_id)%y = RODY - (sc%positions(sc%moved_ring_id)-0.5_cf)*RING_HEIGHT

    ! mark new top on the originating column (if exists)
    if (sc%top(from)/=0) sc%istop(sc%top(from)) = .true.

    ! update counter
    sc%moves_counter = sc%moves_counter+1
  end subroutine move


  integer function get_focus() result(res)
    integer :: i
    type(vector2_type) :: mpos

    mpos = get_mouse_position()
    do i=1, 3
      if (check_collision_point_rec(mpos, FOCUS_RECT(i))) exit
    end do
    res = i
  end function get_focus


  pure logical function is_valid_move(from, to, sc)
    integer, intent(in) :: from, to
    type(scene_t), intent(in) :: sc

    VALIDATE: block
      ! source must exist
      if (from<1 .or. from>3) exit VALIDATE
      if (sc%top(from)<1) exit VALIDATE

      ! source must not be bigger than destination
      if (to<1 .or. to>3 .or. to==from) exit VALIDATE
      if (sc%top(to)>0) then
        if (sc%top(to) > sc%top(from)) exit VALIDATE
      end if
      is_valid_move = .true.
      return
    end block VALIDATE
    is_valid_move = .false.
  end function is_valid_move


  pure logical function is_win(rods)
    integer, intent(in) :: rods(:)
    is_win = all(rods==3) .and. size(rods)/=0
  end function is_win


  pure logical function is_ring_moved(sc)
    type(scene_t), intent(in) :: sc
    is_ring_moved = sc%moved_ring_id/=0
  end function is_ring_moved


  pure type(rectangle_type) function animate(posb, posf, f, moving_style) result(pos)
    type(rectangle_type), intent(in) :: posb, posf
    real(cf), intent(in) :: f
    integer, intent(in) :: moving_style

    real(cf) :: vertical_distance

    pos%width = posb%width
    pos%height = posb%height
    pos%x = posb%x * (1.0_cf-f) + posf%x * f

    select case (moving_style)
    case(1)
      if (posb%x==posf%x) then
        vertical_distance = 0.0
      else
        vertical_distance = posb%y + posf%y - (2.0_cf*TURNING_POINT)
      end if
      pos%y = posb%y - f*vertical_distance
      if (pos%y < TURNING_POINT) pos%y = 2.0_cf*TURNING_POINT - pos%y
    case(2)
      pos%y = posb%y * (1.0_cf-f) + posf%y * f
    case default
      error stop 'error - unknown animation mode'
    end select
  end function animate

end module hanoigui_mod

!EOF: hanoigui.f90