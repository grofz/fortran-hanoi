! Hanoi towers puzzle mini-game
!
!  ===========================================================
! | ©2026 Grof Z (grofz@vscht.cz)                             |
! | Licensed under the GNU AFFERO GENERAL PUBLIC LICENSE.     |
! | Provided "as is", without warranty.                       |
!  ===========================================================
!
! DEPENDENCIES
! raylib library and fortran bindings for raylib
! - https://www.raylib.com/
! - https://github.com/interkosmos/fortran-raylib
!
! TO-DO LIST
! - none
!
! CONTENT (public entities only)
! module hanoigui_mod
!   subroutine hanoimain()
!
module hanoigui_mod
  use raylib
  use raylib_util
  use iso_c_binding, only : cf=>c_float, c_bool, c_null_char, c_int, c_char, c_f_pointer, c_ptr
  use iso_fortran_env, only : iostat_end
  implicit none(type, external)
  private
  public hanoimain

  integer(c_int), parameter :: &
    WINDOW_WIDTH=800_c_int, WINDOW_HEIGHT=600_c_int, TARGET_FPS=30_c_int

  real(cf), parameter :: &
    MARGIN_VERTICAL = 80.0_cf, &
    RODX(3) = [WINDOW_WIDTH/4.0_cf, WINDOW_WIDTH/2.0_cf, 3*WINDOW_WIDTH/4.0_cf], &
    RODY = WINDOW_HEIGHT - MARGIN_VERTICAL, &
    ANIMATION_PEAK_Y = WINDOW_HEIGHT/5.0_cf, &            ! how high rings jump during movement
    MOVE_DURATION(3) = [0.25_cf, 0.12_cf, 0.02_cf], &     ! animation duration in seconds
    SCRIPT_STEP_DELAY(3) = [0.35_cf, 0.02_cf, 0.00_cf], & ! time between moves in the scriptfile
    RING_MAXW = WINDOW_WIDTH/5.0_cf, RING_MINW = 40.0_cf, &
    RING_HEIGHT = 25.0_cf, RING_MAXDX = 35.0_cf, &
    BASE_WIDTH = RING_MAXW+0.7_cf*RING_MAXDX, &
    BASE_HEIGHT = RING_HEIGHT/3.0_cf, &
    FOCUS_LINE_THICK = 2.0_cf

  integer, parameter :: DEFAULT_MOVING_ANIMATION_MODE = 2 ! 1-jumping or 2-straight

  type(rectangle_type), parameter :: FOCUS_RECT(3) = [ &
    rectangle_type(RODX(1)-BASE_WIDTH/2.0_cf, MARGIN_VERTICAL, BASE_WIDTH, WINDOW_HEIGHT-2*MARGIN_VERTICAL+BASE_HEIGHT+FOCUS_LINE_THICK), &
    rectangle_type(RODX(2)-BASE_WIDTH/2.0_cf, MARGIN_VERTICAL, BASE_WIDTH, WINDOW_HEIGHT-2*MARGIN_VERTICAL+BASE_HEIGHT+FOCUS_LINE_THICK), &
    rectangle_type(RODX(3)-BASE_WIDTH/2.0_cf, MARGIN_VERTICAL, BASE_WIDTH, WINDOW_HEIGHT-2*MARGIN_VERTICAL+BASE_HEIGHT+FOCUS_LINE_THICK) ]

  type(color_type), parameter :: RING_COLORS(*) = [ &
    PINK, LIME, BLUE, ORANGE, GOLD, GREEN, MAROON, RED, DARKGREEN, &
    SKYBLUE, DARKPURPLE, DARKBLUE, PURPLE, VIOLET, BEIGE, BROWN, DARKBROWN ]

  integer, parameter :: STATE_NORMAL=0, STATE_SRC_SELECTED=1, &
    STATE_WAITFORFILE=3, STATE_PLAYSCRIPT=4

  integer, parameter :: INPUT_MODE_DROP=1, INPUT_MODE_KEYBOARD=2
  integer, parameter :: SELECTED_INPUT_MODE = INPUT_MODE_DROP

  type ring_t
    type(rectangle_type) :: rect
    integer :: rod   ! rod id (1, 2 or 3)
    integer :: level ! from the bottom to the top of the rod
    logical :: is_top_ring = .false.
  end type

  type scene_t
    ! all, but selected components initialized/updated by "init" procedure
    type(ring_t), allocatable :: rings(:)
    integer :: state
    integer :: moved_ring_id
    type(rectangle_type) :: moved_ring_old_rect
    integer :: selected_source
    integer :: top_ring_id(3) ! index of the top ring on each rod
    integer :: moves_counter
    real(cf) :: timer, script_timer
    integer :: moving_animation_mode = DEFAULT_MOVING_ANIMATION_MODE ! not updated by "init"
    integer :: speed_level
    character(len=512) :: filename='' ! not updated by "init"
  end type

  interface
    ! had to write C code to get file drop feature working
    function file_path_wrapper(i, fpaths) bind(c, name='filePathWrapper')
      import :: c_ptr, c_char, c_int, file_path_list_type
      implicit none
      integer(kind=c_int), value :: i
      type(file_path_list_type), value :: fpaths
      type(c_ptr) :: file_path_wrapper
    end function
  end interface

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
            if (sc%top_ring_id(focus)/=0) then
              sc%selected_source = focus
              sc%state = STATE_SRC_SELECTED
            end if
          end if
        case(STATE_SRC_SELECTED)
          if (focus>3 .or. focus==sc%selected_source) then
            sc%state = STATE_NORMAL ! un-select
          else if (is_valid_move(sc%selected_source, focus, sc%top_ring_id)) then
            call execute_move(sc%selected_source, focus, sc)
            sc%state = STATE_NORMAL
          end if
        end select
      end if

      ! wait for user to enter filename to the console
      WAITFORFILE: if (sc%state==STATE_WAITFORFILE) then
        INIT_SCRIPT: block
          integer :: ios, nscript
          character(len=512) :: iomsg
          type(file_path_list_type), allocatable :: dropped_files
          character(len=:), allocatable :: dropped_filename

          select case(SELECTED_INPUT_MODE)
          case(INPUT_MODE_KEYBOARD) ! type filename to console
            write(*,'("HANOI - Enter file name: ")',advance='no')
            read(*,*) sc%filename
          case(INPUT_MODE_DROP) ! input filename by dropping file
            if (is_file_dropped()) then
              dropped_files = load_dropped_files()
              ! from raylib_utils
              call c_f_str_ptr(file_path_wrapper(0,dropped_files), dropped_filename)
              call unload_dropped_files(dropped_files)
              print '("Opening """,a,"""")',dropped_filename
              sc%filename = dropped_filename
            else
              ! keep waiting
              exit INIT_SCRIPT
            end if
          end select

          ! try to open file and read the number of rings from the file
          open(newunit=fid,file=sc%filename,iostat=ios,iomsg=iomsg,status='old')
          if (ios/=0) then
            ! error opening the file
            write(*,'(a)') trim(iomsg)
          else
            read(fid,*,iostat=ios,iomsg=iomsg) nscript
            if (ios/=0) then
              ! error reading rhe first line
              write(*,'(a)') trim(iomsg)
              close(fid)
            else if (nscript < 1 .or. nscript > min(size(RING_COLORS),15)) then
              ! enforce reasonable number of rings
              write(*,'("Can not play for the number of rings ",i0)') nscript
              close(fid)
            else
              ! ok to play script
              call initialize(sc, nscript)
              sc%state = STATE_PLAYSCRIPT
              exit INIT_SCRIPT
            end if
          end if
          ! error occured while processing the file, return to normal state
          print '("Could not play the script from file.")'
          sc%state = STATE_NORMAL
        end block INIT_SCRIPT
      end if WAITFORFILE

      ! animate ring movement
      if (is_ring_moved(sc)) then
        sc%timer = sc%timer + get_frame_time()
        if (sc%timer >= MOVE_DURATION(sc%speed_level)) then ! move finished
          sc%moved_ring_id = 0
        end if
      end if

      ! re-play from the file
      PLAYSCRIPT: if (sc%state==STATE_PLAYSCRIPT .and. .not. is_ring_moved(sc)) then
        sc%script_timer = sc%script_timer + get_frame_time()
        if (sc%script_timer >= SCRIPT_STEP_DELAY(sc%speed_level)) then
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
            else if (.not. is_valid_move(from, to, sc%top_ring_id)) then
              ! invalid move
              write(*,'("The move from ",i0," to ",i0," is not a valid move")') from, to
              print '("Closing file due to invalid move")'
            else
              ! step ok
              if (sc%speed_level < size(MOVE_DURATION)) &
                print '("Moving ring from ",i0," to ",i0)',from, to
              call execute_move(from, to, sc)
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
        sc%moving_animation_mode = mod(sc%moving_animation_mode,2)+1
        print '("Moving style is now ",i0)',sc%moving_animation_mode
      end if
      if (is_key_pressed(KEY_KP_ADD) .and. sc%speed_level<size(MOVE_DURATION)) then
        sc%speed_level = sc%speed_level+1
        print '("Speed is now increased to ",i0)',sc%speed_level
      end if
      if (is_key_pressed(KEY_KP_SUBTRACT) .and. sc%speed_level>1) then
        sc%speed_level = sc%speed_level-1
        print '("Speed is now decreased to ",i0)',sc%speed_level
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
        if (is_valid_move(sc%selected_source,i,sc%top_ring_id)) call draw_rectangle_lines_ex(FOCUS_RECT(i), FOCUS_LINE_THICK, BLACK)
      end if
    else if (sc%state==STATE_NORMAL) then
      if (i<=3) then
        if (sc%top_ring_id(i)/=0) call draw_rectangle_lines_ex(FOCUS_RECT(i), FOCUS_LINE_THICK, BLACK)
      end if
    end if

    ! rings
    do i=1, size(sc%rings)
      if (i /= sc%moved_ring_id) then
        call draw_rectangle_pro(sc%rings(i)%rect, &
          vector2_type(sc%rings(i)%rect%width/2.0_cf, RING_HEIGHT/2.0_cf), 0.0_cf, RING_COLORS(i))
      else
        block ! animate movement
          type(rectangle_type) :: box
          box = animate(sc%moved_ring_old_rect, sc%rings(i)%rect, sc%timer/MOVE_DURATION(sc%speed_level), &
            sc%moving_animation_mode)
          call draw_rectangle_pro(box, &
            vector2_type(box%width/2.0_cf, RING_HEIGHT/2.0_cf), 0.0_cf, RING_COLORS(i))
        end block
      end if
      ! outline of the top-most ring
      if (sc%rings(i)%is_top_ring) then
        if (sc%state==STATE_SRC_SELECTED .and. sc%selected_source==sc%rings(i)%rod) then
          call draw_rectangle_lines_ex(rectangle_type( &
            x=sc%rings(i)%rect%x-sc%rings(i)%rect%width/2.0_cf, &
            y=sc%rings(i)%rect%y-RING_HEIGHT/2.0_cf, &
            width=sc%rings(i)%rect%width, height=sc%rings(i)%rect%height), 2.0_cf, BLACK)
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
        if (is_win(sc%rings)) then
          text_width = measure_text(text, fsize)
          call draw_text(text, (WINDOW_WIDTH-text_width)/2, 150, fsize, BLACK)
          text_width = measure_text(text2, fsize2)
          call draw_text(text2, (WINDOW_WIDTH-text_width)/2, 250, fsize2, BLACK)
        else if (all(sc%rings%rod==1) .and. sc%moves_counter<1 .and. size(sc%rings)/=0) then
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
        call draw_text(text, (WINDOW_WIDTH-text_width)/2, 150, fsize, BLUE)
      end block
    else if (sc%state==STATE_WAITFORFILE) then
      block
        integer(c_int) :: text_width
        integer(c_int), parameter :: fsize = 20
        character(len=512) :: text
        select case(SELECTED_INPUT_MODE)
        case (INPUT_MODE_DROP)
          text = 'Drag and drop the script file...'//c_null_char
        case (INPUT_MODE_KEYBOARD)
          text = 'Enter file name in the console...'//c_null_char
        case default
          text = 'Internal error: input mode unknown'//c_null_char
        end select
        text_width = measure_text(text, fsize)
        call draw_text(text, (WINDOW_WIDTH-text_width)/2, 150, fsize, BLACK)
      end block
    end if
  end subroutine render_scene


  pure subroutine initialize(sc, n)
    type(scene_t), intent(inout) :: sc
    integer, intent(in) :: n

    if (allocated(sc%rings)) deallocate(sc%rings)
    allocate(sc%rings(n))

    ! place all rings at the leftmost rod
    block
      integer :: i
      real(cf) :: dx
      dx = min((RING_MAXW - RING_MINW) / (n-1), RING_MAXDX) ! difference between rings width
      do i=1, size(sc%rings)
        sc%rings(i)%rect%x = RODX(1)
        sc%rings(i)%rect%y = RODY - (i-0.5_cf)*RING_HEIGHT
        sc%rings(i)%rect%width = RING_MAXW - (i-1)*dx
        sc%rings(i)%rect%height = RING_HEIGHT
        sc%rings(i)%rod = 1
        sc%rings(i)%level = i
        sc%rings(i)%is_top_ring = .false.
      end do
    end block
    sc%rings(n)%is_top_ring = .true.

    sc%state = STATE_NORMAL
    sc%moved_ring_id = 0
    sc%moved_ring_old_rect = rectangle_type(1.2_cf, 3.4_cf, 56.7_cf, 89.0_cf)
    sc%selected_source = 0
    sc%top_ring_id = [n, 0, 0]
    sc%moves_counter = 0
    sc%timer = 0.0_cf
    sc%script_timer = 0.0_cf
    sc%speed_level = 1
  end subroutine initialize


  pure subroutine execute_move(from, to, sc)
    integer, intent(in) :: from, to
    type(scene_t), intent(inout) :: sc
!
! Move ring between rods. Update state to animate move by "render_scene"
!
    integer :: i

    if (.not. is_valid_move(from,to,sc%top_ring_id)) error stop 'Internal logic error: invalid move'

    ! mark ring to be moved
    sc%moved_ring_id = sc%top_ring_id(from)
    sc%timer = 0.0_cf

    ! find the next top ring on the originating rod
    sc%top_ring_id(from) = 0
    do i=size(sc%rings), 1, -1
      if (i==sc%moved_ring_id .or. sc%rings(i)%rod/=from) cycle
      sc%top_ring_id(from) = i
      exit
    end do

    ! unmark top on the destination rod (if exists)
    ! update position of moved ring
    if (sc%top_ring_id(to)/=0) then
       sc%rings(sc%top_ring_id(to))%is_top_ring = .false.
       sc%rings(sc%moved_ring_id)%level = sc%rings(sc%top_ring_id(to))%level + 1
    else
      sc%rings(sc%moved_ring_id)%level = 1
    end if
    sc%top_ring_id(to) = sc%moved_ring_id

    ! update "rods" and the rectangle for moved ring
    sc%rings(sc%moved_ring_id)%rod = to
    sc%moved_ring_old_rect = sc%rings(sc%moved_ring_id)%rect
    sc%rings(sc%moved_ring_id)%rect%x = RODX(to)
    sc%rings(sc%moved_ring_id)%rect%y = RODY - (sc%rings(sc%moved_ring_id)%level-0.5_cf)*RING_HEIGHT

    ! mark new top on the originating column (if exists)
    if (sc%top_ring_id(from)/=0) sc%rings(sc%top_ring_id(from))%is_top_ring = .true.

    ! update counter
    sc%moves_counter = sc%moves_counter+1
  end subroutine execute_move


  integer function get_focus() result(res)
!
! If mouse hoovers above a rod, return 1, 2 or 3.
! Return 4 if mouse is positioned elsewhere.
!
    integer :: i
    type(vector2_type) :: mpos

    mpos = get_mouse_position()
    do i=1, 3
      if (check_collision_point_rec(mpos, FOCUS_RECT(i))) exit
    end do
    res = i
  end function get_focus


  pure logical function is_valid_move(from, to, top_ring_id)
    integer, intent(in) :: from, to
    integer, intent(in) :: top_ring_id(:)

    VALIDATE: block
      ! source must exist
      if (from<1 .or. from>3) exit VALIDATE
      if (top_ring_id(from)<1) exit VALIDATE

      ! source must not be bigger than destination
      if (to<1 .or. to>3 .or. to==from) exit VALIDATE
      if (top_ring_id(to)>0) then
        if (top_ring_id(to) > top_ring_id(from)) exit VALIDATE
      end if
      is_valid_move = .true.
      return
    end block VALIDATE
    is_valid_move = .false.
  end function is_valid_move


  pure logical function is_win(rings)
    type(ring_t), intent(in) :: rings(:)
    is_win = all(rings%rod==3) .and. size(rings)/=0
  end function is_win


  pure logical function is_ring_moved(sc)
    type(scene_t), intent(in) :: sc
    is_ring_moved = sc%moved_ring_id/=0
  end function is_ring_moved


  pure type(rectangle_type) function animate(start_rect, end_rect, f, animation_mode) result(rect)
    type(rectangle_type), intent(in) :: start_rect, end_rect
    real(cf), intent(in) :: f ! move progress, between 0.0 and 1.0
    integer, intent(in) :: animation_mode

    real(cf) :: vertical_distance

    rect%width = start_rect%width
    rect%height = start_rect%height
    rect%x = start_rect%x * (1.0_cf-f) + end_rect%x * f

    select case (animation_mode)
    case(1)
      if (start_rect%x==end_rect%x) then
        vertical_distance = 0.0_cf
      else
        vertical_distance = start_rect%y + end_rect%y - (2.0_cf*ANIMATION_PEAK_Y)
      end if
      rect%y = start_rect%y - f*vertical_distance
      if (rect%y < ANIMATION_PEAK_Y) rect%y = 2.0_cf*ANIMATION_PEAK_Y - rect%y
    case(2)
      rect%y = start_rect%y * (1.0_cf-f) + end_rect%y * f
    case default
      error stop 'error - unknown animation mode'
    end select
  end function animate

end module hanoigui_mod

!EOF: hanoigui.f90
