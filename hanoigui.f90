module hanoigui_mod
  use raylib
  use iso_c_binding, only : cf=>c_float, c_bool, c_null_char, c_int, c_char, c_f_pointer
  use iso_fortran_env, only : iostat_end
  implicit none(type, external)

  integer(c_int), parameter :: WINDOW_WIDTH=800, WINDOW_HEIGHT=600, TARGET_FPS=30

  real(cf), parameter :: &
    MARGIN_VERTICAL = 80.0_cf, &
    AXLEX(3) = [WINDOW_WIDTH/4.0_cf, WINDOW_WIDTH/2.0_cf, 3*WINDOW_WIDTH/4.0_cf], &
    AXLEY = WINDOW_HEIGHT - MARGIN_VERTICAL, &
    TURNING_POINT = WINDOW_HEIGHT/5.0_cf, &               ! how high rings jump during movement
    MOVING_TIMES(3) = [0.25_cf, 0.12_cf, 0.02_cf], &      ! animation duration in seconds
    SCRIPT_STEP_TIMES(3) = [0.35_cf, 0.02_cf, 0.00_cf], & ! time between moves in the scriptfile
    RING_MAXW = WINDOW_WIDTH/5.0_cf, RING_MINW = 40.0_cf, &
    RING_HEIGHT = 25.0_cf, RING_MAXDX = 35.0_cf, &
    BASE_WIDTH = RING_MAXW+0.7_cf*RING_MAXDX, &
    BASE_HEIGHT = RING_HEIGHT/3.0_cf, &
    FOCUS_LINE_THICK = 2.0_cf

  integer, parameter :: MOVING_STYLE_DEF = 2 ! 1-jumping or 2-straight

  type(rectangle_type), parameter :: FOCUS_RECT(3) = [ &
    rectangle_type(AXLEX(1)-BASE_WIDTH/2.0_cf, MARGIN_VERTICAL, BASE_WIDTH, WINDOW_HEIGHT-2*MARGIN_VERTICAL+BASE_HEIGHT+FOCUS_LINE_THICK), &
    rectangle_type(AXLEX(2)-BASE_WIDTH/2.0_cf, MARGIN_VERTICAL, BASE_WIDTH, WINDOW_HEIGHT-2*MARGIN_VERTICAL+BASE_HEIGHT+FOCUS_LINE_THICK), &
    rectangle_type(AXLEX(3)-BASE_WIDTH/2.0_cf, MARGIN_VERTICAL, BASE_WIDTH, WINDOW_HEIGHT-2*MARGIN_VERTICAL+BASE_HEIGHT+FOCUS_LINE_THICK) ]

  type(color_type), parameter :: box_colors(*) = [ &
    PINK, LIME, BLUE, ORANGE, GOLD, GREEN, MAROON, RED, DARKGREEN, &
    SKYBLUE, DARKPURPLE, DARKBLUE, PURPLE, VIOLET, BEIGE, BROWN, DARKBROWN ]

  integer, parameter :: STATE_NORMAL=0, STATE_SRC_SELECTED=1, &
    STATE_WAITFORFILE=3, STATE_PLAYSCRIPT=4

  type scene_t
    type(rectangle_type), allocatable :: boxes(:), newboxes(:) 
    integer, allocatable :: axles(:)      ! 1, 2 or 3
    integer, allocatable :: positions(:)  ! from the bottom to the top
    logical, allocatable :: istop(:)
    integer :: state
    logical :: is_ring_moved
    integer :: selected_source = 0
    integer :: top(3) = 0
    integer :: moves_counter = 0
    real(cf) :: timer, script_timer
    integer :: moving_style = MOVING_STYLE_DEF
    integer :: speed_id
    character(len=512) :: filename=''
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
      if (is_mouse_button_pressed(MOUSE_BUTTON_LEFT) .and. .not. sc%is_ring_moved) then
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

      ! wait for user to enter name to the console
      if (sc%state==STATE_WAITFORFILE) then
        INIT_SCRIPT: block
          integer :: ios, nscript
          character(len=512) :: iomsg
          write(*,'("HANOI - Enter file name: ")',advance='no')
          read(*,*) sc%filename
          open(newunit=fid,file=sc%filename,iostat=ios,iomsg=iomsg,status='old')
          if (ios/=0) then
            write(*,'(a)') trim(iomsg)
            exit INIT_SCRIPT
          end if
          read(fid,*,iostat=ios,iomsg=iomsg) nscript
          if (ios/=0) then
            write(*,'(a)') trim(iomsg)
            exit INIT_SCRIPT
          else if (nscript < 1 .or. nscript > 10) then
            write(*,'("Can not play for the number of rings ",i0)') nscript
            exit INIT_SCRIPT
          end if
          ! ready to play script
          call initialize(sc, nscript)
          sc%state = STATE_PLAYSCRIPT
          sc%script_timer = 0.0_cf
        end block INIT_SCRIPT
        ! error occured in INIT_SCRIPT, recover 
        if (sc%state==STATE_WAITFORFILE) then
          print '("Could not play the script.")'
          sc%state = STATE_NORMAL
        end if
      end if

      ! animate ring movement
      if (sc%is_ring_moved) then
        sc%timer = sc%timer + get_frame_time()
        if (sc%timer >= MOVING_TIMES(sc%speed_id)) then ! move finished
          sc%is_ring_moved = .false.
          sc%boxes = sc%newboxes
        end if
      end if

      ! re-play from the file
      if (sc%state==STATE_PLAYSCRIPT .and. .not. sc%is_ring_moved) then
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
      end if

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
      if (is_key_pressed(KEY_PRINT_SCREEN)) call take_screenshot('hanoi_screenshot.png'//c_null_char)

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
    do i=1, size(sc%boxes)
      if (.not. sc%is_ring_moved) then
        call draw_rectangle_pro(sc%boxes(i), vector2_type(sc%boxes(i)%width/2.0_cf, RING_HEIGHT/2.0_cf), 0.0_cf, box_colors(i))
      else
        block ! animate movement
          type(rectangle_type) :: box
          box = animate(sc%boxes(i), sc%newboxes(i), sc%timer/MOVING_TIMES(sc%speed_id), sc%moving_style)
          call draw_rectangle_pro(box, vector2_type(box%width/2.0_cf, RING_HEIGHT/2.0_cf), 0.0_cf, box_colors(i))
        end block
      end if
      ! outline of the top-most ring
      if (sc%istop(i)) then
        if (sc%state==STATE_SRC_SELECTED .and. sc%selected_source==sc%axles(i)) then
          call draw_rectangle_lines_ex(rectangle_type(sc%boxes(i)%x-sc%boxes(i)%width/2.0_cf, sc%boxes(i)%y-RING_HEIGHT/2.0_cf, sc%boxes(i)%width, sc%boxes(i)%height), &
            2.0_cf, BLACK)
        end if
      end if
    end do

    ! bases
    call draw_rectangle_pro(rectangle_type(AXLEX(1),AXLEY,base_width,BASE_HEIGHT), &
      vector2_type(base_width/2.0_cf,0.0_cf), 0.0_cf, DARKGRAY)
    call draw_rectangle_pro(rectangle_type(AXLEX(2),AXLEY,base_width,BASE_HEIGHT), &
      vector2_type(base_width/2.0_cf,0.0_cf), 0.0_cf, DARKGRAY)
    call draw_rectangle_pro(rectangle_type(AXLEX(3),AXLEY,base_width,BASE_HEIGHT), &
      vector2_type(base_width/2.0_cf,0.0_cf), 0.0_cf, DARKGRAY)

    ! win banner and move counter
    block
      integer(c_int) :: text_width
      integer(c_int), parameter :: fsize = 50, fsize2 = 20
      character(len=*), parameter :: text = 'Solved!'//c_null_char
      character(len=*), parameter :: text2 = 'Press a number key (1-9) to reset or press A for script mode'//c_null_char
      character(len=*), parameter :: text3 = 'Move all discs to the rightmost base'//c_null_char
      character(len=9) :: buffer
      if (sc%state==STATE_NORMAL) then
        if (is_win(sc%axles)) then
          text_width = measure_text(text, fsize)
          call draw_text(text, (WINDOW_WIDTH-text_width)/2, 150, fsize, BLACK)
          text_width = measure_text(text2, fsize2)
          call draw_text(text2, (WINDOW_WIDTH-text_width)/2, 250, fsize2, BLACK)
        else if (all(sc%axles==1) .and. sc%moves_counter<1 .and. size(sc%axles)/=0) then
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


  subroutine initialize(sc, n)
    type(scene_t), intent(inout) :: sc
    integer, intent(in) :: n

    if (allocated(sc%boxes)) deallocate(sc%boxes)
    if (allocated(sc%newboxes)) deallocate(sc%newboxes)
    if (allocated(sc%axles)) deallocate(sc%axles)
    if (allocated(sc%positions)) deallocate(sc%positions)
    if (allocated(sc%istop)) deallocate(sc%istop)
    allocate(sc%boxes(n), sc%newboxes(n), sc%axles(n), sc%positions(n))
    allocate(sc%istop(n), source=.false.)

    block
      integer :: i
      real(cf) :: dx
      dx = min((RING_MAXW - RING_MINW) / (n-1), RING_MAXDX)
      do i=1, size(sc%boxes)
        sc%boxes(i)%x = AXLEX(1)
        sc%boxes(i)%y = AXLEY - (i-0.5_cf)*RING_HEIGHT
        sc%boxes(i)%width = RING_MAXW - (i-1)*dx
        sc%boxes(i)%height = RING_HEIGHT
        sc%axles(i) = 1
        sc%positions(i) = i
      end do
    end block

    sc%istop(n) = .true.
    sc%top(1) = n
    sc%top(2:3) = 0
    sc%state = STATE_NORMAL
    sc%selected_source = 0
    sc%moves_counter = 0
    sc%newboxes = sc%boxes
    sc%is_ring_moved = .false.
    sc%speed_id = 1
  end subroutine initialize


  subroutine move(source, dest, sc)
    integer, intent(in) :: source, dest
    type(scene_t), intent(inout) :: sc

    integer :: moved, i

    if (.not. is_valid_move(source,dest,sc)) then
      print *, 'ERROR ERROR should not happen'
      return
    end if
    moved = sc%top(source)

    sc%boxes = sc%newboxes
    sc%is_ring_moved = .true.
    sc%timer = 0.0_cf

    ! find new top in source column
    sc%top(source) = 0
    do i=size(sc%axles), 1, -1
      if (i==moved .or. sc%axles(i)/=source) cycle
      sc%top(source) = i
      exit
    end do

    ! unmark top in destination column (if exists)
    ! update position of moved ring
    if (sc%top(dest)/=0) then
       sc%istop(sc%top(dest)) = .false.
       sc%positions(moved) = sc%positions(sc%top(dest)) + 1
    else
      sc%positions(moved) = 1
    end if
    sc%top(dest) = moved

    ! update axles and position of moved ring
    sc%axles(moved) = dest
    sc%newboxes(moved)%x = AXLEX(dest)
    sc%newboxes(moved)%y = AXLEY - (sc%positions(moved)-0.5_cf)*RING_HEIGHT

    ! mark new top in source column (if exists)
    if (sc%top(source)/=0) sc%istop(sc%top(source)) = .true.

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
  end function


  logical function is_valid_move(source, dest, sc)
    integer, intent(in) :: source, dest
    type(scene_t), intent(in) :: sc

    VALIDATE: block
      ! source must exist
      if (source<1 .or. source>3) exit VALIDATE
      if (sc%top(source)<1) exit VALIDATE

      ! source must not be bigger than destination
      if (dest<1 .or. dest>3 .or. dest==source) exit VALIDATE
      if (sc%top(dest)>0) then
        if (sc%top(dest) > sc%top(source)) exit VALIDATE
      end if
      is_valid_move = .true.
      return
    end block VALIDATE
    is_valid_move = .false.
  end function is_valid_move


  logical function is_win(axles)
    integer, intent(in) :: axles(:)
    is_win = all(axles==3) .and. size(axles)/=0
  end function


  type(rectangle_type) function animate(posb, posf, f, moving_style) result(pos)
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
      print *, 'error - not implemented'
    end select
  end function animate

end module