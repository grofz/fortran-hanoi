module hanoigui_mod
  use raylib
  use iso_c_binding, only : cf=>c_float, c_bool, c_null_char, c_int, c_char, c_f_pointer
  implicit none(type, external)

  integer(c_int), parameter :: WINDOW_WIDTH=800, WINDOW_HEIGHT=600, TARGET_FPS=30

  real(cf), parameter :: &
    MARGIN_VERTICAL = 80.0_cf, &
    AXLEX(3) = [WINDOW_WIDTH/4.0_cf, WINDOW_WIDTH/2.0_cf, 3*WINDOW_WIDTH/4.0_cf], &
    AXLEY = WINDOW_HEIGHT - MARGIN_VERTICAL, &
    TURNING_POINT = WINDOW_HEIGHT/5.0_cf, & ! how high rings jump during movement
    MOVING_TIME = 0.25_cf, &                ! animation duration in seconds
    BOX_MAXW = WINDOW_WIDTH/5.0_cf, BOX_MINW = 40.0_cf, &
    BOX_HEIGHT = 25.0_cf, BOX_MAXDX = 35.0_cf, &
    BASE_WIDTH = BOX_MAXW+0.7_cf*BOX_MAXDX, &
    BASE_HEIGHT = BOX_HEIGHT/3.0_cf, &
    FOCUS_LINE_THICK = 2.0_cf

  integer, parameter :: MOVING_STYLE_DEF = 2 ! 1-jumping or 2-straight

  type(rectangle_type), parameter :: FOCUS_RECT(3) = [ &
    rectangle_type(AXLEX(1)-BASE_WIDTH/2.0_cf, MARGIN_VERTICAL, BASE_WIDTH, WINDOW_HEIGHT-2*MARGIN_VERTICAL+BASE_HEIGHT+FOCUS_LINE_THICK), &
    rectangle_type(AXLEX(2)-BASE_WIDTH/2.0_cf, MARGIN_VERTICAL, BASE_WIDTH, WINDOW_HEIGHT-2*MARGIN_VERTICAL+BASE_HEIGHT+FOCUS_LINE_THICK), &
    rectangle_type(AXLEX(3)-BASE_WIDTH/2.0_cf, MARGIN_VERTICAL, BASE_WIDTH, WINDOW_HEIGHT-2*MARGIN_VERTICAL+BASE_HEIGHT+FOCUS_LINE_THICK) ]

  type(color_type), parameter :: box_colors(*) = [ &
    PINK, LIME, BLUE, ORANGE, GOLD, GREEN, MAROON, RED, DARKGREEN, &
    SKYBLUE, DARKPURPLE, DARKBLUE, PURPLE, VIOLET, BEIGE, BROWN, DARKBROWN ]

  integer, parameter :: STATE_NORMAL=0, STATE_SRC_SELECTED=1, STATE_MOVING=2, &
    STATE_DROP_SCRIPTFILE=3

  type scene_t
    type(rectangle_type), allocatable :: boxes(:), newboxes(:) 
    integer, allocatable :: axles(:)      ! 1, 2 or 3
    integer, allocatable :: positions(:)  ! from the bottom to the top
    logical, allocatable :: istop(:)
    integer :: state = STATE_NORMAL
    integer :: selected_source = 0
    integer :: top(3) = 0
    integer :: moves_counter = 0
    real(cf) :: timer
    integer :: moving_style = MOVING_STYLE_DEF
  end type

  type(scene_t) :: sc ! global to keep the state

  type(file_path_list_type) :: dropped_files

contains

  subroutine hanoimain()
    integer :: focus

    call initialize(4)
    call init_window(WINDOW_WIDTH, WINDOW_HEIGHT, 'Hanoi Tower puzzle using Fortran (raylib library) 1.0'//c_null_char)
    call set_target_fps(TARGET_FPS)
    do while (.not. window_should_close())
      call begin_drawing()
      call clear_background(RAYWHITE)
      call render_scene()
      call end_drawing()

      ! enter moving command
      if (is_mouse_button_pressed(MOUSE_BUTTON_LEFT)) then
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
            sc%state = STATE_NORMAL
          else if (is_valid_move(sc%selected_source, focus)) then
            call move(sc%selected_source, focus)
          else
            !sc%state = STATE_NORMAL
          end if
        end select
      end if

      ! animation
      if (sc%state==STATE_MOVING) then
        sc%timer = sc%timer + get_frame_time()
        if (sc%timer >= MOVING_TIME) then
          sc%state = STATE_NORMAL
          sc%boxes = sc%newboxes
        end if
      end if

      ! restarting the game
      if (is_key_pressed(KEY_ONE)) call initialize(1)
      if (is_key_pressed(KEY_TWO)) call initialize(2)
      if (is_key_pressed(KEY_THREE)) call initialize(3)
      if (is_key_pressed(KEY_FOUR)) call initialize(4)
      if (is_key_pressed(KEY_FIVE)) call initialize(5)
      if (is_key_pressed(KEY_SIX)) call initialize(6)
      if (is_key_pressed(KEY_SEVEN)) call initialize(7)
      if (is_key_pressed(KEY_EIGHT)) call initialize(8)
      if (is_key_pressed(KEY_NINE)) call initialize(9)
      if (is_key_pressed(KEY_Z)) then
        sc%moving_style = mod(sc%moving_style,2)+1
        print *, 'Now moving style changed to ',sc%moving_style
      end if
      if (is_key_pressed(KEY_A)) then
        call initialize(0)
        sc%state = STATE_DROP_SCRIPTFILE
      end if

      ! drop file
      if (sc%state==STATE_DROP_SCRIPTFILE) then
        if (is_file_dropped()) then
          dropped_files = load_dropped_files()
          block
            !character(kind=c_char,len=1), pointer :: paths
            integer(1), pointer :: paths(:)
            integer :: i
            call c_f_pointer(dropped_files%paths, paths, [100])! [dropped_files%capacity])
            print *, 'count ',dropped_files%count
            print *, 'capacity ',dropped_files%capacity
            do i=1, 20 !dropped_files%capacity
             !if (paths(i)==0) exit
              write(*,'(i0,1x)',advance='no') paths(i)
            end do
            write(*,*)
           !call draw_text(paths(1), 20,300,10,BLACK)
          end block
          call unload_dropped_files(dropped_files)
        end if
      end if
    end do
  end subroutine hanoimain


  subroutine render_scene()
    integer :: i

    ! focus rectangle
    i = get_focus()
    if (sc%state==STATE_SRC_SELECTED) then
      call draw_rectangle_lines_ex(FOCUS_RECT(sc%selected_source), FOCUS_LINE_THICK, BLUE)
      if (i<=3 .and. i/=sc%selected_source) then
        if (is_valid_move(sc%selected_source,i)) call draw_rectangle_lines_ex(FOCUS_RECT(i), FOCUS_LINE_THICK, BLACK)
      end if
    else if (sc%state==STATE_NORMAL) then
      if (i<=3) then
        if (sc%top(i)/=0) call draw_rectangle_lines_ex(FOCUS_RECT(i), FOCUS_LINE_THICK, BLACK)
      end if
    end if

    ! boxes
    do i=1, size(sc%boxes)
      if (sc%state /= STATE_MOVING) then
        call draw_rectangle_pro(sc%boxes(i), vector2_type(sc%boxes(i)%width/2.0_cf, BOX_HEIGHT/2.0_cf), 0.0_cf, box_colors(i))
      else
        block ! animate movement
          type(rectangle_type) :: box
          box = animate(sc%boxes(i), sc%newboxes(i), sc%timer)
          call draw_rectangle_pro(box, vector2_type(box%width/2.0_cf, BOX_HEIGHT/2.0_cf), 0.0_cf, box_colors(i))
         !call draw_rectangle_lines_ex(rectangle_type(box%x-box%width/2.0_cf, box%y-BOX_HEIGHT/2.0_cf, box%width, box%height), &
         !  2.0_cf, BLACK)
        end block
      end if
     !focus on the top-most element
      if (sc%istop(i)) then
     !  if (sc%axles(i)==hoover_axle() .and. sc%state==STATE_NORMAL) then
     !    call draw_rectangle_lines_ex(rectangle_type(sc%boxes(i)%x-sc%boxes(i)%width/2.0_cf, sc%boxes(i)%y-BOX_HEIGHT/2.0_cf, sc%boxes(i)%width, sc%boxes(i)%height), &
     !      3.0_cf, GRAY)
     !  else if (sc%state==STATE_SRC_SELECTED .and. sc%selected_source==sc%axles(i)) then
        if (sc%state==STATE_SRC_SELECTED .and. sc%selected_source==sc%axles(i)) then
          call draw_rectangle_lines_ex(rectangle_type(sc%boxes(i)%x-sc%boxes(i)%width/2.0_cf, sc%boxes(i)%y-BOX_HEIGHT/2.0_cf, sc%boxes(i)%width, sc%boxes(i)%height), &
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
      character(len=*), parameter :: text2 = 'Press a number key to reset'//c_null_char
      character(len=*), parameter :: text3 = 'Move all discs to the rightmost base'//c_null_char
      character(len=9) :: buffer
      if (is_win()) then
        text_width = measure_text(text, fsize)
        call draw_text(text, (WINDOW_WIDTH-text_width)/2, 150, fsize, BLACK)
        text_width = measure_text(text2, fsize2)
        call draw_text(text2, (WINDOW_WIDTH-text_width)/2, 250, fsize2, BLACK)
      else if (all(sc%axles==1) .and. sc%moves_counter<1 .and. size(sc%axles)/=0) then
        text_width = measure_text(text3, fsize2)
        call draw_text(text3, (WINDOW_WIDTH-text_width)/2, 250, fsize2, BLACK)
      end if
      write(buffer,'(i0)') sc%moves_counter
      call draw_text(trim(buffer)//c_null_char,10, fsize2, fsize2, BLACK)

    end block
  end subroutine render_scene


  subroutine initialize(n)
    integer, intent(in) :: n

    integer :: i
    real(cf) :: dx

    if (allocated(sc%boxes)) deallocate(sc%boxes)
    if (allocated(sc%newboxes)) deallocate(sc%newboxes)
    if (allocated(sc%axles)) deallocate(sc%axles)
    if (allocated(sc%positions)) deallocate(sc%positions)
    if (allocated(sc%istop)) deallocate(sc%istop)

    allocate(sc%boxes(n), sc%newboxes(n), sc%axles(n), sc%positions(n))
    allocate(sc%istop(n), source=.false.)

    dx = min((BOX_MAXW - BOX_MINW) / (n-1), BOX_MAXDX)
    do i=1, size(sc%boxes)
      sc%boxes(i)%x = AXLEX(1)
      sc%boxes(i)%y = AXLEY - (i-0.5_cf)*BOX_HEIGHT
      sc%boxes(i)%width = BOX_MAXW - (i-1)*dx
      sc%boxes(i)%height = BOX_HEIGHT
      sc%axles(i) = 1
      sc%positions(i) = i
    end do
    sc%istop(n) = .true.
    sc%top(1) = n
    sc%top(2:3) = 0
    sc%state = STATE_NORMAL
    sc%selected_source = 0
    sc%moves_counter = 0
    sc%newboxes = sc%boxes
  end subroutine initialize


  subroutine move(source, dest)
    integer, intent(in) :: source, dest

    integer :: moved, i

    if (.not. is_valid_move(source,dest)) then
      print *, 'ERROR ERROR'
      return
    end if
    moved = sc%top(source)

    sc%boxes = sc%newboxes
    sc%state = STATE_MOVING
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
    sc%newboxes(moved)%y = AXLEY - (sc%positions(moved)-0.5_cf)*BOX_HEIGHT

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


  logical function is_valid_move(source, dest)
    integer, intent(in) :: source, dest

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


  logical function is_win()
    is_win = all(sc%axles==3) .and. size(sc%axles)/=0
  end function


  type(rectangle_type) function animate(posb, posf, timer) result(pos)
    type(rectangle_type), intent(in) :: posb, posf
    real(cf), intent(in) :: timer

    real(cf) :: f, vertical_distance

    f = timer / MOVING_TIME
    pos%width = posb%width
    pos%height = posb%height
    pos%x = posb%x * (1.0_cf-f) + posf%x * f

    select case (sc%moving_style)
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