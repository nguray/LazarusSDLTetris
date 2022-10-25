type
  PTShape = ^TShape;
  TShape = Class
  public
    m_type : Integer;
    m_x,m_y : Integer;
    v : Array[0..3] of TVector2i;
    Constructor create(t,ix,iy : Integer);
    Procedure Draw(screen: PSDL_Renderer);
    Procedure RotateLeft();
    Procedure RotateRight();
    Procedure Init(typ,ix,iy : Integer);
    Function  IsInBoard() : Boolean;
    Function  HitGround(board : PTBoard) : Boolean;
    Function  MaxX() : Integer;
    Function  MinX() : Integer;

  end;

  Constructor TShape.create(t,ix,iy : Integer);
  begin
    WriteLn('Class constructor TShape');
    Self.Init( t, ix, iy);

  end;

  Procedure TShape.Init(typ,ix,iy : Integer);
  begin
    Self.m_type := typ;
    Self.m_x := ix;
    Self.m_y := iy;
    case typ of
      0 :
        begin
          Self.v[0] := TVector2i.new(0,0);
          Self.v[1] := TVector2i.new(0,0);
          Self.v[2] := TVector2i.new(0,0);
          Self.v[3] := TVector2i.new(0,0);
        end;
      1 :
        begin
          Self.v[0] := TVector2i.new(0,-1);
          Self.v[1] := TVector2i.new(0,0);
          Self.v[2] := TVector2i.new(-1,0);
          Self.v[3] := TVector2i.new(-1,1);
        end;
      2 :
        begin
          Self.v[0] := TVector2i.new(0,-1);
          Self.v[1] := TVector2i.new(0,0);
          Self.v[2] := TVector2i.new(1,0);
          Self.v[3] := TVector2i.new(1,1);
        end;
      3 :
        begin
          Self.v[0] := TVector2i.new(0,-1);
          Self.v[1] := TVector2i.new(0,0);
          Self.v[2] := TVector2i.new(0,1);
          Self.v[3] := TVector2i.new(0,2);
        end;
      4 :
        begin
          Self.v[0] := TVector2i.new(-1,0);
          Self.v[1] := TVector2i.new(0,0);
          Self.v[2] := TVector2i.new(1,0);
          Self.v[3] := TVector2i.new(0,1);
        end;
      5 :
        begin
          Self.v[0] := TVector2i.new(0,0);
          Self.v[1] := TVector2i.new(1,0);
          Self.v[2] := TVector2i.new(0,1);
          Self.v[3] := TVector2i.new(1,1);
        end;
      6 :
        begin
          Self.v[0] := TVector2i.new(-1,-1);
          Self.v[1] := TVector2i.new(0,-1);
          Self.v[2] := TVector2i.new(0,0);
          Self.v[3] := TVector2i.new(0,1);
        end;
      7 :
        begin
          Self.v[0] := TVector2i.new(1,-1);
          Self.v[1] := TVector2i.new(0,-1);
          Self.v[2] := TVector2i.new(0,0);
          Self.v[3] := TVector2i.new(0,1);
        end;
    end;

  end;

  Procedure TShape.Draw(screen: PSDL_Renderer);
  var
    sdlRect1: TSDL_Rect;
    color : TColor;
    d   : Integer;
    i,y : Integer;
  begin

    color := TetrisColors[Self.m_type];
    SDL_SetRenderDrawColor(screen, Red(color), Green(color), Blue(color), SDL_ALPHA_OPAQUE);

    d := CELL_SIZE - 2;

    for i:=0 to 3 do
    begin
      y := Self.v[i].y + Self.m_y;
      if y>=0 then
      begin
        sdlRect1.x := (Self.v[i].x + Self.m_x) * CELL_SIZE + 1 + LEFT;
        sdlRect1.y := y * CELL_SIZE + 1 + TOP;
        sdlRect1.w := d;
        sdlRect1.h := d;
        SDL_RenderFillRect(screen, @sdlRect1);
      end;
    end;

  end;

  Procedure TShape.RotateLeft();
  var
    i,x,y : integer;
  begin
    for i:=0 to 3 do
    begin
      x := Self.v[i].y;
      y := -Self.v[i].x;
      Self.v[i].x := x;
      Self.v[i].y := y;

    end;
  end;

  Procedure TShape.RotateRight();
  var
    i,x,y : integer;
  begin
    for i:=0 to 3 do
    begin
      x := -Self.v[i].y;
      y := Self.v[i].x;
      Self.v[i].x := x;
      Self.v[i].y := y;

    end;
  end;

  Function TShape.MaxX() : Integer;
  var
    i,x,maxVal : integer;
  begin
    maxVal := Self.v[0].x + Self.m_x;
    for i:=1 to 3 do
    begin
      x := Self.v[i].x+Self.m_x;
      if  x>maxVal then
        maxVal := x;
    end;
    MaxX := maxVal;
  end;

  Function TShape.MinX() : Integer;
  var
    i,x,minVal : integer;
  begin
    minVal := Self.v[0].x + Self.m_x;
    for i:=1 to 3 do
    begin
      x := Self.v[i].x+Self.m_x;
      if  x<minVal then
        minVal := x;
    end;
    MinX := minVal;
  end;

  Function TShape.IsInBoard() : Boolean;
  var
    i,x,y : integer;
  begin
    IsInBoard := true;
    for i:=0 to 3 do
    begin
      x := Self.v[i].x + Self.m_x;
      y := Self.v[i].y + Self.m_y;
      if (x<0) or (x>=NB_COLUMNS) or (y>=NB_ROWS) then
        begin
          IsInBoard := false;
          break;
        end;
    end;
  end;

  Function TShape.HitGround(board : PTBoard) : Boolean;
  var
    i,x,y : integer;
  begin
    HitGround := false;
    for i:=0 to 3 do
    begin
      x := Self.v[i].x + Self.m_x;
      y := Self.v[i].y + Self.m_y;
      if board^[x+y*NB_COLUMNS]<>0 then
      begin
        HitGround := true;
        break;
      end;
    end;
  end;



