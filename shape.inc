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
    Function  HitGround(board : PTBoard) : Boolean;
    Function  MaxX() : Integer;
    Function  MinX() : Integer;
    Function  MaxY() : Integer;
    Function  IsOutLeftLimit() : Boolean;
    Function  IsOutRightLimit() : Boolean;
    Function  IsAlwaysOutLimit() : Boolean;
    Function  IsOutBottomLimit() : Boolean;
    Function  Column() : Integer;
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
    i,sx,sy : Integer;
  begin

    color := TetrisColors[Self.m_type];
    SDL_SetRenderDrawColor(screen, Red(color), Green(color), Blue(color), SDL_ALPHA_OPAQUE);

    d := CELL_SIZE - 2;

    sx := LEFT + Self.m_x;
    sy := TOP + Self.m_y;
    for i:=Low(Self.v) to High(Self.v) do
    begin
         sdlRect1.x := Self.v[i].x*CELL_SIZE + sx + 1;
         sdlRect1.y := Self.v[i].y*CELL_SIZE + sy + 1;
         sdlRect1.w := d;
         sdlRect1.h := d;
         SDL_RenderFillRect(screen, @sdlRect1);
    end;

  end;

  Procedure TShape.RotateLeft();
  var
    i,x,y : integer;
  begin
    for i:=Low(Self.v) to High(Self.v) do
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
    for i:=Low(Self.v) to High(Self.v) do
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
    maxVal := Self.v[0].x;
    for i:=1 to 3 do
    begin
      x := Self.v[i].x;
      if  x>maxVal then
        maxVal := x;
    end;
    MaxX := maxVal;
  end;

  Function TShape.MinX() : Integer;
  var
    i,x,minVal : integer;
  begin
    minVal := Self.v[0].x;
    for i:=1 to 3 do
    begin
      x := Self.v[i].x;
      if  x<minVal then
        minVal := x;
    end;
    MinX := minVal;
  end;

  Function TShape.MaxY() : Integer;
  var
    i,y,maxVal : integer;
  begin
    maxVal := Self.v[0].y;
    for i:=1 to 3 do
    begin
      y := Self.v[i].y;
      if  y>maxVal then
        maxVal := y;
    end;
    MaxY := maxVal;
  end;

  Function TShape.IsOutLeftLimit() : Boolean;
  begin
    IsOutLeftLimit := (Self.MinX()*CELL_SIZE + Self.m_x)<0;
  end;

  Function TShape.IsOutRightLimit() : Boolean;
  begin
    Result := (Self.MaxX()*CELL_SIZE + CELL_SIZE + Self.m_x)>NB_COLUMNS*CELL_SIZE;
  end;

  Function TShape.IsOutBottomLimit() : Boolean;
  begin
    Result := (Self.MaxY()*CELL_SIZE + CELL_SIZE+ Self.m_y)>NB_ROWS*CELL_SIZE;
  end;

  Function TShape.IsAlwaysOutLimit() : Boolean;
  begin
    Result := true;
  end;

  Function TShape.HitGround(board : PTBoard) : Boolean;
  var
    i,x,y : integer;
    ix1,iy1 : Integer;
    Function Hit(x1:Integer;y1:Integer) : Boolean;
    var
       ix,iy : Integer;
    begin
       ix := Trunc(x1/CELL_SIZE);
       iy := Trunc(y1/CELL_SIZE);
       if (ix>=0) and (ix<NB_COLUMNS) and (iy>=0) and (iy<NB_ROWS) then
         begin
              Hit := board^[ix+iy*NB_COLUMNS]<>0;
         end
       else
         Hit := false;
    end;
  begin

    for i:=0 to 3 do
      begin

        x := Self.v[i].x*CELL_SIZE + Self.m_x + 1;
        y := Self.v[i].y*CELL_SIZE + Self.m_y + 1;
        if Hit(x,y) then
        begin
          HitGround := true;
          exit;
        end;

        x := Self.v[i].x*CELL_SIZE + CELL_SIZE - 2 + Self.m_x;
        y := Self.v[i].y*CELL_SIZE + Self.m_y + 1;
        if Hit(x,y) then
        begin
          HitGround := true;
          exit;
        end;

        x := Self.v[i].x*CELL_SIZE + CELL_SIZE - 2 + Self.m_x;
        y := Self.v[i].y*CELL_SIZE + CELL_SIZE - 2 + Self.m_y;
        if Hit(x,y) then
        begin
          HitGround := true;
          exit;
        end;

        x := Self.v[i].x*CELL_SIZE + Self.m_x + 1;
        y := Self.v[i].y*CELL_SIZE + CELL_SIZE - 2 + Self.m_y;
        if Hit(x,y) then
        begin
          HitGround := true;
          exit;
        end;

      end;

    HitGround := false;

  end;

  Function TShape.Column() : Integer;
  begin
    Column := Trunc(Self.m_x/CELL_SIZE);
  end;




