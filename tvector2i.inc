type
  TVector2i = Class
      x : integer;
      y : integer;
      Constructor new(ix,iy : integer);
  end;

  Constructor TVector2i.new(ix,iy:integer);
  begin
    Self.x := ix;
    Self.y := iy;
  end;


