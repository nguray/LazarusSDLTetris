program SDLTetris;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
   fgl,classes,SysUtils,FileUtil,Graphics,SDL2,SDL2_ttf,SDL2_mixer
  { you can add units after this };

const

  SCREEN_WIDTH  = 480;
  SCREEN_HEIGHT = 560;
  NB_ROWS = 20;
  NB_COLUMNS = 12;
  CELL_SIZE = Trunc(SCREEN_WIDTH/(NB_COLUMNS+7));
  TOP  = 10;
  LEFT = 10;

  TetrisColors : Array[0..7] of TColor =
    (
     TColor($000000),
     TColor($6060FF),
     TColor($60FF66),
     TColor($FF6060),
     TColor($60CCCC),
     TColor($CC60CC),
     TColor($CCCC60),
     TColor($00AADA)
    );


type

  PSDL_bool = ^TSDL_bool;

  PTBoard = ^TBoard;
  TBoard = Array[0..NB_ROWS*NB_COLUMNS] of Integer;
  GameMode = (STAND_BY,PLAY,GAME_OVER,HIGHT_SCORES);

  TMethodPtr = procedure(done : PSDL_bool) of object;
  TFuncPtr = function():integer of object;

  TDictKeys  = specialize TFPGMap<Integer, String>;

{$I tvector2i.inc}

{$I shape.inc}


type
  THightScore = Class
    m_name  : String;
    m_score : Integer;
    Constructor create(name : string;score : Integer);

  end;
  Constructor THightScore.create(name : string;score : Integer);
  begin
    Self.m_name := name;
    Self.m_score := score;
  end;

type
  PTGame = ^TGame;
  TGame = Class
    board      : TBoard;
    score      : Integer;
    playerName : String;
    mode       : GameMode;
    VeloH      : Integer;
    fFastDown  : boolean;
    fDropTetromino : boolean;
    event          : TSDL_Event;
    processEvent   : TMethodPtr;
    curTetromino   : TShape;
    nextTetromino  : TShape;
    ttfFont : PTTF_Font;
    bag     : Array [1..7] of Integer;
    hightScores  : TList;
    idHightScore : Integer;

    dictKeys : TDictKeys;

    Sound: PMix_Chunk;

    Constructor Create();
    Destructor Free();
    Procedure FreezeTetromino();

    Procedure Draw(screen: PSDL_Renderer);
    Procedure DrawStandby(screen: PSDL_Renderer);

    Function EraseCompletedLines():Integer;
    Function ComputeScore(nbL:Integer):Integer;
    Procedure DrawScore(screen: PSDL_Renderer);
    Function IsGameOver():boolean;
    Procedure ClearBoard();
    Procedure ProcessEventStandBy(done : PSDL_bool);
    Procedure ProcessEventPlay(done : PSDL_bool);
    Procedure ProcessEventGameOver(done : PSDL_bool);
    Procedure ProcessEventHightScores(done : PSDL_bool);
    Procedure DrawGameOver(screen: PSDL_Renderer);
    Procedure DrawHightScores(screen: PSDL_Renderer);
    Function  BagIsCompleted():boolean;
    Procedure EmptyBag();
    Function  GetFirstUnUsedType(): Integer;
    Procedure GenerateNextTetromino();
    Procedure SaveHightScores();
    Procedure LoadHightScores();
    Function  IsHightScore(newscore : Integer) : Integer;
    Procedure SetHightScore(idScore : Integer; playerNm : String;playerScore : Integer );
    Procedure SetHightScoreName(idScore : Integer; playerNm : String);
    Procedure InsertHightScore(idScore : Integer;playerNm : String;playerScore : Integer );
  end;

  function compareByScore(Item1, Item2 : Pointer) : Integer;
  var
    player1, player2 : THightScore;
  begin
    // We start by viewing the object pointers as TCustomer objects
    player1 := THightScore(Item1);
    player2 := THightScore(Item2);

    // Now compare by string
    if player1.m_score < player2.m_score then
      result := 1
    else if player1.m_score = player2.m_score then
      result := 0
    else
      result := -1;
  end;

  Constructor TGame.Create();
  var
    i      : Integer;
    hscore : THightScore;
  begin
    WriteLn('Class Constructor TGame');
    Self.idHightScore := -1;
    Self.score := 0;
    Self.mode  := STAND_BY;
    Self.VeloH := 0;
    Self.fFastDown := false;
    Self.fDropTetromino := false;

    Self.processEvent := @Self.ProcessEventStandBy;

    Self.curTetromino := TShape.create(0,6,1);
    Self.nextTetromino := TShape.create(0,6,1);

    Self.GenerateNextTetromino();

    Self.ClearBoard();

    hightScores := TList.create();
    for i := 0 to 9 do
    begin
      hscore := THightScore.create('XXX1',0);
      hightScores.Add(hscore);
    end;

    Self.LoadHightScores();

    hightScores.Sort(@compareByScore);

    for i := 0 to hightScores.Count-1 do
      begin
        WriteLn(THightScore(hightScores[i]).m_name,'-->',THightScore(hightScores[i]).m_score);
      end;

    Self.ttfFont := TTF_OpenFont('sansation.ttf', 18);
    if Self.ttfFont<>nil then
    begin
      TTF_SetFontStyle(Self.ttfFont, TTF_STYLE_BOLD or TTF_STYLE_ITALIC);
      TTF_SetFontHinting(Self.ttfFont, TTF_HINTING_NORMAL);
    end;

    // Load sound
    Sound := Mix_LoadWAV('109662__grunz__success.wav');
    if Sound = nil then Exit;
    Mix_VolumeChunk(Sound, 20);

    dictKeys := TDictKeys.Create;
    dictKeys[SDLK_a] := 'A';
    dictKeys[SDLK_b] := 'B';
    dictKeys[SDLK_c] := 'C';
    dictKeys[SDLK_d] := 'D';
    dictKeys[SDLK_e] := 'E';
    dictKeys[SDLK_f] := 'F';
    dictKeys[SDLK_g] := 'G';
    dictKeys[SDLK_h] := 'H';
    dictKeys[SDLK_i] := 'I';
    dictKeys[SDLK_j] := 'J';
    dictKeys[SDLK_k] := 'K';
    dictKeys[SDLK_l] := 'L';
    dictKeys[SDLK_m] := 'M';
    dictKeys[SDLK_n] := 'N';
    dictKeys[SDLK_o] := 'O';
    dictKeys[SDLK_p] := 'P';
    dictKeys[SDLK_q] := 'Q';
    dictKeys[SDLK_r] := 'R';
    dictKeys[SDLK_s] := 'S';
    dictKeys[SDLK_t] := 'T';
    dictKeys[SDLK_u] := 'U';
    dictKeys[SDLK_v] := 'V';
    dictKeys[SDLK_w] := 'W';
    dictKeys[SDLK_x] := 'X';
    dictKeys[SDLK_y] := 'Y';
    dictKeys[SDLK_z] := 'Z';
    dictKeys[SDLK_0] := '0';
    dictKeys[SDLK_1] := '1';
    dictKeys[SDLK_2] := '2';
    dictKeys[SDLK_3] := '3';
    dictKeys[SDLK_4] := '4';
    dictKeys[SDLK_5] := '5';
    dictKeys[SDLK_6] := '6';
    dictKeys[SDLK_7] := '7';
    dictKeys[SDLK_8] := '8';
    dictKeys[SDLK_9] := '9';
    dictKeys[SDLK_KP_0] := '0';
    dictKeys[SDLK_KP_1] := '1';
    dictKeys[SDLK_KP_2] := '2';
    dictKeys[SDLK_KP_3] := '3';
    dictKeys[SDLK_KP_4] := '4';
    dictKeys[SDLK_KP_5] := '5';
    dictKeys[SDLK_KP_6] := '6';
    dictKeys[SDLK_KP_7] := '7';
    dictKeys[SDLK_KP_8] := '8';
    dictKeys[SDLK_KP_9] := '9';

  end;

  Destructor TGame.Free();
  begin
    WriteLn('Class Destructor TGame');
    TTF_CloseFont(Self.ttfFont);
    if Assigned(Self.Sound) then Mix_FreeChunk(Self.Sound);
  end;

  Procedure TGame.SaveHightScores();
  var
     i: Integer;
     f: TFileStream;
     strLine: string;
     hscore : THightScore;
  begin
    f:= TFileStream.Create('hightscores.txt', fmCreate);
    for i := 0 to hightScores.Count-1 do
    begin
      hscore := THightScore(hightScores[i]);
      FmtStr(strLine,'%s;%d;',[hscore.m_name,hscore.m_score]);
      strLine += #13#10;
      f.Write(Pointer(strLine)^, Length(strLine));
    end;
    f.Free;
  end;

  Procedure TGame.LoadHightScores();
  var
     i: Integer;
     strLine: string;
     hscore : THightScore;
     productFileIn: TextFile;
     L : TStringlist;
  begin
    If FileExists('hightscores.txt') Then
       begin

          assignFile(productFileIn, 'hightscores.txt');
          try
            L:=TStringlist.Create;
            L.Delimiter := ';';
            L.StrictDelimiter := true;
            reset(productFileIn);
            i := 0;
            while not EOF(productFileIn) do
            begin
              hscore := THightScore(hightScores[i]);
              i := i + 1;
              readLn(productFileIn, strLine);
              L.DelimitedText := strLine;
              if L.Count>=2 then
              begin
                //WriteLn(L.Strings[0],'<>',L.Strings[1]);
                hscore.m_name := L.Strings[0];
                hscore.m_score := StrToInt(L.Strings[1]);
              end;

            end;
          finally
            closefile(productFileIn);
          end;

       end;

  end;

  Function TGame.IsHightScore(newscore : Integer) : Integer;
  var
     i : Integer;
     hscore : THightScore;
  begin
    for i:=0 to Self.hightScores.Count-1 do
      begin
        hscore := THightScore(hightScores[i]);
        if newscore>hscore.m_score then
        begin
           result := i;
          exit;
        end;
      end;
    result := -1;
  end;

  Procedure TGame.SetHightScore(idScore : Integer;playerNm : String;playerScore : Integer );
  var
    hscore : THightScore;
  begin
    if (idScore>=0) and (idScore<10) then
    begin
      hscore := THightScore(hightScores[idScore]);
      hscore.m_name := playerNm;
      hscore.m_score := playerScore;
    end;

  end;

  Procedure TGame.InsertHightScore(idScore : Integer;playerNm : String;playerScore : Integer );
  var
    hscore : THightScore;
    ptrItem   : Pointer;
  begin
    if (idScore>=0) and (idScore<10) then
    begin
      hscore := THightScore.Create(playerNm,playerScore);
      Self.hightScores.insert(idScore,hscore);
      ptrItem := hightScores.Last();
      hightScores.Remove(ptrItem);
    end;

  end;

  Procedure TGame.SetHightScoreName(idScore : Integer ;playerNm : String );
  var
    hscore : THightScore;
  begin
    if (idScore>=0) and (idScore<10) then
    begin
      hscore := THightScore(hightScores[idScore]);
      hscore.m_name := playerNm;
    end;

  end;

  Function TGame.BagIsCompleted():boolean;
  var
    i : Integer;
  begin
    for i:=1 to 7 do
    begin
      if Self.bag[i]=0 then
      begin
        BagIsCompleted := false;
        Exit;
      end;
    end;
    BagIsCompleted := true;
  end;

  Procedure TGame.EmptyBag();
  var
    i : Integer;
  begin
    for i:=1 to 7 do
    begin
      Self.bag[i] := 0;
    end;
  end;

  Function TGame.GetFirstUnUsedType(): Integer;
  var
    i : Integer;
  begin
    for i:=1 to 7 do
    begin
      if Self.bag[i]=0 then
      begin
        GetFirstUnUsedType := i;
        Exit;
      end;
    end;
    GetFirstUnUsedType := 0;
  end;

  Procedure TGame.GenerateNextTetromino();
  var
    nbTry : Integer = 0;
    iType : Integer;
  begin
    if Self.BagIsCompleted() then
    begin
      Self.EmptyBag();
    end;

    //-- Look randomly for unuse type in bag
    repeat
          iType := Random(7)+1;
          nbTry += 1;
          if nbTry>9 then
          begin
            iType := Self.GetFirstUnUsedType();
            break;
          end;
    until Self.bag[iType]=0;

    WriteLn('Essais : ',nbTry);

    Self.nextTetromino.Init(iType,NB_COLUMNS + 3,Trunc(NB_ROWS/2));
    //-- Flag as use type
    Self.bag[iType] := 1;

  end;

  Function TGame.ComputeScore(nbL:Integer):Integer;
  var
    sc : Integer;
  begin
    sc := 0;
    case nbL of
      1:
        sc := 40;
      2:
        sc := 100;
      3:
        sc := 300;
      4:
        sc := 1200;
      else
        sc := 2000;
    end;
    ComputeScore := sc;
  end;

  Procedure TGame.FreezeTetromino();
  var
    i,x,y,nbL : integer;
    strScore : string = '';
  begin
    for i:=0 to 3 do
    begin
      x := Self.curTetromino.v[i].x + Self.curTetromino.m_x;
      y := Self.curTetromino.v[i].y + Self.curTetromino.m_y;
      Self.board[x+y*NB_COLUMNS] := Self.curTetromino.m_type;
    end;
    nbL := Self.EraseCompletedLines();
    if nbL>0 then
    begin
      WriteLn('Score  ',Self.score);
      Self.score += Self.ComputeScore(nbL);
      FmtStr(strScore,'SCORE : %10.8d',[Self.score]);
      WriteLn(strScore);
      if Mix_PlayChannel(-1, Sound, 0) < 0 then Writeln(SDL_GetError);
    end;
  end;

  Procedure TGame.Draw(screen: PSDL_Renderer);
  var
    col       : TColor;
    typ,x,y,d : Integer;
    sdlRect1  : TSDL_Rect;
  begin

    //-- Board background
    sdlRect1.x := LEFT;
    sdlRect1.y := TOP;
    sdlRect1.w := NB_COLUMNS*CELL_SIZE;
    sdlRect1.h := NB_ROWS*CELL_SIZE;
    SDL_SetRenderDrawColor(screen, 10, 10, 100, SDL_ALPHA_OPAQUE);
    SDL_RenderFillRect(screen, @sdlRect1);

    d := CELL_SIZE - 2;
    for y:=0 to NB_ROWS-1 do
    begin
      for x:=0 to NB_COLUMNS-1 do
      begin
        typ := board[x+y*NB_COLUMNS];
        if typ<>0 then
          begin
            col := TetrisColors[typ];
            SDL_SetRenderDrawColor(screen, Red(col), Green(col), Blue(col), SDL_ALPHA_OPAQUE);
            sdlRect1.x := x * CELL_SIZE + 1 + LEFT;
            sdlRect1.y := y * CELL_SIZE + 1 + TOP;
            sdlRect1.w := d;
            sdlRect1.h := d;
            SDL_RenderFillRect(screen, @sdlRect1);

          end;
      end;
    end;

    //--
    Self.DrawScore(screen);

  end;

  Procedure TGame.DrawStandby(screen: PSDL_Renderer);
  const
    sdlColor1   : TSDL_Color = ( r : 255; g : 255; b : 0; a : 255);
  var
    sdlSurface1 : PSDL_Surface;
    sdlTexture1 : PSDL_Texture;
    strLine     : string;
    w,h,y         : Integer;
    sdlRect1    : TSDL_Rect;
  begin
    strLine := 'SDL Tetris';
    sdlSurface1 := TTF_RenderUTF8_Blended(ttfFont, PAnsiChar(strLine), sdlColor1);
    if sdlSurface1<>nil then
    begin
      sdlTexture1 := SDL_CreateTextureFromSurface(screen, sdlSurface1);
      if sdlTexture1<>nil then
        begin
          SDL_QueryTexture(sdlTexture1, NIL, NIL,@w,@h);
          sdlRect1.x := LEFT + Trunc(NB_COLUMNS*CELL_SIZE/2-w/2);
          sdlRect1.y := TOP + Trunc(NB_ROWS/3)*CELL_SIZE;
          sdlRect1.w := w;
          sdlRect1.h := h;
          SDL_RenderCopy(screen, sdlTexture1, nil, @sdlRect1);
          SDL_DestroyTexture(sdlTexture1);
        end;
      SDL_FreeSurface(sdlSurface1);
    end;

    sdlRect1.y += Trunc(3*h/2);
    strLine := 'in';
    sdlSurface1 := TTF_RenderUTF8_Blended(ttfFont, PAnsiChar(strLine), sdlColor1);
    if sdlSurface1<>nil then
    begin
      sdlTexture1 := SDL_CreateTextureFromSurface(screen, sdlSurface1);
      if sdlTexture1<>nil then
        begin
          SDL_QueryTexture(sdlTexture1, NIL, NIL,@w,@h);
          sdlRect1.x := LEFT + Trunc(NB_COLUMNS*CELL_SIZE/2-w/2);
          sdlRect1.w := w;
          sdlRect1.h := h;
          SDL_RenderCopy(screen, sdlTexture1, nil, @sdlRect1);
          SDL_DestroyTexture(sdlTexture1);
        end;
      SDL_FreeSurface(sdlSurface1);
    end;

    sdlRect1.y += Trunc(3*h/2);
    strLine := 'FreePascal';
    sdlSurface1 := TTF_RenderUTF8_Blended(ttfFont, PAnsiChar(strLine), sdlColor1);
    if sdlSurface1<>nil then
    begin
      sdlTexture1 := SDL_CreateTextureFromSurface(screen, sdlSurface1);
      if sdlTexture1<>nil then
        begin
          SDL_QueryTexture(sdlTexture1, NIL, NIL,@w,@h);
          sdlRect1.x := LEFT + Trunc(NB_COLUMNS*CELL_SIZE/2-w/2);
          sdlRect1.w := w;
          sdlRect1.h := h;
          SDL_RenderCopy(screen, sdlTexture1, nil, @sdlRect1);
          SDL_DestroyTexture(sdlTexture1);
        end;
      SDL_FreeSurface(sdlSurface1);
    end;

    sdlRect1.y += 2*h;
    strLine := 'Press SPACE to start';
    sdlSurface1 := TTF_RenderUTF8_Blended(ttfFont, PAnsiChar(strLine), sdlColor1);
    if sdlSurface1<>nil then
    begin
      sdlTexture1 := SDL_CreateTextureFromSurface(screen, sdlSurface1);
      if sdlTexture1<>nil then
        begin
          SDL_QueryTexture(sdlTexture1, NIL, NIL,@w,@h);
          sdlRect1.x := LEFT + Trunc(NB_COLUMNS*CELL_SIZE/2-w/2);
          sdlRect1.w := w;
          sdlRect1.h := h;
          SDL_RenderCopy(screen, sdlTexture1, nil, @sdlRect1);
          SDL_DestroyTexture(sdlTexture1);
        end;
      SDL_FreeSurface(sdlSurface1);
    end;

  end;

  Procedure TGame.DrawGameOver(screen: PSDL_Renderer);
  const
    sdlColor1   : TSDL_Color = ( r : 255; g : 255; b : 0; a : 255);
  var
    sdlSurface1 : PSDL_Surface;
    sdlTexture1 : PSDL_Texture;
    strLine     : string;
    w,h,y         : Integer;
    sdlRect1    : TSDL_Rect;
  begin
    strLine := 'Game Over';
    sdlSurface1 := TTF_RenderUTF8_Blended(ttfFont, PAnsiChar(strLine), sdlColor1);
    if sdlSurface1<>nil then
    begin
      sdlTexture1 := SDL_CreateTextureFromSurface(screen, sdlSurface1);
      if sdlTexture1<>nil then
        begin
          SDL_QueryTexture(sdlTexture1, NIL, NIL,@w,@h);
          sdlRect1.x := LEFT + Trunc(NB_COLUMNS*CELL_SIZE/2-w/2);
          sdlRect1.y := TOP + Trunc(NB_ROWS/3)*CELL_SIZE;
          sdlRect1.w := w;
          sdlRect1.h := h;
          SDL_RenderCopy(screen, sdlTexture1, nil, @sdlRect1);
          SDL_DestroyTexture(sdlTexture1);
        end;
      SDL_FreeSurface(sdlSurface1);
    end;

    sdlRect1.y += 3*h;
    strLine := 'Press SPACE to continue';
    sdlSurface1 := TTF_RenderUTF8_Blended(ttfFont, PAnsiChar(strLine), sdlColor1);
    if sdlSurface1<>nil then
    begin
      sdlTexture1 := SDL_CreateTextureFromSurface(screen, sdlSurface1);
      if sdlTexture1<>nil then
        begin
          SDL_QueryTexture(sdlTexture1, NIL, NIL,@w,@h);
          sdlRect1.x := LEFT + Trunc(NB_COLUMNS*CELL_SIZE/2-w/2);
          sdlRect1.w := w;
          sdlRect1.h := h;
          SDL_RenderCopy(screen, sdlTexture1, nil, @sdlRect1);
          SDL_DestroyTexture(sdlTexture1);
        end;
      SDL_FreeSurface(sdlSurface1);
    end;

  end;

  Procedure TGame.DrawHightScores(screen: PSDL_Renderer);
  const
    sdlColor1   : TSDL_Color = ( r : 255; g : 255; b : 0; a : 255);
  var
    sdlSurface1 : PSDL_Surface;
    sdlTexture1 : PSDL_Texture;
    strLine     : string;
    w,h,y,i     : Integer;
    sdlRect1    : TSDL_Rect;
    hscore      : THightScore;
  begin
    strLine := 'HIGHT SCORES';
    sdlSurface1 := TTF_RenderUTF8_Blended(ttfFont, PAnsiChar(strLine), sdlColor1);
    if sdlSurface1<>nil then
    begin
      sdlTexture1 := SDL_CreateTextureFromSurface(screen, sdlSurface1);
      if sdlTexture1<>nil then
        begin
          SDL_QueryTexture(sdlTexture1, NIL, NIL,@w,@h);
          sdlRect1.x := LEFT + Trunc(NB_COLUMNS*CELL_SIZE/2-w/2);
          sdlRect1.y := TOP + 1*CELL_SIZE;
          sdlRect1.w := w;
          sdlRect1.h := h;
          SDL_RenderCopy(screen, sdlTexture1, nil, @sdlRect1);
          SDL_DestroyTexture(sdlTexture1);
        end;
      SDL_FreeSurface(sdlSurface1);
    end;
    sdlRect1.y += 3*h;

    for i:=0 to hightscores.Count-1 do
    begin
      hscore := THightScore(hightScores[i]);
      //-- Player Name
      strLine := hscore.m_name;
      sdlSurface1 := TTF_RenderUTF8_Blended(ttfFont, PAnsiChar(strLine), sdlColor1);
      if sdlSurface1<>nil then
      begin
        sdlTexture1 := SDL_CreateTextureFromSurface(screen, sdlSurface1);
        if sdlTexture1<>nil then
          begin
            SDL_QueryTexture(sdlTexture1, NIL, NIL,@w,@h);
            sdlRect1.x := LEFT;
            sdlRect1.w := w;
            sdlRect1.h := h;
            SDL_RenderCopy(screen, sdlTexture1, nil, @sdlRect1);
            SDL_DestroyTexture(sdlTexture1);
          end;
        SDL_FreeSurface(sdlSurface1);
      end;
      //-- Player Score
      FmtStr(strLine,'%8.6d',[hscore.m_score]);
      sdlSurface1 := TTF_RenderUTF8_Blended(ttfFont, PAnsiChar(strLine), sdlColor1);
      if sdlSurface1<>nil then
      begin
        sdlTexture1 := SDL_CreateTextureFromSurface(screen, sdlSurface1);
        if sdlTexture1<>nil then
          begin
            SDL_QueryTexture(sdlTexture1, NIL, NIL,@w,@h);
            sdlRect1.x := LEFT + (Trunc(NB_COLUMNS/2)+1)*CELL_SIZE;
            sdlRect1.w := w;
            sdlRect1.h := h;
            SDL_RenderCopy(screen, sdlTexture1, nil, @sdlRect1);
            SDL_DestroyTexture(sdlTexture1);
          end;
        SDL_FreeSurface(sdlSurface1);
      end;

     sdlRect1.y += Trunc(3/2)*h;

    end;

  end;

  Procedure TGame.DrawScore(screen: PSDL_Renderer);
  const
  sdlColor1   : TSDL_Color = ( r : 255; g : 255; b : 0; a : 255);
  var
    sdlSurface1 : PSDL_Surface;
    sdlTexture1 : PSDL_Texture;
    strScore    : string = 'SCORE : 000000';
    w,h         : Integer;
    sdlRect1   : TSDL_Rect;
  begin
    FmtStr(strScore,'SCORE : %8.6d',[Self.score]);
    sdlSurface1 := TTF_RenderUTF8_Blended(ttfFont, PAnsiChar(strScore), sdlColor1);
    if sdlSurface1<>nil then
      begin
        sdlTexture1 := SDL_CreateTextureFromSurface(screen, sdlSurface1);
        if sdlTexture1<>nil then
          begin
            SDL_QueryTexture(sdlTexture1, NIL, NIL,@w,@h);
            sdlRect1.x := LEFT;
            sdlRect1.y := TOP + NB_ROWS*CELL_SIZE+8;
            sdlRect1.w := w;
            sdlRect1.h := h;
            SDL_RenderCopy(screen, sdlTexture1, nil, @sdlRect1);
            SDL_DestroyTexture(sdlTexture1);
          end;
        SDL_FreeSurface(sdlSurface1);
      end;
  end;

  Function TGame.EraseCompletedLines() : Integer;
  var
    fCompleted : Boolean;
    r,r1,c,nbL : Integer;
  begin
    nbL := 0;
    for r:=0 to NB_ROWS-1 do
    begin
      fCompleted := true;
      for c:=0 to NB_COLUMNS-1 do
      begin
        if Self.board[c+r*NB_COLUMNS]=0 then
          begin
            fCompleted := false;
            break;
          end;
      end;

      if fCompleted then
        begin
          nbL += 1;
          //--
          for r1:=r downto 1 do
          begin
            for c:=0 to NB_COLUMNS-1 do
            begin
              Self.board[r1*NB_COLUMNS+c]:=Self.board[(r1-1)*NB_COLUMNS+c];
            end;
          end;
        end;

    end;

    EraseCompletedLines := nbL;

  end;

  Function TGame.IsGameOver() : boolean;
  var
    i : Integer;
  begin
    IsGameOver := false;
    for i:=0 to NB_COLUMNS-1 do
    begin
      if Self.board[i]<>0 then
        begin
          IsGameOver := true;
          exit;
        end;
    end;
  end;

  Procedure TGame.ClearBoard();
  var
    i : Integer;
  begin
    for i:=0 to NB_ROWS*NB_COLUMNS-1 do
      begin
        Self.board[i] := 0;
      end;
  end;

  Procedure TGame.ProcessEventPlay(done : PSDL_bool);
  var
    max_x,min_x,dx : integer;
  begin
    done^ := SDL_FALSE;
    case Self.event.type_ of
      SDL_QUITEV:
        begin
           done^ := SDL_TRUE;
        end;
      SDL_KEYDOWN:
        begin
          {
            Press the L key to lag for 3 seconds, to see what happens
            when SDL doesn't service the event loop quickly.
          }

          case Self.event.key.keysym.sym of
            SDLK_LEFT:
                Self.VeloH := -1;
            SDLK_RIGHT:
                Self.VeloH := 1;
            SDLK_UP:
              begin
                Self.curTetromino.RotateLeft();
                if Self.curTetromino.HitGround(@Self.board) then
                  begin
                    //-- Undo Rotate
                    Self.curTetromino.RotateRight();
                  end
                else
                  if not Self.curTetromino.IsInBoard() then
                    begin
                      max_x := Self.curTetromino.MaxX();
                      if max_x>=NB_COLUMNS then
                        begin
                          dx := max_x - NB_COLUMNS+1;
                          Self.curTetromino.m_x -= dx;
                          if Self.curTetromino.HitGround(@Self.board) then
                            begin
                              Self.curTetromino.m_x += dx;
                              Self.curTetromino.RotateRight();
                            end;
                        end
                      else
                        begin
                          min_x := Self.curTetromino.MinX();
                          if min_x<0 then
                            begin
                              dx := min_x;
                              Self.curTetromino.m_x -= dx;
                              if Self.curTetromino.HitGround(@Self.board) then
                                begin
                                  Self.curTetromino.m_x += dx;
                                  Self.curTetromino.RotateRight();
                                end;
                            end;
                        end;
                    end;
              end;
            SDLK_DOWN:
              begin
                Self.fFastDown := true;
              end;
            SDLK_l:
              begin
                SDL_Log('Lagging for 3 seconds...', []);
                SDL_Delay(3000);
              end;
            SDLK_ESCAPE:
              begin
                done^ := SDL_TRUE;
              end;
            SDLK_SPACE:
              begin
                Self.fDropTetromino := true;
              end;
          end;

        end;
      SDL_KEYUP:
        begin
          case Self.event.key.keysym.sym of
            SDLK_LEFT:
              begin
                Self.VeloH := 0;
              end;
            SDLK_RIGHT:
              begin
                Self.VeloH := 0;
              end;
            SDLK_DOWN:
              begin
                Self.fFastDown := false;
              end;
          end;
        end;
    end;

  end;

  Procedure TGame.ProcessEventStandBy(done : PSDL_bool);
  begin
    done^ := SDL_FALSE;
    case Self.event.type_ of
      SDL_QUITEV:
        begin
           done^ := SDL_TRUE;
        end;
      SDL_KEYDOWN:
        begin
          {
            Press the L key to lag for 3 seconds, to see what happens
            when SDL doesn't service the event loop quickly.
          }
          case Self.event.key.keysym.sym of
            SDLK_ESCAPE:
              begin
                done^ := SDL_TRUE;
              end;
            SDLK_SPACE:
              begin
                Self.EmptyBag();
                Self.mode := PLAY;
                Self.processEvent := @Self.ProcessEventPlay;
                Self.curTetromino.Init(Self.nextTetromino.m_type,5,1);
                Self.GenerateNextTetromino();
              end;
          end;

        end;
    end;

  end;

  Procedure TGame.ProcessEventGameOver(done : PSDL_bool);
  begin
    done^ := SDL_FALSE;
    case Self.event.type_ of
      SDL_QUITEV:
        begin
           done^ := SDL_TRUE;
        end;
      SDL_KEYDOWN:
        begin
          {
            Press the L key to lag for 3 seconds, to see what happens
            when SDL doesn't service the event loop quickly.
          }
          case Self.event.key.keysym.sym of
            SDLK_ESCAPE:
              begin
                done^ := SDL_TRUE;
              end;
            SDLK_SPACE:
              begin
                Self.ClearBoard();
                Self.mode := STAND_BY;
                Self.processEvent := @Self.ProcessEventStandBy;
              end;
          end;

        end;
    end;

  end;

  Procedure TGame.ProcessEventHightScores(done : PSDL_bool);
  var
    l : Integer;
  begin
    done^ := SDL_FALSE;
    case Self.event.type_ of
      SDL_QUITEV:
        begin
           done^ := SDL_TRUE;
        end;
      SDL_KEYDOWN:
        begin
          {
            Press the L key to lag for 3 seconds, to see what happens
            when SDL doesn't service the event loop quickly.
          }
          case Self.event.key.keysym.sym of
            SDLK_RETURN:
               begin
                Self.mode := STAND_BY;
                Self.processEvent := @Self.ProcessEventStandBy;
                Self.SaveHightScores();
              end;
            SDLK_ESCAPE:
              begin
                //done^ := SDL_TRUE;
              end;
            SDLK_BACKSPACE:
              begin
                l := Length(playerName);
                if l>0 then
                  begin
                    SetLength(playerName, l - 1);
                    Self.SetHightScoreName(self.IdHightScore,Self.playerName);
                  end;
              end
          else
            begin
              if dictKeys.IndexOf(Self.event.key.keysym.sym)<>-1 then
              begin
                playerName := playerName + dictKeys[Self.event.key.keysym.sym];
                Self.SetHightScoreName(self.IdHightScore,Self.playerName);
              end;
            end;

          end;

        end;
    end;

  end;


var

  window: PSDL_Window = nil;
  screen: PSDL_Renderer = nil;
  Music : PMix_Music;

  done : TSDL_bool = SDL_FALSE;

  curTicks : comp;
  ticks1   : comp;
  ticks2   : comp;

  limit    : Integer;
  game     : TGame;


begin

  randomize;

  { Initialize SDL (Note: video is required to start event loop) }
  if SDL_Init(SDL_INIT_VIDEO or SDL_INIT_AUDIO) < 0 then
  begin
  SDL_LogError(Ord(SDL_LOG_CATEGORY_APPLICATION), 'Couldn''t initialize SDL: %s', [SDL_GetError()]);
  Halt(1);
  end;

  if TTF_Init() = -1 then Halt();

  { Create a window to display joystick axis position }
  window := SDL_CreateWindow('FreePascal Tetris SDL2', SDL_WINDOWPOS_CENTERED,
                             SDL_WINDOWPOS_CENTERED, SCREEN_WIDTH,
                             SCREEN_HEIGHT, 0);

  if window = nil then
  begin
    SDL_LogError(Ord(SDL_LOG_CATEGORY_APPLICATION), 'Couldn''t create window: %s', [SDL_GetError()]);
    Halt(0);
  end;

  screen := SDL_CreateRenderer(window, -1, 0);

  if screen = nil then
  begin
    SDL_LogError(Ord(SDL_LOG_CATEGORY_APPLICATION), 'Couldn''t create renderer: %s', [SDL_GetError()]);
    SDL_DestroyWindow(window);
    Halt(0);
  end;


  // Prepare mixer
  if Mix_OpenAudio(MIX_DEFAULT_FREQUENCY, MIX_DEFAULT_FORMAT,
    MIX_DEFAULT_CHANNELS, 4096) < 0 then Exit;

  // Load music
  Music := Mix_LoadMUS('Tetris.ogg');
  if Music = nil then Exit;
  Mix_VolumeMusic(15);

  Mix_PlayMusic(Music, -1);

  //game.score := 1000;

  game := TGame.create();

  ticks1 := TimeStampToMSecs(DateTimeToTimeStamp (Now));
  ticks2 := ticks1;

  { Loop, getting joystick events! }
  while done = SDL_FALSE do
  begin

    while SDL_PollEvent(@game.event) = 1 do
    begin
      game.processEvent(@done);
      if done = SDL_TRUE then
      begin
        //-- Check Hight Score
        game.idHightScore := game.IsHightScore(game.score);
        if game.idHightScore>=0 then
        begin
          done := SDL_FALSE;
          game.SetHightScore(game.idHightScore,game.playerName,game.score);
          game.score := 0;
          game.curTetromino.m_type := 0;
          game.mode := HIGHT_SCORES;
          game.processEvent:=@game.ProcessEventHightScores;

        end;
      end;
    end;

    //-- Update Game State
    if game.mode = PLAY then
    begin
      curTicks := TimeStampToMSecs(DateTimeToTimeStamp (Now));
      if (curTicks-ticks1)>100 then
        begin
          ticks1 := curTicks;
          game.curTetromino.m_x += game.VeloH;
          if game.curTetromino.IsInBoard()=false then
            begin
              game.curTetromino.m_x -= game.VeloH;
            end
          else
            if game.curTetromino.HitGround(@game.board) then
              begin
                game.curTetromino.m_x -= game.VeloH;
              end;
        end;

      if game.fDropTetromino then
        begin
          ticks2 := curTicks;
          game.curTetromino.m_y += 1;
          if not game.curTetromino.IsInBoard() then
            begin
             game.curTetromino.m_y -= 1;
             game.FreezeTetromino();
             game.curTetromino.Init(game.nextTetromino.m_type,5,1);
             game.GenerateNextTetromino();
             game.fDropTetromino := false;
            end
          else
            if game.curTetromino.HitGround(@game.board) then
              begin
                game.curTetromino.m_y -= 1;
                game.FreezeTetromino();
                game.curTetromino.Init(game.nextTetromino.m_type,5,1);
                game.GenerateNextTetromino();
                game.fDropTetromino := false;
              end;
        end
      else
        begin

          if game.fFastDown then
            limit := 100
          else
            limit := 800;

          if (curTicks-ticks2)>limit then
          begin

            ticks2 := curTicks;
            game.curTetromino.m_y += 1;

            game.curTetromino.m_x += game.VeloH;
            if game.curTetromino.IsInBoard()=false then
              begin
                game.curTetromino.m_x -= game.VeloH;
              end
            else
              if game.curTetromino.HitGround(@game.board) then
                begin
                  game.curTetromino.m_x -= game.VeloH;
                end;

            if not game.curTetromino.IsInBoard() then
              begin
               game.curTetromino.m_y -= 1;
               game.FreezeTetromino();
               game.curTetromino.Init(game.nextTetromino.m_type,5,1);
               game.GenerateNextTetromino();
              end
            else
              if game.curTetromino.HitGround(@game.board) then
                begin
                  game.curTetromino.m_y -= 1;
                  game.FreezeTetromino();
                  game.curTetromino.Init(game.nextTetromino.m_type,5,1);
                  game.GenerateNextTetromino();
                end;

          end;

          if game.IsGameOver() then
            begin
              //-- Check Hight Score
              game.idHightScore := game.IsHightScore(game.score);
              if game.idHightScore>=0 then
                begin

                  game.SetHightScore(game.idHightScore,game.playerName,game.score);
                  game.score := 0;
                  game.mode := HIGHT_SCORES;
                  game.processEvent:=@game.ProcessEventHightScores;

                end
              else
                begin
                     game.curTetromino.m_type := 0;
                     game.mode := GAME_OVER;
                     game.processEvent:=@game.ProcessEventGameOver;
                end;
            end;

        end;

    end;

    //-- Draw Game
    SDL_SetRenderDrawColor(screen, 30, 30, 80, SDL_ALPHA_OPAQUE);
    SDL_RenderClear(screen);

    //--
    game.Draw(screen);

    //--
    case game.mode of
      STAND_BY:
        game.DrawStandby(screen);
      GAME_OVER:
        game.DrawGameOver(screen);
      HIGHT_SCORES:
        game.DrawHightScores(screen);
    end;

    //--
    if game.curTetromino.m_type<>0 then
       game.curTetromino.Draw(screen);

    //--
    if game.nextTetromino.m_type<>0 then
       game.nextTetromino.Draw(screen);

    //--
    SDL_RenderPresent(screen);


  end;

  game.Free();

  if Assigned(screen) then SDL_DestroyRenderer(screen);

  if Assigned(window) then SDL_DestroyWindow(window);

  if Assigned(Music) then Mix_FreeMusic(Music);

  TTF_Quit();

  SDL_QuitSubSystem(SDL_INIT_VIDEO or SDL_INIT_AUDIO);


end.

