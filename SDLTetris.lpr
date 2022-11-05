program SDLTetris;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
   math,fgl,classes,SysUtils,FileUtil,Graphics,SDL2,SDL2_ttf,SDL2_mixer
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
    veloH      : Integer;
    fFastDown  : Boolean;
    fPause     : Boolean;
    fQuit      : Boolean;
    fDropTetromino : boolean;
    event          : TSDL_Event;
    processEvent   : TMethodPtr;
    curTetromino   : TShape;
    nextTetromino  : TShape;
    ttfFont : PTTF_Font;
    tetrisBag      : Array of Integer;
    idTetrisBag  : Integer;
    hightScores  : TList;
    idHightScore : Integer;

    horizontalMove         : Integer;
    horizontalStartColumn : Integer;
    nbCompletedLines      : Integer;

    dictKeys : TDictKeys;

    Constructor Create();
    Destructor Free();
    Procedure FreezeTetromino();

    Procedure Draw(screen: PSDL_Renderer);
    Procedure DrawStandby(screen: PSDL_Renderer);

    Procedure EraseFirstCompletedLine();
    Function  ComputeScore(nbL:Integer):Integer;
    Procedure DrawScore(screen: PSDL_Renderer);
    Function  IsGameOver():boolean;
    Procedure ClearBoard();
    Procedure ProcessEventStandBy(done : PSDL_bool);
    Procedure ProcessEventPlay(done : PSDL_bool);
    Procedure ProcessEventGameOver(done : PSDL_bool);
    Procedure ProcessEventHightScores(done : PSDL_bool);
    Procedure DrawGameOver(screen: PSDL_Renderer);
    Procedure DrawHightScores(screen: PSDL_Renderer);
    Procedure SaveHightScores();
    Procedure LoadHightScores();
    Function  IsHightScore(newscore : Integer) : Integer;
    Procedure SetHightScore(idScore : Integer; playerNm : String;playerScore : Integer );
    Procedure SetHightScoreName(idScore : Integer; playerNm : String);
    Procedure InsertHightScore(idScore : Integer;playerNm : String;playerScore : Integer );
    Function  ComputeCompletedLines() : Integer;
    Procedure NewTetromino();
    function  TetrisRandomizer() : Integer;

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
    Self.processEvent := @Self.ProcessEventStandBy;
    Self.veloH := 0;
    Self.horizontalMove := 0;
    Self.horizontalStartColumn := 0;
    Self.fFastDown := false;
    Self.fPause := false;
    Self.fDropTetromino := false;
    Self.nbCompletedLines := 0;
    Self.fQuit := false;

    Self.idTetrisBag := 14;
    Self.tetrisBag := [1,2,3,4,5,6,7,1,2,3,4,5,6,7];
    Self.curTetromino := TShape.create(0,0,0);
    Self.nextTetromino := TShape.create(Self.TetrisRandomizer(),(NB_COLUMNS+3)*CELL_SIZE,7*CELL_SIZE);

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
  end;

  Procedure TGame.SaveHightScores();
  var
     i: Integer;
     f: TFileStream;
     strLine: string;
     hscore : THightScore;
  begin
    f:= TFileStream.Create('highscores.txt', fmCreate);
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
    If FileExists('highscores.txt') Then
       begin

          assignFile(productFileIn, 'highscores.txt');
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


  function TGame.TetrisRandomizer() : Integer;
  var
    i     : Integer;
    iSrc  : Integer;
    iTyp  : Integer;
  begin

    if Self.idTetrisBag<14 then
      begin
        iTyp := Self.tetrisBag[Self.idTetrisBag];
        Self.idTetrisBag += 1;
      end
    else
      begin
        //-- Shuttle Bag
        for i:=0 to 13 do
        begin
          iSrc := Random(14);
          iTyp := Self.tetrisBag[iSrc];
          Self.tetrisBag[iSrc] := Self.tetrisBag[0];
          Self.tetrisBag[0] := iTyp;
        end;
        Self.idTetrisBag := 1;

      end;

    TetrisRandomizer := iTyp;

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

  Procedure TGame.NewTetromino();
  begin
    Self.curTetromino.Init(Self.nextTetromino.m_Type,5*CELL_SIZE,0);
    Self.curTetromino.m_y := -Self.curTetromino.MaxY()*CELL_SIZE;
    Self.nextTetromino.Init(Self.TetrisRandomizer(),(NB_COLUMNS+3)*CELL_SIZE,7*CELL_SIZE);
  end;

  Function TGame.ComputeCompletedLines() : Integer;
  var
    r,c        : Integer;
    fCompleted : Boolean;
    nbLines    : Integer;
  begin
    nbLines := 0;
    for r:=0 to (NB_ROWS-1) do
      begin
        fCompleted := true;
        for c:=0 to (NB_COLUMNS-1) do
          begin
            if Self.board[r*NB_COLUMNS + c]=0 then
              begin
                fCompleted := false;
                break;
              end;
          end;
        if fCompleted then nbLines += 1;
      end;
      ComputeCompletedLines := nbLines;
  end;

  Procedure TGame.FreezeTetromino();
  var
    i,x,y     : Integer;
    ix,iy     : Integer;
  begin
    ix := Trunc((Self.curTetromino.m_x+1)/CELL_SIZE);
    iy := Trunc((Self.curTetromino.m_y+1)/CELL_SIZE);
    for i:=0 to 3 do
    begin
      x := Self.curTetromino.v[i].x + ix;
      y := Self.curTetromino.v[i].y + iy;
      if (x>=0) and (x<NB_COLUMNS) and (y>=0) and (y<NB_ROWS) then
        begin
          Self.board[x+y*NB_COLUMNS] := Self.curTetromino.m_type;
        end;
    end;
    Self.nbCompletedLines := Self.ComputeCompletedLines();
    if Self.nbCompletedLines>0 then
      Self.score += Self.ComputeScore(Self.nbCompletedLines);

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

  Procedure TGame.EraseFirstCompletedLine();
  var
    fCompleted : Boolean;
    r,r1,c     : Integer;
  begin
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
          //--
          for r1:=r downto 1 do
          begin
            for c:=0 to NB_COLUMNS-1 do
            begin
              Self.board[r1*NB_COLUMNS+c]:=Self.board[(r1-1)*NB_COLUMNS+c];
            end;
          end;
          break;
        end;

    end;

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
    backupX        : Integer;
  begin
    done^ := SDL_FALSE;
    case Self.event.type_ of
      SDL_QUITEV:
        begin
           done^ := SDL_TRUE;
           Self.fQuit := true;
        end;
      SDL_KEYDOWN:
        begin
          {
            Press the L key to lag for 3 seconds, to see what happens
            when SDL doesn't service the event loop quickly.
          }

          case Self.event.key.keysym.sym of
            SDLK_P:
              Self.fPause := not (Self.fPause);
            SDLK_LEFT:
                Self.VeloH := -1;
            SDLK_RIGHT:
                Self.VeloH := 1;
            SDLK_UP:
              begin
                if Self.curTetromino.m_type<>0 then
                  begin
                    Self.curTetromino.RotateLeft();
                    if Self.curTetromino.HitGround(@Self.board) then
                      begin
                        //-- Undo Rotate
                        Self.curTetromino.RotateRight();
                      end
                    else
                      if Self.curTetromino.IsOutRightLimit() then
                        begin
                          backupX := Self.curTetromino.m_x;
                          repeat
                            Self.curTetromino.m_x -= 1
                          until not Self.curTetromino.IsOutRightLimit();
                          if  Self.curTetromino.HitGround(@Self.board) then
                            begin
                              Self.curTetromino.m_x := backupX;
                              //-- Undo Rotate
                              Self.curTetromino.RotateRight();
                            end;
                        end
                      else
                        if Self.curTetromino.IsOutLeftLimit() then
                          begin
                            backupX := Self.curTetromino.m_x;
                            repeat
                              Self.curTetromino.m_x += 1
                            until not Self.curTetromino.IsOutLeftLimit();
                            if  Self.curTetromino.HitGround(@Self.board) then
                              begin
                                Self.curTetromino.m_x := backupX;
                                //-- Undo Rotate
                                Self.curTetromino.RotateRight();
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
           Self.fQuit := true;
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
                Self.fQuit:= true;
              end;
            SDLK_SPACE:
              begin
                Self.mode := PLAY;
                Self.processEvent := @Self.ProcessEventPlay;
                Self.NewTetromino();
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
           Self.fQuit := true;
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
                Self.fQuit := true;
              end;
            SDLK_SPACE:
              begin
                Self.curTetromino.m_Type := 0;
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
           Self.fQuit := true;
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
                Self.mode := STAND_BY;
                Self.processEvent := @Self.ProcessEventStandBy;
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
  Sound: PMix_Chunk;

  done : TSDL_bool = SDL_FALSE;

  curTicks    : comp;
  startTicksV : comp;
  startTicksH : comp;

  game     : TGame;
  i        : Integer;
  fMove    : Boolean;
  backupX  : Integer;

  strMsg   : String;

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

  screen := SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED + SDL_RENDERER_PRESENTVSYNC);

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


  // Load sound
  Sound := Mix_LoadWAV('109662__grunz__success.wav');
  if Sound = nil then Exit;
  Mix_VolumeChunk(Sound, 20);


  game := TGame.create();
  //game.score := 1000;

  startTicksV := TimeStampToMSecs(DateTimeToTimeStamp (Now));
  startTicksH := startTicksV;

  { Loop, getting joystick events! }
  while done = SDL_FALSE do
  begin

    while SDL_PollEvent(@game.event) = 1 do
    begin
      game.processEvent(@done);

      if game.fQuit then break;

      if done = SDL_TRUE then
      begin
        //-- Check Hight Score
        game.idHightScore := game.IsHightScore(game.score);
        if game.idHightScore>=0 then
          begin
            done := SDL_FALSE;
            game.InsertHightScore(game.idHightScore,game.playerName,game.score);
            game.score := 0;
            game.curTetromino.m_type := 0;
            game.mode := HIGHT_SCORES;
            game.processEvent:=@game.ProcessEventHightScores;
            game.curTetromino.m_Type := 0;
            game.ClearBoard();
          end
        else
          begin
            done := SDL_FALSE;
            game.score := 0;
            game.mode  := STAND_BY;
            game.processEvent := @game.ProcessEventStandBy;
            game.curTetromino.m_Type := 0;
            game.ClearBoard();
          end;
      end;
    end;

    //-- Update Game State
    if (game.mode = PLAY) and (not game.fPause) then
    begin
      curTicks := TimeStampToMSecs(DateTimeToTimeStamp (Now));

      if (game.nbCompletedLines>0) then
         begin
         if (curTicks-startTicksV)>200 then
           begin
             startTicksV := curTicks;
             game.nbCompletedLines -= 1;
             game.EraseFirstCompletedLine();
             if Mix_PlayChannel(-1, Sound, 0) < 0 then Writeln(SDL_GetError);
           end;
         end
      else if (game.horizontalMove<>0) then
         begin
           if (curTicks-startTicksH)>20 then
             begin
                 startTicksH := curTicks;
                 for i:=0 to 3 do
                   begin
                     backupX := game.curTetromino.m_x;
                     game.curTetromino.m_x += game.horizontalMove;

                     if game.horizontalMove<0 then
                       begin
                       if game.curTetromino.IsOutLeftLimit() then
                         begin
                           game.curTetromino.m_x := backupX;
                           game.horizontalMove := 0;
                           break;
                         end;
                       end
                     else
                        if game.horizontalMove>0 then
                          begin
                            if game.curTetromino.IsOutRightLimit() then
                              begin
                                game.curTetromino.m_x := backupX;
                                game.horizontalMove := 0;
                                break;
                              end
                          end;
                     if game.curTetromino.HitGround(@game.board) then
                       begin
                         game.curTetromino.m_x := backupX;
                         game.horizontalMove := 0;
                         break;
                       end;
                     //FmtStr(strMsg,'%d <=> %d',[game.horizontalStartColumn,game.curTetromino.Column()]);
                     //Writeln(strMsg);
                     if game.horizontalStartColumn<>game.curTetromino.Column() then
                       begin
                         startTicksH := curTicks;
                         game.curTetromino.m_x := backupX;
                         game.horizontalMove := 0;
                         break;
                       end;
                 end
             end
         end
      else if game.fDropTetromino then
        begin

          if (curTicks-startTicksV)>10 then
            begin
                 startTicksV := curTicks;
                 for i:= 0 to 5 do
                   begin
                     //-- Move down to check
                     game.curTetromino.m_y += 1;
                     if game.curTetromino.HitGround(@game.board) then
                       begin
                         game.curTetromino.m_y -= 1;
                         game.FreezeTetromino();
                         game.NewTetromino();
                         game.fDropTetromino := false;
                       end
                     else if game.curTetromino.IsOutBottomLimit() then
                       begin
                         game.curTetromino.m_y -= 1;
                         game.FreezeTetromino();
                         game.NewTetromino();
                         game.fDropTetromino := false;
                       end;
                     if game.fDropTetromino then
                       begin
                         if (game.veloH<>0) then
                           begin
                             if (curTicks-startTicksH)>10 then
                               begin
                                 backupX := game.curTetromino.m_x;
                                 game.curTetromino.m_x += game.veloH;
                                 if game.curTetromino.IsOutLeftLimit() or game.curTetromino.IsOutRightLimit() then
                                   begin
                                        game.curTetromino.m_x := backupX;
                                   end
                                 else if game.curTetromino.HitGround(@game.board) then
                                   begin
                                      game.curTetromino.m_x := backupX;
                                   end
                                 else
                                   begin
                                      startTicksH := curTicks;
                                      game.horizontalMove := game.veloH;
                                      game.horizontalStartColumn := game.curTetromino.Column();
                                      break;
                                   end
                               end;
                           end;

                       end;

                   end;
            end;
        end
      else
        begin

          if (curTicks-startTicksV)>IfThen(game.fFastDown,10,25) then
          begin

            startTicksV := curTicks;

            for i:= 0 to 4 do
              begin

                game.curTetromino.m_y += 1;
                fMove := true;
                if game.curTetromino.HitGround(@game.board) then
                   begin
                        game.curTetromino.m_y -= 1;
                        game.FreezeTetromino();
                        game.NewTetromino();
                        fMove := false;
                   end
                else if game.curTetromino.IsOutBottomLimit() then
                   begin
                      game.curTetromino.m_y -= 1;
                      game.FreezeTetromino();
                      game.NewTetromino();
                      fMove := false;
                   end;

                if fMove and (game.veloH<>0)then
                  begin
                    if (curTicks-startTicksH)>10 then
                       begin
                            backupX := game.curTetromino.m_x;
                            game.curTetromino.m_x += game.veloH;

                            if game.curTetromino.IsOutLeftLimit() then
                               game.curTetromino.m_x := backupX
                            else
                               if game.curTetromino.IsOutRightLimit() then
                                  game.curTetromino.m_x := backupX
                               else
                                  if game.curTetromino.HitGround(@game.board) then
                                    game.curTetromino.m_x := backupX
                                  else
                                    begin
                                      startTicksH := curTicks;
                                      game.horizontalMove := game.veloH;
                                      game.horizontalStartColumn := game.curTetromino.Column();
                                      break;
                                    end;
                       end
                  end;

              end;

          end;

          if game.IsGameOver() then
            begin
              //-- Check Hight Score
              game.idHightScore := game.IsHightScore(game.score);
              if game.idHightScore>=0 then
                begin
                  game.InsertHightScore(game.idHightScore,game.playerName,game.score);
                  game.score := 0;
                  game.mode := HIGHT_SCORES;
                  game.processEvent:=@game.ProcessEventHightScores;
                  game.ClearBoard();
                end
              else
                begin
                  game.curTetromino.m_type := 0;
                  game.mode := GAME_OVER;
                  game.processEvent:=@game.ProcessEventGameOver;
                  game.ClearBoard();
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

  if Assigned(Sound) then Mix_FreeChunk(Sound);

  TTF_Quit();

  SDL_QuitSubSystem(SDL_INIT_VIDEO or SDL_INIT_AUDIO);


end.

