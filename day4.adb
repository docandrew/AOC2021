with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Containers.Vectors;

-- Note: need to manually add extra newline after last line of input
procedure Day4 is
    input : File_Type;
    count : Integer := 0;
    curr  : Integer := 0;

    type BingoNumber is record
        val    : Integer;
        called : Boolean := False;
    end record;

    -- 100 5x5 boards
    type BoardType is array(Integer range 1..5, Integer range 1..5) of BingoNumber;
    type BoardsType is array (1..100) of BoardType;
    boards : BoardsType;

    package IntVecs is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Integer);
    winningNumbers : IntVecs.Vector;

    type AlreadyWonArr is array(1..100) of Boolean;
    alreadyWon : AlreadyWonArr := (others => False);

    -- Given a called number and array of boards, mark that called number on all
    -- the boards
    procedure MarkBoards (calledNumber : Integer; b : in out BoardsType) is
    begin
        for board in 1..100 loop
            for col in 1..5 loop
                for row in 1..5 loop
                    if b(board)(col,row).val = calledNumber then
                        b(board)(col,row).called := True;
                    end if;
                end loop;
            end loop;
        end loop;
    end MarkBoards;

    -- Given the index of a winning board and the last called number that made it
    -- a winner, calculate the answer
    function CalcAnswer (b : Integer; n : Integer) return Integer is
        sumUnmarked : Integer := 0;
    begin
        for i in 1..5 loop
            for j in 1..5 loop
                if not boards(b)(i,j).called then
                    sumUnmarked := sumUnmarked + boards(b)(i,j).val;
                end if;
            end loop;
        end loop;

        return n * sumUnmarked;
    end CalcAnswer;

    -- Given an array of boards and the last called number n, determine if any
    -- of the boards are a winner.
    procedure CheckBoards (b : in out BoardsType; n : Integer) is
        ans : Integer := 0;
        use ASCII;
    begin
        for board in 1..100 loop
            if not alreadyWon(board) then
                -- check rows
                for row in 1..5 loop
                    if b(board)(row, 1).called and 
                       b(board)(row, 2).called and
                       b(board)(row, 3).called and
                       b(board)(row, 4).called and
                       b(board)(row, 5).called then
                        Put_Line ("Board" & board'Image & " is a winner");
                        ans := CalcAnswer (board, n);
                        Put_Line ("Answer: " & ans'Image & CR & LF);
                        alreadyWon(board) := True;
                    end if;
                end loop;

                -- check cols
                for col in 1..5 loop
                    if b(board)(1,col).called and
                       b(board)(2,col).called and
                       b(board)(3,col).called and
                       b(board)(4,col).called and
                       b(board)(5,col).called then
                        Put_Line ("Board" & board'Image & " is a winner");
                        ans := CalcAnswer (board, n);
                        Put_Line ("Answer: " & ans'Image & CR & LF);
                        alreadyWon(board) := True;
                    end if;
                end loop;

                -- check diag
                if b(board)(1,1).called and
                   b(board)(2,2).called and
                   b(board)(3,3).called and
                   b(board)(4,4).called and
                   b(board)(5,5).called then
                    Put_Line ("Board" & board'Image & " is a winner");
                    ans := CalcAnswer (board, n);
                    Put_Line ("Answer: " & ans'Image & CR & LF);
                    alreadyWon(board) := True;
                end if;

                if b(board)(1,5).called and
                   b(board)(2,4).called and
                   b(board)(3,3).called and
                   b(board)(4,2).called and
                   b(board)(5,1).called then
                    Put_Line ("Board" & board'Image & " is a winner");
                    ans := CalcAnswer (board, n);
                    Put_Line ("Answer: " & ans'Image & CR & LF);
                    alreadyWon(board) := True;
                end if;
            end if;
        end loop;
    end CheckBoards;

    -- Given a string with a list of comma-separated or whitespace separated
    -- 1 or 2-digit numbers, fill a vector with those numbers
    procedure parseNumbers (s : String; v : in out IntVecs.Vector) is
        numDigits   : Integer := 0;
        firstDigit  : Integer := 0;
        secondDigit : Integer := 0;
        curNum      : Integer;

        use ASCII;
    begin

        for c of s loop
            if c = ',' or c = ' ' or c = ASCII.CR or c = ASCII.LF then
                
                if numDigits = 2 then
                    curNum := 10 * firstDigit + secondDigit;
                elsif numDigits = 1 then
                    curNum := firstDigit;
                else
                    -- extra whitespace, skip to next char.
                    goto Continue;
                end if;

                IntVecs.Append (v, curNum);
                firstDigit := 0;
                secondDigit := 0;
                numDigits := 0;
            else
                -- got a number - is it the first, or second number?
                if numDigits = 1 then
                    secondDigit := Integer'Value("" & c);
                    numDigits := 2;
                else
                    firstDigit := Integer'Value("" & c);
                    numDigits := 1;
                end if;
            end if;
            <<Continue>>
        end loop;
    end parseNumbers;

    boardIdx : Integer := 1;
    rowIdx   : Integer := 1;
    use ASCII;
begin

    Open (input, Ada.Text_IO.In_File, "input4.txt");

    -- read winning numbers
    parseNumbers (Get_Line(input), winningNumbers);

    -- read boards
    while not End_Of_File (input) loop
       
        declare
            currLine : String := Get_Line(input);
            lineVec  : IntVecs.Vector := IntVecs.Empty_Vector;
        begin
            if currLine(1) = ASCII.LF or currLine(1) = ASCII.CR then
                rowIdx := 1;
            else
                -- parse board row and put those numbers into our array
                parseNumbers (currLine, lineVec);

                for i in 1..5 loop
                    boards(boardIdx)(i, rowIdx).val := lineVec(i);
                end loop;

                if rowIdx = 5 then
                    rowIdx := 1;
                    boardIdx := boardIdx + 1;
                else
                    rowIdx := rowIdx + 1;
                end if;
            end if;
        end;
    end loop;

    Close (input);

    -- now mark boards with winning numbers one by one, see if any of them win
    -- after each iteration.
    MarkWinnerLoop:
    for n of winningNumbers loop
        Put_Line ("Calling " & n'Image);
        MarkBoards (n, boards);

        CheckBoards (boards, n);
    end loop MarkWinnerLoop;

end Day4;
