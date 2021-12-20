with Ada.Text_IO; use Ada.Text_IO;

procedure Day20 is
    input : File_Type;

    width : constant := 100;
    pad   : constant := 52;      -- how many zeroes around the image to use initially

    -- every time we enhance, image grows in width and height by 1
    subtype Rows is Natural range 1..width+2*pad;
    subtype Cols is Natural range 1..width+2*pad;
    type ImgArray is array (Rows, Cols) of Character;
    img : ImgArray := (others => (others => '0'));

    algo : String(1..512);

    boundary : Character := '0';

    function enhance (input : in ImgArray) return ImgArray is
        ret : ImgArray := (others => (others => boundary));
    begin
        for y in Rows'First+1..Rows'Last-1 loop
            for x in Cols'First+1..Cols'Last-1 loop
                declare
                    looks : String := "2#" & input(x-1,y-1) & input(x,y-1) & input(x+1,y-1) &
                                             input(x-1,y)   & input(x,y)   & input(x+1,y) &
                                             input(x-1,y+1) & input(x,y+1) & input(x+1,y+1) & "#";
                    lookup : Natural;
                begin
                    lookup := Natural'Value (looks) + 1;
                    ret(x,y) := (if algo(lookup) = '.' then '0' else '1');
                end;
            end loop;
        end loop;

        boundary := (if boundary = '1' then '0' else '1');

        -- fill boundary edge
        for y in Rows loop
            ret(Cols'First,y) := boundary;
            ret(Cols'Last, y) := boundary;
        end loop;

        for x in Cols loop
            ret(x, Rows'First) := boundary;
            ret(x, Rows'Last) := boundary;
        end loop;

        return ret;
    end enhance;

    procedure dumpImg (im : ImgArray) is
    begin
        for y in Rows loop
            for x in Cols loop
                Put (img(x,y));
            end loop;
            Put_Line ("");
        end loop;
    end dumpImg;

    function count (im : ImgArray) return Natural is
        ret : Natural := 0;
    begin
        for y in Rows loop
            for x in Cols loop
                if img(x,y) = '1' then
                    ret := ret + 1;
                end if;
            end loop;
        end loop;
        return ret;
    end count;

begin

    Open (input, Ada.Text_IO.In_File, "input20.txt");

    algo := Get_Line (input)(1..512);
    declare
        blank : String := Get_Line (input);
        y     : Natural := pad + 1;
    begin
        while not End_Of_File (input) loop
            -- image is square
            declare
                curLine : String := Get_Line (input);
                x       : Natural := pad + 1;
            begin
                for ch of curLine loop
                    if ch = '.' then
                        img(x,y) := '0';
                    elsif ch = '#' then
                        img(x,y) := '1';
                    end if;
                    x := x + 1;
                end loop;
                y := y + 1;
            end;
        end loop;
    end;

    Close (input);

    img := enhance (img);
    img := enhance (img);

    Put_Line ("part 1: number of lit pixels is " & count(img)'Image);

    -- enhance 48 more times
    for i in 3..50 loop
        img := enhance (img);
    end loop;

    Put_Line ("part 2: number of lit pixels is " & count(img)'Image);

end Day20;