with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;

procedure Day9 is

    subtype Cols is Positive range 1..100;
    subtype Rows is Positive range 1..100;

    type GridType is array (Cols, Rows) of Natural;
    grid : GridType;

    -- keep track of which elements we've already added to our basin.
    type DirtyGrid is array (Cols, Rows) of Boolean;
    dirty : DirtyGrid := (others => (others => False));

    input : File_Type;

    curr  : Natural := 0;

    row : Natural := 1;
    col : Natural := 1;

    part1 : Natural := 0;
    part2 : Natural := 0;

    function Risk (n : Natural) return Natural is
    begin
        return 1 + n;
    end Risk;

    function FindBasinSize (x : Natural; y : Natural; g : in out GridType) return Natural is
    begin
        -- base cases
        if x < 1 or y < 1 or x > Cols'Last or y > Rows'Last then
            return 0;
        end if;

        -- check this separately from array bounds since Ada will evaluate all
        -- the clauses and this can result in an array bound error.
        if g(x,y) = 9 then
            return 0;
        end if;

        -- check/mark this square as dirty so it doesn't get double-counted
        if dirty(x,y) then
            return 0;
        else
            dirty(x,y) := True;
            return 1 + FindBasinSize(x, y-1, g) +
                       FindBasinSize(x, y+1, g) +
                       FindBasinSize(x-1, y, g) +
                       FindBasinSize(x+1, y, g);
        end if;
    end FindBasinSize;

    curBasin : Natural;
    basin1   : Natural := 0;
    basin2   : Natural := 0;
    basin3   : Natural := 0;
begin
    Open (input, Ada.Text_IO.In_File, "input9.txt");

    -- parse input
    while not End_Of_File (input) loop
        col := 1;
        for c of Get_Line (input) loop
            if c in '0'..'9' then
                curr := Natural'Value ((1 => c));
                grid (col, row) := curr;
                col := col + 1;
            end if;
        end loop;
        row := row + 1;
    end loop;

    -- find low points
    for x in Cols'Range loop
        for y in Rows'Range loop
            if x = Cols'First and y = Rows'First then
                -- top left corner
                if grid(x,y) < grid(x+1,y) and grid(x,y) < grid(x,y+1) then
                    part1 := part1 + Risk(grid(x,y));
                end if;
            elsif x = Cols'Last and y = Rows'First then
                -- top right corner
                if grid(x,y) < grid(x-1,y) and grid(x,y) < grid(x,y+1) then
                    part1 := part1 + Risk(grid(x,y));
                end if;
            elsif x = Cols'First and y = Rows'Last then
                -- bottom left corner
                if grid(x,y) < grid(x+1,y) and grid(x,y) < grid(x,y-1) then
                    part1 := part1 + Risk(grid(x,y));
                end if;
            elsif x = Cols'Last and y = Rows'Last then
                -- bottom right corner
                if grid(x,y) < grid(x-1,y) and grid(x,y) < grid(x,y-1) then
                    part1 := part1 + Risk(grid(x,y));
                end if;
            elsif x = Cols'First then
                -- left edge (non corner)
                if grid(x,y) < grid(x,y-1) and grid(x,y) < grid(x,y+1) and grid(x,y) < grid(x+1,y) then
                    part1 := part1 + Risk(grid(x,y));
                end if;
            elsif x = Cols'Last then
                -- right edge (non corner)
                if grid(x,y) < grid(x,y-1) and grid(x,y) < grid(x,y+1) and grid(x,y) < grid(x-1,y) then
                    part1 := part1 + Risk(grid(x,y));
                end if;
            elsif y = Rows'First then
                -- top edge (non corner)
                if grid(x,y) < grid(x-1,y) and grid(x,y) < grid(x+1,y) and grid(x,y) < grid(x,y+1) then
                    part1 := part1 + Risk(grid(x,y));
                end if;
            elsif y = Rows'Last then
                -- bottom edge (non corner)
                if grid(x,y) < grid(x-1,y) and grid(x,y) < grid(x+1,y) and grid(x,y) < grid(x,y-1) then
                    part1 := part1 + Risk(grid(x,y));
                end if;
            else
                -- anything else
                if grid(x,y) < grid(x-1,y) and grid(x,y) < grid(x+1,y) and grid(x,y) < grid(x,y-1) and grid(x,y) < grid(x, y+1) then
                    part1 := part1 + Risk(grid(x,y));
                end if;
            end if;
        end loop;
    end loop;

    Close (input);
    Put_Line ("part 1: " & part1'Image);

    -- Find largest basins.
    for x in Cols'Range loop
        for y in Rows'Range loop
            curBasin := FindBasinSize (x, y, grid);

            if curBasin > basin1 then
                basin2 := basin1;
                basin3 := basin2;
                basin1 := curBasin;
            elsif curBasin > basin2 then
                basin3 := basin2;
                basin2 := curBasin;
            elsif curBasin > basin3 then
                basin3 := curBasin;
            end if;
        end loop;
    end loop;

    part2 := basin1 * basin2 * basin3;
    Put_Line ("part 2: " & part2'Image);

end Day9;
