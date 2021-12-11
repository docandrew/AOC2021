with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Interfaces; use Interfaces;

procedure Day11 is
  
    subtype Cols is Positive range 1..10;
    subtype Rows is Positive range 1..10;

    row : Natural := 1;
    col : Natural := 1;

    type GridType is array (Natural range 0..11, Natural range 0..11) of Natural;
    grid : GridType;

    type FlashType is array (Cols, Rows) of Boolean;
    flashed : FlashType := (others => (others => False));

    input : File_Type;
    
    flashes : Natural := 0;
    steps   : Natural := 0;

    procedure zeroizeEdges (g : in out GridType) is
    begin
        for i in Cols'Range loop
            g(0,i) := 0;
            g(11,i) := 0;
            g(i,0) := 0;
            g(i,11) := 0;
        end loop;
    end zeroizeEdges;

    -- raise energy levels of adjacent octopi by 1.
    procedure flashAdjacent (g : in out GridType; x, y : Natural) is
    begin
        g(x-1,y-1) := @ + 1;
        g(x,  y-1) := @ + 1;
        g(x+1,y-1) := @ + 1;
        g(x+1,y)   := @ + 1;
        g(x-1,y)   := @ + 1;
        g(x+1,y+1) := @ + 1;
        g(x,  y+1) := @ + 1;
        g(x-1,y+1) := @ + 1;
    end flashAdjacent;

    procedure step (g : in out GridType) is
        flashOccurred : Boolean := True;
    begin
        --reset flashed and edges
        zeroizeEdges (g);
        flashed := (others => (others => False));

        -- energy level of each octopus increases by 1.
        for x in Cols'Range loop
            for y in Rows'Range loop
                grid (x,y) := g(x,y) + 1;
            end loop;
        end loop;

        -- now figure out the flashes, continue until flashing stops.
        while flashOccurred loop
            flashOccurred := False;
            for x in Cols'Range loop
                for y in Rows'Range loop
                    if g(x,y) > 9 and not flashed(x,y) then
                        flashAdjacent (g,x,y);
                        flashOccurred := True;
                        flashes := flashes + 1;
                        flashed(x,y) := True;
                    end if;
                end loop;
            end loop;
        end loop;

        -- Anybody that flashed gets reset to 0.
        for x in Cols'Range loop
            for y in Rows'Range loop
                if flashed(x,y) then
                    g(x,y) := 0;
                end if;
            end loop;
        end loop;

        steps := steps + 1;
    end step;

    procedure outputGrid is
    begin
        Put_Line ("After step " & steps'Image);
        
        for y in Cols'Range loop
            for x in Cols'Range loop
                Put (grid(x,y)'Image);
            end loop;
            Put_Line ("");
        end loop;

        Put_Line ("");
    end outputGrid;

    beforeFlashes : Natural;
begin
    Open (input, Ada.Text_IO.In_File, "input11.txt");

    -- parse input
    while not End_Of_File (input) loop
        declare
            line : String := Get_Line (input);
            curr : Natural;
        begin
            col := 1;
            for c of line loop
                if c in '0'..'9' then
                    curr := Natural'Value ((1 => c));
                    grid (col, row) := curr;
                    col := col + 1;
                end if;
            end loop;
            row := row + 1;
        end;
    end loop;

    Close (input);

    for i in 1..100 loop
        step (grid);
        outputGrid;
    end loop;

    Put_Line ("Part 1: Flashes after 100 steps: " & flashes'Image);

    -- continue stepping until we get everyone flashing.
    flashLoop: loop
        beforeFlashes := flashes;
        step (grid);
        outputGrid;
        if flashes = beforeFlashes + 100 then
            Put_Line ("Part 2: First step when everyone flashes: " & steps'Image);
            exit flashLoop;
        end if;
    end loop flashLoop;

end Day11;
