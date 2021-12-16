with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;
with Interfaces; use Interfaces;

-- when using full input data, probably want to compile with -O3 -gnatp
procedure Day15 is
  
    -- input width, height (can change this for smaller test inputs)
    inWidth  : constant := 100;
    inHeight : constant := 100;

    subtype Cols is Positive range 1..5*inWidth;
    subtype Rows is Positive range 1..5*inHeight;

    row : Natural := 1;
    col : Natural := 1;

    -- original inputs. Make this bigger than actual input, so we can simplify
    -- the search for adjacent vertices.
    type GridType is array (0..Cols'Last+1, 0..Rows'Last+1) of Natural;
    grid : GridType := (others => (others => Natural'Last));

    answer : GridType;

    input : File_Type;

    procedure outputGrid (g : GridType) is
    begin
        for y in Rows'Range loop
            for x in Cols'Range loop
                Put (g(x,y)'Image);
            end loop;
            Put_Line ("");
        end loop;

        Put_Line ("");
    end outputGrid;

    -- Dijkstra's algo
    procedure calcRisk (input : in out GridType; riskTo : out GridType) is

        type Coord is record
            risk : Natural;
            x : Natural;
            y : Natural;
        end record;

        function Get_Priority (Element : Coord) return Natural is
        begin
            return Element.risk; -- + heur (Element.x, Element.y);
        end Get_Priority;

        function Before (Left, Right : Natural) return Boolean is
        begin
            return Left < Right;
        end Before;

        package RiskQ is new Ada.Containers.Synchronized_Queue_Interfaces (Element_Type => Coord);
        package PriorityQ is new Ada.Containers.Unbounded_Priority_Queues (Queue_Interfaces => RiskQ, Queue_Priority => Natural);
        pq : PriorityQ.Queue;

        type AdjList is Array (1..4) of Coord;
        adj : AdjList;

        u : Coord;
        ux : Cols;
        uy : Rows;

        vx : Natural;
        vy : Natural;

        blocksChecked : Natural := 0;
    begin
        riskTo := (others => (others => Natural'Last));
        riskTo (1,1) := 0; -- no risk for starting point
        pq.Enqueue (New_Item => (0, x => 1, y => 1));

        while pq.Current_Use > 0 loop
            -- min risk from set of vertices not yet visited.
            pq.Dequeue (u);
            ux := u.x;
            uy := u.y;
            blocksChecked := blocksChecked + 1;

            -- update risk value of adjacent vertices of picked vertex.
            -- look at 4 adjacent vertices.
            adj(1) := (0, ux-1,uy);
            adj(2) := (0, ux+1,uy);
            adj(3) := (0, ux, uy-1);
            adj(4) := (0, ux, uy+1);
            for v of adj loop
                vx := v.x;
                vy := v.y;

                if vx /= 0 and vy /= 0 and vx /= Cols'Last + 1 and vy /= Rows'Last + 1 then
                    if riskTo (ux,uy) /= Natural'Last then
                        if riskTo (vx,vy) > riskTo (ux,uy) + input (vx,vy) then
                            riskTo (vx,vy) := riskTo (ux,uy) + input (vx,vy);
                            pq.Enqueue ((risk => riskTo (vx,vy), x => vx, y => vy));
                        end if;
                    end if;
                end if;
            end loop;

        end loop;
    end calcRisk;
begin
    Open (input, Ada.Text_IO.In_File, "input15.txt");

    -- parse input
    while not End_Of_File (input) loop
        declare
            line : String := Get_Line (input);
            curr : Natural;
            inc  : Natural := 0;
            curr2 : Natural;
        begin
            col := 1;
            for c of line loop
                if c in '0'..'9' then
                    curr := Natural'Value ((1 => c));

                    -- For Part 2, need to duplicate/increment this value
                    for tiley in 0..4 loop
                        for tilex in 0..4 loop
                            inc := tiley + tilex;
                            if curr + inc > 9 then
                                curr2 := curr + inc - 9;
                            else
                                curr2 := curr + inc;
                            end if;

                            grid (tilex * inWidth + col, tiley * inHeight + row) := curr2;
                        end loop;
                    end loop;

                    col := col + 1;
                end if;
            end loop;
            row := row + 1;
        end;
    end loop;

    Close (input);

    calcRisk (grid, answer);

    -- Note that when the full expanded grid is used, this may be a different answer, since other paths
    -- from the new grids may have a lower cost.
    Put_Line ("Part 1: Lowest Total Risk:" & answer(inWidth, inHeight)'Image);

    Put_Line ("Part 2: Lowest Total Risk for expanded grid:" & answer(Cols'Last, Rows'Last)'Image);

end Day15;
