with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;

-- Note: need to manually add extra newline after last line of input
procedure Day5 is
    input : File_Type;

    type Segment is record
        x1 : Natural;
        y1 : Natural;
        x2 : Natural;
        y2 : Natural;
    end record;

    -- 500 line segments in input
    type SegmentArr is array (1..500) of Segment;
    -- type SegmentArr is array (1..10) of Segment; -- for test data
    segments : SegmentArr;

    -- hacky... but no collisions...
    function HashN (n : Natural) return Ada.Containers.Hash_Type is
    begin
        return Ada.Containers.Hash_Type (n);
    end HashN;

    -- not memory-friendly but fast lookups!
    type LookupList is array (0..999_999) of Natural;
    -- type LookupList is array (0..99) of Natural; -- for test data
    lookup : LookupList := (others => 0);

    -- Convert 2D coord into a 1D unique position that we can use for a hashmap
    function get1D (x : Natural; y : Natural) return Natural is
    begin
        return y * 1000 + x;
        -- return y * 10 + x;  --for test data
    end get1D;

    -- Walk a vertical line segment, updating the lookup list as we do so
    procedure walkVert (s : Segment; l : in out LookupList) is
    begin
        if s.y2 > s.y1 then
            for y in s.y1..s.y2 loop
                l (get1D(s.x1, y)) := l (get1D(s.x1, y)) + 1;
            end loop;
        else
            for y in s.y2..s.y1 loop
                l (get1D(s.x1, y)) := l (get1D(s.x1, y)) + 1;
            end loop;
        end if;
    end walkVert;

    -- Walk a horizontal line segment, updating the lookup list as we do so
    procedure walkHoriz (s : Segment; l : in out LookupList) is
    begin
        if s.x2 > s.x1 then
            for x in s.x1..s.x2 loop
                l(get1D(x, s.y1)) := l(get1D(x, s.y1)) + 1;
            end loop;
        else
            for x in s.x2..s.x1 loop
                l(get1D(x, s.y1)) := l(get1D(x, s.y1)) + 1;
            end loop;
        end if;
    end walkHoriz;

    -- Walk any diagonal line segment, updating the lookup list as we do so
    procedure walk (s : Segment; l : in out LookupList) is
        startX : Natural;
        endX   : Natural;
        y      : Integer;
        dy     : Integer;
    begin
        if s.x2 > s.x1 then
            startX := s.x1;
            endX   := s.x2;
            y      := s.y1;
            dy := (if s.y2 > s.y1 then 1 else -1);
        else
            startX := s.x2;
            endX   := s.x1;
            y      := s.y2;
            dy := (if s.y2 > s.y1 then -1 else 1);
        end if;


        for x in startX..endX loop
            l(get1D(x, y)) := l(get1D(x, y)) + 1;
            y := y + dy;
        end loop;
    end walk;

    -- Given a line segment in the format prescribed, return a Segment record
    function parseSegment (s : String) return Segment is
        ret  : Segment;

        i    : Natural := 1;
        from : Natural := 1;
        to   : Natural := 1;

        use ASCII;
    begin
        -- find first comma 
        while s(i) /= ',' loop
            i := i + 1;
        end loop;

        to := i - 1;

        ret.x1 := Integer'Value (s(from..to));

        from := i + 1;

        -- find whitespace
        while s(i) /= ' ' loop
            i := i + 1;
        end loop;

        to := i;

        ret.y1 := Integer'Value (s(from..to));

        from := to;

        -- skip " -> "
        while s(i) not in '0'..'9' loop
            i := i + 1;
        end loop;

        from := i;

        while s(i) in '0'..'9' loop
            i := i + 1;
        end loop;

        to := i - 1;

        ret.x2 := Integer'Value (s(from..to));

        from := i + 1;

        to := s'Length;
        ret.y2 := Integer'Value(s(from..to));

        return ret;
    end parseSegment;

    segIdx : Natural := 1;
    num2s  : Natural := 0;

    use ASCII;
begin
    Open (input, Ada.Text_IO.In_File, "input5.txt");

    -- read segments
    while not End_Of_File (input) loop
       
        declare
            currLine : String := Get_Line(input);
        begin
            segments(segIdx) := parseSegment (currLine);
            segIdx := segIdx + 1;
        end;
    end loop;
    Close (input);

    -- now put these into our lookup table
    for i in segments'Range loop
        if segments(i).x1 = segments(i).x2 then
            walkVert (segments(i), lookup);
        elsif segments(i).y1 = segments(i).y2 then
            walkHoriz (segments(i), lookup);
        end if;
    end loop;

    -- go through lookup table looking for any 2's or greater
    for i of lookup loop
        if i > 1 then
            num2s := num2s + 1;
        end if;
    end loop;

    Put_Line ("Part 1 Answer: " & num2s'Image);

    -- walk rest of non-orthogonal segments, update list again.
    for i in segments'Range loop
        if segments(i).x1 /= segments(i).x2 and segments(i).y1 /= segments(i).y2 then
            walk (segments(i), lookup);
        end if;
    end loop;

    num2s := 0;

    -- go through lookup table again
    for i of lookup loop
        if i > 1 then
            num2s := num2s + 1;
        end if;
    end loop;

    Put_Line ("Part 2 Answer: " & num2s'Image);
end Day5;
