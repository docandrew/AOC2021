with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure Day22 is

    input : File_Type;

    type Instruction is record
        valid  : Boolean := False;
        on     : Boolean := False;
        startx : Long_Long_Integer := 0;
        endx   : Long_Long_Integer := 0;
        starty : Long_Long_Integer := 0;
        endy   : Long_Long_Integer := 0;
        startz : Long_Long_Integer := 0;
        endz   : Long_Long_Integer := 0;
    end record;

    package InstructionV is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Instruction);
    instructions : InstructionV.Vector;

    procedure printInst (i : Instruction) is
    begin
        Put_Line (i.on'Image & " x=" & i.startx'Image & ".." & i.endx'Image & ",y=" & i.starty'Image & ".." & i.endy'Image & ",z=" & i.startz'Image & ".." & i.endz'Image);
    end printInst;

    function parseCoord (s : String) return Instruction is
        space : Natural := Index (s, " ", s'First);
        equal1 : Natural := Index (s, "=", s'First);
        dots1  : Natural := Index (s, "..", s'First);

        comma1 : Natural := Index (s, ",", s'First + 1);
        equal2 : Natural := Index (s, "=", comma1 + 1);
        dots2  : Natural := Index (s, "..", comma1 + 1);        
        
        comma2 : Natural := Index (s, ",", dots2 + 1);
        equal3 : Natural := Index (s, "=", comma2 + 1);
        dots3  : Natural := Index (s, "..", comma2 + 1);

        turn : String := (s(s'First..space-1));

        sx : String := (s(equal1+1..dots1-1));
        ex : String := (s(dots1+2..comma1-1));
        sy : String := (s(equal2+1..dots2-1));
        ey : String := (s(dots2+2..comma2-1));
        sz : String := (s(equal3+1..dots3-1));
        ez : String := (s(dots3+2..s'Last-1));
        
        ret : Instruction;
    begin
        ret.startx := Long_Long_Integer(Integer'Value (sx));
        ret.endx   := Long_Long_Integer(Integer'Value (ex));
        ret.starty := Long_Long_Integer(Integer'Value (sy));
        ret.endy   := Long_Long_Integer(Integer'Value (ey));
        ret.startz := Long_Long_Integer(Integer'Value (sz));
        ret.endz   := Long_Long_Integer(Integer'Value (ez));
        ret.on     := (if turn = "on" then True else False);

        return ret;
    end parseCoord;

    function min (l,r : Long_Long_Integer) return Long_Long_Integer is
    begin
        return (if l <= r then l else r);
    end min;

    function max (l,r : Long_Long_Integer) return Long_Long_Integer is
    begin
        return (if l > r then l else r);
    end max;

    curInst : Instruction;
begin

    Open (input, Ada.Text_IO.In_File, "input22.txt");
    
    while not End_Of_File (input) loop
        instructions.Append ( parseCoord (Get_Line (input)));
    end loop;

    Close (input);

    -- For part 1, we can just track a big multidimensional array. Won't fly for part 2 though.
    part1: declare
        type ReactorArr is array (Integer range -50..50, Integer range -50..50, Integer range -50..50) of Boolean;
        reactor : ReactorArr := (others => (others => (others => False)));

        count : Natural := 0;
    begin
        for i of instructions loop
            if i.startx in -50..50 and i.starty in -50..50 and i.startz in -50..50 then
                for x in i.startx..i.endx loop
                    for y in i.starty..i.endy loop
                        for z in i.startz..i.endz loop
                            reactor (Integer(x),Integer(y),Integer(z)) := i.on;
                        end loop;
                    end loop;
                end loop;
            end if;
        end loop;

        for z in -50..50 loop
            for y in -50..50 loop
                for x in -50..50 loop
                    count := count + (if reactor(x,y,z) then 1 else 0);
                end loop;
            end loop;
        end loop;

        Put_Line ("Part 1 count: " & count'Image);

    end part1;

    part2: declare
        totalBits : Long_Long_Integer := 0;
        newIns : InstructionV.Vector;

        function intersect (a, b : Instruction) return Instruction is
            ret : Instruction;
        begin
            if a.endx < b.startx or a.startx > b.endx or
               a.endy < b.starty or a.starty > b.endy or
               a.endz < b.startz or a.startz > b.endz then
                ret.valid := False;
                return ret;
            else
                -- intersecting volume
                ret.startx := max (a.startx, b.startx);
                ret.endx   := min (a.endx, b.endx);
                ret.starty := max (a.starty, b.starty);
                ret.endy   := min (a.endy, b.endy);
                ret.startz := max (a.startz, b.startz);
                ret.endz   := min (a.endz, b.endz);

                -- put this intersecting region on list
                ret.valid := True;
                if not a.on and not b.on then
                    -- off intersects off: we need to turn _on_ this intersection so we don't double-count the original off
                    ret.on := True;
                elsif not a.on and b.on then
                    -- off intersects on: turn off the intersection only
                    ret.on := False;
                elsif a.on and b.on then
                    -- on intersects on: turn off the intersection so we don't double-count.
                    ret.on := False;
                elsif a.on and not b.on then
                    -- on intersects off: turn on the intersection.
                    ret.on := True;
                end if;

                return ret;
            end if;
        end intersect;

        -- Return the volume for a given instruction, or negative volume if it's an "off" instruction
        function volume (i : Instruction) return Long_Long_Integer is
            tmp : Long_Long_Integer;
        begin
            tmp := Long_Long_Integer((i.endx - i.startx + 1) *
                                     (i.endy - i.starty + 1) *
                                     (i.endz - i.startz + 1));
            if i.on then 
                return tmp;
            else
                return -tmp;
            end if;
        end volume;

        tmp : Instruction;
        count : Long_Long_Integer := 0;
    begin
        for i in 1..Natural(instructions.Length) loop
            for j in 1..Natural(newIns.Length) loop
                tmp := intersect (instructions(i), newIns(j));

                if tmp.valid then
                    -- add it to list before current instruction.
                    newIns.Append (tmp);
                end if;
            end loop;

            -- if an "off" instruction already intersected with something else before it,
            -- that intersection would have been added already.
            if instructions(i).on then
                newIns.Append (instructions(i));
            end if;
        end loop;

        -- count our expanded instruction list
        for i of newIns loop
            count := count + volume (i);
        end loop;

        Put_Line ("Final count: " & count'Image);
    end part2;

end Day22;
