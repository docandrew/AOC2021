with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Vectors;
with Interfaces; use Interfaces;

procedure Day7 is
  
    package NatVecs is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Natural);
    dists : NatVecs.Vector;

    input : File_Type;
    curr  : Natural := 0;

    max   : Natural := Natural'First;
    min   : Natural := Natural'Last;

    currenergy : Natural := 0;
    minenergy  : Natural := Natural'Last;
    currburn   : Natural := 0;
    minburn    : Natural := Natural'Last;

    -- absolute difference
    function Distance (d1 : Natural; d2 : Natural) return Natural is
    begin
        return (if d1 < d2 then d2 - d1 else d1 - d2);
    end Distance;

    -- Part 2 fuel burn. Given a distance d, actual fuel burn is
    -- 0 + ... (d - 1) = d(d - 1)/2
    -- so 0 + ... (d) = d + d(d - 1)/2
    -- I forget where I read that trick, I think it was an anecdote about a
    -- Microsoft interview question in the 80s...
    function Burn (d : Natural) return Natural is
    begin
        if d = 0 or d = 1 then return d; end if;

        return d + ((d * (d - 1)) / 2);
    end Burn;
begin
    Open (input, Ada.Text_IO.In_File, "input7.txt");

    -- parse input
    declare
        line : String := Get_Line (input);
        from : Natural := 1;
        to   : Natural := 1;
        cnt  : Natural := Ada.Strings.Fixed.Count (Source => line, Pattern => ",");
    begin
        for i in 1..cnt loop
            -- find next comma or LF
            to := Ada.Strings.Fixed.Index (Source  => line(from..line'Length),
                                           Pattern => ",",
                                           From    => from);
            
            curr := Natural'Value(line(from..to-1));

            -- Keep track of min/max values
            min := (if curr < min then curr else min);
            max := (if curr > max then curr else max);
            NatVecs.Append (dists, curr);

            from := to + 1;
        end loop;

        -- get last value
        curr := Natural'Value (line(from..line'Length-1));
        min := (if curr < min then curr else min);
        max := (if curr > max then curr else max);
        NatVecs.Append (dists, curr);
    end;

    Close (input);

    -- Try a bunch of horizontal positions, calculate the sum of the distances
    -- between each crab and the proposed destination.
    for dest in min..max loop
        currenergy := 0;
        currburn   := 0;
        
        for d of dists loop
            currenergy := currenergy + Distance (d, dest);
            currburn   := currburn   + Burn (Distance (d, dest));
        end loop;

        minenergy := (if currenergy < minenergy then currenergy else minenergy);
        minburn   := (if currburn < minburn then currburn else minburn);
    end loop;

    Put_Line ("Part 1: Lowest energy: " & minenergy'Image);
    Put_Line ("Part 2: Lowest burn:   " & minburn'Image);
end Day7;
