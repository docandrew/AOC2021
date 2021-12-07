with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;

-- Need to compile this one with -gnatX for 'Reduce attribute, -m64 may be needed too.
procedure Day6 is
  
    subtype FishAge is Natural range 0..8;

    type FishPopulations is array (FishAge) of Unsigned_64;
    fish : FishPopulations := (others => 0);

    input : File_Type;

    count : Unsigned_64 := 0;
    curr  : Natural := 0;

    zerofish : Unsigned_64;

    numDays : Natural := 256;
begin
    Open (input, Ada.Text_IO.In_File, "input6.txt");

    -- parse initial ages
    for c of Get_Line (input) loop
        if c in '0'..'8' then
            curr := Natural'Value ((1 => c));
            fish(curr) := @ + 1;
            -- Put(curr'Image & ",");
        end if;
    end loop;

    Close (input);

    for day in 1..numDays loop
        -- "Age" the fish
        -- Tmp the zero fish
        zerofish := fish(0);

        -- Then every other fish ages down a day
        for i in 0..7 loop
            fish(i) := fish(i+1);
        end loop;

        -- First, for every 0-aged fish we had, add a new 8-aged fish
        fish(8) := zerofish;

        -- Then, all the 0-aged fish become 6-aged fish
        fish(6) := @ + zerofish;
    end loop;

    -- Tally everything up
    count := fish'Reduce("+", 0);

    Put_Line ("Total Fish after" & numDays'Image & " days: " & count'Image);
end Day6;
