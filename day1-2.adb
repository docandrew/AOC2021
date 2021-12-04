with Ada.Text_IO; use Ada.Text_IO;

procedure Day1_2 is
    input : File_Type;
    line  : Integer := 0;
    count : Integer := 0;

    windowA : Integer := 0;
    windowB : Integer := 0;
    windowC : Integer := 0;

    curr : Integer := 0;
begin
    Open (input, Ada.Text_IO.In_File, "input.txt");
    
    while not End_Of_File (input) loop
        curr := Integer'Value (Get_Line (input));
        
        -- Reset window based on line.
        if (line mod 3) = 0 then

            if line > 0 then
                windowB := windowB + curr;
            end if;

            if line > 1 then
                windowC := windowC + curr;
            end if;

            if windowB > windowA and line > 2 then
                -- Put_Line ("Increase B > A");
                count := count + 1;
            end if;
            
            windowA := curr;

        elsif (line mod 3) = 1 then

            windowA := windowA + curr;

            if line > 1 then
                windowC := windowC + curr;
            end if;

            if windowC > windowB and line > 3 then
                -- Put_Line ("Increase C > B");
                count := count + 1;
            end if;

            windowB := curr;
        else

            windowA := windowA + curr;
            windowB := windowB + curr;

            if windowA > windowC and line > 4 then
                -- Put_Line ("Increase A > C");
                count := count + 1;
            end if;
            
            windowC := curr;
        end if;

        -- Put_Line ("Line: " & line'Image & " A: " & windowA'Image & " B: " & windowB'Image & " C: " & windowC'Image);
        line := line + 1;
    end loop;

    Put_Line ("total increases: " & count'Image);
end Day1_2;