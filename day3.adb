with Ada.Text_IO; use Ada.Text_IO;

procedure Day3 is
    input : File_Type;
    lineNum : Integer := 1;

    type CountArr is array (1..12) of Integer;

    numOnes     : CountArr := (others => 0);
    numZeroes   : CountArr := (others => 0);
    gammabits   : CountArr := (others => 0);
    epsilonbits : CountArr := (others => 0);

    gamma   : Integer := 0; -- most common bits
    epsilon : Integer := 0; -- least common bits
    answer  : Integer := 0;

    -- for part 2
    type Filter is record
        valid : Boolean;
        line  : CountArr;
    end record;

    type AllCounts is array (1..1000) of Filter;

    oxygenvals : AllCounts;
    carbonvals : AllCounts;
    
    oxygenNumValid : Integer := 1000;
    carbonNumValid : Integer := 1000;

    oxygenbits : CountArr := (others => 0);
    carbonbits : CountArr := (others => 0);

    oxygen  : Integer := 0;
    carbon  : Integer := 0;
    answer2 : Integer := 0;
begin
    Open (input, Ada.Text_IO.In_File, "input3.txt");
    
    while not End_Of_File (input) loop
        declare
            line : String := Get_Line (input);

        begin
            -- fill in oxygenvals and carbonvals
            oxygenvals(lineNum).valid := True;
            carbonvals(lineNum).valid := True;

            for i in 1..12 loop

                oxygenvals(lineNum).line(i) := Integer'Value("" & line(i));
                carbonvals(lineNum).line(i) := Integer'Value("" & line(i));

                if line(i) = '1' then
                    numOnes(i) := numOnes(i) + 1;
                else
                    numZeroes(i) := numZeroes(i) + 1;
                end if;
            end loop;

            lineNum := lineNum + 1;
        end;
    end loop;

    for i in 1..12 loop
        if numOnes(i) >= numZeroes(i) then
            gammabits(i) := 1;
            epsilonbits(i) := 0;
        else
            gammabits(i) := 0;
            epsilonbits(i) := 1;
        end if;
    end loop;

    for i in 0..11 loop
        gamma := gamma + gammabits(12 - i) * (2 ** i);
        epsilon := epsilon + epsilonbits(12 - i) * (2 ** i);
    end loop;

    Put_Line ("");
    Put_Line ("gamma:   " & gamma'Image);
    Put_Line ("epsilon: " & epsilon'Image);

    answer := gamma * epsilon;
    Put_Line ("answer:" & answer'Image);
    Put_Line ("");

    -- Determine oxygen generator rating
    -- First, mark invalid all values whose MSB doesn't match most common bit (epsilon(1)).
    for i in 1..1000 loop

        if oxygenvals(i).line(1) /= gammabits(1) then
            oxygenvals(i).valid := False;
            oxygenNumValid := oxygenNumValid - 1;
        end if;

        if carbonvals(i).line(1) /= epsilonbits(1) then
            carbonvals(i).valid := False;
            carbonNumValid := carbonNumValid - 1;
        end if;
    end loop;

    -- Now, for each remaining bit position, from all the valid entries, determine MSB and LSB
    for bit in 2..12 loop

        declare
            oxygenOneCount : Integer := 0;
            oxygenZeroCount : Integer := 0;
            oxygenMSB : Integer := Integer'Last;
            
            carbonOneCount : Integer := 0;
            carbonZeroCount : Integer := 0;
            carbonLSB : Integer := Integer'Last;

        begin
            -- determine MSB and LSB
            for j in 1..1000 loop
                if oxygenvals(j).valid then
                    if oxygenvals(j).line(bit) = 1 then
                        oxygenOneCount := oxygenOneCount + 1;
                    else
                        oxygenZeroCount := oxygenZeroCount + 1;
                    end if;
                end if;

                if carbonvals(j).valid then
                    if carbonvals(j).line(bit) = 1 then
                        carbonOneCount := carbonOneCount + 1;
                    else
                        carbonZeroCount := carbonZeroCount + 1;
                    end if;
                end if;
            end loop;

            if oxygenOneCount >= oxygenZeroCount then
                oxygenMSB := 1;
            else
                oxygenMSB := 0;
            end if;

            if carbonOneCount >= carbonZeroCount then
                carbonLSB := 0;
            else
                carbonLSB := 1;
            end if;

            -- see if we're down to one valid number, if so, no need to filter further, just find it.
            if oxygenNumValid = 1 then
                for i in 1..1000 loop
                    if oxygenvals(i).valid then
                        Put_Line ("Found oxygen value on line" & i'Image);
                        oxygen := 0;    -- in case we find it twice
                        oxygenbits := oxygenvals(i).line;
        
                        for j in 0..11 loop
                            oxygen := oxygen + oxygenbits(12 - j) * (2 ** j);
                        end loop;
                    end if;
                end loop;
            end if;

            if carbonNumValid = 1 then
                for i in 1..1000 loop
                    if carbonvals(i).valid then
                        Put_Line ("Found carbon value on line" & i'Image);
                        carbon := 0;    -- in case we find it twice
                        carbonbits := carbonvals(i).line;

                        for j in 0..11 loop
                            carbon := carbon + carbonbits(12 - j) * (2 ** j);
                        end loop;
                    end if;
                end loop;
            end if;

            -- Now filter out remaining values based on MSB, LSB
            for j in 1..1000 loop
                if oxygenvals(j).valid and oxygenvals(j).line(bit) /= oxygenMSB then
                    oxygenvals(j).valid := False;
                    oxygenNumValid := oxygenNumValid - 1;
                end if;

                if carbonvals(j).valid and carbonvals(j).line(bit) /= carbonLSB then
                    carbonvals(j).valid := False;
                    carbonNumValid := carbonNumValid - 1;
                end if;
            end loop;

            -- kind of a hack...
            -- dirty way to re-check in case we miss something on the last filter run
            if oxygenNumValid = 1 then
                for i in 1..1000 loop
                    if oxygenvals(i).valid then
                        Put_Line ("Found oxygen value on line" & i'Image);
                        oxygenbits := oxygenvals(i).line;
                        oxygen := 0;
                        for j in 0..11 loop
                            oxygen := oxygen + oxygenbits(12 - j) * (2 ** j);
                        end loop;
                    end if;
                end loop;
            end if;

            if carbonNumValid = 1 then
                for i in 1..1000 loop
                    if carbonvals(i).valid then
                        Put_Line ("Found carbon value on line" & i'Image);
                        carbonbits := carbonvals(i).line;
                        carbon := 0;
                        for j in 0..11 loop
                            carbon := carbon + carbonbits(12 - j) * (2 ** j);
                        end loop;
                    end if;
                end loop;
            end if;
        end;
    end loop;

    Put_Line ("Oxygen Value: " & oxygen'Image);
    Put_Line ("Carbon Value: " & carbon'Image);
    answer2 := oxygen * carbon;
    Put_Line ("Second answer: " & answer2'Image);

end Day3;