with Ada.Text_IO; use Ada.Text_IO;

procedure Day3_2 is
    input : File_Type;

    type CountArr is array (1..12) of Integer;

    type Diagnostic is array (1..1000) of CountArr;

    numOnes     : CountArr := (others => 0);
    numZeroes   : CountArr := (others => 0);

    oxygenbits  : CountArr := (others => 0);
    carbonbits  : CountArr := (others => 0);

    oxygen : Integer := 0; -- most common bits
    carbon : Integer := 0; -- least common bits
    answer : Integer := 0;
    
begin
    Open (input, Ada.Text_IO.In_File, "input3.txt");
    
    while not End_Of_File (input) loop
        declare
            line : String := Get_Line (input);
        begin
            Put_Line ("line: " & line);

            for i in 1..12 loop
                if line(i) = '1' then
                    numOnes(i) := numOnes(i) + 1;
                else
                    numZeroes(i) := numZeroes(i) + 1;
                end if;
            end loop;
        end;
    end loop;

    for i in 1..12 loop
        Put_Line ("Number of ones in column  " & i'Image & " :" & numOnes(i)'Image);
        Put_Line ("Number of zeroes in column" & i'Image & " :" & numZeroes(i)'Image);

        if numOnes(i) > numZeroes(i) then
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

    Put_Line ("gammabits: ");
    for i in 1..12 loop
        Put (gammabits(i)'Image);
    end loop;

    Put_Line ("");
    Put_Line ("epsilonbits: ");
    for i in 1..12 loop
        Put (epsilonbits(i)'Image);
    end loop;
    Put_Line ("");
    Put_Line ("gamma:   " & gamma'Image);
    Put_Line ("epsilon: " & epsilon'Image);

    answer := gamma * epsilon;
    Put_Line ("answer:" & answer'Image);
end Day3_2;