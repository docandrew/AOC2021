with Ada.Text_IO; use Ada.Text_IO;

procedure Day1 is
    input : File_Type;
    count : Integer := 0;
    prev  : Integer := Integer'Last;
    curr  : Integer := 0;
begin
    Open (input, Ada.Text_IO.In_File, "input.txt");
    
    while not End_Of_File (input) loop
        curr := Integer'Value (Get_Line (input));

        if curr > prev then
            count := count + 1;
        end if;

        prev := curr;
    end loop;

    Put_Line ("total increases: " & count'Image);
end Day1;
