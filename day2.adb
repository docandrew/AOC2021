with Ada.Text_IO; use Ada.Text_IO;

procedure Day2 is
    input : File_Type;

    depth : Integer := 0;
    horiz : Integer := 0;
    aim   : Integer := 0;
    
    answer : Integer := 0;
begin
    Open (input, Ada.Text_IO.In_File, "input2.txt");
    
    while not End_Of_File (input) loop
        declare
            line : String := Get_Line (input);

        begin
            -- just need first char of string to determine command
            if line(1) = 'f' then
                declare
                    numval : String := line(9..line'Length);
                    d : Integer := Integer'Value(numval);
                begin
                    horiz := horiz + d;
                    depth := depth + (aim * d);
                end;
            elsif line(1) = 'd' then
                declare
                    numval : String := line(6..line'Length);
                    d : Integer := Integer'Value(numval);
                begin
                    aim := aim + d;
                end;
            else
                declare
                    numval : String := line(4..line'Length);
                    d : Integer := Integer'Value(numval);
                begin
                    aim := aim - d;
                end;
            end if;
        end;

        Put_Line ("horiz:" & horiz'Image & " aim: " & aim'Image & " depth:" & depth'Image);
    end loop;

    answer := depth * horiz;
    Put_Line ("answer:" & answer'Image);
end Day2;