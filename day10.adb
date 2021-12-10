with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Interfaces; use Interfaces;

procedure Day10 is
  
    package CharStack is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Character);

    package ScoreStack is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Unsigned_64);
    scores : ScoreStack.Vector;

    package ScoreSort is new ScoreStack.Generic_Sorting;

    type MatchArray is array (Character) of Character;
    match : MatchArray := ('(' => ')', '<' => '>', '{' => '}', '[' => ']', others => 'X');

    type MismatchPoints is array (Character) of Natural;
    mmpoints : MismatchPoints := (')' => 3, ']' => 57, '}' => 1197, '>' => 25137, others => 0);

    input : File_Type;
    linenum : Natural := 1;
    
    part1 : Natural := 0;
    part2 : Unsigned_64 := 0;

    procedure mismatch (c : Character) is
    begin
        part1 := part1 + mmpoints(c);
    end mismatch;

    function autocomplete (stack : in out CharStack.Vector) return Unsigned_64 is
        total : Unsigned_64 := 0;
        
        type PointArray is array (Character) of Unsigned_64;
        points : PointArray := ('(' => 1, '[' => 2, '{' => 3, '<' => 4, others => 0);
    begin
        while stack.Length /= 0 loop
            total := 5 * total + points(stack.Last_Element);
            stack.Delete_Last;
        end loop;

        return total;
    end autocomplete;

    use CharStack;
begin
    Open (input, Ada.Text_IO.In_File, "input10.txt");

    -- parse input
    while not End_Of_File (input) loop
        declare
            line : String := Get_Line (input);
            peek : Character;
            incomplete : Boolean := True;
            stack : CharStack.Vector;
        begin
            parseline: for c of line loop
                if c = ASCII.CR or c = ASCII.LF then
                    exit parseline;
                end if;

                if c = '[' or c = '<' or c = '(' or c= '{' then
                    CharStack.Append (stack, c);
                else
                    -- edge case for first char is closing brace
                    if stack.Length = 0 then
                        mismatch (c);
                        incomplete := False;
                    end if;

                    -- peek at the end of the stack
                    peek := stack.Last_Element;
                    stack.Delete_Last;

                    if c /= match(peek) then
                        mismatch (c);
                        incomplete := False;
                        exit parseline;
                    end if;
                end if;
            end loop parseline;

            -- Now that we get here - what do we have left on the stack?
            if incomplete then
                scores.Append (autocomplete (stack));
            end if;
        end;

        linenum := linenum + 1;
    end loop;

    Close (input);

    -- Sort scores
    ScoreSort.Sort (scores);

    -- find middle score
    part2 := scores.Element(Natural(scores.Length) / 2 + 1);

    Put_Line ("Part 1: Score of syntax errors:       " & part1'Image);
    Put_Line ("Part 2: Middle score of autocomplete: " & part2'Image);
end Day10;
