with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Interfaces; use Interfaces;

procedure Day14 is
  
    input : File_Type;

    type PairInsertion is record
        pair   : String(1..2);
        insert : Character;
    end record;

    package PairVec is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => PairInsertion);
    pairs : PairVec.Vector;

    template : Unbounded_String;

    type HistArray is array (Character range 'A'..'Z') of Unsigned_64;
    hist : HistArray := (others => 0);
    hist2 : HistArray := (others => 0);

    -- Determine frequency of each character
    procedure histogram (ubs : Unbounded_String; h : in out HistArray) is
    begin
        for c of To_String (ubs) loop
            h(c) := h(c) + 1;
        end loop;
    end histogram;

    -- Find largest count in histogram
    function max (h : HistArray) return Unsigned_64 is
        mc : Unsigned_64 := 0;
    begin
        for c of h loop
            mc := (if c > mc then c else mc);
        end loop;

        return mc;
    end max;

    -- Find smallest count in histogram
    function min (h : HistArray) return Unsigned_64 is
        lc : Unsigned_64 := Unsigned_64'Last;
    begin
        for c of h loop
            lc := (if c < lc and c > 0 then c else lc);
        end loop;

        return lc;
    end min;

    part1 : Unsigned_64;
    part2 : Unsigned_64;

    -- For part 2, don't create the string, just update char (pair) counts.
    -- we can represent a pair of chars as a Natural. i.e. AA = 0, AB = 1, BA = 26, etc.
    -- store the count of each of these pairs
    type CountArr is array (Natural range 0..675) of Unsigned_64;
    pairCounts : CountArr := (others => 0);

    function pairToNum (s : String) return Natural is
        a : Natural := Character'Pos (s(s'First)) - 65;
        b : Natural := Character'Pos (s(s'First+1)) - 65;
    begin
        return a * 26 + b;
    end pairToNum;

    -- Determine initial pairs in the string.
    procedure initPairCounts (t : Unbounded_String) is
    begin
        pairCounts := (others => 0);
        for i in 1..Length (t)-1 loop
            declare
                pair : String := To_String (t)(i..i+1);
            begin
                pairCounts (pairToNum (pair)) := pairCounts (pairToNum (pair)) + 1;
            end;
        end loop;
    end initPairCounts;

    -- Now, for every substitution, determine first how many of those pairs exist.
    -- Then determine which 2 new pairs are created. For every original pair,
    -- add that count to the counts of those 2 new pairs. However, we lose the
    -- original pair, which is why newCounts is initialized to zero.
    procedure updatePairs (h : in out HistArray) is
        -- need to do this since template isn't modified in-place.
        newCounts : CountArr := (others => 0);
    begin
        for p of pairs loop
            if pairCounts (pairToNum (p.pair)) > 0 then
                declare
                    new1 : String := "" & p.pair(1) & p.insert;
                    new2 : String := "" & p.insert & p.pair(2);
                begin
                    h (p.insert) := h (p.insert) + pairCounts (pairToNum (p.pair));

                    -- something like NN -> C adds 2 pairs, NC and CN
                    newCounts (pairToNum (new1)) := newCounts (pairToNum (new1)) + pairCounts (pairToNum (p.pair));
                    newCounts (pairToNum (new2)) := newCounts (pairToNum (new2)) + pairCounts (pairToNum (p.pair));
                end;
            end if;
        end loop;

        pairCounts := newCounts;
    end updatePairs;

begin
    Open (input, Ada.Text_IO.In_File, "input14.txt");

    -- first line polymer template (strip trailing newline)
    declare
        firstline : String := Get_Line (input);
    begin
        template := To_Unbounded_String (firstline (1..firstline'Length-1));
    end;

    while not End_Of_File (input) loop
        declare
            line  : String := Get_Line (input);
            arrow : Positive;
        begin
            if line(1) in 'A'..'Z' then
                for i in line'Range loop
                    -- find arrow
                    if line(i) = '-' then
                        arrow := i;
                        declare
                            pair   : String := line(1..arrow-2);
                            insert : String := line(arrow+3..line'Length-1);
                        begin
                            pairs.Append ((pair, insert(insert'Last)));
                        end;
                    end if;
                end loop;
            end if;
        end;
    end loop;

    Close (input);

     -- Get initial histogram which we'll update as substitutions happen
    initPairCounts (template);
    histogram (template, hist);

    for i in 1..10 loop
        updatePairs (hist);
    end loop;

    part1 := max (hist) - min (hist);
    Put_Line ("Part 1:" & part1'Image);

    initPairCounts (template);
    histogram (template, hist2);

    for i in 1..40 loop
        updatePairs (hist2);
    end loop;

    part2 := max (hist2) - min (hist2);
    Put_Line ("Part 2:" & part2'Image);
end Day14;
