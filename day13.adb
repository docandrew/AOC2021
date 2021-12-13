with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;
with Interfaces; use Interfaces;

procedure Day13 is
  
    input : File_Type;

    paperWidth  : Natural := 0;
    paperHeight : Natural := 0;

    type Coord is record
        x : Natural;
        y : Natural;
    end record;

    function Hash (c : Coord) return Hash_Type is
    begin
        return Hash_Type (c.y * 2000 + c.x);
    end Hash;

    function Same (left : Coord; right : Coord) return Boolean is
    begin
        return left.x = right.x and left.y = right.y;
    end Same;

    package CoordSets is new Ada.Containers.Hashed_Sets (Element_Type => Coord, Hash => Hash, Equivalent_Elements => Same);
    coords : CoordSets.Set;

    type FoldDirection is (Up, Left);

    type Fold is record
        dir  : FoldDirection;
        dist : Natural;
    end record;

    package FoldV is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Fold);
    folds : FoldV.Vector;

    -- if folding up, everything below the fold gets "mirrored" up.
    -- if folding right, everything to the left of the fold gets "mirrored" right.
    -- when done, adjust paper size.
    function doFold (f : Fold) return CoordSets.Set is
        tmp : Coord;
        newCoords : CoordSets.Set;
    begin
        if f.dir = Up then
            for c of coords loop
                if c.y > f.dist then
                    -- need to mirror this. the further away from the fold, the smaller the new coord will be.
                    tmp.x := c.x;
                    tmp.y := 2 * f.dist - c.y;
                    newCoords.Include (tmp);
                elsif c.y < f.dist then
                    newCoords.Include (c);
                else
                    null;   -- discard dots along the fold
                end if;
            end loop;

            paperHeight := paperHeight - f.dist;
        else
            for c of coords loop
                if c.x > f.dist then
                    tmp.x := 2 * f.dist - c.x;
                    tmp.y := c.y;

                    newCoords.Include (tmp);
                elsif c.x < f.dist then
                    newCoords.Include (c);
                else
                    null;
                end if;
            end loop;

            paperWidth := paperWidth - f.dist;
        end if;
        
        return newCoords;
    end doFold;
begin
    Open (input, Ada.Text_IO.In_File, "input13.txt");

    -- parse input
    while not End_Of_File (input) loop
        declare
            line  : String := Get_Line (input);
            comma : Positive;
        begin
            if line(1) in '0'..'9' then
                -- parse coord
                for i in line'Range loop
                    -- find comma
                    if line(i) = ',' then
                        comma := i;
                        declare
                            xs : String := line(1..comma-1);
                            ys : String := line(comma+1..line'Length-1);
                            x : Natural := Natural'Value (xs);
                            y : Natural := Natural'Value (ys);
                        begin
                            coords.Include ((x => x, y => y));

                            if x > paperWidth then
                                paperWidth := x;
                            end if;

                            if y > paperHeight then
                                paperHeight := y;
                            end if;
                        end;
                    end if;
                end loop;
            elsif line(1) = 'f' then
                -- parse fold instruction.
                for i in line'Range loop
                    -- find equal sign
                    if line(i) = '=' then
                        declare
                            equal   : Positive := i;
                            foldDir : FoldDirection := (if line(equal-1) = 'x' then Left else Up);
                            dists   : String := line(equal+1..line'Length-1);
                            dist    : Natural := Natural'Value (dists);
                        begin
                            folds.Append ((dir => foldDir, dist => dist));
                        end;
                    end if;
                end loop;
            end if;
        end;
    end loop;

    Close (input);

    paperWidth := paperWidth + 1;
    paperHeight := paperHeight + 1;

    -- Do the first fold to see part 1 answer
    coords := doFold(folds(1));

    Put_Line ("Part 1: Number of dots after first fold:" & Count_Type'Image (coords.Length));

    -- Now do the rest of the folds
    for i in 2..folds.Length loop
        coords := doFold(folds(Positive(i)));
    end loop;

    Put_Line ("Part 2:");
    for y in 0..paperHeight-1 loop
        for x in 0..paperWidth-1 loop
            if coords.Contains ((x => x, y => y)) then
                Put ("#");
            else
                Put (" ");
            end if;
        end loop;
        Put_Line ("");
    end loop;

end Day13;
