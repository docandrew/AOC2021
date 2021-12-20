with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Multiway_Trees;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;
with Interfaces; use Interfaces;

procedure Day19 is

    input : File_Type;

    type Position is record
        x : Integer := 0;
        y : Integer := 0;
        z : Integer := 0;
    end record;

    function Hash (p : Position) return Hash_Type is
    begin
        return Hash_Type (abs (10000 * p.z + 1000 * p.y + p.x));
    end Hash;

    function Same (left : Position; right : Position) return Boolean is
    begin
        return left.x = right.x and left.y = right.y and left.z = right.z;
    end Same;

    package CoordSets is new Ada.Containers.Hashed_Sets (Element_Type => Position, Hash => Hash, Equivalent_Elements => Same);
    
    function Hash2 (n : Natural) return Hash_Type is
    begin
        return Hash_Type (n);
    end Hash2;
    
    package FinishedScanners is new Ada.Containers.Hashed_Sets (Element_Type => Natural, Hash => Hash2, Equivalent_Elements => "=");
    
    type RotIndex is new Natural range 1..24;

    -- It doesn't really matter what rotations these represent, but these are the
    -- 24 unique ways to "swizzle" the input coords to get a new orientation.
    function rotAll (t : RotIndex; p : Position) return Position is
    begin
        case t is
            when 19 => return ( p.x, p.y, p.z);
            when 17 => return ( p.x,-p.y,-p.z);
            when 16 => return ( p.x, p.z,-p.y);
            when 18 => return ( p.x,-p.z, p.y);
            when 11 => return (-p.x, p.y,-p.z);
            when 9  => return (-p.x,-p.y, p.z);
            when 22 => return (-p.x, p.z, p.y);
            when 24 => return (-p.x,-p.z,-p.y);
            when 4  => return ( p.y, p.x,-p.z);
            when 6  => return ( p.y,-p.x, p.z);
            when 10 => return ( p.y, p.z, p.x);
            when 7  => return ( p.y,-p.z,-p.x);
            when 20 => return (-p.y, p.x, p.z);
            when 23 => return (-p.y,-p.x,-p.z);
            when 3  => return (-p.y, p.z,-p.x);
            when 2  => return (-p.y,-p.z, p.x);
            when 8  => return ( p.z, p.x, p.y);
            when 5  => return ( p.z,-p.x,-p.y);
            when 21 => return ( p.z, p.y,-p.x);
            when 1  => return ( p.z,-p.y, p.x);
            when 15 => return (-p.z, p.x,-p.y);
            when 13 => return (-p.z,-p.x, p.y);
            when 12 => return (-p.z, p.y, p.x);
            when 14 => return (-p.z,-p.y,-p.x);
        end case;
    end rotAll;

    function Translate (a : Position; deltaX : Integer; deltaY : Integer; deltaZ : Integer) return Position
    is
    begin
        return (a.x + deltaX, a.y + deltaY, a.z + deltaZ);
    end Translate;

    -- Keep array of this scanner's points.
    type BeaconArray is array (Natural range 1..30) of Position;
    package ScannerVector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => BeaconArray);

    beacons : BeaconArray := (others => (Integer'Last, Integer'Last, Integer'Last));
    scanners : ScannerVector.Vector;
    scanIdx : Natural := 0;
    beacIdx : Natural := 0;

    procedure printCoord (p : Position) is
    begin
        Put_Line (p.x'Image & "," & p.y'Image & "," & p.z'Image);
    end printCoord;

    function parseCoord (s : String) return Position
    is
        comma1 : Natural := Index (s, ",", s'First);
        comma2 : Natural := Index (s, ",", comma1 + 1);
        s1 : String := (s(s'First..comma1-1));
        s2 : String := (s(comma1+1..comma2-1));
        s3 : String := (s(comma2+1..s'Last-1));
        val1 : Integer := Integer'Value (s1);
        val2 : Integer := Integer'Value (s2);
        val3 : Integer := Integer'Value (s3);
        p : Position := (val1, val2, val3);
    begin
        return p;
    end parseCoord;

    set0 : CoordSets.Set;       -- original beacons in set 0
    scannerSet : CoordSets.Set;
    finishedSet : FinishedScanners.Set;

    -- no idea what else to call this thing we're attempting to do.
    function gonkulate return Boolean is
        -- if we didn't determine a scanner's translation/rotation this first
        -- pass, we'll need to do it again.
        regonkulate : Boolean := False;
    begin

        -- now, rotate and translate this beacon's coordinates and compare with those of the first beacon
        -- to determine which ones are overlapping.
        for i in Natural(1)..Natural(scanners.Length-1) loop
            
            -- if we already solved this, no need to go through it again.
            if finishedSet.Contains (i) then
                goto SkipScanner;
            end if;

            declare
                scanner : BeaconArray := scanners (i);
                rotated : Position;
                rotated2 : Position;
                rotated3 : Position;
                trans : Position;
                setn : CoordSets.Set;
                matches : Natural := 0;
            begin
                rot: for r in RotIndex loop
                    -- take each element in the known set, and get its translation from the current scanner.
                    -- attempt to apply that translation to the rest of the beacons in the current scanner.
                    -- if we get >= 12 that match what's in the known set, the rotation and this translation
                    -- are accurate for the rest of the beacons, and they can be added.

                    for known of set0 loop
                        for beac1 of scanner loop
                            if beac1.x /= Integer'Last then

                                matches := 0;

                                -- store translation as a position too
                                rotated := rotAll (r, beac1);
                                trans := (known.x - rotated.x, known.y - rotated.y, known.z - rotated.z);

                                for beac2 of scanner loop
                                    rotated2 := rotAll (r, beac2);
                                    if beac2.x /= Integer'Last then
                                        -- attempt to apply this rotation/translation to the remaining elements
                                        if set0.Contains (Translate (rotated2, trans.x, trans.y, trans.z)) then
                                            matches := matches + 1;
                                        end if;
                                    end if;
                                end loop;

                                if matches >= 12 then
                                    -- scanner will be at the translated position relative to scanner 0
                                    scannerSet.Include ((trans.x, trans.y, trans.z));
                                    finishedSet.Include (i);

                                    for beac3 of scanner loop
                                        if beac3.x /= Integer'Last then
                                            rotated3 := rotAll (r, beac3);
                                            setn.Include (Translate (rotated3, trans.x, trans.y, trans.z));
                                        end if;
                                    end loop;
                                    
                                    exit rot;
                                end if;
                            end if;
                        end loop;
                    end loop;
                end loop rot;

                if setn.Length > 0 then 
                    set0.Union (setn);
                else
                    regonkulate := True;
                end if;

                setn.Clear;
            end;

            <<skipScanner>>
        end loop;

        return regonkulate;
    end gonkulate;


    use CoordSets;
begin

    Open (input, Ada.Text_IO.In_File, "input19.txt");
    
    while not End_Of_File (input) loop
        declare
            curLine : String := Get_Line (input);
            p : Position;
            beacIdx : Natural;
        begin
            if curLine(curLine'First) = ASCII.CR or curLine(curLine'First) = ASCII.LF then
                -- finished a beacon array
                scanners.Append (beacons);
            elsif curLine(curLine'First) = '-' and curLine(curLine'First+1) = '-' then
                -- starting a new beacon array
                beacons := (others => (Integer'Last, Integer'Last, Integer'Last));
                beacIdx := 1;
            else
                -- parse a beacon.
                beacons(beacIdx) := parseCoord (curLine);
                beacIdx := beacIdx + 1;
            end if;
        end;
    end loop;

    Close (input);

    scanIdx := 0;

    -- put all of scanner 0's beacons in known good set
    for beac of scanners (0) loop
        if beac.x /= Integer'Last then
            set0.Include (beac);
        end if;
    end loop;

    -- need to call this multiple times to find all the beacons.
    while gonkulate loop
        null;
    end loop;

    Put_Line ("Part 1: number of beacons: " & set0.Length'Image);

    part2: declare
        manhattan : Natural;
        largest : Natural := 0;
        sc1 : Position;
        sc2 : Position;
    begin
        scannerSet.Include ((0,0,0)); -- <sigh...>

        for scan1 of scannerSet loop
            for scan2 of scannerSet loop
                manhattan := abs(scan2.x - scan1.x) + abs(scan2.y - scan1.y) + abs(scan2.z - scan1.z);
                if manhattan > largest then
                    largest := manhattan;
                    sc1 := scan1;
                    sc2 := scan2;
                end if;
            end loop;
        end loop;

        Put_Line ("Part 2: largest manhattan distance between any 2 scanners:" & largest'Image);
    end part2;

end Day19;
