with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces; use Interfaces;

procedure Day16 is

    input : File_Type;

    subtype Bindigit is String (1..4);

    function hexToBin (c : Character) return Bindigit is
    begin
        case c is
            when '0' => return "0000";
            when '1' => return "0001";
            when '2' => return "0010";
            when '3' => return "0011";
            when '4' => return "0100";
            when '5' => return "0101";
            when '6' => return "0110";
            when '7' => return "0111";
            when '8' => return "1000";
            when '9' => return "1001";
            when 'A' => return "1010";
            when 'B' => return "1011";
            when 'C' => return "1100";
            when 'D' => return "1101";
            when 'E' => return "1110";
            when 'F' => return "1111";
            when others => return "NAN!";
        end case;
    end hexToBin;

    packetBin : Unbounded_String;
    idx : Natural := 1;

    procedure decode (p : String; s : in out Unbounded_String) is
    begin
        for c of p loop
            Append (s, hexToBin (c));
        end loop;
    end decode;

    function version (s : String; i : in out Natural) return Natural is
        slice : String := "2#" & s (i..i+2) & "#";
        ret : Natural := Natural'Value (slice);
    begin
        -- version is first 3 digits
        i := i + 3;
        return ret;
    end version;

    function typeid (s : String; i : in out Natural) return Natural is
    begin
        return version (s, i);
    end typeid;

    function parseLiteral (s : String; i : in out Natural) return Unsigned_64 is
        lastNibble : Boolean := False;
        numstr : Unbounded_String;
    begin
        loop
            if s(i) = '0' then
                lastNibble := True;
            end if;

            Append (numstr, s(i+1..i+4));
            i := i + 5;

            exit when lastNibble;
        end loop;

        return Unsigned_64'Value ("2#" & To_String (numstr) & "#");
    end parseLiteral;

    function parsePacket (p : String; i : in out Natural) return Unsigned_64;

    type OpType is (Sum, Product, Minimum, Maximum, LIT_DONT_USE, GreaterThan, LessThan, EqualTo);

    function parseOperator (s : String; i : in out Natural; op : OpType) return Unsigned_64 is
        lengthSubPackets : Natural;
        numSubPackets    : Natural;
        starti           : Natural;

        retsum  : Unsigned_64 := 0;
        prod    : Unsigned_64 := 1;

        curr  : Unsigned_64; -- for min/max comparisons
        min   : Unsigned_64 := Unsigned_64'Last;
        max   : Unsigned_64 := Unsigned_64'First;
        left  : Unsigned_64;
        right : Unsigned_64;
    begin
        -- next bit is length type
        if s(i) = '0' then
            -- next 15 bits are total length in bits of sub-packets
            lengthSubPackets := Natural'Value ("2#" & s(i+1..i+15) & "#");
            i := i + 16;
            starti := i;

            case op is
                when Sum =>            
                    while i < starti + lengthSubPackets loop
                        retsum := retsum + parsePacket (s, i);
                    end loop;
                    return retsum;
                when Product =>
                    while i < starti + lengthSubPackets loop
                        prod := prod * parsePacket (s, i);
                    end loop;
                    return prod;
                when Minimum =>
                    while i < starti + lengthSubPackets loop
                        curr := parsePacket (s, i);
                        min := (if curr < min then curr else min);
                    end loop;
                    return min;
                when Maximum =>
                    while i < starti + lengthSubPackets loop
                        curr := parsePacket (s, i);
                        max := (if curr > max then curr else max);
                    end loop;
                    return max;
                when GreaterThan =>
                    -- better be two packets here
                    left := parsePacket (s, i);
                    right := parsePacket (s, i);
                    if left > right then return 1; else return 0; end if;
                when LessThan =>
                    left := parsePacket (s, i);
                    right := parsePacket (s, i);
                    if left < right then return 1; else return 0; end if;
                when EqualTo =>
                    left := parsePacket (s, i);
                    right := parsePacket (s, i);
                    if left = right then return 1; else return 0; end if;
                when others =>
                    return Unsigned_64'Last;
            end case;
        else
            -- next 11 bits are number of sub-packets contained by this packet
            numSubPackets := Natural'Value ("2#" & s(i+1..i+11) & "#");
            i := i + 12;

            case op is
                when Sum =>            
                    for j in 1..numSubPackets loop
                        retsum := retsum + parsePacket (s, i);
                    end loop;
                    return retsum;
                when Product =>
                    for j in 1..numSubPackets loop
                        prod := prod * parsePacket (s, i);
                    end loop;
                    return prod;
                when Minimum =>
                    for j in 1..numSubPackets loop
                        curr := parsePacket (s, i);
                        min := (if curr < min then curr else min);
                    end loop;
                    return min;
                when Maximum =>
                    for j in 1..numSubPackets loop
                        curr := parsePacket (s, i);
                        max := (if curr > max then curr else max);
                    end loop;
                    return max;
                when GreaterThan =>
                    -- better be two packets here
                    if numSubPackets /= 2 then
                        Put_Line ("***** PARSE ERROR - EXPECTED 2 SUBPACKETS FOR > OPERATOR *****");
                    end if;
                    left := parsePacket (s, i);
                    right := parsePacket (s, i);
                    if left > right then return 1; else return 0; end if;
                when LessThan =>
                    if numSubPackets /= 2 then
                        Put_Line ("***** PARSE ERROR - EXPECTED 2 SUBPACKETS FOR < OPERATOR *****");
                    end if;
                    left := parsePacket (s, i);
                    right := parsePacket (s, i);
                    if left < right then return 1; else return 0; end if;
                when EqualTo =>
                    if numSubPackets /= 2 then
                        Put_Line ("***** PARSE ERROR - EXPECTED 2 SUBPACKETS FOR = OPERATOR *****");
                    end if;
                    left := parsePacket (s, i);
                    right := parsePacket (s, i);
                    if left = right then return 1; else return 0; end if;
                when others =>
                    return Unsigned_64'Last;
            end case;
        end if;
    end parseOperator;

    versum : Natural := 0;
    part2 : Unsigned_64 := 0;

    function parsePacket (p : String; i : in out Natural) return Unsigned_64 is
        ver : Natural;
        pid : Natural;
    begin
        ver := version (p, i);
        pid := typeid (p, i);
        versum := versum + ver;

        if pid = 4 then
            return parseLiteral (p, i);
        else
            return parseOperator (p, i, OpType'Val (pid));
        end if;
    end parsePacket;

begin
    Open (input, Ada.Text_IO.In_File, "input16.txt");
    
    declare
        packet : String := Get_Line (input);
    begin
        decode (packet, packetBin);
    end;

    part2 := parsePacket (To_String(packetBin), idx);

    Put_Line ("Part 1 - sum of versions:    " & versum'Image);
    Put_Line ("Part 2 - value of expression:" & part2'Image);

end Day16;