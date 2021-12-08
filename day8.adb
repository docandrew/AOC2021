with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Bounded;
with Ada.Strings.Fixed;

procedure Day8 is

    package BStr is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 7);
    use BStr;
  
    input : File_Type;
    curr  : Natural := 0;

    count1 : Natural := 0;
    count4 : Natural := 0;
    count7 : Natural := 0;
    count8 : Natural := 0;

    part1 : Natural;

    type Segment is (Top, Bottom, Middle, TopLeft, TopRight, BottomLeft, BottomRight, Unknown);
    type CharSegmentMap is array (Character range 'a'..'g') of Segment;
    charSegment : CharSegmentMap := (others => Unknown);

    type Display is array (Segment) of Boolean;

    function has (w : Bounded_String; c : Character) return Boolean is
    begin
        for ubc in 1..Length (w) loop
            if Element (w, ubc) = c then
                return True;
            end if;
        end loop;

        return False;
    end has;

    function numCommon (w1 : Bounded_String; w2 : Bounded_String) return Natural is
        num : Natural := 0;
    begin
        for c of To_String(w1) loop
            if has (w2, c) then
                num := num + 1;
            end if;
        end loop;

        return num;
    end numCommon;

    function Decode (leftHalf : String) return CharSegmentMap is
        type WordArr is array (0..9) of Bounded_String;
        words : WordArr;

        word0 : Bounded_String;
        word1 : Bounded_String;
        word2 : Bounded_String;
        word3 : Bounded_String;
        word4 : Bounded_String;
        word5 : Bounded_String;
        word6 : Bounded_String;
        word7 : Bounded_String;
        word8 : Bounded_String;
        word9 : Bounded_String;

        from : Natural := leftHalf'First;
        to   : Natural := leftHalf'First;

        map  : CharSegmentMap := (others => Unknown);

        middleChar : Character := 'X';
        use Ada.Strings.Bounded;
    begin
        for i in words'Range loop
            while leftHalf(to) in 'a'..'g' loop
                to := to + 1;
            end loop;

            words(i) := To_Bounded_String(leftHalf(from..to-1));

            from := to + 1;
            to := from;
        end loop;

        -- Get the words with unique lengths.
        for w of words loop
            if Length (w) = 2 then
                word1 := w;
            elsif Length (w) = 3 then
                word7 := w;
            elsif Length (w) = 4 then
                word4 := w;
            elsif Length (w) = 7 then
                word8 := w;
            end if;
        end loop;

        -- Whichever segment in 7 is not in the 1 is our _top segment_.
        findTopSegment: for c of To_String(word7) loop
            if not has (word1, c) then
                map(c) := Top;
            end if;
        end loop findTopSegment;

        -- the other 2 are bottom right and bottom left

        -- 3: only five-segment character that has all of 7's segments
        --    2 more segments. If in common with 4, that's _the middle_. The other is the bottom.
        findMiddleSegment: for w of words loop
            if Length (w) = 5 then
                -- either a 2,3 or 5. it has to have all of 7's segments to be a 3.
                -- if word7(1) in w and word7(2) in w and word7(3) in w then
                if has (w, Element (word7, 1)) and
                   has (w, Element (word7, 2)) and
                   has (w, Element (word7, 3)) then

                    -- found our 3.
                    word3 := w;

                    -- the segment that's in 4 but not 7 is our middle segment.
                    for c of To_String(word3) loop
                        if has (word4, c) and
                           not has (word7, c) then
                            middleChar := c;
                            map(c) := Middle;
                        end if;
                    end loop;
                end if;
            end if;
        end loop findMiddleSegment;

        -- 0 is the only six-segment character without the middle.
        zero: for w of words loop
            if Length (w) = 6 and not has (w, middleChar) then
                word0 := w;
            end if;
        end loop zero;

        -- 5: has 3 of 4 of 4's segments.
        --    the one in 4 but not 5 is _our top right_
        --    the one in 1 and 5 is _our bottom right_
        --    the one that is not in 3 is _our top left_
        five: for w of words loop
            if Length (w) = 5 and w /= word3 then
                if numCommon (w, word4) = 3 then

                    -- found our 5
                    word5 := w;

                    for c of To_String(word4) loop
                        if not has (word5, c) then
                            map(c) := TopRight;
                        end if;
                    end loop;

                    for c of To_String(word5) loop
                        if has (word1, c) then
                            map(c) := BottomRight;
                        end if;

                        if not has (word3, c) then
                            map(c) := TopLeft;
                        end if;
                    end loop;
                end if;
            end if;
        end loop five;

        two: for w of words loop
            if Length (w) = 5 and not (w = word3) and not (w = word5) then
                word2 := w;
            end if;
        end loop two;

        -- bottom left is in 2, but not 3 and not the bottom right
        findBottomLeft: for c of To_String (word2) loop
            if not has (word3, c) and map(c) /= BottomRight then
                map(c) := BottomLeft;
            end if;
        end loop findBottomLeft;

        -- now just need bottom. It's the one in 3 that we haven't found.
        bot: for c of To_String (word3) loop
            if map(c) /= Top and
               map(c) /= Middle and
               map(c) /= TopRight and
               map(c) /= BottomRight 
            then
                map(c) := Bottom;
            end if;
        end loop bot;
        
        return map;
    end Decode;

    -- Turn on the segments of a given display to determine the character.
    function LightEmUp (d : Display) return Natural is
    begin
        if  d(Top) and 
            d(TopLeft) and 
            d(TopRight) and 
            d(Middle) and 
            d(BottomLeft) and 
            d(BottomRight) and 
            d(Bottom) then
            return 8;
        elsif
            d(Top) and 
            d(TopLeft) and 
            d(TopRight) and 
            not d(Middle) and 
            d(BottomLeft) and 
            d(BottomRight) and 
            d(Bottom) then
            return 0;
        elsif
            d(Top) and 
            d(TopLeft) and 
            not d(TopRight) and 
            d(Middle) and 
            d(BottomLeft) and 
            d(BottomRight) and 
            d(Bottom) then
            return 6;
        elsif
            d(Top) and 
            d(TopLeft) and 
            d(TopRight) and 
            d(Middle) and 
            not d(BottomLeft) and 
            d(BottomRight) and 
            d(Bottom) then
            return 9;
        elsif
            d(Top) and 
            not d(TopLeft) and 
            d(TopRight) and 
            d(Middle) and 
            not d(BottomLeft) and 
            d(BottomRight) and 
            d(Bottom) then
            return 3;
        elsif
            d(Top) and 
            d(TopLeft) and 
            not d(TopRight) and 
            d(Middle) and 
            not d(BottomLeft) and 
            d(BottomRight) and 
            d(Bottom) then
            return 5;
        elsif
            d(Top) and 
            not d(TopLeft) and 
            d(TopRight) and 
            d(Middle) and 
            d(BottomLeft) and 
            not d(BottomRight) and 
            d(Bottom) then
           return 2;
        elsif
            not d(Top) and
            d(TopLeft) and 
            d(TopRight) and 
            d(Middle) and 
            not d(BottomLeft) and 
            d(BottomRight) and 
            not d(Bottom) then
            return 4;
        elsif
            d(Top) and
            not d(TopLeft) and 
            d(TopRight) and 
            not d(Middle) and 
            not d(BottomLeft) and 
            d(BottomRight) and 
            not d(Bottom) then
            return 7;
        else
            return 1;
        end if;
    end LightEmUp;

    part2 : Natural := 0;
begin
    Open (input, Ada.Text_IO.In_File, "input8.txt");

    -- parse input
    while not End_Of_File (input) loop
        declare
            line : String := Get_Line (input);
            rh   : Natural := 1;
            from : Natural := 1;
            to   : Natural := 1;

            type DigitsType is array (1..4) of Natural;
            ds : DigitsType;

            lineNum : Natural := 0;
        begin
            -- find |
            rh := Ada.Strings.Fixed.Index (Source  => line,
                                           Pattern => "|",
                                           From    => 1);
            -- Parse left half
            from := 1;
            to   := rh;

            charSegment := Decode (line(from..to));

            -- Now parse words on right half
            from := rh + 2;
            to   := from;

            for i in 1..4 loop
                -- find next blank
                findBlank: for c of line(from..line'Length-1) loop
                    if c in 'a'..'g' then
                        to := to + 1;
                    else
                        exit findBlank;
                    end if;
                end loop findBlank;

                declare
                    curWord : String := line(from..to-1);
                    numSegs : Natural := curWord'Length;

                    litSegs : Display := (others => False);
                begin
                    -- go through word, put segments into the display.
                    for c of curWord loop
                        litSegs (charSegment (c)) := True;
                    end loop;

                    ds(i) := LightEmUp (litSegs);

                    case numSegs is
                        when 2 =>
                            count1 := count1 + 1;
                        when 3 => 
                            count7 := count7 + 1;
                        when 4 => 
                            count4 := count4 + 1;
                        when 7 => 
                            count8 := count8 + 1;
                        when others => 
                            null;
                    end case;
                end;
                
                from := to + 1;
                to   := from;
            end loop;

            lineNum := ds(1) * 1000 + ds(2) * 100 + ds(3) * 10 + ds(4);
            part2 := part2 + lineNum;
        end;
    end loop;

    Close (input);

    part1 := count1 + count4 + count7 + count8;
    Put_Line ("Part 1: Number of 1,4,7,8s: " & part1'Image);
    Put_Line ("Part 2: Sum of digits:      " & part2'Image);
end Day8;
