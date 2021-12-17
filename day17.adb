with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces; use Interfaces;

procedure Day17 is

    input : File_Type;

    -- initial velocities
    xin  : Integer;
    yin  : Integer;

    -- actual velocities;
    xvel : Integer;
    yvel : Integer;

    xpos : Integer := 0;
    ypos : Integer := 0;
    bestx : Integer;
    besty : Integer;

    targetstartx : Integer := 88;
    targetendx   : Integer := 125;
    targetstarty : Integer := -157;
    targetendy   : Integer := -103;

    highesty : Integer := Integer'First;
    highestyYet : Integer := Integer'First;

    procedure step is
    begin
        xpos := xpos + xvel;
        ypos := ypos + yvel;

        if ypos > highesty then
            highesty := ypos;
        end if;

        -- drag / gravity
        xvel := xvel + (if xvel < 0 then 1 elsif xvel > 0 then -1 else 0);
        yvel := yvel - 1;
    end step;

    function targetHit return Boolean is
    begin
        if xpos >= targetstartx and xpos <= targetendx and
            ypos >= targetstarty and ypos <= targetendy then
            return True;
        end if;

        return False;
    end targetHit;

    hit : Boolean := False;
    hitCount : Natural := 0;
begin

    for xv in 0..1000 loop
        for yv in -1000..1000 loop
            -- reset for this simulation.
            hit := False;
            highesty := Integer'First;
            xpos := 0;
            ypos := 0;
            xin := xv;
            yin := yv;
            xvel := xin;
            yvel := yin;

            steploop: loop
                step;
                if targetHit then
                    hit := True;
                    exit steploop;
                end if;

                -- no need to keep going if we've already overshot the target
                if xpos > targetendx or ypos < targetstarty then
                    exit steploop;
                end if;
            end loop steploop;

            if hit then
                hitCount := hitCount + 1;
            end if;
                
            if hit and highesty > highestyYet then
                highestyYet := highesty;
                bestx := xin;
                besty := yin;
            end if;
        end loop;
    end loop;

    Put_Line ("Part 1 - " & highestyYet'Image & " with " & bestx'Image & "," & besty'Image);
    Put_Line ("Part 2 - number of unique velocities to get a hit:" & hitCount'Image);

end Day17;