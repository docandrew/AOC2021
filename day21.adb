with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;

procedure Day21 is
    input : File_Type;

    subtype SpaceT is Positive range 1..10;
    subtype DiceT is Positive range 1..100;

    type Player is record
        space : SpaceT;
        score : Natural;
    end record;

    function turn (p : in out Player; d : in out DiceT; r : in out Natural) return Boolean is
        tmp : Natural;
    begin
        for roll in 1..3 loop
            tmp := Natural(p.space + d);

            -- Put (" roll: " & d'Image);
            if tmp mod SpaceT'Last = 0 then
                p.space := 10;
            else
                p.space := tmp mod SpaceT'Last;
            end if;
            -- Put_Line ("  space:" & p.space'Image);

            d := (if d = 100 then 1 else d + 1);
            r := r + 1;
        end loop;
        p.score := p.score + p.space;

        return (p.score >= 1000);
    end turn;

    dice  : DiceT := 1;
    p1    : Player := (space => 6, score => 0);
    p2    : Player := (space => 9, score => 0);
    rolls : Natural := 0;

    part1 : Natural := 0;

    -- -- How many ways can we roll such that we get a particular distance?
    -- -- can't roll less than 3 (1,1,1) or more than 9 (3,3,3)
    function waysToRoll (dist : Integer) return Unsigned_64 is
    begin
        case dist is
            when 3 => return 1;
            when 4 => return 3;
            when 5 => return 6;
            when 6 => return 7;
            when 7 => return 6;
            when 8 => return 3;
            when 9 => return 1;
            when others => return 0;    -- no way to roll anything else
        end case;
    end waysToRoll;

begin
    loop
        -- player 1
        if turn (p1, dice, rolls) then
            part1 := p2.score * rolls;
            Put_Line ("Part 1:");
            Put_Line ("Player 1 win, score: " & p1.score'Image & " rolls: " & rolls'Image & " part 1 answer: " & part1'Image);
            exit;
        end if;

        -- player 2
        if turn (p2, dice, rolls) then
            part1 := p1.score * rolls;
            Put_Line ("Part 1:");
            Put_Line ("Player 2 win, score: " & p2.score'Image & " rolls: " & rolls'Image & " part 1 answer: " & part1'Image);
            exit;
        end if;
    end loop;

    Put_Line ("");

    -- Part 2:
    part2: declare

        type WhoseTurn is (Player1, Player2);

        p1Wins : Unsigned_64 := 0;
        p2Wins : Unsigned_64 := 0;

        function qturn (p : Player; r : DiceT) return Player is
            sp : Natural;
            sc : Natural;
        begin
            sp := (if (p.space + r) mod 10 = 0 then 10 else (p.space + r) mod 10);
            sc := p.score + sp;
            return (sp, sc);
        end qturn;

        procedure wins (p1 : Player; p2 : Player; next : WhoseTurn; prob : Unsigned_64) is
        begin
            if p1.score >= 21 then
                p1wins := p1wins + prob;
                return;
            elsif p2.score >= 21 then
                p2wins := p2wins + prob;
                return;
            end if;

            if next = Player1 then
                for r in 3..9 loop
                    wins (qturn (p1, r), p2, Player2, prob * waysToRoll (r));
                end loop;
            else
                for r in 3..9 loop
                    wins (p1, qturn (p2, r), Player1, prob * waysToRoll (r));
                end loop;
            end if;
        end wins;

    begin
        wins ((6,0), (9,0), Player1, 1);

        Put_Line ("Part 2:");
        Put_Line ("Player 1 wins:" & p1Wins'Image & " Player 2 wins: " & p2Wins'Image);
    end part2;
end Day21;