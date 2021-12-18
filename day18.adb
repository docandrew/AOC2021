with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Multiway_Trees;
with Ada.Containers.Vectors;
with Interfaces; use Interfaces;

procedure Day18 is

    input : File_Type;

    package SFMath is new Ada.Containers.Multiway_Trees (Natural);
    use SFMath;

    function printSubTree (sfnum : Tree; C : Cursor) return Unbounded_String is
        ret : Unbounded_String;
    begin
        if Is_Leaf (C) then
            Append (ret, Trim(Element (C)'Image, Ada.Strings.Left));
        else
            Append (ret, "[");
            Append (ret, printSubTree (sfnum, First_Child (C)));
            Append (ret, ",");
            Append (ret, printSubTree (sfnum, Last_Child (C)));
            Append (ret, "]");
        end if;

        return ret;
    end printSubTree;

    function printTree (sfnum : Tree) return Unbounded_String is
        C : Cursor := Root (sfnum);
        ret : Unbounded_String;
    begin
        Append(ret, printSubTree (sfnum, C));
        return ret;
    end printTree;

    function makeTree (str : String) return Tree is
        sfnum : Tree := Empty_Tree;
        C : Cursor := Root (sfnum);
    begin
        for ch of str (str'First+1..str'Last-1) loop
            if ch = '[' then
                Insert_Child (sfnum, C, No_Element, C);
            elsif ch in '0'..'9' then
                Insert_Child (sfnum, C, No_Element, Natural'Value ("" & ch));
            elsif ch = ',' then
                null;
            elsif ch = ']' then
                C := Parent (C);
            else
                null;
            end if;
        end loop;

        return sfnum;
    end makeTree;
   
    package CurVec is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => SFMath.Cursor);

    function flatten (C : SFMath.Cursor) return CurVec.Vector is
        ret : CurVec.Vector;

        procedure flattenHelper (C : SFMath.Cursor) is
        begin
            if Is_Leaf (C) then
                ret.Append (C);
            else
                flattenHelper (First_Child (C));
                flattenHelper (Last_Child (C));
            end if;
        end flattenHelper;
    begin
        flattenHelper (C);
        return ret;
    end flatten;

    function explode (t : in out Tree) return Boolean is
        didExplode : Boolean := False;

        function getLeft (C : Cursor) return Cursor is
            flattened : CurVec.Vector := flatten (Root (t));
        begin
            for i in flattened.First_Index..flattened.Last_Index loop
                if flattened(i) = C and i /= flattened.First_Index then
                    return flattened(i-1);
                end if;
            end loop;

            return No_Element;
        end getLeft;

        function getRight (C : Cursor) return Cursor is
            flattened : CurVec.Vector := flatten (Root(t));
        begin
            for i in flattened.First_Index..flattened.Last_Index loop
                if flattened(i) = C and i /= flattened.Last_Index then
                    return flattened(i+1);
                end if;
            end loop;

            return No_Element;
        end getRight;

        l : Cursor;                         -- element to our left
        r : Cursor;                         -- element to our right
        d : Cursor := No_Element;           -- element to delete
        ignore : Cursor;

        newl : Natural := Natural'Last;
        newr : Natural := Natural'Last;
    begin
        for C in t.Iterate loop
            -- looking for a nested pair w/ depth > 4.
            if Is_Leaf (First_Child (C)) and Is_Leaf (Last_Child (C)) and Depth (C) > 4 then

                l := getLeft ( First_Child (C));
                if l = No_Element then
                    d := C;
                else
                    newl := Element (l) + Element (First_Child (C));
                    Replace_Element (t, l, newl);
                    d := C;
                end if;

                r := getRight ( Last_Child (C));
                if r = No_Element then
                    d := C;
                else
                    newr := Element (r) + Element (Last_Child (C));
                    Replace_Element (t, r, newr);
                    d := C;
                end if;

                didExplode := True;
            end if;

            exit when didExplode;
        end loop;

        if didExplode then
            -- insert new 0 before deleted node
            Insert_Child (t, Parent(d), d, 0, ignore);
            Delete_Subtree (t, d);
        end if;

        return didExplode;
    end explode;

    function split (t : in out Tree) return Boolean is
        didSplit : Boolean := False;
        p : Cursor;     -- parent of number we're splitting
        s : Cursor;     -- number we're splitting
        s2 : Cursor;    -- new node w/ 2 elements
        lh : Natural;
        rh : Natural;
    begin
        -- Look for node with value >= 10
        for C in t.Iterate loop
            if Is_Leaf (C) and then Element (C) >= 10 then
                lh := Element (C) / 2;
                rh := Element (C) / 2 + Element (C) mod 2;
                -- keep track of cursor for new node insertion
                s := C;
                didSplit := True;
            end if;

            exit when didSplit;
        end loop;

        if didSplit then
            -- not a leaf node anymore, destroying it's original value.
            Prepend_Child (t, s, lh);   -- add lh
            Append_Child (t, s, rh);   -- add rh
        end if;

        return didSplit;
    end split;

    function add (t1 : Tree; t2 : Tree) return Tree is
        result : Tree := Empty_Tree;
        -- C : Cursor := Root (result);
        didExplode : Boolean := False;
        didSplit : Boolean := False;
    begin
        -- This is a dirty hack, but for some reason Copy_Subtree doesn't work from
        -- the root, which is annoying.
        result := makeTree ("[" & To_String (printTree (t1)) & "," & To_String (printTree (t2)) & ']');
        
        -- now explode / split to reduce it. exit when neither an explode nor a split occurred.
        <<TryAgain>>
            -- First, try and explode it. Keep trying this until its reduced.
            didExplode := False;
            didExplode := explode (result);
            
            if didExplode then
                goto TryAgain;
            end if;

            -- if no explode happened, then try and split.
            if not didExplode then
                didSplit := False;
                didSplit := split (result);
            end if;

            -- if a split occurred, then try exploding again.
            if didSplit then
                goto TryAgain;
            end if;

        return result;
    end add;

    procedure Leaf_Count (t : Tree) is
        count : Natural := 0;
    begin
        for st in t.Iterate loop
            if Is_Leaf (st) then
                count := count + 1;
            end if;
        end loop;
        
        Put_Line ("leaf count: " & count'Image);
    end Leaf_Count;

    function getMagnitude (C : Cursor) return Natural is
    begin
        if Is_Leaf (C) then
            return Element (C);
        else
            return 3 * getMagnitude (First_Child (C)) + 2 * getMagnitude (Last_Child (C));
        end if;
    end getMagnitude;

    ignore : Boolean;
    t : Tree;

    -- for part 2
    package TreeVecs is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => SFMath.Tree);
    alltrees : TreeVecs.Vector;
begin
    Open (input, Ada.Text_IO.In_File, "input18.txt");
    
    part1: declare
        sum : Tree := makeTree (Get_Line (input));
        t1 : Tree;
    begin
        alltrees.Append(sum);   -- save these for part 2

        while not End_Of_File (input) loop
            t1 := makeTree (Get_Line (input));
            alltrees.Append(t1);
            sum := add (sum, t1);
        end loop;

        -- Put_Line (To_String (printTree (sum)));
        Put_Line ("part 1: magnitude of final sum: " & getMagnitude (Root(sum))'Image);
    end part1;

    part2: declare
        curMag : Natural;
        bestMag : Natural := Natural'First;
    begin
        for i in alltrees.First_Index..alltrees.Last_Index loop
            for j in alltrees.First_Index..alltrees.Last_Index loop
                if i /= j then
                    curMag := getMagnitude (Root (add (alltrees(i), alltrees(j))));
                    bestMag := (if curMag > bestMag then curMag else bestMag);
                end if;
            end loop;
        end loop;
        
        Put_Line ("part 2: best magnitude from addition of any 2 pairs: " & bestMag'Image);
    end part2;

end Day18;
