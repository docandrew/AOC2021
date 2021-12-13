with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Interfaces; use Interfaces;

-- H/T geeksforgeeks for the BFS algo.
procedure Day12 is
  
    input : File_Type;

    -- Each unique node name will have a number associated with it for convenience
    package NameMaps is new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                                                   Element_Type    => Positive,
                                                                   Hash            => Ada.Strings.Hash,
                                                                   Equivalent_Keys => "=");
    nodeNums : NameMaps.Map;

    -- list of indices into the main graph object
    package AdjacencyList is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Positive);
    type AdjPtr is access AdjacencyList.Vector;

    -- If its a lower-case node, we can visit it only once.
    type Node is record
        num : Positive;
        lowercase : Boolean;
        adj : AdjPtr;
    end record;

    package Graphs is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Node);
    graph : Graphs.Vector;

    package Paths is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Positive);
    type PathPtr is access Paths.Vector;

    function isLowerCase (n : String) return Boolean is
    begin
        return n(n'First) in 'a'..'z';
    end isLowerCase;

    pathCount : Natural := 0;

    procedure printPath (p : Paths.Vector) is
    begin
        -- Put_Line ("Found Path");
        pathCount := pathCount + 1;
        -- for n of p loop
        --     -- Put (n'Image & ",");
        -- end loop;

        -- Put_Line ("");
    end printPath;

    package PathQueues is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => PathPtr);

    function isVisited (num : Positive; path : PathPtr) return Boolean is
    begin
        for n of path.all loop
            if num = n then
                return True;
            end if;
        end loop;

        return False;
    end isVisited;

    -- a single small cave can be visited at most twice. So check first if this
    -- cave has already been visited. If not, we can visit it. If it has, then
    -- do a check to see if any other cave appears twice.
    function isVisited2 (num : Positive; path : PathPtr) return Boolean is
        visitedOnce : Boolean := False;

        package CountVectors is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Natural);
        visitCount : CountVectors.Vector;
    begin
        for n of path.all loop
            if num = n then
                visitedOnce := True;
                exit;
            end if;
        end loop;

        if visitedOnce then
            -- see if there are any other twofers.
            visitCount.Set_Length (graph.Length);
            for i of visitCount loop
                i := 0;
            end loop;

            -- for every element in this path, how many times does it occur?
            for n of path.all loop
                visitCount (n) := visitCount (n) + 1;

                -- if any lowercase has already been visited twice, since we've already
                -- been visited, no use in checking again.
                if visitCount (n) = 2 and graph(n).lowercase then
                    return True;
                end if;
            end loop;
        end if;

        return False;
    end isVisited2;

    procedure findPaths (g : Graphs.Vector) is
        path : PathPtr := new Paths.Vector; --Paths.Vector;
        queue : PathQueues.Vector;
        last : Natural;
    begin
        path.Append (nodeNums ("start"));
        queue.Prepend (path);

        while queue.Length > 0 loop
            path := queue.First_Element;
            queue.Delete_First;

            last := path.Last_Element;

            if last = nodeNums ("end") then
                printPath (path.all);
            end if;

            for n of g(last).adj.all loop
                if not g(n).lowercase or (g(n).lowercase and not isVisited2 (n, path)) then
                    declare
                        newPath : PathPtr := new Paths.Vector;
                    begin
                        for i of path.all loop
                            newPath.all.Append(i);
                        end loop;

                        newPath.Append (n);
                        queue.Prepend (newPath);
                    end;
                end if;
            end loop;
        end loop;
    end findPaths;

    nodeNum : Positive := 1;
begin
    Open (input, Ada.Text_IO.In_File, "input12.txt");

    -- parse input
    while not End_Of_File (input) loop
        declare
            line    : String := Get_Line (input);
            dashIdx : Positive;
        begin
            for i in line'Range loop
                -- find dash
                if line(i) = '-' then
                    dashIdx := i;
                    declare
                        node1 : String := line(1..dashIdx-1);
                        node2 : String := line(dashIdx+1..line'Length-1);
                    begin
                        -- Put ("Node 1: " & node1 & " ");
                        -- Put_Line ("Node 2: " & node2);
                        
                        if not nodeNums.Contains (node1) then
                            -- Put_Line ("Naming node" & nodeNum'Image & " " & node1);
                            nodeNums.Include (node1, nodeNum);
                            graph.Append ( (num       => nodeNum, 
                                            lowercase => isLowerCase (node1),
                                            adj       => new AdjacencyList.Vector));
                            nodeNum := nodeNum + 1;
                        end if;

                        if not nodeNums.Contains (node2) then
                            -- Put_Line ("Naming node" & nodeNum'Image & " " & node2);
                            nodeNums.Include (node2, nodeNum);
                            graph.Append ( (num       => nodeNum, 
                                            lowercase => isLowerCase (node2),
                                            adj       => new AdjacencyList.Vector));
                            nodeNum := nodeNum + 1;
                        end if;

                        if node1 = "start" then
                            -- Put_Line ("Drawing edge from start to " & node2);
                            graph (nodeNums("start")).adj.Append (nodeNums(node2));
                        elsif node2 = "start" then
                            -- Put_Line ("Drawing edge from start to " & node1);
                            graph (nodeNums("start")).adj.Append (nodeNums(node1));
                        elsif node1 = "end" then
                            -- Put_Line ("Drawing edge from " & node2 & " to end");
                            graph (nodeNums(node2)).adj.Append (nodeNums("end"));
                        elsif node2 = "end" then
                            -- Put_Line ("Drawing edge from " & node1 & " to end");
                            graph (nodeNums(node1)).adj.Append (nodeNums("end"));
                        else
                            -- Put_Line ("Drawing edges between " & node1 & " and " & node2);
                            graph (nodeNums(node1)).adj.Append (nodeNums(node2));
                            graph (nodeNums(node2)).adj.Append (nodeNums(node1));
                        end if;
                    end;
                end if;
            end loop;
        end;
    end loop;

    Close (input);
    findPaths (graph);

    Put_Line ("Number of unique paths: " & pathCount'Image);

end Day12;
