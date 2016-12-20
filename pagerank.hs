-- Author : Sumod Madhavan
import           Data.Map    (Map, empty, insert, insertWith,
lookup,
                              mapWithKey, member, size)
import           Data.Maybe  (fromJust)
import           Debug.Trace (trace)
import           Prelude     hiding (lookup)
import           Text.Printf (printf)

type Node = Int
type PRValue = Double
type PageRank = Map Node PRValue
type InboundEdges = Map Node [Node]
type OutboundEdges = InboundEdges

parseLine :: (InboundEdges, OutboundEdges, Node) -> String ->
(InboundEdges, OutboundEdges, Node)
parseLine (iEdges, oEdges, maxNode) line =
    let ws = words line
        (from, to) = (read $ ws !! 0, read $ ws !! 1)
        in (insertWith plusNode to [from] iEdges,
            insertWith plusNode from [to] oEdges,
            max to (max maxNode from))
    where
        plusNode :: [Node] -> [Node] -> [Node]
        plusNode new_node old_node =
            new_node ++ old_node

newPageRank :: Int -> PageRank
newPageRank n =
    let v :: Double; v = 1 / (fromIntegral n)
        in go n v empty
    where
        go :: Int -> Double -> PageRank -> PageRank
        go 0 _ pr = pr

        go n v pr =
            go (n-1) v $ insert (n-1) v pr

-- The goal of postProcess is to deal with the nodes that have no
outbound
-- edges, in which case they should be treated like they have
outbound edges
-- to every other node.
postProcess :: (InboundEdges, OutboundEdges, Node) ->
(InboundEdges, OutboundEdges)
postProcess (iEdges, oEdges, maxNode) =
    let numNodes = maxNode + 1
        newIEdges = addAllNodes (numNodes-1) iEdges
        in loop (numNodes-1) newIEdges oEdges

    where
        loop :: Int -> InboundEdges -> OutboundEdges ->
(InboundEdges, OutboundEdges)
        loop n iEdges oEdges
            | n < 0 = (iEdges, oEdges)
            | otherwise =
                if member n oEdges then
                    loop (n-1) iEdges oEdges
                else
                    let numNodes = maxNode + 1
                        newOEdges = insert n (filter (/= n)
[0..maxNode]) oEdges
                        newIEdges = mapWithKey (\k v -> if k /= n
then v ++ [n] else v) iEdges
                        in loop (n-1) newIEdges newOEdges

        -- This function makes sure that every node is a key in the
InboundEdges map
        addAllNodes :: Int -> InboundEdges -> InboundEdges
        addAllNodes n iEdges
            | n < 0 = iEdges
            | otherwise =
                addAllNodes (n-1) $ insertWith (\new old -> new ++
old) n [] iEdges


parseGraph :: String -> (InboundEdges, OutboundEdges, PageRank)
parseGraph input =
    let ls = lines input
        (iEdges, oEdges) = postProcess $ foldl parseLine (empty,
empty, 0) ls
        numNodes = size iEdges
        in (iEdges, oEdges, newPageRank numNodes)


loopProcess :: Int -> Double -> InboundEdges -> OutboundEdges ->
PageRank -> PageRank
loopProcess 0 _ _ _ pageRank = pageRank
loopProcess n dampingFactor iEdges oEdges pageRank =
    let newPageRank = loop' ((size pageRank) - 1) empty
        in loopProcess (n-1) dampingFactor iEdges oEdges
newPageRank

    where
        loop' :: Int -> PageRank -> PageRank
        loop' n pr
            | n < 0 = pr
            | otherwise =
                let inbounds = fromJust $ lookup n iEdges
                    newPrValue = (+)
                        ((1 - dampingFactor) / (fromIntegral $ size
iEdges))
                        (dampingFactor * (foldl calc 0 inbounds))
                    in loop' (n-1) $ insert n newPrValue pr

                where
                    calc acc node =
                        let outbounds = fromJust $ lookup node
oEdges
                            prValue = fromJust $ lookup node
pageRank
                            in acc + prValue / (fromIntegral $
length outbounds)


process :: String -> Int -> Double -> PageRank
process input numIters dampingFactor =
    let (iEdges, oEdges, pageRank) = parseGraph input
        in loopProcess numIters dampingFactor iEdges oEdges
pageRank

main :: IO ()
main = do
    putStrLn "How many iters?"
    numIters <- getLine
    f <- readFile "input.txt"
    -- damping factor defaults to 0.85
    writeFile "output.txt" $ show $ process f (read numIters ::
Int) 0.85
