-- | Convert a DataFlow 'C.Diagram' to a Graphviz 'Graph'.
module DataFlow.DFD (
  asDFD
) where

import Debug.Trace

import Text.Printf
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M

import qualified DataFlow.Core as C
import DataFlow.Attributes
import DataFlow.Graphviz
import DataFlow.Graphviz.EdgeNormalization

type DFDState = Int
type DFD v = State DFDState v

incrStep :: DFD ()
incrStep = modify (+ 1)

-- | Get the next \"step\" number (the sequence number of flow arrows in the
-- | diagram).
nextStep :: DFD Int
nextStep = do
  incrStep
  get

inQuotes :: String -> String
inQuotes s = "\"" ++ s ++ "\""

inAngleBrackets :: String -> String
inAngleBrackets s = "<" ++ s ++ ">"

label :: String -> Attr
label "" = Attr "label" ""
label s = Attr "label" $ inAngleBrackets s

bold :: String -> String
bold "" = ""
bold s = "<b>" ++ s ++ "</b>"

italic :: String -> String
italic "" = ""
italic s = "<i>" ++ s ++ "</i>"

small :: String -> String
small "" = ""
small s = printf "<font point-size=\"10\">%s</font>" s

-- | Display the text with the given color (Graphviz color format, e.g. @grey35@).
color :: String -> String -> String
color _ "" = ""
color c s = printf "<font color=\"%s\">%s</font>" c s

convertNode :: C.Node -> DFD StmtList

convertNode (C.InputOutput id' attrs) = return [
    NodeStmt id' [
      Attr "shape" "rect",
      Attr "style" "bold",
      label $
        printf "<table border=\"0\" cellborder=\"0\"><tr><td>%s</td></tr></table>"
                (bold $ getTitleOrBlank attrs)
    ]
  ]

convertNode (C.Function id' attrs) = return [
    NodeStmt id' [
      Attr "shape" "component",
      label $ printf "<table border=\"0\" cellborder=\"0\"><tr><td>%s</td></tr></table>"
              (bold $ getTitleOrBlank attrs)
    ]
  ]

convertNode (C.Database id' attrs) = return [
    NodeStmt id' [
      Attr "shape" "cylinder",
      label $ printf "<table border=\"0\" cellborder=\"0\"><tr><td>%s</td></tr></table>"
              (bold $ getTitleOrBlank attrs)
    ]
  ]

convertNode (C.Document id' attrs) = return [
    NodeStmt id' [
      Attr "shape" "folder",
      label $ printf "<table border=\"0\" cellborder=\"0\"><tr><td>%s</td></tr></table>"
              (bold $ getTitleOrBlank attrs)
    ]
  ]

convertNodes :: [C.Node] -> DFD StmtList
convertNodes = liftM concat . mapM convertNode

convertFlow :: C.Flow -> DFD StmtList
convertFlow (C.Flow i1 i2 attrs) = do
    s <- nextStep
    let stepStr = color "#3184e4" $ bold $ printf "(%d) " s
        opStr = show <$> M.lookup "operation" attrs
        clr = case show <$> M.lookup "color" attrs of
            Nothing -> case opStr of
              Just "DELETE" -> "\"#e06666\""
              Just "INSERT" -> "\"#93c47d\""
              Just "UPDATE" -> "\"#93c47d\""
              Just "COPY-PASTE" -> "\"#93c47d\""
              Just "SELECT" -> "\"#6d9eeb\""
              Just "CALL"   -> "\"#ffd966\""
              _        -> "black"
            Just v -> v
        op = if opStr == Just "SELECT" || opStr == Just "DELETE" 
          then Just "" 
          else opStr

        weight = case show <$> M.lookup "operation" attrs of
          Just "DELETE" -> "100"
          Just "INSERT" -> "100"
          Just "UPDATE" -> "100"
          Just "COPY-PASTE" -> "80"
          Just "CALL" -> "80"
          Just "SELECT" -> "20"
          _ -> "50"

        penWidth = case show <$> M.lookup "operation" attrs of
          Just "DELETE" -> "3"
          Just "INSERT" -> "3"
          Just "UPDATE" -> "3"
          Just "COPY-PASTE" -> "3"
          Just "CALL" -> "2"
          Just "SELECT" -> "1"
          _ -> "2"

        asRows :: C.Value -> [String]
        asRows (C.String s) = lines s
        asRows (C.Array vs) = concatMap asRows vs

        rowsToTable :: [String] -> String
        rowsToTable rows =
          printf "<table border=\"0\" cellborder=\"0\" cellpadding=\"2\">%s</table>" r
          where r = concatMap (printf "<tr><td>%s</td></tr>") rows :: String

        rows = case (op, M.lookup "data" attrs) of
                (Just op, Just d) -> (stepStr ++ bold (op)) : map small (asRows d)
                (Just op, Nothing) -> [stepStr ++ bold (op)]
                (Nothing, Just d) -> stepStr : map small (asRows d)
                _ -> []
    return [
        EdgeStmt (EdgeExpr (IDOperand (NodeID i1 Nothing))
                          Arrow
                          (IDOperand (NodeID i2 Nothing))) [
          Attr "color" clr,
          Attr "weight" weight,
          Attr "penwidth" penWidth,
          label $ rowsToTable rows
        ]
      ]

convertFlows :: [C.Flow] -> DFD StmtList
convertFlows = liftM concat . mapM convertFlow

convertRootNode :: C.RootNode -> DFD StmtList
convertRootNode (C.TrustBoundary id' attrs nodes) = do
  nodeStmts <- convertNodes nodes
  let sgId = "cluster_" ++ id'
      defaultSgAttrs = [
          Attr "fontsize" "10",
          Attr "fontcolor" "grey35",
          Attr "style" "dashed",
          Attr "color" "grey35"]
      sgAttrs = case getTitle attrs of
                  Just title -> defaultSgAttrs ++ [label $ italic title]
                  Nothing -> defaultSgAttrs
      sgAttrStmt = AttrStmt Graph sgAttrs
      stmts = sgAttrStmt : nodeStmts
  return [SubgraphStmt $ Subgraph sgId stmts]

convertRootNode (C.Node n) = convertNode n

convertRootNodes :: [C.RootNode] -> DFD StmtList
convertRootNodes = liftM concat . mapM convertRootNode

defaultGraphStmts :: StmtList
defaultGraphStmts = [
    AttrStmt Graph [
      Attr "fontname" "Arial",
      Attr "fontsize" "14"
    ],
    AttrStmt Node [
      Attr "fontname" "Arial",
      Attr "fontsize" "14"
    ],
    AttrStmt Edge [
      Attr "shape" "none",
      Attr "fontname" "Arial",
      Attr "fontsize" "12"
    ],
    EqualsStmt "labelloc" (inQuotes "t"),
    EqualsStmt "fontsize" "20",
    EqualsStmt "nodesep" "1",
    EqualsStmt "rankdir" "t"
  ]

convertDiagram :: C.Diagram -> DFD Graph
convertDiagram (C.Diagram attrs rootNodes flows) = do
  n <- convertRootNodes rootNodes
  f <- convertFlows flows
  return $ case M.lookup "title" attrs of
              Just title ->
                let lbl = EqualsStmt "label" (inAngleBrackets $ show title)
                    stmts = lbl : defaultGraphStmts ++ n ++ f
                in normalize $ Digraph (inQuotes $ show title) stmts
              Nothing ->
                normalize $ Digraph "Untitled" $ defaultGraphStmts ++ n ++ f

-- | Converts a 'C.Diagram' to a 'Graph', with predefined styling, that can be
--   rendered as a Graphviz document.
asDFD :: C.Diagram -> Graph
asDFD d = evalState (convertDiagram d) 0

