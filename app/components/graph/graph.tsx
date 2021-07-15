import * as React from "react"
import { useState, useEffect, useRef, useMemo, useCallback } from "react"
import { StyleProp, TextStyle, View, ViewStyle } from "react-native"
import { observer } from "mobx-react-lite"
import { color, typography } from "../../theme"
import { Text } from "../"
import { flatten } from "ramda"

//import data from "../../data/miserables.json"
//import genRandomTree from "../../data/randomdata";
//import rando from "../../data/rando.json"
import rando from "../../data/randorev.json"

import { ForceGraph2D, ForceGraph3D, ForceGraphVR, ForceGraphAR } from "react-force-graph"
import * as d3 from "d3-force"

const CONTAINER: ViewStyle = {
  justifyContent: "center",
}

const TEXT: TextStyle = {
  fontFamily: typography.primary,
  fontSize: 14,
  color: color.primary,
}

export interface GraphProps {
  /**
   * An optional style override useful for padding & margin.
   */
  style?: StyleProp<ViewStyle>
  physics
  gData
}

/**
 * Describe your component here
 */
export const Graph = observer(function Graph(props: GraphProps): JSX.Element {
  const { style, physics, gData } = props
  const styles = flatten([CONTAINER, style])

  const fgRef = useRef()

  const GROUPS: number = 12
  const NODE_R: number = 8
  //const gData = genRandomTree(200);

  //const [charge, setCharge] = useState(-30);
  //const [link, setLink] = useState(-30);

  useEffect(() => {
    const fg = fgRef.current

    //fg.d3Force('center').strength(0.05);
    fg.d3Force("link").strength(physics.linkStrength)
    fg.d3Force("link").iterations(physics.linkIts)
    physics.collision
      ? fg.d3Force("collide", d3.forceCollide().radius(20))
      : fg.d3Force("collide", null)
    fg.d3Force("charge").strength(physics.charge)
  })

  // For the expandable version of the graph
  const rootId = 0

  const nodesById = useMemo(() => {
    const nodesById = Object.fromEntries(rando.nodes.map((node) => [node.id, node]))

    // link parent/children
    rando.nodes.forEach((node) => {
      node.collapsed = node.id !== rootId
      node.childLinks = []
      node.parentLink = []
    })
    rando.links.forEach((link) => nodesById[link.source].childLinks.push(link))

    return nodesById
  }, [rando])

  const getPrunedTree = useCallback(() => {
    const visibleNodes = []
    const visibleLinks = []
      ; (function traverseTree(node = nodesById[rootId]) {
        visibleNodes.push(node)
        if (node.collapsed) return
        visibleLinks.push(...node.childLinks)
        node.childLinks
          .map((link) => (typeof link.target === "object" ? link.target : nodesById[link.target])) // get child node
          .forEach(traverseTree)
      })()

    return { nodes: visibleNodes, links: visibleLinks }
  }, [nodesById])

  const [prunedTree, setPrunedTree] = useState(getPrunedTree())

  const handleNodeClick = useCallback((node) => {
    node.collapsed = !node.collapsed // toggle collapse state
    setPrunedTree(getPrunedTree())
  }, []);


  // Highlight Graph
  /**
/* const data = useMemo(() => {
*         // cross-link node objects
*         rando.links.forEach(link => {
*           const a = rando.nodes[link.source];
*           const b = rando.nodes[link.target];
*           !a.neighbors && (a.neighbors = []);
*           !b.neighbors && (b.neighbors = []);
*           a.neighbors.push(b);
*           b.neighbors.push(a);
*
*           !a.links && (a.links = []);
*           !b.links && (b.links = []);
*           a.links.push(link);
*           b.links.push(link);
*         });
*
*         return rando;
*       }, []);
* const [highlightNodes, setHighlightNodes] = useState(new Set());
*       const [highlightLinks, setHighlightLinks] = useState(new Set());
*       const [hoverNode, setHoverNode] = useState(null);
*
*       const updateHighlight = () => {
*         setHighlightNodes(highlightNodes);
*         setHighlightLinks(highlightLinks);
*       };
*
*       const handleNodeHover = node => {
*         highlightNodes.clear();
*         highlightLinks.clear();
*         if (node) {
*           highlightNodes.add(node);
*           node.neighbors.forEach(neighbor => highlightNodes.add(neighbor));
*           node.links.forEach(link => highlightLinks.add(link));
*         }
*
*         setHoverNode(node || null);
*         updateHighlight();
*       };
*
*       const handleLinkHover = link => {
*         highlightNodes.clear();
*         highlightLinks.clear();
*
*         if (link) {
*           highlightLinks.add(link);
*           highlightNodes.add(link.source);
*           highlightNodes.add(link.target);
*         }
*
*         updateHighlight();
*       };
*
*       const paintRing = useCallback((node, ctx) => {
*         // add ring just for highlighted nodes
*         ctx.beginPath();
*         ctx.arc(node.x, node.y, NODE_R * 1.4, 0, 2 * Math.PI, false);
*         ctx.fillStyle = node === hoverNode ? 'red' : 'orange';
*         ctx.fill();
*       }, [hoverNode]);
*/

          /* autoPauseRedraw={false}
        linkWidth={link => highlightLinks.has(link) ? 5 : 1}
        linkDirectionalParticles={4}
        linkDirectionalParticleWidth={link => highlightLinks.has(link) ? 4 : 0}
        nodeCanvasObjectMode={node => highlightNodes.has(node) ? 'before' : undefined}
        nodeCanvasObject={paintRing}
        onNodeHover={handleNodeHover}
        onLinkHover={handleLinkHover}
                nodeRelSize={NODE_R} */

  return (
    <View>
      {!physics.threedim ? (
        <ForceGraph2D
          ref={fgRef}
          graphData={physics.collapse ? prunedTree : rando}
          // nodeAutoColorBy={d => d.id%GROUPS}
          linkAutoColorBy={(d) => rando.nodes[d.source].id % GROUPS}
          linkColor={"#ffffff"}
          linkDirectionalParticles={physics.particles}
          nodeColor={(node) =>
            !node.childLinks.length ? "green" : node.collapsed ? "red" : "yellow"
          }
          onNodeClick={!physics.collapse ? null : handleNodeClick}
          nodeLabel={(node) => "label"}
          // nodeVal ={(node)=> node.childLinks.length * 0.5 + 1}
          //d3VelocityDecay={visco}
          linkWidth={physics.linkWidth}
          linkOpacity={physics.linkOpacity}
          nodeRelSize={physics.nodeRel}
          linkDirectionalParticleWidth={physics.particleWidth}
        />
      ) : (
        <ForceGraph3D
          ref={fgRef}
          graphData={physics.collapse ? prunedTree : rando}
          // nodeAutoColorBy={d => d.id%GROUPS}
          linkAutoColorBy={(d) => rando.nodes[d.source].id % GROUPS}
          linkColor={"#ffffff"}
          linkWidth={2}
          linkDirectionalParticles={physics.particles}
          nodeColor={(node) =>
            !node.childLinks.length ? "green" : node.collapsed ? "red" : "yellow"
          }
          onNodeClick={!physics.collapse ? null : handleNodeClick}
          nodeVal={(node) => node.childLinks.length + 1}
        //d3VelocityDecay={visco}
          linkWidth={physics.linkWidth}
          linkOpacity={physics.linkOpacity}
          nodeRelSize={physics.nodeRel}
          linkDirectionalParticleWidth={physics.particleWidth}
          backgroundColor="#1d1d1d"
        />
      )}
    </View>
  )
})
