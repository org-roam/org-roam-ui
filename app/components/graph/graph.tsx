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


import { ForceGraph2D, ForceGraph3D, ForceGraphVR, ForceGraphAR } from 'react-force-graph';
import * as d3 from "d3-force";

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
export const Graph = observer(function Graph(props: GraphProps) {
  const { style, physics, gData } = props
  const styles = flatten([CONTAINER, style])

    const fgRef= useRef();


    const GROUPS: number =12;
    //const gData = genRandomTree(200);

    //const [charge, setCharge] = useState(-30);
    //const [link, setLink] = useState(-30);

    useEffect(()=> {
      const fg = fgRef.current;

        //fg.d3Force('center').strength(0.05);
        fg.d3Force('link').strength(physics.linkStrength);
        fg.d3Force('link').iterations(physics.linkIts);
        physics.collision ? fg.d3Force('collide', d3.forceCollide().radius(20)) : fg.d3Force('collide',null);
        fg.d3Force('charge').strength(physics.charge);
  });

    const rootId = 0;

    const nodesById = useMemo(() => {
      const nodesById = Object.fromEntries(rando.nodes.map(node => [node.id, node]));

      // link parent/children
      rando.nodes.forEach(node => {
        node.collapsed = node.id !== rootId;
        node.childLinks = [];
      });
      rando.links.forEach(link => nodesById[link.source].childLinks.push(link));

      return nodesById;
    }, [rando]);

    const getPrunedTree = useCallback(() => {
      const visibleNodes = [];
      const visibleLinks = [];
      (function traverseTree(node = nodesById[rootId]) {
        visibleNodes.push(node);
        if (node.collapsed) return;
        visibleLinks.push(...node.childLinks);
        node.childLinks
          .map(link => ((typeof link.target) === 'object') ? link.target : nodesById[link.target]) // get child node
          .forEach(traverseTree);
      })();

      return { nodes: visibleNodes, links: visibleLinks };
    }, [nodesById]);

    const [prunedTree, setPrunedTree] = useState(getPrunedTree());

    const handleNodeClick = useCallback(node => {
      node.collapsed = !node.collapsed; // toggle collapse state
      setPrunedTree(getPrunedTree())
    }, []);

    return (
    <View>
    {!physics.threedim ?
    <ForceGraph2D
      ref={fgRef}
      graphData={physics.collapse ? prunedTree : rando}
     // nodeAutoColorBy={d => d.id%GROUPS}
      linkAutoColorBy={d => rando.nodes[d.source].id%GROUPS}
      linkColor={"#ffffff"}
      linkWidth={2}
      linkDirectionalParticles={2}
     nodeColor={node => !node.childLinks.length ? 'green' : node.collapsed ? 'red' : 'yellow'}
      onNodeClick={handleNodeClick}
      //d3VelocityDecay={visco}
      />
        :
    <ForceGraph3D
      ref={fgRef}
      graphData={physics.collapse ? prunedTree : rando}
     // nodeAutoColorBy={d => d.id%GROUPS}
      linkAutoColorBy={d => rando.nodes[d.source].id%GROUPS}
      linkColor={"#ffffff"}
      linkWidth={2}
      linkDirectionalParticles={2}
     nodeColor={node => !node.childLinks.length ? 'green' : node.collapsed ? 'red' : 'yellow'}
      onNodeClick={handleNodeClick}
      //d3VelocityDecay={visco}
      />
    }
    </View>
  )
})
