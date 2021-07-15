import * as React from "react"
import { useState, useEffect, useRef } from "react"
import { StyleProp, TextStyle, View, ViewStyle } from "react-native"
import { observer } from "mobx-react-lite"
import { color, typography } from "../../theme"
import { Text } from "../"
import { flatten } from "ramda"

//import data from "../../data/miserables.json"
//import genRandomTree from "../../data/randomdata";
import rando from "../../data/rando.json"

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

    return (
    <View>
    <ForceGraph2D
      ref={fgRef}
      graphData={rando}
      nodeAutoColorBy={d => d.id%GROUPS}
      linkAutoColorBy={d => rando.nodes[d.source].id%GROUPS}
      linkColor={"#ffffff"}
      linkWidth={2}
      //d3VelocityDecay={visco}
      />
    </View>
  )
})
