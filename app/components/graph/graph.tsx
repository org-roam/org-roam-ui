import * as React from "react"
import { useState, useEffect, useRef } from "react"
import { StyleProp, TextStyle, View, ViewStyle } from "react-native"
import { observer } from "mobx-react-lite"
import { color, typography } from "../../theme"
import { Text } from "../"
import { flatten } from "ramda"

import data from "../../data/miserables.json"
import genRandomTree from "../../data/randomdata";

import { ForceGraph2D, ForceGraph3D, ForceGraphVR, ForceGraphAR } from 'react-force-graph';
import { GraphData, ForceGraphMethods } from "react-force-graph-2d";
import * as d3 from "d3-force";
import  Slider  from '@react-native-community/slider';

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
}

/**
 * Describe your component here
 */
export const Graph = observer(function Graph(props: GraphProps) {
  const { style } = props
  const styles = flatten([CONTAINER, style])

    const fgRef= useRef();


    const GROUPS: number =12;
    const gData = genRandomTree();

    const [charge, setCharge] = useState(-30);
    const [link, setLink] = useState(-30);

    useEffect(()=> {
      const fg = fgRef.current;

        fg.d3Force('charge').strength(charge);
        fg.d3Force('center').strength(0.05);
        fg.d3Force('link').strength(0.1);
        fg.d3Force('link').iterations(4);
        fg.d3Force('collide', d3.forceCollide().radius(20));
  });

    return (
    <View>
    <Slider style={{position: "absolute", zIndex: 100, width: "20%", height: 40}}
        minimumValue={-100}
        maximumValue={10}
        onValueChange={(value)=>{setCharge(value)}}
        value={charge}
    />
    <Slider style={{position: "absolute", top: 50, zIndex: 100, width: "20%", height: 40}}
        minimumValue={-100}
        maximumValue={0}
        onValueChange={(value)=>{setCharge(value)}}
        value={charge}
    />
    <ForceGraph2D
      ref={fgRef}
      graphData={gData}
      nodeAutoColorBy={d => d.id%GROUPS}
      linkAutoColorBy={d => gData.nodes[d.source].id%GROUPS}
      linkColor={"#ffffff"}
      linkWidth={2}
      //d3VelocityDecay={visco}
      />
    </View>
  )
})
