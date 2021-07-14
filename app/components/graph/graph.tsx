import * as React from "react"
import { StyleProp, TextStyle, View, ViewStyle } from "react-native"
import { observer } from "mobx-react-lite"
import { color, typography } from "../../theme"
import { Text } from "../"
import { flatten } from "ramda"

import data from "../../data/miserables.json"
import genRandomTree from "../../data/randomdata";

import { ForceGraph2D, ForceGraph3D, ForceGraphVR, ForceGraphAR } from 'react-force-graph';
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

  //  const fgRef= React.useRef();


    const GROUPS=12;
    const gData = genRandomTree();

    const [visco, setVisco] = React.useState(0.4);

 // React.useEffect(()=> {
 //     const fg = fgRef.current;

 //     fg.d3Force('center', visco);
 // });

    return (
    <View>
    <Slider style={{position: "absolute", zIndex: 100, width: "20%", height: 40}}
        minimumValue={0}
        maximumValue={1}
        onValueChange={(value)=>{setVisco(value)}}
        value={visco}
    />
    <ForceGraph2D
   //   ref={fgRef}
      graphData={gData}
      nodeAutoColorBy={d => d.id%GROUPS}
      linkAutoColorBy={d => gData.nodes[d.source].id%GROUPS}
      linkColor={"#ffffff"}
      linkWidth={2}
      d3VelocityDecay={visco}
      />
    </View>
  )
})
