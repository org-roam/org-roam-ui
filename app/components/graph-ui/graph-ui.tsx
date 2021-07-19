import * as React from "react"
import { StyleProp, TextStyle, View, ViewStyle } from "react-native"
import { observer } from "mobx-react-lite"
import { color, typography } from "../../theme"
import { LocalButton, Text, Tweaks } from "../"
import { flatten } from "ramda"

const CONTAINER: ViewStyle = {
  justifyContent: "center",
}

const TEXT: TextStyle = {
  fontFamily: typography.primary,
  fontSize: 14,
  color: color.primary,
}

export interface GraphUiProps {
  /**
   * An optional style override useful for padding & margin.
   */
  style?: StyleProp<ViewStyle>
    physics
    setPhysics
}

/**
 * Describe your component here
 */
export const GraphUi = observer(function GraphUi(props: GraphUiProps) {
  const { style, physics, setPhysics } = props
  const styles = flatten([CONTAINER, style])

  return (
      <View style={{height: "100%", width: "100%", borderStyle: "solid", borderWidth: 5,position:"relative"}}>
        <Tweaks physics={physics} setPhysics={setPhysics} />
        <LocalButton physics={physics} setPhysics={setPhysics}/>
    </View>
  )
})
