import * as React from "react"
import { StyleProp, Switch, TextStyle, View, ViewStyle } from "react-native"
import { observer } from "mobx-react-lite"
import { color, typography } from "../../theme"
import { Text } from "../"
import { flatten } from "ramda"
import Slider from "@react-native-community/slider"
import { useState } from "react"

const CONTAINER: ViewStyle = {
    justifyContent: "center",
}

const TEXT: TextStyle = {
    fontFamily: typography.primary,
    fontSize: 14,
    color: color.primary,
}

export interface TweaksProps {
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
export const Tweaks = observer(function Tweaks(props: TweaksProps) {
    const { style, physics, setPhysics } = props
    const styles = flatten([CONTAINER, style])

    return (
        <>
            <Slider style={{ position: "absolute", top: 50,  zIndex: 100, width: "20%", height: 40 }}
                minimumValue={-100}
                maximumValue={0}
        onValueChange={(value) => { setPhysics({...physics, charge: value}) }}
                value={physics.charge}
                step={1}/>
            <Slider style={{ position: "absolute", top: 100,  zIndex: 100, width: "20%", height: 40 }}
                minimumValue={0}
                maximumValue={10}
                    onValueChange={(value) => { setPhysics({...physics, linkStrength: value}) }}
                value={physics.linkStrength}
                step={1}/>
            <Slider style={{ position: "absolute", top: 150,  zIndex: 100, width: "20%", height: 40 }}
                minimumValue={1}
                maximumValue={5}
                    onValueChange={(value) => { setPhysics({...physics, linkIts: value}) }}
                value={physics.linkIts}
                step={1}/>
            <Switch style={{ position: "absolute", top: 200,  zIndex: 100, width: "5", height: 40 }}
                value={physics.collision}
                    onValueChange={()=>{setPhysics({...physics, collision: !physics.collision})}}
                />
        </>
    )
})
