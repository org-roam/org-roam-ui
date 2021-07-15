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
        <View style={{position: "absolute", top: 50, left: 50, zIndex: 100, width: "20%"}}>
        <Text preset="fieldLabel" text={"Repulsive force: " + physics.charge}/>
            <Slider style={{height: 40 , width: "90%"}}
                minimumValue={-400}
                maximumValue={0}
        onValueChange={(value) => { setPhysics({...physics, charge: value}) }}
                value={physics.charge}
                step={1}/>
        <Text preset="fieldLabel" text={"Link Force: " + physics.linkStrength}/>
            <Slider style={{height: 40 , width: "90%"}}
                minimumValue={0}
                maximumValue={2}
                    onValueChange={(value) => { setPhysics({...physics, linkStrength: value}) }}
                value={physics.linkStrength}
                step={0.1}
                />
            <Text preset="fieldLabel" text={"'Link Iterations': " + physics.linkIts}/>
            <Slider style={{ height: 40 , width: "90%"}}
                minimumValue={1}
                maximumValue={10}
                    onValueChange={(value) => { setPhysics({...physics, linkIts: value}) }}
                value={physics.linkIts}
                step={1}/>
            <Text preset="fieldLabel" text="Collision"/>
            <Switch style={{width: "5", height: 20 }}
                value={physics.collision}
                    onValueChange={()=>{setPhysics({...physics, collision: !physics.collision})}}
                />

            <Text preset="fieldLabel" text="Expandable Graph"/>
            <Switch style={{width: "5", height: 20 }}
                value={physics.collapse}
                    onValueChange={()=>{setPhysics({...physics, collapse: !physics.collapse})}}
                />
            <Text preset="fieldLabel" text="3D"/>
            <Switch style={{width: "5", height: 20 }}
                value={physics.threedim}
                    onValueChange={()=>{setPhysics({...physics, threedim: !physics.threedim})}}
                />
        </View>
    )
})
