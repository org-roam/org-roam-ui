import * as React from "react"
import { ScrollView, StyleProp, Switch, TextStyle, TouchableOpacity, View, ViewStyle, StyleSheet, Button } from "react-native"
import { observer } from "mobx-react-lite"
import { color, typography } from "../../theme"
import { Text } from "../"
import { flatten } from "ramda"
import Slider from "@react-native-community/slider"
import { useState } from "react"
import Accordion from 'react-native-collapsible/Accordion'
import * as Animatable from 'react-native-animatable'
import Icon from 'react-native-vector-icons/MaterialCommunityIcons'

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
export const Tweaks = observer(function Tweaks(props: TweaksProps): JSX.Element {
    const { style, physics, setPhysics } = props
    //    const styles = flatten([CONTAINER, style])

    const content = [
        {
            title: "Physics",
            content:
                <View>
                    <Text preset="fieldLabel" text={"Repulsive force: " + physics.charge} />
                    <Slider style={{ height: 40, width: "90%" }}
                        minimumValue={-400}
                        maximumValue={100}
                        onValueChange={(value) => { setPhysics({ ...physics, charge: value }) }}
                        value={physics.charge}
                        step={1} />
                    <Text preset="fieldLabel" text={"Link Force: " + physics.linkStrength} />
                    <Slider style={{ height: 40, width: "90%" }}
                        minimumValue={-2}
                        maximumValue={2}
                        onValueChange={(value) => { setPhysics({ ...physics, linkStrength: value }) }}
                        value={physics.linkStrength}
                        step={0.1}
                    />
                    <Text preset="fieldLabel" text={"'Link Iterations': " + physics.linkIts} />
                    <Slider style={{ height: 40, width: "90%" }}
                        minimumValue={1}
                        maximumValue={10}
                        onValueChange={(value) => { setPhysics({ ...physics, linkIts: value }) }}
                        value={physics.linkIts}
                        step={1} />
                    <Text preset="fieldLabel" text="Collision" />
                    <Switch style={{ width: "5", height: 20, marginVertical: 10 }}
                        value={physics.collision}
                        onValueChange={() => { setPhysics({ ...physics, collision: !physics.collision }) }}
                    />
                </View>,
        },
        {
            title: 'Visual',
            content:
                <View>
                    <Text preset="fieldLabel" text={"Line Opacity: " + physics.linkOpacity} />
                    <Slider style={{ height: 40, width: "90%" }}
                        minimumValue={0}
                        maximumValue={1}
                        onValueChange={(value) => { setPhysics({ ...physics, linkOpacity: value }) }}
                        value={physics.linkOpacity}
                        step={.01} />
                    <Text preset="fieldLabel" text={"Line width: " + physics.linkWidth} />
                    <Slider style={{ height: 40, width: "90%" }}
                        minimumValue={.1}
                        maximumValue={10}
                        onValueChange={(value) => { setPhysics({ ...physics, linkWidth: value }) }}
                        value={physics.linkWidth}
                        step={0.1} />
                    <Text preset="fieldLabel" text={"Node size: " + physics.nodeRel} />
                    <Slider style={{ height: 40, width: "90%" }}
                        minimumValue={1}
                        maximumValue={10}
                        onValueChange={(value) => { setPhysics({ ...physics, nodeRel: value }) }}
                        value={physics.nodeRel}
                        step={.01} />
                    <Text preset="fieldLabel" text={"Particles: " + physics.particles} />
                    <Slider style={{ height: 40, width: "90%" }}
                        minimumValue={0}
                        maximumValue={10}
                        onValueChange={(value) => { setPhysics({ ...physics, particles: value }) }}
                        value={physics.particles}
                        step={1} />
                    <Text preset="fieldLabel" text={"Particle Size: " + physics.particleWidth} />
                    <Slider style={{ height: 40, width: "90%" }}
                        minimumValue={1}
                        maximumValue={10}
                        onValueChange={(value) => { setPhysics({ ...physics, particleWidth: value }) }}
                        value={physics.particleWidth}
                        step={.1} />
                </View>,
        },
        {
            title: 'Modes',
            content:
                <View>
                    <Text preset="fieldLabel" text="Expandable Graph" />
                    <Switch style={{ width: "5", height: 20, marginVertical: 10 }}
                        value={physics.collapse}
                        onValueChange={() => { setPhysics({ ...physics, collapse: !physics.collapse }) }}
                    />
                    <Text preset="fieldLabel" text="3D" />
                    <Switch style={{ width: "5", height: 20, marginVertical: 10 }}
                        value={physics.threedim}
                        onValueChange={() => { setPhysics({ ...physics, threedim: !physics.threedim }) }}
                    />
                </View>
        },
    ];

    const [activeSections, setActiveSections] = useState([]);

    const setSections = (sections) => {
        setActiveSections(
            sections.includes(undefined) ? [] : sections
        );
    };

    const renderHeader = (section, _, isActive) => {
        return (
            <Animatable.View
                duration={400}
                style={[styles.header, isActive ? styles.active : styles.inactive]}
                transition="backgroundColor"
            >
                <Text style={styles.headerText}>{section.title}</Text>
            </Animatable.View>
        );
    };

    const renderContent = (section, _, isActive) => {
        return (
            <Animatable.View
                duration={400}
                style={[styles.content, isActive ? styles.active : styles.inactive]}
                transition="backgroundColor"
            >
                {section.content}
            </Animatable.View>
        );
    }
    const [tweaks, setTweaks] = useState(true);
    if (true) {
        if (tweaks) {
            return (
                <View style={styles.container}>
                    <View style={{ height: 30, width: "100%", backgroundColor: "rgb(20,20,20)" }}>
                        <TouchableOpacity style={{ width: 30, color: "#ffffff", textAlign: "center", marginLeft: "auto", padding: 5 }}
                            onPress={() => { setTweaks(false) }}>
                            <Icon name="close-circle" color="#ffffff" size={20} />
                        </TouchableOpacity>
                    </View>
                    <ScrollView>
                        <Accordion
                            activeSections={activeSections}
                            sections={content}
                            touchAbleComponent={TouchableOpacity}
                            expandMultiple={true}
                            renderHeader={renderHeader}
                            renderContent={renderContent}
                            duration={200}
                            onChange={setSections}
                            renderAsFlatList={false}

                        />
                    </ScrollView>
                </View>
            );
        } else {
            return (
                <TouchableOpacity
                    onPress={() => { setTweaks(true) }}
                    style={{ position: "absolute", top: 50, left: 50, width: 30, color: "#ffffff", zIndex: 100 }}>
                    <Icon name="cog" color="#ffffff" size={30} />
                </TouchableOpacity>
            )
        }
    } else {
        return (
            <View style={{ position: "absolute", top: "5%", left: "5%", zIndex: 100, width: 300, backgroundColor: "#000000", padding: 20 }}>
                <Text preset="bold" text="Physics" />
                <Text preset="fieldLabel" text={"Repulsive force: " + physics.charge} />
                <Slider style={{ height: 40, width: "90%" }}
                    minimumValue={-400}
                    maximumValue={100}
                    onValueChange={(value) => { setPhysics({ ...physics, charge: value }) }}
                    value={physics.charge}
                    step={1} />
                <Text preset="fieldLabel" text={"Link Force: " + physics.linkStrength} />
                <Slider style={{ height: 40, width: "90%" }}
                    minimumValue={0}
                    maximumValue={2}
                    onValueChange={(value) => { setPhysics({ ...physics, linkStrength: value }) }}
                    value={physics.linkStrength}
                    step={0.1}
                />
                <Text preset="fieldLabel" text={"'Link Iterations': " + physics.linkIts} />
                <Slider style={{ height: 40, width: "90%" }}
                    minimumValue={1}
                    maximumValue={10}
                    onValueChange={(value) => { setPhysics({ ...physics, linkIts: value }) }}
                    value={physics.linkIts}
                    step={1} />
                <Text preset="fieldLabel" text="Collision" />
                <Switch style={{ width: "5", height: 20, marginVertical: 10 }}
                    value={physics.collision}
                    onValueChange={() => { setPhysics({ ...physics, collision: !physics.collision }) }}
                />
                <Text preset="bold" text="Visual" />
                <Text preset="fieldLabel" text={"Particles: " + physics.particles} />
                <Slider style={{ height: 40, width: "90%" }}
                    minimumValue={0}
                    maximumValue={5}
                    onValueChange={(value) => { setPhysics({ ...physics, particles: value }) }}
                    value={physics.particles}
                    step={1} />
                <Text preset="bold" text="Modes" />
                <Text preset="fieldLabel" text="Expandable Graph" />
                <Switch style={{ width: "5", height: 20, marginVertical: 10 }}
                    value={physics.collapse}
                    onValueChange={() => { setPhysics({ ...physics, collapse: !physics.collapse }) }}
                />
                <Text preset="fieldLabel" text="3D" />
                <Switch style={{ width: "5", height: 20, marginVertical: 10 }}
                    value={physics.threedim}
                    onValueChange={() => { setPhysics({ ...physics, threedim: !physics.threedim }) }}
                />
            </View>
        );
    }
})

const styles = StyleSheet.create({
    container: {
        flex: 1,
        backgroundColor: 'rgb(20,20,20)',
        position: "absolute",
        zIndex: 100,
        left: 50,
        top: 50,
        width: 250,
        borderRadius: 5,
        borderStyle: "solid",
        maxHeight: "70%",
        paddingBottom: 20,
    },
    title: {
        textAlign: 'left',
        fontSize: 22,
        fontWeight: '300',
        marginBottom: 20,
        paddingLeft: 20,
    },
    header: {
        backgroundColor: '#111111',
        padding: 10,
        paddingBottom: 20,
        textAlign: "left",
    },
    headerText: {
        textAlign: 'left',
        paddingLeft: 30,
        fontSize: 16,
        fontWeight: '500',
    },
    content: {
        padding: 20,
        paddingLeft: 60,
        backgroundColor: '#000000',
    },
    active: {
        backgroundColor: 'rgba(0,0,0,1)',
    },
    inactive: {
        backgroundColor: 'rgba(20,20,20,1)',
    },
    selectors: {
        marginBottom: 10,
        flexDirection: 'row',
        justifyContent: 'center',
    },
    selector: {
        backgroundColor: '#111111',
        padding: 10,
    },
    activeSelector: {
        fontWeight: 'bold',
    },
    selectTitle: {
        fontSize: 14,
        fontWeight: '500',
        padding: 10,
    },
    multipleToggle: {
        flexDirection: 'row',
        justifyContent: 'center',
        marginVertical: 30,
        alignItems: 'center',
    },
    multipleToggle__title: {
        fontSize: 16,
        marginRight: 8,
    },
});
