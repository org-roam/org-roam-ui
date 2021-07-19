import * as React from 'react'
import {
  ScrollView,
  StyleProp,
  TextStyle,
  TouchableOpacity,
  View,
  ViewStyle,
  StyleSheet,
  Button,
} from 'react-native'
import { observer } from 'mobx-react-lite'
import { color, typography } from '../../theme'
import { Text } from '../'
import { flatten } from 'ramda'
import Slider from '@react-native-community/slider'
import { useState } from 'react'
import Accordion from 'react-native-collapsible/Accordion'
import * as Animatable from 'react-native-animatable'
import Icon from 'react-native-vector-icons/MaterialCommunityIcons'
import { Switch } from 'react-native-elements'

const CONTAINER: ViewStyle = {
  justifyContent: 'center',
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
export const Tweaks = observer(function Tweaks(
  props: TweaksProps,
): JSX.Element {
  const { style, physics, setPhysics } = props
  //    const styles = flatten([CONTAINER, style])

  const content = [
    {
      title: 'Physics',
      content: (
        <View>
          <Text preset="fieldLabel" text="Gravity" />
          <Switch
            color="#a991f1"
            trackColor={{
              false: '#62686E',
              true: '#a991f1',
            }}
            style={styles.switch}
            value={physics.gravityOn}
            onValueChange={() => {
              setPhysics({ ...physics, gravityOn: !physics.gravityOn })
            }}
          />
          <Text preset="fieldLabel" text={'Gravity: ' + physics.gravity} />
          <Slider
            minimumTrackTintColor="#a991f1"
            maximumTrackTintColor="#242730"
            thumbTintColor="#a991f1"
            style={styles.slider}
            minimumValue={0}
            maximumValue={1}
            onValueChange={(value) => {
              setPhysics({ ...physics, gravity: value })
            }}
            value={physics.gravity}
            step={0.01}
          />
          <Text
            preset="fieldLabel"
            text={'Repulsive force: ' + physics.charge}
          />
          <Slider
            minimumTrackTintColor="#a991f1"
            maximumTrackTintColor="#242730"
            thumbTintColor="#a991f1"
            style={styles.slider}
            minimumValue={-400}
            maximumValue={100}
            onValueChange={(value) => {
              setPhysics({ ...physics, charge: value })
            }}
            value={physics.charge}
            step={1}
          />
          <Text
            preset="fieldLabel"
            text={'Link Force: ' + physics.linkStrength}
          />
          <Slider
            minimumTrackTintColor="#a991f1"
            maximumTrackTintColor="#242730"
            thumbTintColor="#a991f1"
            style={styles.slider}
            minimumValue={0}
            maximumValue={2}
            onValueChange={(value) => {
              setPhysics({ ...physics, linkStrength: value })
            }}
            value={physics.linkStrength}
            step={0.01}
          />
          <Text
            preset="fieldLabel"
            text={"'Link Iterations': " + physics.linkIts}
          />
          <Slider
            minimumTrackTintColor="#a991f1"
            maximumTrackTintColor="#242730"
            thumbTintColor="#a991f1"
            style={styles.slider}
            minimumValue={1}
            maximumValue={10}
            onValueChange={(value) => {
              setPhysics({ ...physics, linkIts: value })
            }}
            value={physics.linkIts}
            step={1}
          />
          <Text preset="fieldLabel" text="Collision" />
          <Switch
            color="#a991f1"
            trackColor={{
              false: '#62686E',
              true: '#a991f1',
            }}
            style={styles.switch}
            value={physics.collision}
            onValueChange={() => {
              setPhysics({ ...physics, collision: !physics.collision })
            }}
          />
          <Text
            preset="fieldLabel"
            text={'Alpha Decay: ' + physics.alphaDecay}
          />
          <Slider
            style={styles.slider}
            minimumTrackTintColor="#a991f1"
            maximumTrackTintColor="#242730"
            thumbTintColor="#a991f1"
            minimumValue={0}
            maximumValue={1}
            onValueChange={(value) => {
              setPhysics({ ...physics, alphaDecay: value })
            }}
            value={physics.alphaDecay}
            step={0.01}
          />
          <Text
            preset="fieldLabel"
            text={'Alhpa Target: ' + physics.alphaTarget}
          />
          <Slider
            minimumTrackTintColor="#a991f1"
            maximumTrackTintColor="#242730"
            thumbTintColor="#a991f1"
            style={styles.slider}
            minimumValue={0}
            maximumValue={1}
            onValueChange={(value) => {
              setPhysics({ ...physics, alphaTarget: value })
            }}
            value={physics.alphaTarget}
            step={0.1}
          />
          <Text
            preset="fieldLabel"
            text={'Viscosity: ' + physics.velocityDecay}
          />
          <Slider
            minimumTrackTintColor="#a991f1"
            maximumTrackTintColor="#242730"
            thumbTintColor="#a991f1"
            style={styles.slider}
            minimumValue={0}
            maximumValue={1}
            onValueChange={(value) => {
              setPhysics({ ...physics, velocityDecay: value })
            }}
            value={physics.velocityDecay}
            step={0.01}
          />
          <Text preset="fieldLabel" text={'Galaxy Mode (3D-only)'} />
          <Switch
            color="#a991f1"
            trackColor={{
              false: '#62686E',
              true: '#a991f1',
            }}
            style={styles.switch}
            value={physics.galaxy}
            onValueChange={() => {
              setPhysics({ ...physics, galaxy: !physics.galaxy })
            }}
          />
        </View>
      ),
    },
    {
      title: 'Visual',
      content: (
        <View>
          <Text preset="fieldLabel" text="Colorful" />
          <Switch
            color="#a991f1"
            trackColor={{
              false: '#62686E',
              true: '#a991f1',
            }}
            style={styles.switch}
            value={physics.colorful}
            onValueChange={() => {
              setPhysics({ ...physics, colorful: !physics.colorful })
            }}
          />
          <Text preset="fieldLabel" text="Hover highlight" />
          <Switch
            color="#a991f1"
            trackColor={{
              false: '#62686E',
              true: '#a991f1',
            }}
            style={styles.switch}
            value={physics.hover}
            onValueChange={() => {
              setPhysics({ ...physics, hover: !physics.hover })
            }}
          />
          <Text
            preset="fieldLabel"
            text={'Line Opacity: ' + physics.linkOpacity}
          />
          <Slider
            minimumTrackTintColor="#a991f1"
            maximumTrackTintColor="#242730"
            thumbTintColor="#a991f1"
            style={styles.slider}
            minimumValue={0}
            maximumValue={1}
            onValueChange={(value) => {
              setPhysics({ ...physics, linkOpacity: value })
            }}
            value={physics.linkOpacity}
            step={0.01}
          />
          <Text preset="fieldLabel" text={'Line width: ' + physics.linkWidth} />
          <Slider
            minimumTrackTintColor="#a991f1"
            maximumTrackTintColor="#242730"
            thumbTintColor="#a991f1"
            style={styles.slider}
            minimumValue={0.1}
            maximumValue={10}
            onValueChange={(value) => {
              setPhysics({ ...physics, linkWidth: value })
            }}
            value={physics.linkWidth}
            step={0.1}
          />
          <Text preset="fieldLabel" text={'Node size: ' + physics.nodeRel} />
          <Slider
            minimumTrackTintColor="#a991f1"
            maximumTrackTintColor="#242730"
            thumbTintColor="#a991f1"
            style={styles.slider}
            minimumValue={1}
            maximumValue={10}
            onValueChange={(value) => {
              setPhysics({ ...physics, nodeRel: value })
            }}
            value={physics.nodeRel}
            step={0.01}
          />
          <Text preset="fieldLabel" text={'Particles: ' + physics.particles} />
          <Slider
            minimumTrackTintColor="#a991f1"
            maximumTrackTintColor="#242730"
            thumbTintColor="#a991f1"
            style={styles.slider}
            minimumValue={0}
            maximumValue={10}
            onValueChange={(value) => {
              setPhysics({ ...physics, particles: value })
            }}
            value={physics.particles}
            step={1}
          />
          <Text
            preset="fieldLabel"
            text={'Particle Size: ' + physics.particleWidth}
          />
          <Slider
            minimumTrackTintColor="#a991f1"
            maximumTrackTintColor="#242730"
            thumbTintColor="#a991f1"
            style={styles.slider}
            minimumValue={1}
            maximumValue={10}
            onValueChange={(value) => {
              setPhysics({ ...physics, particleWidth: value })
            }}
            value={physics.particleWidth}
            step={0.1}
          />
          <Text preset="fieldLabel" text="Labels" />
          <Switch
            color="#a991f1"
            trackColor={{
              false: '#62686E',
              true: '#a991f1',
            }}
            style={styles.switch}
            value={physics.labels}
            onValueChange={() => {
              setPhysics({ ...physics, labels: !physics.labels })
            }}
          />
          <Text
            preset="fieldLabel"
            text={'Scale when labels become visible: ' + physics.labelScale}
          />
          <Slider
            minimumTrackTintColor="#a991f1"
            maximumTrackTintColor="#242730"
            thumbTintColor="#a991f1"
            style={styles.slider}
            minimumValue={0.1}
            maximumValue={5}
            onValueChange={(value) => {
              setPhysics({ ...physics, labelScale: value })
            }}
            value={physics.labelScale}
            step={0.1}
          />
        </View>
      ),
    },
    {
      title: 'Modes',
      content: <View></View>,
    },
  ]

  const [activeSections, setActiveSections] = useState([])

  const setSections = (sections) => {
    setActiveSections(sections.includes(undefined) ? [] : sections)
  }

  const renderHeader = (section, _, isActive) => {
    return (
      <Animatable.View
        duration={400}
        style={[styles.header, isActive ? styles.active : styles.inactive]}
        transition="backgroundColor"
      >
        <Text style={styles.headerText}>{section.title}</Text>
      </Animatable.View>
    )
  }

  const renderContent = (section, _, isActive) => {
    return (
      <Animatable.View
        duration={400}
        style={[styles.content, isActive ? styles.active : styles.inactive]}
        transition="backgroundColor"
      >
        {section.content}
      </Animatable.View>
    )
  }
  const [tweaks, setTweaks] = useState(true)
  if (true) {
    if (tweaks) {
      return (
        <View style={styles.container}>
          <View
            style={{ height: 30, width: '100%', backgroundColor: '#2a2e38' }}
          >
            <TouchableOpacity
              style={{
                width: 30,
                color: '#a991f1',
                textAlign: 'center',
                marginLeft: 'auto',
                padding: 5,
              }}
              onPress={() => {
                setTweaks(false)
              }}
            >
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
      )
    } else {
      return (
        <TouchableOpacity
          onPress={() => {
            setTweaks(true)
          }}
          style={{
            position: 'absolute',
            top: 50,
            left: 50,
            width: 30,
            color: '#ffffff',
            zIndex: 100,
          }}
        >
          <Icon name="cog" color="#ffffff" size={30} />
        </TouchableOpacity>
      )
    }
  } else {
    return (
      <View
        style={{
          position: 'absolute',
          top: '5%',
          left: '5%',
          zIndex: 100,
          width: 300,
          backgroundColor: '#000000',
          padding: 20,
        }}
      >
        <Text preset="bold" text="Physics" />
        <Text preset="fieldLabel" text={'Repulsive force: ' + physics.charge} />
        <Slider
          minimumTrackTintColor="#a991f1"
          maximumTrackTintColor="#242730"
          thumbTintColor="#a991f1"
          style={styles.slider}
          minimumValue={-400}
          maximumValue={100}
          onValueChange={(value) => {
            setPhysics({ ...physics, charge: value })
          }}
          value={physics.charge}
          step={1}
        />
        <Text
          preset="fieldLabel"
          text={'Link Force: ' + physics.linkStrength}
        />
        <Slider
          minimumTrackTintColor="#a991f1"
          maximumTrackTintColor="#242730"
          thumbTintColor="#a991f1"
          style={styles.slider}
          minimumValue={0}
          maximumValue={2}
          onValueChange={(value) => {
            setPhysics({ ...physics, linkStrength: value })
          }}
          value={physics.linkStrength}
          step={0.1}
        />
        <Text
          preset="fieldLabel"
          text={"'Link Iterations': " + physics.linkIts}
        />
        <Slider
          minimumTrackTintColor="#a991f1"
          maximumTrackTintColor="#242730"
          thumbTintColor="#a991f1"
          style={styles.slider}
          minimumValue={1}
          maximumValue={10}
          onValueChange={(value) => {
            setPhysics({ ...physics, linkIts: value })
          }}
          value={physics.linkIts}
          step={1}
        />
        <Text preset="fieldLabel" text="Collision" />
        <Switch
          color="#a991f1"
          trackColor={{
            false: '#62686E',
            true: '#a991f1',
          }}
          style={styles.switch}
          value={physics.collision}
          onValueChange={() => {
            setPhysics({ ...physics, collision: !physics.collision })
          }}
        />
        <Text preset="bold" text="Visual" />
        <Text preset="fieldLabel" text={'Particles: ' + physics.particles} />
        <Slider
          minimumTrackTintColor="#a991f1"
          maximumTrackTintColor="#242730"
          thumbTintColor="#a991f1"
          style={styles.slider}
          minimumValue={0}
          maximumValue={5}
          onValueChange={(value) => {
            setPhysics({ ...physics, particles: value })
          }}
          value={physics.particles}
          step={1}
        />
        <Text preset="bold" text="Modes" />
        <Text preset="fieldLabel" text="Expandable Graph" />
        <Switch
          color="#a991f1"
          trackColor={{
            false: '#62686E',
            true: '#a991f1',
          }}
          style={styles.switch}
          value={physics.collapse}
          onValueChange={() => {
            setPhysics({ ...physics, collapse: !physics.collapse })
          }}
        />
        <Text preset="fieldLabel" text="3D" />
        <Switch
          color="#a991f1"
          trackColor={{
            false: '#62686E',
            true: '#a991f1',
          }}
          style={styles.switch}
          value={physics.threedim}
          onValueChange={() => {
            setPhysics({ ...physics, threedim: !physics.threedim })
          }}
        />
      </View>
    )
  }
})

const styles = StyleSheet.create({
  container: {
    display: 'flex',
    backgroundColor: '#2a2e38',
    position: 'absolute',
    zIndex: 5,
    marginLeft: '2%',
    marginTop: '2%',
    maxWidth: 275,
    borderRadius: 10,
    borderStyle: 'solid',
    borderWidth: 10,
    borderColor: '#2a2e38',
    maxHeight: '80%',
    paddingBottom: 20,
  },
  title: {
    textAlign: 'left',
    fontSize: 22,
    fontWeight: '300',
    marginBottom: 20,
    paddingLeft: 10,
  },
  header: {
    backgroundColor: '#2a2e38',
    padding: 10,
    paddingBottom: 20,
    textAlign: 'left',
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
    backgroundColor: '#2a2e38',
  },
  inactive: {
    backgroundColor: '#2a2e38',
  },
  selectors: {
    marginBottom: 10,
    flexDirection: 'row',
    justifyContent: 'center',
  },
  selector: {
    backgroundColor: '#2a2e38',
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
  slider: {
    minimumTrackTintColor: '#a991f1',
    thumbTintColor: '#a991f1',
    height: 40,
    width: '90%',
  },
  switch: {
    width: '5',
    height: 20,
    marginVertical: 10,
  },
})
