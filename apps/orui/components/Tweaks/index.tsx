import { CloseIcon, RepeatClockIcon, SettingsIcon } from '@chakra-ui/icons'
import {
  Accordion,
  AccordionButton,
  AccordionIcon,
  AccordionItem,
  AccordionPanel,
  Box,
  Button,
  IconButton,
  Tooltip,
  Heading,
} from '@chakra-ui/react'

import React, { useContext } from 'react'
import Scrollbars from 'react-custom-scrollbars-2'
import {
  initialPhysics,
  initialFilter,
  initialVisuals,
  initialMouse,
  initialBehavior,
  initialLocal,
  TagColors,
  colorList,
  initialColoring,
} from '../config'

import FilterPanel from './Filter/FilterPanel'

import { ThemeContext } from '../../util/themecontext'
import { usePersistantState } from '../../util/persistant-state'
import { PhysicsPanel } from './Physics/PhysicsPanel'
import { BehaviorPanel } from './Behavior/BehaviorPanel'
import { VisualsPanel } from './Visual/VisualsPanel'

export interface TweakProps {
  physics: typeof initialPhysics
  setPhysics: any
  threeDim: boolean
  setThreeDim: (newValue: boolean) => void
  filter: typeof initialFilter
  setFilter: any
  visuals: typeof initialVisuals
  setVisuals: any
  mouse: typeof initialMouse
  setMouse: any
  behavior: typeof initialBehavior
  setBehavior: any
  tags: string[]
  tagColors: TagColors
  setTagColors: any
  coloring: typeof initialColoring
  setColoring: any
  local: typeof initialLocal
  setLocal: any
}

export const Tweaks = (props: TweakProps) => {
  const {
    physics,
    setPhysics,
    threeDim,
    setThreeDim,
    filter,
    setFilter,
    visuals,
    setVisuals,
    mouse,
    setMouse,
    behavior,
    setBehavior,
    tags,
    tagColors,
    setTagColors,
    coloring,
    setColoring,
    local,
    setLocal,
  } = props

  const [showTweaks, setShowTweaks] = usePersistantState('showTweaks', false)
  const { highlightColor, setHighlightColor } = useContext(ThemeContext)

  return !showTweaks ? (
    <Box
      position="absolute"
      zIndex="overlay"
      marginTop={1}
      marginLeft={0}
      display={showTweaks ? 'none' : 'block'}
    >
      <IconButton
        variant="subtle"
        aria-label="Settings"
        icon={<SettingsIcon />}
        onClick={() => setShowTweaks(true)}
      />
    </Box>
  ) : (
    <Box
      position="absolute"
      bg="alt.100"
      w="xs"
      marginTop={2}
      marginLeft={2}
      borderRadius="lg"
      paddingBottom={5}
      zIndex={10}
      boxShadow="xl"
      maxH={'95vh'}
      fontSize="sm"
    >
      <Box
        display="flex"
        justifyContent="space-between"
        alignItems="center"
        paddingRight={2}
        paddingTop={1}
      >
        <Tooltip label={'Switch to ' + threeDim ? '2D' : '3D' + ' view'}>
          <Button onClick={() => setThreeDim(!threeDim)} variant="subtle" zIndex="overlay">
            {threeDim ? '3D' : '2D'}
          </Button>
        </Tooltip>
        <Box display="flex" alignItems="center">
          <Tooltip label="Reset settings to defaults">
            <IconButton
              aria-label="Reset Defaults"
              icon={<RepeatClockIcon />}
              onClick={() => {
                setVisuals(initialVisuals)
                setFilter(initialFilter)
                setMouse(initialMouse)
                setPhysics(initialPhysics)
                setBehavior(initialBehavior)
                setColoring(initialColoring)
                setHighlightColor('purple.500')
                setLocal(initialLocal)
              }}
              variant="subtle"
              size="sm"
            />
          </Tooltip>
          <IconButton
            size="sm"
            icon={<CloseIcon />}
            aria-label="Close Tweak Panel"
            variant="subtle"
            onClick={() => setShowTweaks(false)}
          />
        </Box>
      </Box>
      <Scrollbars
        autoHeight
        autoHeightMax={0.85 * globalThis.innerHeight}
        autoHide
        renderThumbVertical={({ style, ...props }) => (
          <Box
            {...props}
            style={{
              ...style,
              borderRadius: 10,
            }}
            bg={highlightColor}
          />
        )}
      >
        <Accordion allowMultiple allowToggle color="black">
          <AccordionItem>
            <AccordionButton>
              <AccordionIcon marginRight={2} />
              <Heading size="sm">Filter</Heading>
            </AccordionButton>
            <AccordionPanel>
              <FilterPanel
                filter={filter}
                setFilter={setFilter}
                tagColors={tagColors}
                setTagColors={setTagColors}
                highlightColor={highlightColor}
                colorList={colorList}
                tags={tags}
                {...{ local, setLocal }}
              />
            </AccordionPanel>
          </AccordionItem>
          <AccordionItem>
            <AccordionButton display="flex" justifyContent="space-between">
              <Box display="flex">
                <AccordionIcon marginRight={2} />
                <Heading size="sm">Physics</Heading>
              </Box>
            </AccordionButton>
            <AccordionPanel>
              <PhysicsPanel physics={physics} setPhysics={setPhysics} />
            </AccordionPanel>
          </AccordionItem>
          <AccordionItem>
            <AccordionButton>
              <AccordionIcon marginRight={2} />
              <Heading size="sm">Visual</Heading>
            </AccordionButton>
            <AccordionPanel>
              <VisualsPanel
                visuals={visuals}
                setVisuals={setVisuals}
                highlightColor={highlightColor}
                setHighlightColor={setHighlightColor}
                threeDim={threeDim}
                {...{
                  coloring,
                  setColoring,
                }}
              />
            </AccordionPanel>
          </AccordionItem>
          <AccordionItem>
            <AccordionButton>
              <AccordionIcon marginRight={2} />
              <Heading size="sm">Behavior</Heading>
            </AccordionButton>
            <AccordionPanel>
              <BehaviorPanel
                behavior={behavior}
                setBehavior={setBehavior}
                mouse={mouse}
                setMouse={setMouse}
              />
            </AccordionPanel>
          </AccordionItem>
        </Accordion>
      </Scrollbars>
    </Box>
  )
}
